using Microsoft.AspNetCore.Http;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace RefitControllerGenerator.CodeFixes
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(RefitControllerCodeFixProvider))]
    public sealed class RefitControllerCodeFixProvider : CodeFixProvider
    {
        public override ImmutableArray<string> FixableDiagnosticIds
            => ImmutableArray.Create(DiagnosticIds.RefitInterface);

        public override FixAllProvider GetFixAllProvider()
            => WellKnownFixAllProviders.BatchFixer;

        /// <summary>
        /// Регистрирует действие CodeFix для диагностического предупреждения.
        /// Если контроллер уже существует — предлагает добавить только новые методы.
        /// Если контроллера нет — предлагает сгенерировать его целиком.
        /// </summary>
        public override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var diagnostic = context.Diagnostics.First();
            var cancellationToken = context.CancellationToken;

            var root = await context.Document.GetSyntaxRootAsync(cancellationToken);
            if (root == null)
                return;

            var node = root.FindNode(diagnostic.Location.SourceSpan);
            var semanticModel = await context.Document.GetSemanticModelAsync(cancellationToken);
            if (semanticModel == null)
                return;

            var typeSymbol = semanticModel.GetTypeInfo(node).Type as INamedTypeSymbol;
            if (typeSymbol == null)
                return;

            // Check whether a controller file already exists in the project
            var controllerName = GetControllerName(typeSymbol.Name);
            var existingControllerDoc = FindExistingControllerDocument(context.Document.Project, controllerName);

            if (existingControllerDoc != null)
            {
                // Controller exists — offer to add only new methods
                context.RegisterCodeFix(
                    CodeAction.Create(
                        $"Add new methods to existing {controllerName}",
                        ct => AddNewMethodsToControllerAsync(context.Document, existingControllerDoc, typeSymbol, ct),
                        nameof(RefitControllerCodeFixProvider) + ".AddMethods"),
                    diagnostic);
            }
            else
            {
                // Controller does not exist — offer full generation
                context.RegisterCodeFix(
                    CodeAction.Create(
                        "Generate controller from Refit interface",
                        ct => GenerateControllerAsync(context.Document, typeSymbol, ct),
                        nameof(RefitControllerCodeFixProvider)),
                    diagnostic);
            }
        }

        // -------------------------------------------------------------------------
        // Lookup helpers
        // -------------------------------------------------------------------------

        /// <summary>
        /// Ищет уже существующий документ контроллера в проекте по имени файла.
        /// </summary>
        private static Document? FindExistingControllerDocument(Project project, string controllerName)
        {
            var fileName = $"{controllerName}.cs";
            return project.Documents.FirstOrDefault(d =>
                string.Equals(d.Name, fileName, StringComparison.OrdinalIgnoreCase));
        }

        /// <summary>
        /// Возвращает множество имён методов, уже объявленных в контроллере.
        /// </summary>
        private static async Task<HashSet<string>> GetExistingMethodNamesAsync(
            Document controllerDoc, CancellationToken cancellationToken)
        {
            var root = await controllerDoc.GetSyntaxRootAsync(cancellationToken);
            if (root == null)
                return new HashSet<string>(StringComparer.Ordinal);

            var methods = root
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>()
                .Select(m => m.Identifier.ValueText);

            return new HashSet<string>(methods, StringComparer.Ordinal);
        }

        // -------------------------------------------------------------------------
        // Code action: add new methods to an existing controller
        // -------------------------------------------------------------------------

        /// <summary>
        /// Находит в интерфейсе методы, которых ещё нет в контроллере, генерирует их
        /// и вставляет в конец объявления класса контроллера.
        /// </summary>
        private static async Task<Solution> AddNewMethodsToControllerAsync(
            Document sourceDocument,
            Document controllerDocument,
            INamedTypeSymbol interfaceSymbol,
            CancellationToken cancellationToken)
        {
            // 1. Determine which methods are missing
            var existingNames = await GetExistingMethodNamesAsync(controllerDocument, cancellationToken);

            var newMethods = interfaceSymbol
                .GetMembers()
                .OfType<IMethodSymbol>()
                .Where(m => m.MethodKind == MethodKind.Ordinary)
                .Where(m => !existingNames.Contains(m.Name))
                .ToList();

            if (newMethods.Count == 0)
                return controllerDocument.Project.Solution; // nothing to add

            // 2. Parse the existing controller file
            var root = await controllerDocument.GetSyntaxRootAsync(cancellationToken)
                       as CompilationUnitSyntax;
            if (root == null)
                return controllerDocument.Project.Solution;

            // 3. Find the class declaration
            var classDecl = root.DescendantNodes()
                .OfType<ClassDeclarationSyntax>()
                .FirstOrDefault();

            if (classDecl == null)
                return controllerDocument.Project.Solution;

            // 4. Derive the base route the same way as during initial generation
            var baseRoute = TryGetBaseRoute(interfaceSymbol) ?? "api/[controller]";

            // 5. Build the new method declarations
            var generatedMembers = newMethods
                .Select(m => GenerateControllerMethod(m, baseRoute))
                .ToArray<MemberDeclarationSyntax>();

            // 6. Insert generated members at the end of the class
            var updatedClass = classDecl.AddMembers(generatedMembers);

            // 7. Replace the class in the tree and format
            var updatedRoot = root.ReplaceNode(classDecl, updatedClass);

            var workspace = new AdhocWorkspace();
            var formattedRoot = Formatter.Format(updatedRoot, workspace, cancellationToken: cancellationToken)
                                as CompilationUnitSyntax ?? updatedRoot;

            // 8. Update the document and return the new solution
            var updatedDoc = controllerDocument.WithSyntaxRoot(formattedRoot);
            return updatedDoc.Project.Solution;
        }

        // -------------------------------------------------------------------------
        // Code action: generate a brand-new controller file
        // -------------------------------------------------------------------------

        /// <summary>
        /// Создаёт новый файл контроллера, формирует синтаксическое дерево, форматирует его и добавляет документ в проект
        /// </summary>
        private static async Task<Solution> GenerateControllerAsync(
            Document document,
            INamedTypeSymbol interfaceSymbol,
            CancellationToken cancellationToken)
        {
            var controllerName = GetControllerName(interfaceSymbol.Name);
            var controllerNamespace = GetControllersNamespace(document);

            var controllerSyntax = GenerateControllerSyntax(
                controllerName,
                interfaceSymbol.Name,
                interfaceSymbol.ContainingNamespace.ToDisplayString(),
                interfaceSymbol,
                controllerNamespace);

            var workspace = new AdhocWorkspace();
            var formattedNode = (CompilationUnitSyntax)Formatter.Format(controllerSyntax, workspace);

            var newDoc = document.Project.AddDocument(
                $"{controllerName}.cs",
                formattedNode.GetText(),
                folders: new[] { "Controllers" });

            return newDoc.Project.Solution;
        }

        // -------------------------------------------------------------------------
        // Syntax generation — controller file
        // -------------------------------------------------------------------------

        /// <summary>
        /// Генерирует синтаксическое дерево CompilationUnit для контроллера API.
        /// </summary>
        private static CompilationUnitSyntax GenerateControllerSyntax(
            string controllerName,
            string interfaceName,
            string interfaceNamespace,
            INamedTypeSymbol interfaceSymbol,
            string controllerNamespace)
        {
            var interfaceUsings = ExtractUsingsFromInterface(interfaceSymbol);

            var controllerUsings = new List<UsingDirectiveSyntax>
            {
                SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("Microsoft.AspNetCore.Mvc")),
                SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("System.Net")),
                SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("Chulpan.Refit.WebApi.Common.Entities"))
            };

            var authorizeUsing = SyntaxFactory.UsingDirective(
                    SyntaxFactory.ParseName("Microsoft.AspNetCore.Authorization.AuthorizeAttribute"))
                .WithAlias(
                    SyntaxFactory.NameEquals(
                        SyntaxFactory.IdentifierName("AuthorizeAttribute")));

            var allUsings = new List<UsingDirectiveSyntax>();
            allUsings.AddRange(controllerUsings);
            allUsings.Add(authorizeUsing);
            allUsings.AddRange(interfaceUsings);

            return SyntaxFactory.CompilationUnit()
                .AddUsings(allUsings.ToArray())
                .AddMembers(
                    SyntaxFactory.NamespaceDeclaration(
                        SyntaxFactory.ParseName(controllerNamespace))
                    .AddMembers(
                        GenerateControllerClass(controllerName, interfaceName, interfaceSymbol)));
        }

        // -------------------------------------------------------------------------
        // Using extraction
        // -------------------------------------------------------------------------

        private static List<UsingDirectiveSyntax> ExtractUsingsFromInterface(INamedTypeSymbol interfaceSymbol)
        {
            var usings = new HashSet<string>(StringComparer.Ordinal);
            CollectTypesFromSymbol(interfaceSymbol, usings);

            return usings
                .Where(ns => !string.IsNullOrWhiteSpace(ns))
                .Where(ns => !ns.StartsWith("System.") &&
                             !ns.StartsWith("Microsoft.") &&
                             !ns.StartsWith("Chulpan.Refit.WebApi.Common.Entities") &&
                             !ns.StartsWith("Refit") &&
                             !ns.StartsWith("WebAPI.Services.Logger"))
                .Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.ParseName(ns)))
                .ToList();
        }

        private static void CollectTypesFromSymbol(INamedTypeSymbol symbol, HashSet<string> usings)
        {
            if (symbol == null) return;

            foreach (var member in symbol.GetMembers().OfType<IMethodSymbol>())
            {
                AddTypeNamespace(member.ReturnType, usings);
                foreach (var parameter in member.Parameters)
                    AddTypeNamespace(parameter.Type, usings);
                foreach (var typeParam in member.TypeParameters)
                    foreach (var constraint in typeParam.ConstraintTypes)
                        AddTypeNamespace(constraint, usings);
            }

            foreach (var property in symbol.GetMembers().OfType<IPropertySymbol>())
                AddTypeNamespace(property.Type, usings);

            foreach (var baseInterface in symbol.AllInterfaces)
                CollectTypesFromSymbol(baseInterface, usings);
        }

        private static void AddTypeNamespace(ITypeSymbol typeSymbol, HashSet<string> usings)
        {
            if (typeSymbol == null) return;

            if (typeSymbol is IArrayTypeSymbol arrayType)
            {
                AddTypeNamespace(arrayType.ElementType, usings);
                return;
            }

            if (typeSymbol is INamedTypeSymbol namedType)
            {
                var ns = namedType.ContainingNamespace?.ToDisplayString();
                if (!string.IsNullOrWhiteSpace(ns) &&
                    !ns.StartsWith("System") &&
                    !ns.StartsWith("Microsoft"))
                {
                    usings.Add(ns);
                }

                foreach (var typeArg in namedType.TypeArguments)
                    AddTypeNamespace(typeArg, usings);
                return;
            }

            var namespaceStr = typeSymbol.ContainingNamespace?.ToDisplayString();
            if (!string.IsNullOrWhiteSpace(namespaceStr) &&
                !namespaceStr.StartsWith("System") &&
                !namespaceStr.StartsWith("Microsoft"))
            {
                usings.Add(namespaceStr);
            }
        }

        // -------------------------------------------------------------------------
        // Route helpers
        // -------------------------------------------------------------------------

        private static string? TryGetBaseRoute(INamedTypeSymbol interfaceSymbol)
        {
            var routes = interfaceSymbol
                .GetMembers()
                .OfType<IMethodSymbol>()
                .SelectMany(m => m.GetAttributes())
                .Where(a =>
                    a.AttributeClass?.Name is "GetAttribute" or "PostAttribute" or "PutAttribute" or "DeleteAttribute")
                .Select(a => a.ConstructorArguments.FirstOrDefault().Value as string)
                .Where(r => !string.IsNullOrWhiteSpace(r))
                .Select(r => r!.Trim('/'))
                .ToList();

            return FindLongestCommonPrefix(routes);
        }

        private static string? FindLongestCommonPrefix(List<string> routes)
        {
            if (routes == null || routes.Count == 0) return null;

            var segmentsList = routes.Select(r => r.Split('/')).ToList();
            var firstSegments = segmentsList[0];
            var commonSegments = new List<string>();

            for (int i = 0; i < firstSegments.Length; i++)
            {
                var segment = firstSegments[i];
                if (segment.StartsWith("{")) break;
                bool allMatch = segmentsList.All(s => i < s.Length && s[i] == segment);
                if (!allMatch) break;
                commonSegments.Add(segment);
            }

            return commonSegments.Count > 0 ? string.Join("/", commonSegments) : null;
        }

        // -------------------------------------------------------------------------
        // Class declaration
        // -------------------------------------------------------------------------

        private static SyntaxTriviaList ElasticBlankLine()
            => SyntaxFactory.TriviaList(SyntaxFactory.ElasticCarriageReturnLineFeed);

        private static ClassDeclarationSyntax GenerateControllerClass(
            string controllerName,
            string interfaceName,
            INamedTypeSymbol interfaceSymbol)
        {
            var serviceFieldName = GetServiceFieldName(interfaceName);

            var constructor = GenerateConstructor(controllerName, interfaceName);
            var constructorDocs = GenerateConstructorDocs(new[] { serviceFieldName, "logger" });
            if (constructorDocs.Count > 0)
                constructor = constructor.WithLeadingTrivia(ElasticBlankLine().AddRange(constructorDocs));

            var serviceField = GenerateServiceField(interfaceName);
            var loggerField = GenerateLoggerField();
            var baseRoute = TryGetBaseRoute(interfaceSymbol) ?? "api/[controller]";

            var routeAttr = SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("Route"))
                .WithArgumentList(
                    SyntaxFactory.AttributeArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.AttributeArgument(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.StringLiteralExpression,
                                    SyntaxFactory.Literal(baseRoute))))));

            var classDecl = SyntaxFactory.ClassDeclaration(controllerName)
                .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword))
                .AddBaseListTypes(
                    SyntaxFactory.SimpleBaseType(SyntaxFactory.ParseTypeName("IdentityController")))
                .AddAttributeLists(
                    SyntaxFactory.AttributeList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("ApiController")))),
                    SyntaxFactory.AttributeList(
                        SyntaxFactory.SingletonSeparatedList(routeAttr)),
                    SyntaxFactory.AttributeList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("Authorize")))))
                .AddMembers(serviceField, loggerField)
                .AddMembers(constructor)
                .AddMembers(GenerateControllerMethods(interfaceSymbol, baseRoute));

            var docsTrivia = GenerateControllerDocs(interfaceSymbol);
            if (docsTrivia.Count > 0)
                classDecl = classDecl.WithLeadingTrivia(docsTrivia);

            return classDecl;
        }

        // -------------------------------------------------------------------------
        // Method generation
        // -------------------------------------------------------------------------

        private static MemberDeclarationSyntax[] GenerateControllerMethods(
            INamedTypeSymbol interfaceSymbol, string baseRoute)
        {
            return interfaceSymbol
                .GetMembers()
                .OfType<IMethodSymbol>()
                .Where(m => m.MethodKind == MethodKind.Ordinary)
                .Select(m => GenerateControllerMethod(m, baseRoute))
                .ToArray();
        }

        private static MethodDeclarationSyntax GenerateControllerMethod(IMethodSymbol method, string baseRoute)
        {
            var httpAttr = GetRefitHttpAttribute(method);
            var httpMethod = httpAttr.httpMethod;
            var route = httpAttr.route;

            var returnType = GetActionResultReturnType(method.ReturnType);

            var methodDecl = SyntaxFactory.MethodDeclaration(returnType, method.Name)
                .AddModifiers(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.AsyncKeyword))
                .AddParameterListParameters(
                    method.Parameters.Select(GenerateParameter).ToArray())
                .AddAttributeLists(GenerateHttpAttribute(httpMethod, route, baseRoute))
                .AddAttributeLists(GenerateProducesAttributes())
                .WithBody(GenerateMethodBody(httpMethod, method));

            var docsTrivia = GenerateMethodDocs(method);
            if (docsTrivia.Count > 0)
                methodDecl = methodDecl.WithLeadingTrivia(docsTrivia);

            return methodDecl;
        }

        // -------------------------------------------------------------------------
        // XML documentation
        // -------------------------------------------------------------------------

        private static SyntaxTriviaList GenerateControllerDocs(INamedTypeSymbol interfaceSymbol)
        {
            var xml = interfaceSymbol.GetDocumentationCommentXml();
            if (string.IsNullOrWhiteSpace(xml))
                return default;

            xml = xml.Replace("<doc>", "")
                     .Replace("</doc>", "")
                     .Replace("Интерфейс", "Контроллер")
                     .Replace("\r\n", "\n");

            var lines = xml
                .Split('\n')
                .Select(l => l.Trim())
                .Where(l => !string.IsNullOrEmpty(l))
                .Select(l => "/// " + l);

            return SyntaxFactory.ParseLeadingTrivia(string.Join("\r\n", lines) + "\r\n");
        }

        private static SyntaxTriviaList GenerateConstructorDocs(IEnumerable<string> parameterNames)
        {
            var indent = "    ";
            var nl = Environment.NewLine;

            var commentLines = new List<string>
            {
                $"{indent}/// <summary>",
                $"{indent}/// DI-конструктор",
                $"{indent}/// </summary>"
            };

            foreach (var name in parameterNames)
                commentLines.Add($"{indent}/// <param name=\"{name}\"></param>");

            commentLines.Add(indent);

            return SyntaxFactory.ParseLeadingTrivia(string.Join(nl, commentLines));
        }

        private static SyntaxTriviaList GenerateMethodDocs(ISymbol symbol)
        {
            var xml = symbol.GetDocumentationCommentXml();
            if (string.IsNullOrWhiteSpace(xml))
                return default;

            xml = xml.Replace("<doc>", "")
                     .Replace("</doc>", "")
                     .Replace("\r\n", "\n");

            var lines = xml
                .Split('\n')
                .Select(l => l.Trim())
                .Where(l => !string.IsNullOrEmpty(l))
                .Select(l => "/// " + l);

            return SyntaxFactory.ParseLeadingTrivia(string.Join("\r\n", lines) + "\r\n");
        }

        // -------------------------------------------------------------------------
        // Attribute helpers
        // -------------------------------------------------------------------------

        private static AttributeSyntax CreateProduces(int statusCode, string? type = null)
        {
            var statusName = GetStatusCodeName(statusCode);

            var args = new List<AttributeArgumentSyntax>
            {
                SyntaxFactory.AttributeArgument(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("StatusCodes"),
                        SyntaxFactory.IdentifierName(statusName)))
            };

            if (type != null)
            {
                args.Add(
                    SyntaxFactory.AttributeArgument(
                            SyntaxFactory.TypeOfExpression(SyntaxFactory.ParseTypeName(type)))
                        .WithNameEquals(SyntaxFactory.NameEquals("Type")));
            }

            return SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("ProducesResponseType"))
                .WithArgumentList(SyntaxFactory.AttributeArgumentList(SyntaxFactory.SeparatedList(args)));
        }

        private static AttributeListSyntax[] GenerateProducesAttributes()
        {
            return new[]
            {
                SyntaxFactory.AttributeList(SyntaxFactory.SingletonSeparatedList(CreateProduces(StatusCodes.Status200OK))),
                SyntaxFactory.AttributeList(SyntaxFactory.SingletonSeparatedList(CreateProduces(StatusCodes.Status401Unauthorized))),
                SyntaxFactory.AttributeList(SyntaxFactory.SingletonSeparatedList(CreateProduces(StatusCodes.Status400BadRequest, "ApiResult"))),
                SyntaxFactory.AttributeList(SyntaxFactory.SingletonSeparatedList(CreateProduces(StatusCodes.Status500InternalServerError, "ApiResult")))
            };
        }

        private static string GetStatusCodeName(int code) => code switch
        {
            StatusCodes.Status200OK => "Status200OK",
            StatusCodes.Status401Unauthorized => "Status401Unauthorized",
            StatusCodes.Status400BadRequest => "Status400BadRequest",
            StatusCodes.Status500InternalServerError => "Status500InternalServerError",
            _ => $"Status{code}"
        };

        private static (string httpMethod, string? route) GetRefitHttpAttribute(IMethodSymbol method)
        {
            foreach (var attr in method.GetAttributes())
            {
                var name = attr.AttributeClass?.Name;
                if (name is "GetAttribute" or "PostAttribute" or "DeleteAttribute" or "PutAttribute")
                {
                    var httpMethod = name.Replace("Attribute", "").ToUpperInvariant();
                    var route = attr.ConstructorArguments.FirstOrDefault().Value as string;
                    return (httpMethod, route);
                }
            }

            throw new InvalidOperationException($"Method {method.Name} has no Refit HTTP attribute");
        }

        private static AttributeListSyntax GenerateHttpAttribute(string method, string? fullRoute, string? baseRoute)
        {
            var attrName = "Http" + method.Substring(0, 1) + method.Substring(1).ToLowerInvariant();
            var attr = SyntaxFactory.Attribute(SyntaxFactory.IdentifierName(attrName));

            if (!string.IsNullOrWhiteSpace(fullRoute) && !string.IsNullOrWhiteSpace(baseRoute))
            {
                var normalized = fullRoute.Trim('/');
                var remainder = normalized.StartsWith(baseRoute)
                    ? normalized.Substring(baseRoute.Length).Trim('/')
                    : normalized;

                if (!string.IsNullOrWhiteSpace(remainder))
                {
                    attr = attr.AddArgumentListArguments(
                        SyntaxFactory.AttributeArgument(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.StringLiteralExpression,
                                SyntaxFactory.Literal(remainder))));
                }
            }

            return SyntaxFactory.AttributeList(SyntaxFactory.SingletonSeparatedList(attr));
        }

        // -------------------------------------------------------------------------
        // Parameter & return-type helpers
        // -------------------------------------------------------------------------

        private static ParameterSyntax GenerateParameter(IParameterSymbol parameter)
        {
            return SyntaxFactory.Parameter(SyntaxFactory.Identifier(parameter.Name))
                .WithType(SyntaxFactory.ParseTypeName(
                    parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)));
        }

        private static TypeSyntax GetActionResultReturnType(ITypeSymbol returnType)
        {
            if (returnType is INamedTypeSymbol named &&
                named.Name == "Task" &&
                named.TypeArguments.Length == 1)
            {
                var typeName = named.TypeArguments[0]
                    .ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                return SyntaxFactory.ParseTypeName($"Task<ActionResult<{typeName}>>");
            }

            return SyntaxFactory.ParseTypeName("Task<IActionResult>");
        }

        // -------------------------------------------------------------------------
        // Method body
        // -------------------------------------------------------------------------

        private enum ResultKind { None, SingleObject, Collection }

        private static (ResultKind Kind, string? TypeName) AnalyzeResultType(IMethodSymbol method)
        {
            ITypeSymbol? type = method.ReturnType;

            if (type is INamedTypeSymbol taskType && taskType.Name == "Task" && taskType.TypeArguments.Length == 1)
                type = taskType.TypeArguments[0];

            if (type is INamedTypeSymbol actionResultType &&
                actionResultType.Name == "ActionResult" &&
                actionResultType.TypeArguments.Length == 1)
            {
                type = actionResultType.TypeArguments[0];
            }
            else if (type is INamedTypeSymbol plainTask &&
                     plainTask.Name == "Task" &&
                     plainTask.TypeArguments.Length == 0)
            {
                return (ResultKind.None, null);
            }

            if (type == null) return (ResultKind.None, null);

            if (type is INamedTypeSymbol namedType &&
                namedType.AllInterfaces.Any(i => i.Name == "IEnumerable" && i.TypeArguments.Length == 1))
                return (ResultKind.Collection, null);

            if (IsPrimitiveOrSimpleType(type)) return (ResultKind.None, null);

            return (ResultKind.SingleObject, type.Name);
        }

        private static BlockSyntax GenerateMethodBody(string? httpMethod, IMethodSymbol method)
        {
            var serviceFieldName = GetServiceFieldName(method.ContainingType.Name);
            var callArgs = string.Join(", ", method.Parameters.Select(p => p.Name));

            var isVoidTask = method.ReturnType is INamedTypeSymbol rt &&
                             rt.Name == "Task" &&
                             rt.TypeArguments.Length == 0;

            if (isVoidTask)
            {
                var voidTryBody = SyntaxFactory.Block(
                    SyntaxFactory.ParseStatement($"logger.Debug(\"Вызов метода {method.Name}\");")
                        .WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed),
                    SyntaxFactory.ParseStatement($"await {serviceFieldName}.{method.Name}({callArgs});"),
                    SyntaxFactory.ParseStatement("return Ok();"));

                return SyntaxFactory.Block(
                    SyntaxFactory.TryStatement()
                        .WithBlock(voidTryBody)
                        .WithCatches(SyntaxFactory.List(new[]
                        {
                            GenerateCatch("UnauthorizedAccessException", "return Unauthorized(e);"),
                            GenerateCatch("ArgumentException", "return BadRequest(new ApiResult((int)HttpStatusCode.BadRequest, e.Message, e.Message));"),
                            GenerateCatch("Exception", "return StatusCode((int)HttpStatusCode.InternalServerError, new ApiResult((int)HttpStatusCode.InternalServerError, e.Message, e.Message));")
                        })));
            }

            var serviceCall = SyntaxFactory.ParseStatement(
                $"var result = await {serviceFieldName}.{method.Name}({callArgs});");

            var resultInfo = AnalyzeResultType(method);

            StatementSyntax returnStatement = SyntaxFactory.IfStatement(
                SyntaxFactory.ParseExpression("result != null"),
                SyntaxFactory.Block(
                    SyntaxFactory.ParseStatement(
                        resultInfo.Kind switch
                        {
                            ResultKind.SingleObject => $"logger.Debug(\"Успешно. {{@{resultInfo.TypeName}}}\", result);",
                            ResultKind.Collection => "logger.Debug(\"Успешно. {Count} объектов\", result?.Count);",
                            _ => "logger.Debug(\"Успешно\");"
                        }).WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed),
                    SyntaxFactory.ParseStatement("return Ok(result);")),
                SyntaxFactory.ElseClause(
                    SyntaxFactory.Block(
                        SyntaxFactory.ParseStatement("logger.Error(\"Объект не найден\");")
                            .WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed),
                        SyntaxFactory.ParseStatement(
                            httpMethod != null && httpMethod.Equals("GET", StringComparison.OrdinalIgnoreCase)
                                ? "return NotFound();"
                                : "return BadRequest();"))));

            var tryBody = SyntaxFactory.Block(
                SyntaxFactory.ParseStatement($"logger.Debug(\"Вызов метода {method.Name}\");")
                    .WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed),
                serviceCall,
                returnStatement);

            return SyntaxFactory.Block(
                SyntaxFactory.TryStatement()
                    .WithBlock(tryBody)
                    .WithCatches(SyntaxFactory.List(new[]
                    {
                        GenerateCatch("UnauthorizedAccessException", "return Unauthorized(e);"),
                        GenerateCatch("ArgumentException", "return BadRequest(new ApiResult((int)HttpStatusCode.BadRequest, e.Message, e.Message));"),
                        GenerateCatch("Exception", "return StatusCode((int)HttpStatusCode.InternalServerError, new ApiResult((int)HttpStatusCode.InternalServerError, e.Message, e.Message));")
                    })));
        }

        private static bool IsPrimitiveOrSimpleType(ITypeSymbol type)
        {
            switch (type.SpecialType)
            {
                case SpecialType.System_Object:
                case SpecialType.System_Void:
                case SpecialType.System_Boolean:
                case SpecialType.System_Char:
                case SpecialType.System_SByte:
                case SpecialType.System_Byte:
                case SpecialType.System_Int16:
                case SpecialType.System_UInt16:
                case SpecialType.System_Int32:
                case SpecialType.System_UInt32:
                case SpecialType.System_Int64:
                case SpecialType.System_UInt64:
                case SpecialType.System_Decimal:
                case SpecialType.System_Single:
                case SpecialType.System_Double:
                case SpecialType.System_String:
                case SpecialType.System_DateTime:
                case SpecialType.System_IntPtr:
                case SpecialType.System_UIntPtr:
                    return true;
            }

            if (type.ToDisplayString() == "System.Guid") return true;
            if (type.TypeKind == TypeKind.Enum) return true;

            if (type is INamedTypeSymbol namedType &&
                namedType.IsGenericType &&
                namedType.ConstructedFrom.SpecialType == SpecialType.System_Nullable_T)
                return IsPrimitiveOrSimpleType(namedType.TypeArguments[0]);

            return false;
        }

        // -------------------------------------------------------------------------
        // Field & constructor generation
        // -------------------------------------------------------------------------

        private static CatchClauseSyntax GenerateCatch(string exceptionType, string returnStatement)
        {
            return SyntaxFactory.CatchClause()
                .WithDeclaration(
                    SyntaxFactory.CatchDeclaration(
                        SyntaxFactory.ParseTypeName(exceptionType),
                        SyntaxFactory.Identifier("e")))
                .WithBlock(
                    SyntaxFactory.Block(
                        SyntaxFactory.ParseStatement($"logger.Error($\"{{e.Message}} {{e}}\");")
                            .WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed),
                        SyntaxFactory.ParseStatement(returnStatement)));
        }

        private static ConstructorDeclarationSyntax GenerateConstructor(string controllerName, string interfaceName)
        {
            var fieldName = GetServiceFieldName(interfaceName);

            return SyntaxFactory.ConstructorDeclaration(controllerName)
                .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword))
                .AddParameterListParameters(
                    SyntaxFactory.Parameter(SyntaxFactory.Identifier(fieldName))
                        .WithType(SyntaxFactory.ParseTypeName(interfaceName)),
                    SyntaxFactory.Parameter(SyntaxFactory.Identifier("logger"))
                        .WithType(SyntaxFactory.ParseTypeName("ITraceableLogger")))
                .WithBody(
                    SyntaxFactory.Block(
                        SyntaxFactory.ExpressionStatement(
                            SyntaxFactory.AssignmentExpression(
                                SyntaxKind.SimpleAssignmentExpression,
                                SyntaxFactory.MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    SyntaxFactory.ThisExpression(),
                                    SyntaxFactory.IdentifierName(fieldName)),
                                SyntaxFactory.IdentifierName(fieldName))),
                        SyntaxFactory.ExpressionStatement(
                            SyntaxFactory.AssignmentExpression(
                                SyntaxKind.SimpleAssignmentExpression,
                                SyntaxFactory.MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    SyntaxFactory.ThisExpression(),
                                    SyntaxFactory.IdentifierName("logger")),
                                SyntaxFactory.IdentifierName("logger")))));
        }

        private static FieldDeclarationSyntax GenerateLoggerField()
        {
            return SyntaxFactory.FieldDeclaration(
                    SyntaxFactory.VariableDeclaration(SyntaxFactory.ParseTypeName("ITraceableLogger"))
                    .AddVariables(SyntaxFactory.VariableDeclarator("logger")))
                .AddModifiers(
                    SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                    SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword))
                .WithTrailingTrivia(
                    SyntaxFactory.TriviaList(
                        SyntaxFactory.CarriageReturnLineFeed,
                        SyntaxFactory.CarriageReturnLineFeed));
        }

        private static FieldDeclarationSyntax GenerateServiceField(string interfaceName)
        {
            var fieldName = GetServiceFieldName(interfaceName);

            return SyntaxFactory.FieldDeclaration(
                    SyntaxFactory.VariableDeclaration(SyntaxFactory.ParseTypeName(interfaceName))
                    .AddVariables(SyntaxFactory.VariableDeclarator(fieldName)))
                .AddModifiers(
                    SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                    SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword));
        }

        // -------------------------------------------------------------------------
        // Name helpers
        // -------------------------------------------------------------------------

        private static string GetServiceFieldName(string interfaceName)
        {
            var name = interfaceName.StartsWith("I") ? interfaceName.Substring(1) : interfaceName;
            if (name.EndsWith("Api", StringComparison.OrdinalIgnoreCase))
                name = name.Substring(0, name.Length - 3);
            return char.ToLowerInvariant(name[0]) + name.Substring(1) + "Service";
        }

        private static string GetControllerName(string interfaceName)
        {
            var name = interfaceName.StartsWith("I") ? interfaceName.Substring(1) : interfaceName;
            if (name.EndsWith("Api", StringComparison.OrdinalIgnoreCase))
                name = name.Substring(0, name.Length - 3);
            return name + "Controller";
        }

        private static string GetControllersNamespace(Document document)
            => $"{document.Project.Name}.Controllers";
    }
}