using Microsoft.AspNetCore.Http;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
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
        /// Регистрирует действие CodeFix для диагностического предупреждения и инициализирует генерацию контроллера на основании Refit-интерфейса
        /// </summary>
        /// <param name="context"></param>
        /// <returns></returns>
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

            context.RegisterCodeFix(
                CodeAction.Create(
                    "Generate controller from Refit interface",
                    ct => GenerateControllerAsync(context.Document, typeSymbol, ct),
                    nameof(RefitControllerCodeFixProvider)),
                diagnostic);
        }

        /// <summary>
        /// Создаёт новый файл контроллера, формирует синтаксическое дерево, форматирует его и добавляет документ в проект
        /// </summary>
        /// <param name="document"></param>
        /// <param name="interfaceSymbol"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
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

            var formatted = Microsoft.CodeAnalysis.Formatting.Formatter.Format(
                controllerSyntax,
                document.Project.Solution.Workspace,
                cancellationToken: cancellationToken);

            var newDoc = document.Project.AddDocument(
                $"{controllerName}.cs",
                formatted.GetText(),
                folders: new[] { "Controllers" });

            return newDoc.Project.Solution;
        }

        /// <summary>
        /// Формирует CompilationUnitSyntax для файла контроллера: директивы using, пространство имён и объявление класса
        /// </summary>
        /// <param name="controllerName"></param>
        /// <param name="interfaceName"></param>
        /// <param name="interfaceNamespace"></param>
        /// <param name="interfaceSymbol"></param>
        /// <param name="controllerNamespace"></param>
        /// <returns></returns>
        private static CompilationUnitSyntax GenerateControllerSyntax(
            string controllerName,
            string interfaceName,
            string interfaceNamespace,
            INamedTypeSymbol interfaceSymbol,
            string controllerNamespace)
        {
            var authorizeUsing =
                SyntaxFactory.UsingDirective(
                    SyntaxFactory.ParseName("Microsoft.AspNetCore.Authorization.AuthorizeAttribute"))
                .WithAlias(
                    SyntaxFactory.NameEquals(
                        SyntaxFactory.IdentifierName("AuthorizeAttribute")));

            return SyntaxFactory.CompilationUnit()
                .AddUsings(
                    SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("Microsoft.AspNetCore.Mvc")),
                    SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("Microsoft.AspNetCore.Authorization")),
                    SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("System.Net")),
                    SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("Chulpan.Refit.WebApi.Common.Entities")),
                    SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("Refit")),
                    SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("WebAPI.Services.Logger")),
                    authorizeUsing,
                    SyntaxFactory.UsingDirective(SyntaxFactory.ParseName(interfaceNamespace)))
                .AddMembers(
                    SyntaxFactory.NamespaceDeclaration(
                        SyntaxFactory.ParseName(controllerNamespace))
                    .AddMembers(
                        GenerateControllerClass(controllerName, interfaceName, interfaceSymbol)))
                .NormalizeWhitespace();
        }

        /// <summary>
        /// Извлекает базовый маршрут контроллера на основании маршрута первого метода интерфейса Refit
        /// </summary>
        /// <param name="interfaceSymbol"></param>
        /// <returns></returns>
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

            if (!routes.Any())
                return null;

            var split = routes
                .Select(r => r.Split('/'))
                .ToList();

            var commonSegments = split
                .First()
                .TakeWhile((segment, index) =>
                    split.All(s => s.Length > index && s[index] == segment))
                .ToArray();

            return commonSegments.Length == 0
                ? null
                : string.Join("/", commonSegments);
        }

        /// <summary>
        /// Создаёт декларацию класса контроллера, добавляет атрибуты, DI-поля, конструктор, XML-комментарии и сгенерированные методы
        /// </summary>
        /// <param name="controllerName"></param>
        /// <param name="interfaceName"></param>
        /// <param name="interfaceSymbol"></param>
        /// <returns></returns>
        private static ClassDeclarationSyntax GenerateControllerClass(string controllerName, string interfaceName, INamedTypeSymbol interfaceSymbol)
        {
            var serviceFieldName = GetServiceFieldName(interfaceName);

            var constructor = GenerateConstructor(controllerName, interfaceName);
            var constructorDocs = GenerateConstructorDocs(new[] { serviceFieldName, "logger" });
            if (constructorDocs != null)
                constructor = constructor.WithLeadingTrivia(
                    SyntaxFactory.TriviaList(
                        SyntaxFactory.Trivia(constructorDocs),
                        SyntaxFactory.ElasticCarriageReturnLineFeed));

            var baseRoute = TryGetBaseRoute(interfaceSymbol) ?? "api/[controller]";

            var routeAttr = SyntaxFactory.Attribute(
                SyntaxFactory.IdentifierName("Route"))
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
                    SyntaxFactory.SimpleBaseType(
                        SyntaxFactory.ParseTypeName("IdentityController")))
                .AddAttributeLists(
                    SyntaxFactory.AttributeList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Attribute(
                                SyntaxFactory.IdentifierName("ApiController")))),
                    SyntaxFactory.AttributeList(
                        SyntaxFactory.SingletonSeparatedList(routeAttr)),
                    SyntaxFactory.AttributeList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Attribute(
                                    SyntaxFactory.IdentifierName("Authorize")))))
                .AddMembers(
                    GenerateServiceField(interfaceName),
                    GenerateLoggerField(),
                    constructor
                )

                .AddMembers(GenerateControllerMethods(interfaceSymbol, baseRoute));

            var docsTrivia = GenerateControllerDocs(interfaceSymbol);
            if (docsTrivia.Count > 0)
                classDecl = classDecl.WithLeadingTrivia(
                    docsTrivia);

            return classDecl;
        }

        /// <summary>
        /// Проходит по методам интерфейса и генерирует для каждого соответствующий метод контроллера
        /// </summary>
        /// <param name="interfaceSymbol"></param>
        /// <returns></returns>
        private static MemberDeclarationSyntax[] GenerateControllerMethods(INamedTypeSymbol interfaceSymbol, string baseRoute)
        {
            return interfaceSymbol
                .GetMembers()
                .OfType<IMethodSymbol>()
                .Where(m => m.MethodKind == MethodKind.Ordinary)
                .Select(m => GenerateControllerMethod(m, baseRoute))
                .ToArray();
        }

        /// <summary>
        /// Создаёт метод контроллера: атрибут HTTP, Produces, Authorize, тело метода и XML-комментарий
        /// </summary>
        /// <param name="method"></param>
        /// <returns></returns>
        private static MethodDeclarationSyntax GenerateControllerMethod(IMethodSymbol method, string baseRoute)
        {
            var httpAttr = GetRefitHttpAttribute(method);
            var httpMethod = httpAttr.httpMethod;
            var route = httpAttr.route;

            var returnType = GetActionResultReturnType(method.ReturnType);

            var methodDecl =
                SyntaxFactory.MethodDeclaration(
                        returnType,
                        method.Name)
                    .AddModifiers(
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.AsyncKeyword))
                    .AddParameterListParameters(
                        method.Parameters.Select(GenerateParameter).ToArray())
                    .AddAttributeLists(
                        GenerateHttpAttribute(httpMethod, route, baseRoute))
                    .AddAttributeLists(
                        GenerateProducesAttributes())
                    .AddAttributeLists(
                        SyntaxFactory.AttributeList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Attribute(
                                    SyntaxFactory.IdentifierName("Authorize"))))
                    )
                    .WithBody(GenerateMethodBody(method));

            var docsTrivia = GenerateMethodDocs(method);
            if (docsTrivia.Count > 0)
            {
                methodDecl = methodDecl.WithLeadingTrivia(docsTrivia);
            }

            return methodDecl;
        }

        /// <summary>
        /// Преобразует XML-комментарий интерфейса в комментарий контроллера, заменяя термин «Интерфейс» на «Контроллер»
        /// </summary>
        /// <param name="interfaceSymbol"></param>
        /// <returns></returns>
        private static SyntaxTriviaList GenerateControllerDocs(INamedTypeSymbol interfaceSymbol)
        {
            var xml = interfaceSymbol.GetDocumentationCommentXml();
            if (string.IsNullOrWhiteSpace(xml))
                return default;

            //try
            //{
            //    xml = xml.Replace("<doc>", "")
            //             .Replace("</doc>", "")
            //             .Replace("Интерфейс", "Контроллер")
            //             .Trim();

            //    var lines = xml.Split('\n')
            //                   .Select(l => l.Trim())
            //                   .Where(l => !string.IsNullOrWhiteSpace(l))
            //                   .ToArray();

            //    if (lines.Length == 0)
            //        return null;

            //    var content = string.Join(Environment.NewLine, lines);

            //    // Формируем полный XML комментарий с /// в начале каждой строки
            //    var xmlLines = content.Split(new[] { Environment.NewLine }, StringSplitOptions.None);
            //    var fullXml = string.Join(Environment.NewLine, xmlLines.Select(l => "/// " + l));

            //    // Парсим как синтаксическую тривию
            //    var trivia = SyntaxFactory.ParseLeadingTrivia(fullXml);

            //    // Ищем DocumentationCommentTriviaSyntax в тривиях
            //    var docCommentTrivia = trivia
            //        .Select(t => t.GetStructure())
            //        .OfType<DocumentationCommentTriviaSyntax>()
            //        .FirstOrDefault();

            //    return docCommentTrivia;
            //}
            //catch
            //{
            //    return null;
            //}

            // Remove <doc> wrapper
            xml = xml.Replace("<doc>", "")
                     .Replace("</doc>", "")
                     .Replace("Интерфейс", "Контроллер");

            // Normalize line endings first
            xml = xml.Replace("\r\n", "\n");

            var lines = xml
                .Split('\n')
                .Select(l => l.Trim())
                .Where(l => !string.IsNullOrEmpty(l))
                .Select(l => "/// " + l);

            var text = string.Join("\r\n", lines);

            return SyntaxFactory.ParseLeadingTrivia(text + "\r\n");
        }

        /// <summary>
        /// Формирует XML-комментарий для конструктора на основе списка параметров
        /// </summary>
        /// <param name="parameterNames"></param>
        /// <returns></returns>
        private static DocumentationCommentTriviaSyntax GenerateConstructorDocs(IEnumerable<string> parameterNames)
        {
            var nl = Environment.NewLine;
            var xml = "/// <summary>" + nl +
                      "/// DI-конструктор" + nl +
                      "/// </summary>" + nl;

            foreach (var name in parameterNames)
            {
                xml += $"/// <param name=\"{name}\"></param>" + nl;
            }

            var trivia = SyntaxFactory.ParseLeadingTrivia(xml).FirstOrDefault();
            return trivia.GetStructure() as DocumentationCommentTriviaSyntax;
        }

        /// <summary>
        /// Очищает и форматирует XML-документацию метода интерфейса для вставки в контроллер
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private static SyntaxTriviaList GenerateMethodDocs(ISymbol symbol)
        {
            var xml = symbol.GetDocumentationCommentXml();
            if (string.IsNullOrWhiteSpace(xml))
                return default;

            // Remove <doc> wrapper
            xml = xml.Replace("<doc>", "")
                     .Replace("</doc>", "");

            // Normalize line endings first
            xml = xml.Replace("\r\n", "\n");

            var lines = xml
                .Split('\n')
                .Select(l => l.Trim()) 
                .Where(l => !string.IsNullOrEmpty(l))
                .Select(l => "/// " + l);

            var text = string.Join("\r\n", lines);

            return SyntaxFactory.ParseLeadingTrivia(text + "\r\n");
        }


        /// <summary>
        /// Формирует один атрибут ProducesResponseType с кодом статуса и при необходимости с типом результата
        /// </summary>
        /// <param name="statusCode"></param>
        /// <param name="type"></param>
        /// <returns></returns>
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
                            SyntaxFactory.TypeOfExpression(
                                SyntaxFactory.ParseTypeName(type)))
                        .WithNameEquals(
                            SyntaxFactory.NameEquals("Type")));
            }

            return SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("ProducesResponseType"))
                .WithArgumentList(
                    SyntaxFactory.AttributeArgumentList(
                        SyntaxFactory.SeparatedList(args)));
        }

        /// <summary>
        /// Генерирует набор атрибутов ProducesResponseType для стандартных HTTP-кодов
        /// </summary>
        /// <returns></returns>
        private static AttributeListSyntax[] GenerateProducesAttributes()
        {
            return new[]
            {
                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        CreateProduces(StatusCodes.Status200OK))),

                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        CreateProduces(StatusCodes.Status401Unauthorized))),

                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        CreateProduces(StatusCodes.Status400BadRequest, "ApiResult"))),

                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        CreateProduces(StatusCodes.Status500InternalServerError, "ApiResult")))
            };
        }

        /// <summary>
        /// Возвращает символьное имя HTTP-кода для использования в генерируемом синтаксисе.
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        private static string GetStatusCodeName(int code) => code switch
        {
            StatusCodes.Status200OK => "Status200OK",
            StatusCodes.Status401Unauthorized => "Status401Unauthorized",
            StatusCodes.Status400BadRequest => "Status400BadRequest",
            StatusCodes.Status500InternalServerError => "Status500InternalServerError",
            _ => $"Status{code}"
        };

        /// <summary>
        /// Читает Refit-атрибут метода (Get, Post, и т.д.) и извлекает HTTP-метод и маршрут
        /// </summary>
        /// <param name="method"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
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

        /// <summary>
        /// Создаёт атрибут Http… контроллера с маршрутом или без него (если маршрут пустой)
        /// </summary>
        /// <param name="method"></param>
        /// <param name="route"></param>
        /// <returns></returns>
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

            return SyntaxFactory.AttributeList(
                SyntaxFactory.SingletonSeparatedList(attr));
        }

        /// <summary>
        /// Генерирует параметр метода контроллера на основе параметра интерфейса
        /// </summary>
        /// <param name="parameter"></param>
        /// <returns></returns>
        private static ParameterSyntax GenerateParameter(IParameterSymbol parameter)
        {
            return SyntaxFactory.Parameter(
                    SyntaxFactory.Identifier(parameter.Name))
                .WithType(
                    SyntaxFactory.ParseTypeName(
                        parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)));
        }

        /// <summary>
        /// Преобразует возвращаемый тип Task<T> в Task<ActionResult<T>> либо Task<IActionResult>
        /// </summary>
        /// <param name="returnType"></param>
        /// <returns></returns>
        private static TypeSyntax GetActionResultReturnType(ITypeSymbol returnType)
        {
            // Task<T>
            if (returnType is INamedTypeSymbol named &&
                named.Name == "Task" &&
                named.TypeArguments.Length == 1)
            {
                var typeName =
                    named.TypeArguments[0].ToDisplayString(
                        SymbolDisplayFormat.MinimallyQualifiedFormat);

                return SyntaxFactory.ParseTypeName(
                    $"Task<ActionResult<{typeName}>>");
            }

            return SyntaxFactory.ParseTypeName("Task<IActionResult>");
        }

        /// <summary>
        /// Формирует тело метода контроллера: логирование, вызов сервиса, try/catch, возврат результата
        /// </summary>
        /// <param name="method"></param>
        /// <returns></returns>
        private static BlockSyntax GenerateMethodBody(IMethodSymbol method)
        {
            var serviceFieldName = GetServiceFieldName(method.ContainingType.Name);

            var callArgs = string.Join(", ", method.Parameters.Select(p => p.Name));

            var serviceCall = SyntaxFactory.ParseStatement(
                $"var result = await {serviceFieldName}.{method.Name}({callArgs});");

            var tryBody = SyntaxFactory.Block(
                SyntaxFactory.ParseStatement(
                    $"logger.Debug(\"Вызов метода {method.Name}\");"),
                serviceCall,
                SyntaxFactory.ParseStatement("return Ok(result);")
            );

            return SyntaxFactory.Block(
                SyntaxFactory.TryStatement()
                    .WithBlock(tryBody)
                    .WithCatches(
                        SyntaxFactory.List(new[]
                        {
                            GenerateCatch(
                                "UnauthorizedAccessException",
                                "return Unauthorized(e);"),
                            GenerateCatch(
                                "ArgumentException",
                                "return BadRequestApiResult(e.Message);"),
                            GenerateCatch(
                                "Exception",
                                "return InternalServerErrorApiResult(new ApiResult((int)HttpStatusCode.InternalServerError, e.Message, e.Message));")
                        }))
            );
        }

        /// <summary>
        /// Создаёт блок catch с логированием и соответствующим выражением возврата результата.
        /// </summary>
        /// <param name="exceptionType"></param>
        /// <param name="returnStatement"></param>
        /// <returns></returns>
        private static CatchClauseSyntax GenerateCatch(string exceptionType, string returnStatement)
        {
            return SyntaxFactory.CatchClause()
                .WithDeclaration(
                    SyntaxFactory.CatchDeclaration(
                        SyntaxFactory.ParseTypeName(exceptionType),
                        SyntaxFactory.Identifier("e")))
                .WithBlock(
                    SyntaxFactory.Block(
                        SyntaxFactory.ParseStatement(
                            $"logger.Error($\"{{e.Message}} {{e}}\");"),
                        SyntaxFactory.ParseStatement(returnStatement)));
        }

        /// <summary>
        /// Генерирует DI-конструктор, присваивающий сервис и логгер в поля
        /// </summary>
        /// <param name="controllerName"></param>
        /// <param name="interfaceName"></param>
        /// <returns></returns>
        private static ConstructorDeclarationSyntax GenerateConstructor(string controllerName, string interfaceName)
        {
            var fieldName = GetServiceFieldName(interfaceName);

            return SyntaxFactory.ConstructorDeclaration(controllerName)
                .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword))
                .AddParameterListParameters(
                    SyntaxFactory.Parameter(
                            SyntaxFactory.Identifier(fieldName))
                        .WithType(SyntaxFactory.ParseTypeName(interfaceName)),
                    SyntaxFactory.Parameter(
                            SyntaxFactory.Identifier("logger"))
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
                                SyntaxFactory.IdentifierName("logger")))
                    ));
        }

        /// <summary>
        /// Создаёт приватное readonly-поле логгера
        /// </summary>
        /// <returns></returns>
        private static FieldDeclarationSyntax GenerateLoggerField()
        {
            return SyntaxFactory.FieldDeclaration(
                    SyntaxFactory.VariableDeclaration(
                        SyntaxFactory.ParseTypeName("ITraceableLogger"))
                    .AddVariables(
                        SyntaxFactory.VariableDeclarator("logger")))
                .AddModifiers(
                    SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                    SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword));
        }

        /// <summary>
        /// Создаёт приватное readonly-поле сервиса.
        /// </summary>
        /// <param name="interfaceName"></param>
        /// <returns></returns>
        private static FieldDeclarationSyntax GenerateServiceField(string interfaceName)
        {
            var fieldName = GetServiceFieldName(interfaceName);

            return SyntaxFactory.FieldDeclaration(
                    SyntaxFactory.VariableDeclaration(
                        SyntaxFactory.ParseTypeName(interfaceName))
                    .AddVariables(
                        SyntaxFactory.VariableDeclarator(fieldName)))
                .AddModifiers(
                    SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                    SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword));
        }

        /// <summary>
        /// Формирует имя DI-поля сервиса по имени интерфейса (ITasks → tasksService)
        /// </summary>
        /// <param name="interfaceName"></param>
        /// <returns></returns>
        private static string GetServiceFieldName(string interfaceName)
        {
            var name = interfaceName.StartsWith("I")
                    ? interfaceName.Substring(1)
                    : interfaceName;

            if (name.EndsWith("Api", StringComparison.Ordinal))
                name = name.Substring(0, name.Length - 3);

            return char.ToLowerInvariant(name[0]) + name.Substring(1) + "Service";
        }

        /// <summary>
        /// Строит имя контроллера на основе интерфейса (ITasks → TasksController)
        /// </summary>
        /// <param name="interfaceName"></param>
        /// <returns></returns>
        private static string GetControllerName(string interfaceName)
        {
            var name = interfaceName.StartsWith("I")
                ? interfaceName.Substring(1)
                : interfaceName;

            if (name.EndsWith("Api", StringComparison.Ordinal))
                name = name.Substring(0, name.Length - 3);

            return name + "Controller";
        }

        /// <summary>
        /// Определяет пространство имён контроллеров по имени проекта
        /// </summary>
        /// <param name="document"></param>
        /// <returns></returns>
        private static string GetControllersNamespace(Document document)
            => $"{document.Project.Name}.Controllers";

    }
}
