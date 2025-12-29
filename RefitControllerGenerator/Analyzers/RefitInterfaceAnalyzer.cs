using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System.Collections.Immutable;
using System.Linq;

namespace RefitControllerGenerator.Analyzers
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public sealed class RefitInterfaceAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor Rule =
            new(
                DiagnosticIds.RefitInterface,
                "Refit interface usage detected",
                "Generate controller from Refit interface",
                "Refit",
                DiagnosticSeverity.Info,
                isEnabledByDefault: true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
            => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();

            context.RegisterSyntaxNodeAction(
                AnalyzeTypeUsage,
                SyntaxKind.Parameter,
                SyntaxKind.PropertyDeclaration,
                SyntaxKind.FieldDeclaration,
                SyntaxKind.VariableDeclaration);
        }

        private static void AnalyzeTypeUsage(SyntaxNodeAnalysisContext context)
        {
            TypeSyntax? typeSyntax = context.Node switch
            {
                ParameterSyntax p => p.Type,
                PropertyDeclarationSyntax p => p.Type,
                FieldDeclarationSyntax f => f.Declaration.Type,
                VariableDeclarationSyntax v => v.Type,
                _ => null
            };

            if (typeSyntax == null)
                return;

            var typeSymbol = context.SemanticModel.GetTypeInfo(typeSyntax).Type as INamedTypeSymbol;
            if (typeSymbol == null)
                return;

            // Only interfaces
            if (typeSymbol.TypeKind != TypeKind.Interface)
                return;

            // Must contain Refit attributes
            if (!HasRefitMethods(typeSymbol))
                return;

            context.ReportDiagnostic(
                Diagnostic.Create(
                    Rule,
                    typeSyntax.GetLocation()));
        }

        private static bool HasRefitMethods(INamedTypeSymbol interfaceSymbol)
        {
            foreach (var method in interfaceSymbol.GetMembers().OfType<IMethodSymbol>())
            {
                foreach (var attribute in method.GetAttributes())
                {
                    var attrType = attribute.AttributeClass;
                    if (attrType?.ContainingNamespace?.ToDisplayString() == "Refit")
                        return true;
                }
            }

            return false;
        }
    }
}
