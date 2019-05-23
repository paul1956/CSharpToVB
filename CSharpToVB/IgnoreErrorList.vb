Option Explicit On
Option Infer Off
Option Strict On

Imports CSharpToVBApp.Microsoft.CodeAnalysis.VisualBasic

Public Module IgnoreErrorList
    Public ErrorsToBeIgnored() As String = {
    $"BC{Convert.ToInt32(ERRID.ERR_UndefinedType1)}", ' Type xxx is not defined
    $"BC{Convert.ToInt32(ERRID.ERR_LbNoMatchingIf)}",
    $"BC{Convert.ToInt32(ERRID.ERR_UseOfKeywordFromStructure1)}", ' MyBase is not valid within a structure
    $"BC{Convert.ToInt32(ERRID.ERR_TooManyArgs1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_RequiredConstConversion2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_LValueRequired)}",
    $"BC{Convert.ToInt32(ERRID.ERR_EnumNotExpression1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_StructureNotExpression1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_NamespaceNotExpression1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_UnimplementedMember3)}",
    $"BC{Convert.ToInt32(ERRID.ERR_DuplicateProcDef1)}", ' Multiple Signatures
    $"BC{Convert.ToInt32(ERRID.ERR_NamedParamNotFound2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_OverrideNotNeeded3)}",
    $"BC{Convert.ToInt32(ERRID.ERR_OverloadWithReturnType2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_TypeMismatch2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_InaccessibleSymbol2)}", ''A' is not accessible in this context because it is 'Friend'.
    $"BC{Convert.ToInt32(ERRID.ERR_InaccessibleMember3)}",
    $"BC{Convert.ToInt32(ERRID.ERR_InvalidOverrideDueToReturn2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_NameNotDeclared1)}", ' Error 'x' is not declared
    $"BC{Convert.ToInt32(ERRID.ERR_BinaryOperands3)}",
    $"BC{Convert.ToInt32(ERRID.ERR_OmittedArgument2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_NameNotMember2)}", ' xxx is not a member of yyy
    $"BC{Convert.ToInt32(ERRID.ERR_ObjectReferenceNotSupplied)}",
    $"BC{Convert.ToInt32(ERRID.ERR_NoArgumentCountOverloadCandidates1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_NoViableOverloadCandidates1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_NoCallableOverloadCandidates2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_NoNonNarrowingOverloadCandidates2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_NoMostSpecificOverload2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_BlockLocalShadowing1)}", ' Hide enclosing variable
    $"BC{Convert.ToInt32(ERRID.ERR_ModuleNotAtNamespace)}", ' Module' statements can occur only at file or namespace level.
    $"BC{Convert.ToInt32(ERRID.ERR_UnreferencedAssembly3)}", ' Skip missing references
    $"BC{Convert.ToInt32(ERRID.ERR_NameNotEvent2)}", '  ' is not an event of 'Object'.
    $"BC{Convert.ToInt32(ERRID.ERR_ReferenceComparison3)}", ' Operator '=' is not defined for types
    $"BC{Convert.ToInt32(ERRID.ERR_UndefinedTypeOrNamespace1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_MustBeOverloads2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_FriendAssemblyBadAccessOverride2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_UseOfLocalBeforeDeclaration1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_FunctionResultCannotBeIndexed1)}", 'X' has no parameters and its return type cannot be indexed
    $"BC{Convert.ToInt32(ERRID.ERR_ForEachCollectionDesignPattern1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_StandaloneAttribute)}", ' Attribute specifier is not a complete statement. Use a line continuation to apply the attribute to the following statement.
    $"BC{Convert.ToInt32(ERRID.ERR_TooFewGenericArguments1)}", ' Too few type arguments
    $"BC{Convert.ToInt32(ERRID.ERR_UnboundTypeParam2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_NoTypeArgumentCountOverloadCand1)}", ' Overload resolution failed because no accessible '' accepts this number of type arguments.
    $"BC{Convert.ToInt32(ERRID.ERR_OfExpected)}",
    $"BC{Convert.ToInt32(ERRID.ERR_LambdaParamShadowLocal1)}", ' Lambda parameter ' ' hides a variable in an enclosing block, a previously defined range variable, or an implicitly declared variable in a query expression.
    $"BC{Convert.ToInt32(ERRID.ERR_TypeInferenceFailure2)}",
    $"BC{Convert.ToInt32(ERRID.ERR_TypeInferenceFailureNoBest1)}",
    $"BC{Convert.ToInt32(ERRID.ERR_ExtensionMethodCannotBeLateBound)}"
    }
End Module
