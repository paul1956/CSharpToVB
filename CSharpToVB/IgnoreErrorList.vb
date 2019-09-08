' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports CSharpToVBApp.Microsoft.CodeAnalysis.VisualBasic

Public Module IgnoreErrorList
    Public ReadOnly Property ErrorsToBeIgnored As New List(Of String) From {
    $"BC{Convert.ToInt32(ERRID.ERRUndefinedType1)}", ' Type xxx is not defined
    $"BC{Convert.ToInt32(ERRID.ERRLbNoMatchingIf)}",
    $"BC{Convert.ToInt32(ERRID.ERRUseOfKeywordFromStructure1)}", ' MyBase is not valid within a structure
    $"BC{Convert.ToInt32(ERRID.ERRTooManyArgs1)}",
    $"BC{Convert.ToInt32(ERRID.ERRRequiredConstConversion2)}",
    $"BC{Convert.ToInt32(ERRID.ERRLValueRequired)}",
    $"BC{Convert.ToInt32(ERRID.ERREnumNotExpression1)}",
    $"BC{Convert.ToInt32(ERRID.ERRStructureNotExpression1)}",
    $"BC{Convert.ToInt32(ERRID.ERRNamespaceNotExpression1)}",
    $"BC{Convert.ToInt32(ERRID.ERRUnimplementedMember3)}",
    $"BC{Convert.ToInt32(ERRID.ERRDuplicateProcDef1)}", ' Multiple Signatures
    $"BC{Convert.ToInt32(ERRID.ERRNamedParamNotFound2)}",
    $"BC{Convert.ToInt32(ERRID.ERROverrideNotNeeded3)}",
    $"BC{Convert.ToInt32(ERRID.ERROverloadWithReturnType2)}",
    $"BC{Convert.ToInt32(ERRID.ERRTypeMismatch2)}",
    $"BC{Convert.ToInt32(ERRID.ERRInaccessibleSymbol2)}", ''A' is not accessible in this context because it is 'Friend'.
    $"BC{Convert.ToInt32(ERRID.ERRInaccessibleMember3)}",
    $"BC{Convert.ToInt32(ERRID.ERRInvalidOverrideDueToReturn2)}",
    $"BC{Convert.ToInt32(ERRID.ERRNameNotDeclared1)}", ' Error 'x' is not declared
    $"BC{Convert.ToInt32(ERRID.ERRBinaryOperands3)}",
    $"BC{Convert.ToInt32(ERRID.ERROmittedArgument2)}",
    $"BC{Convert.ToInt32(ERRID.ERRNameNotMember2)}", ' xxx is not a member of yyy
    $"BC{Convert.ToInt32(ERRID.ERRObjectReferenceNotSupplied)}",
    $"BC{Convert.ToInt32(ERRID.ERRNoArgumentCountOverloadCandidates1)}",
    $"BC{Convert.ToInt32(ERRID.ERRNoViableOverloadCandidates1)}",
    $"BC{Convert.ToInt32(ERRID.ERRNoCallableOverloadCandidates2)}",
    $"BC{Convert.ToInt32(ERRID.ERRNoNonNarrowingOverloadCandidates2)}",
    $"BC{Convert.ToInt32(ERRID.ERRNoMostSpecificOverload2)}",
    $"BC{Convert.ToInt32(ERRID.ERRBlockLocalShadowing1)}", ' Hide enclosing variable
    $"BC{Convert.ToInt32(ERRID.ERRModuleNotAtNamespace)}", ' Module' statements can occur only at file or namespace level.
    $"BC{Convert.ToInt32(ERRID.ERRUnreferencedAssembly3)}", ' Skip missing references
    $"BC{Convert.ToInt32(ERRID.ERRNameNotEvent2)}", '  ' is not an event of 'Object'.
    $"BC{Convert.ToInt32(ERRID.ERRReferenceComparison3)}", ' Operator '=' is not defined for types
    $"BC{Convert.ToInt32(ERRID.ERRUndefinedTypeOrNamespace1)}",
    $"BC{Convert.ToInt32(ERRID.ERRMustBeOverloads2)}",
    $"BC{Convert.ToInt32(ERRID.ERRFriendAssemblyBadAccessOverride2)}",
    $"BC{Convert.ToInt32(ERRID.ERRUseOfLocalBeforeDeclaration1)}",
    $"BC{Convert.ToInt32(ERRID.ERRFunctionResultCannotBeIndexed1)}", 'X' has no parameters and its return type cannot be indexed
    $"BC{Convert.ToInt32(ERRID.ERRForEachCollectionDesignPattern1)}",
    $"BC{Convert.ToInt32(ERRID.ERRStandaloneAttribute)}", ' Attribute specifier is not a complete statement. Use a line continuation to apply the attribute to the following statement.
    $"BC{Convert.ToInt32(ERRID.ERRTooFewGenericArguments1)}", ' Too few type arguments
    $"BC{Convert.ToInt32(ERRID.ERRUnboundTypeParam2)}",
    $"BC{Convert.ToInt32(ERRID.ERRNoTypeArgumentCountOverloadCand1)}", ' Overload resolution failed because no accessible '' accepts this number of type arguments.
    $"BC{Convert.ToInt32(ERRID.ERROfExpected)}",
    $"BC{Convert.ToInt32(ERRID.ERRLambdaParamShadowLocal1)}", ' Lambda parameter ' ' hides a variable in an enclosing block, a previously defined range variable, or an implicitly declared variable in a query expression.
    $"BC{Convert.ToInt32(ERRID.ERRTypeInferenceFailure2)}",
    $"BC{Convert.ToInt32(ERRID.ERRTypeInferenceFailureNoBest1)}",
    $"BC{Convert.ToInt32(ERRID.ERRExtensionMethodCannotBeLateBound)}"
    }
End Module
