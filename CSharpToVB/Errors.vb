' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

'''-------------------------------------------------------------------------------------------------
'''
''' Error code and strings for Compiler errors
'''
''' ERRIDs should be defined in the following ranges:
'''
''' 500   -   999    - non localized ERRID (main DLL)
''' 30000 - 59999    - localized ERRID     (international DLL)
'''
''' The convention for naming ERRID's that take replacement strings is to give
''' them a number following the name (from 1-9) that indicates how many
''' arguments they expect.
'''
''' DO NOT USE ANY NUMBERS EXCEPT THOSE EXPLICITLY LISTED AS BEING AVAILABLE.
''' IF YOU REUSE A NUMBER, LOCALIZATION WILL BE SCREWED UP!
'''
'''-------------------------------------------------------------------------------------------------

'''-------------------------------------------------------------------------------------------------
'''
'''
''' Manages the parse and compile errors.
'''
'''-------------------------------------------------------------------------------------------------

Namespace Microsoft.CodeAnalysis.VisualBasic

    Public Enum ERRID
        Void = InternalErrorCode.Void
        Unknown = InternalErrorCode.Unknown
        ERRNone = 0

        ' ERRInitError = 2000 unused in Roslyn
        ERRFileNotFound = 2001

        ' WRNFileAlreadyIncluded = 2002  'unused in Roslyn.
        'ERRDuplicateResponseFile = 2003   unused in Roslyn.
        'ERRNoMemory = 2004
        ERRArgumentRequired = 2006

        WRNBadSwitch = 2007
        ERRNoSources = 2008
        ERRSwitchNeedsBool = 2009

        'ERRCompileFailed = 2010       unused in Roslyn.
        ERRNoResponseFile = 2011

        ERRCantOpenFileWrite = 2012
        ERRInvalidSwitchValue = 2014
        ERRBinaryFile = 2015
        ERRBadCodepage = 2016
        ERRLibNotFound = 2017

        'ERRMaximumErrors = 2020       unused in Roslyn.
        ERRIconFileAndWin32ResFile = 2023

        'WRNReservedReference = 2024       ' unused by native compiler due to bug.
        WRNNoConfigInResponseFile = 2025

        ' WRNInvalidWarningId = 2026       ' unused in Roslyn.
        'ERRWatsonSendNotOptedIn = 2027
        ' WRNSwitchNoBool = 2028     'unused in Roslyn
        ERRNoSourcesOut = 2029

        ERRNeedModule = 2030
        ERRInvalidAssemblyName = 2031
        FTLInvalidInputFileName = 2032 ' new in Roslyn
        ERRConflictingManifestSwitches = 2033
        WRNIgnoreModuleManifest = 2034

        'ERRNoDefaultManifest = 2035
        'ERRInvalidSwitchValue1 = 2036
        WRNBadUILang = 2038 ' new in Roslyn

        ERRVBCoreNetModuleConflict = 2042
        ERRInvalidFormatForGuidForOption = 2043
        ERRMissingGuidForOption = 2044
        ERRBadChecksumAlgorithm = 2045
        ERRMutuallyExclusiveOptions = 2046

        ''' The naming convention is that if your error requires arguments, to append
        ''' the number of args taken, e.g. AmbiguousName2
        '''
        ERRInvalidInNamespace = 30001

        ERRUndefinedType1 = 30002
        ERRMissingNext = 30003
        ERRIllegalCharConstant = 30004

        ''' If you make any change involving these errors, such as creating more specific versions for use
        ''' in other contexts, please make sure to appropriately modify Bindable::ResolveOverloadingShouldSkipBadMember
        ERRUnreferencedAssemblyEvent3 = 30005

        ERRUnreferencedModuleEvent3 = 30006
        ' ERRUnreferencedAssemblyBase3 = 30007
        ' ERRUnreferencedModuleBase3 = 30008           - This has been superseded by ERRUnreferencedModuleEvent3
        ' ERRUnreferencedAssemblyImplements3 = 30009
        'ERRUnreferencedModuleImplements3 = 30010      - This has been superseded by ERRUnreferencedModuleEvent3

        'ERRCodegenError = 30011
        ERRLbExpectedEndIf = 30012

        ERRLbNoMatchingIf = 30013
        ERRLbBadElseif = 30014
        ERRInheritsFromRestrictedType1 = 30015
        ERRInvOutsideProc = 30016
        ERRDelegateCantImplement = 30018
        ERRDelegateCantHandleEvents = 30019
        ERRIsOperatorRequiresReferenceTypes1 = 30020
        ERRTypeOfRequiresReferenceType1 = 30021
        ERRReadOnlyHasSet = 30022
        ERRWriteOnlyHasGet = 30023
        ERRInvInsideProc = 30024
        ERREndProp = 30025
        ERREndSubExpected = 30026
        ERREndFunctionExpected = 30027
        ERRLbElseNoMatchingIf = 30028
        ERRCantRaiseBaseEvent = 30029
        ERRTryWithoutCatchOrFinally = 30030

        ' ERRFullyQualifiedNameTooLong1 = 30031 ' Deprecated in favor of ERRTooLongMetadataName
        ERREventsCantBeFunctions = 30032

        ' ERRIdTooLong = 30033 ' Deprecated in favor of ERRTooLongMetadataName
        ERRMissingEndBrack = 30034

        ERRSyntax = 30035
        ERROverflow = 30036
        ERRIllegalChar = 30037
        ERRStrictDisallowsObjectOperand1 = 30038
        ERRLoopControlMustNotBeProperty = 30039
        ERRMethodBodyNotAtLineStart = 30040
        ERRMaximumNumberOfErrors = 30041
        ERRUseOfKeywordNotInInstanceMethod1 = 30043
        ERRUseOfKeywordFromStructure1 = 30044
        ERRBadAttributeConstructor1 = 30045
        ERRParamArrayWithOptArgs = 30046
        ERRExpectedArray1 = 30049
        ERRParamArrayNotArray = 30050
        ERRParamArrayRank = 30051
        ERRArrayRankLimit = 30052
        ERRAsNewArray = 30053
        ERRTooManyArgs1 = 30057
        ERRExpectedCase = 30058
        ERRRequiredConstExpr = 30059
        ERRRequiredConstConversion2 = 30060
        ERRInvalidMe = 30062
        ERRReadOnlyAssignment = 30064
        ERRExitSubOfFunc = 30065
        ERRExitPropNot = 30066
        ERRExitFuncOfSub = 30067
        ERRLValueRequired = 30068
        ERRForIndexInUse1 = 30069
        ERRNextForMismatch1 = 30070
        ERRCaseElseNoSelect = 30071
        ERRCaseNoSelect = 30072
        ERRCantAssignToConst = 30074
        ERRNamedSubscript = 30075
        ERRExpectedEndIf = 30081
        ERRExpectedEndWhile = 30082
        ERRExpectedLoop = 30083
        ERRExpectedNext = 30084
        ERRExpectedEndWith = 30085
        ERRElseNoMatchingIf = 30086
        ERREndIfNoMatchingIf = 30087
        ERREndSelectNoSelect = 30088
        ERRExitDoNotWithinDo = 30089
        ERREndWhileNoWhile = 30090
        ERRLoopNoMatchingDo = 30091
        ERRNextNoMatchingFor = 30092
        ERREndWithWithoutWith = 30093
        ERRMultiplyDefined1 = 30094
        ERRExpectedEndSelect = 30095
        ERRExitForNotWithinFor = 30096
        ERRExitWhileNotWithinWhile = 30097
        ERRReadOnlyProperty1 = 30098
        ERRExitSelectNotWithinSelect = 30099
        ERRBranchOutOfFinally = 30101
        ERRQualNotObjectRecord1 = 30103
        ERRTooFewIndices = 30105
        ERRTooManyIndices = 30106
        ERREnumNotExpression1 = 30107
        ERRTypeNotExpression1 = 30108
        ERRClassNotExpression1 = 30109
        ERRStructureNotExpression1 = 30110
        ERRInterfaceNotExpression1 = 30111
        ERRNamespaceNotExpression1 = 30112
        ERRBadNamespaceName1 = 30113
        ERRXmlPrefixNotExpression = 30114
        ERRMultipleExtends = 30121

        'ERRNoStopInDebugger = 30122
        'ERRNoEndInDebugger = 30123
        ERRPropMustHaveGetSet = 30124

        ERRWriteOnlyHasNoWrite = 30125
        ERRReadOnlyHasNoGet = 30126
        ERRBadAttribute1 = 30127

        ' ERRBadSecurityAttribute1 = 30128 ' we're now reporting more detailed diagnostics: ERRSecurityAttributeMissingAction or ERRSecurityAttributeInvalidAction
        'ERRBadAssemblyAttribute1 = 30129
        'ERRBadModuleAttribute1 = 30130
        ' ERRModuleSecurityAttributeNotAllowed1 = 30131    ' We now report ERRSecurityAttributeInvalidTarget instead.
        ERRLabelNotDefined1 = 30132

        'ERRNoGotosInDebugger = 30133
        'ERRNoLabelsInDebugger = 30134
        'ERRNoSyncLocksInDebugger = 30135
        ERRErrorCreatingWin32ResourceFile = 30136

        'ERRErrorSavingWin32ResourceFile = 30137   abandoned. no longer "saving" a temporary resource file.
        ERRUnableToCreateTempFile = 30138  'changed from ERRUnableToCreateTempFileInPath1. now takes only one argument

        'ERRErrorSettingManifestOption = 30139
        'ERRErrorCreatingManifest = 30140
        'ERRUnableToCreateALinkAPI = 30141
        'ERRUnableToGenerateRefToMetaDataFile1 = 30142
        'ERRUnableToEmbedResourceFile1 = 30143 ' We now report ERRUnableToOpenResourceFile1 instead.
        'ERRUnableToLinkResourceFile1 = 30144 ' We now report ERRUnableToOpenResourceFile1 instead.
        'ERRUnableToEmitAssembly = 30145
        'ERRUnableToSignAssembly = 30146
        'ERRNoReturnsInDebugger = 30147
        ERRRequiredNewCall2 = 30148

        ERRUnimplementedMember3 = 30149

        ' ERRUnimplementedProperty3 = 30154
        ERRBadWithRef = 30157

        ' ERRExpectedNewableClass1 = 30166 unused in Roslyn. We now report nothing
        ' ERRTypeConflict7 = 30175         unused in Roslyn. We now report BC30179
        ERRDuplicateAccessCategoryUsed = 30176

        ERRDuplicateModifierCategoryUsed = 30177
        ERRDuplicateSpecifier = 30178
        ERRTypeConflict6 = 30179
        ERRUnrecognizedTypeKeyword = 30180
        ERRExtraSpecifiers = 30181
        ERRUnrecognizedType = 30182
        ERRInvalidUseOfKeyword = 30183
        ERRInvalidEndEnum = 30184
        ERRMissingEndEnum = 30185

        'ERRNoUsingInDebugger = 30186
        ERRExpectedDeclaration = 30188

        ERRParamArrayMustBeLast = 30192
        ERRSpecifiersInvalidOnInheritsImplOpt = 30193
        ERRExpectedSpecifier = 30195
        ERRExpectedComma = 30196
        ERRExpectedAs = 30197
        ERRExpectedRparen = 30198
        ERRExpectedLparen = 30199
        ERRInvalidNewInType = 30200
        ERRExpectedExpression = 30201
        ERRExpectedOptional = 30202
        ERRExpectedIdentifier = 30203
        ERRExpectedIntLiteral = 30204
        ERRExpectedEOS = 30205
        ERRExpectedForOptionStmt = 30206
        ERRInvalidOptionCompare = 30207
        ERRExpectedOptionCompare = 30208
        ERRStrictDisallowImplicitObject = 30209
        ERRStrictDisallowsImplicitProc = 30210
        ERRStrictDisallowsImplicitArgs = 30211
        ERRInvalidParameterSyntax = 30213
        ERRExpectedSubFunction = 30215
        ERRExpectedStringLiteral = 30217
        ERRMissingLibInDeclare = 30218
        ERRDelegateNoInvoke1 = 30220
        ERRMissingIsInTypeOf = 30224
        ERRDuplicateOption1 = 30225
        ERRModuleCantInherit = 30230
        ERRModuleCantImplement = 30231
        ERRBadImplementsType = 30232
        ERRBadConstFlags1 = 30233
        ERRBadWithEventsFlags1 = 30234
        ERRBadDimFlags1 = 30235
        ERRDuplicateParamName1 = 30237
        ERRLoopDoubleCondition = 30238
        ERRExpectedRelational = 30239
        ERRExpectedExitKind = 30240
        ERRExpectedNamedArgument = 30241
        ERRBadMethodFlags1 = 30242
        ERRBadEventFlags1 = 30243
        ERRBadDeclareFlags1 = 30244
        ERRBadLocalConstFlags1 = 30246
        ERRBadLocalDimFlags1 = 30247
        ERRExpectedConditionalDirective = 30248
        ERRExpectedEQ = 30249
        ERRConstructorNotFound1 = 30251
        ERRInvalidEndInterface = 30252
        ERRMissingEndInterface = 30253
        ERRInheritsFrom2 = 30256
        ERRInheritanceCycle1 = 30257
        ERRInheritsFromNonClass = 30258
        ERRMultiplyDefinedType3 = 30260
        ERRBadOverrideAccess2 = 30266
        ERRCantOverrideNotOverridable2 = 30267
        ERRDuplicateProcDef1 = 30269
        ERRBadInterfaceMethodFlags1 = 30270
        ERRNamedParamNotFound2 = 30272
        ERRBadInterfacePropertyFlags1 = 30273
        ERRNamedArgUsedTwice2 = 30274
        ERRInterfaceCantUseEventSpecifier1 = 30275
        ERRTypecharNoMatch2 = 30277
        ERRExpectedSubOrFunction = 30278
        ERRBadEmptyEnum1 = 30280
        ERRInvalidConstructorCall = 30282
        ERRCantOverrideConstructor = 30283
        ERROverrideNotNeeded3 = 30284
        ERRExpectedDot = 30287
        ERRDuplicateLocals1 = 30288
        ERRInvInsideEndsProc = 30289
        ERRLocalSameAsFunc = 30290
        ERRRecordEmbeds2 = 30293
        ERRRecordCycle2 = 30294
        ERRInterfaceCycle1 = 30296
        ERRSubNewCycle2 = 30297
        ERRSubNewCycle1 = 30298
        ERRInheritsFromCantInherit3 = 30299
        ERROverloadWithOptional2 = 30300
        ERROverloadWithReturnType2 = 30301
        ERRTypeCharWithType1 = 30302
        ERRTypeCharOnSub = 30303
        ERROverloadWithDefault2 = 30305
        ERRMissingSubscript = 30306
        ERROverrideWithDefault2 = 30307
        ERROverrideWithOptional2 = 30308
        ERRFieldOfValueFieldOfMarshalByRef3 = 30310
        ERRTypeMismatch2 = 30311
        ERRCaseAfterCaseElse = 30321
        ERRConvertArrayMismatch4 = 30332
        ERRConvertObjectArrayMismatch3 = 30333
        ERRForLoopType1 = 30337
        ERROverloadWithByref2 = 30345
        ERRInheritsFromNonInterface = 30354
        ERRBadInterfaceOrderOnInherits = 30357
        ERRDuplicateDefaultProps1 = 30359
        ERRDefaultMissingFromProperty2 = 30361
        ERROverridingPropertyKind2 = 30362
        ERRNewInInterface = 30363
        ERRBadFlagsOnNew1 = 30364
        ERROverloadingPropertyKind2 = 30366
        ERRNoDefaultNotExtend1 = 30367
        ERROverloadWithArrayVsParamArray2 = 30368
        ERRBadInstanceMemberAccess = 30369
        ERRExpectedRbrace = 30370
        ERRModuleAsType1 = 30371
        ERRNewIfNullOnNonClass = 30375

        'ERRNewIfNullOnAbstractClass1 = 30376
        ERRCatchAfterFinally = 30379

        ERRCatchNoMatchingTry = 30380
        ERRFinallyAfterFinally = 30381
        ERRFinallyNoMatchingTry = 30382
        ERREndTryNoTry = 30383
        ERRExpectedEndTry = 30384
        ERRBadDelegateFlags1 = 30385
        ERRNoConstructorOnBase2 = 30387
        ERRInaccessibleSymbol2 = 30389
        ERRInaccessibleMember3 = 30390
        ERRCatchNotException1 = 30392
        ERRExitTryNotWithinTry = 30393
        ERRBadRecordFlags1 = 30395
        ERRBadEnumFlags1 = 30396
        ERRBadInterfaceFlags1 = 30397
        ERROverrideWithByref2 = 30398
        ERRMyBaseAbstractCall1 = 30399
        ERRIdentNotMemberOfInterface4 = 30401
        ERRImplementingInterfaceWithDifferentTupleNames5 = 30402

        ''' We intentionally use argument '3' for the delegate name. This makes generating overload resolution errors
        ''' easy. To make it more clear that were doing this, we name the message DelegateBindingMismatch3_2.
        ''' This differentiates its from DelegateBindingMismatch3_3, which actually takes 3 parameters instead of 2.
        ''' This is a workaround, but it makes the logic for reporting overload resolution errors easier error report more straight forward.
        'ERRDelegateBindingMismatch3_2 = 30408
        ERRWithEventsRequiresClass = 30412

        ERRWithEventsAsStruct = 30413
        ERRConvertArrayRankMismatch2 = 30414
        ERRRedimRankMismatch = 30415
        ERRStartupCodeNotFound1 = 30420
        ERRConstAsNonConstant = 30424
        ERRInvalidEndSub = 30429
        ERRInvalidEndFunction = 30430
        ERRInvalidEndProperty = 30431
        ERRModuleCantUseMethodSpecifier1 = 30433
        ERRModuleCantUseEventSpecifier1 = 30434
        ERRStructCantUseVarSpecifier1 = 30435

        'ERRModuleCantUseMemberSpecifier1 = 30436 Now reporting BC30735
        ERRInvalidOverrideDueToReturn2 = 30437

        ERRConstantWithNoValue = 30438
        ERRExpressionOverflow1 = 30439

        'ERRExpectedEndTryCatch = 30441 - No Longer Reported. Removed per bug 926779
        'ERRExpectedEndTryFinally = 30442 - No Longer Reported. Removed per bug 926779
        ERRDuplicatePropertyGet = 30443

        ERRDuplicatePropertySet = 30444

        ' ERRConstAggregate = 30445        Now giving BC30424
        ERRNameNotDeclared1 = 30451

        ERRBinaryOperands3 = 30452
        ERRExpectedProcedure = 30454
        ERROmittedArgument2 = 30455
        ERRNameNotMember2 = 30456

        'ERRNoTypeNamesAvailable = 30458
        ERREndClassNoClass = 30460

        ERRBadClassFlags1 = 30461
        ERRImportsMustBeFirst = 30465
        ERRNonNamespaceOrClassOnImport2 = 30467
        ERRTypecharNotallowed = 30468
        ERRObjectReferenceNotSupplied = 30469
        ERRMyClassNotInClass = 30470
        ERRIndexedNotArrayOrProc = 30471
        ERREventSourceIsArray = 30476
        ERRSharedConstructorWithParams = 30479
        ERRSharedConstructorIllegalSpec1 = 30480
        ERRExpectedEndClass = 30481
        ERRUnaryOperand2 = 30487
        ERRBadFlagsWithDefault1 = 30490
        ERRVoidValue = 30491
        ERRConstructorFunction = 30493

        'ERRLineTooLong = 30494 - No longer reported. Removed per 926916
        ERRInvalidLiteralExponent = 30495

        ERRNewCannotHandleEvents = 30497
        ERRCircularEvaluation1 = 30500
        ERRBadFlagsOnSharedMeth1 = 30501
        ERRBadFlagsOnSharedProperty1 = 30502
        ERRBadFlagsOnStdModuleProperty1 = 30503
        ERRSharedOnProcThatImpl = 30505
        ERRNoWithEventsVarOnHandlesList = 30506
        ERRAccessMismatch6 = 30508
        ERRInheritanceAccessMismatch5 = 30509
        ERRNarrowingConversionDisallowed2 = 30512
        ERRNoArgumentCountOverloadCandidates1 = 30516
        ERRNoViableOverloadCandidates1 = 30517
        ERRNoCallableOverloadCandidates2 = 30518
        ERRNoNonNarrowingOverloadCandidates2 = 30519
        ERRArgumentNarrowing3 = 30520
        ERRNoMostSpecificOverload2 = 30521
        ERRNotMostSpecificOverload = 30522
        ERROverloadCandidate2 = 30523
        ERRNoGetProperty1 = 30524
        ERRNoSetProperty1 = 30526

        'ERRArrayType2 = 30528
        ERRParamTypingInconsistency = 30529

        ERRParamNameFunctionNameCollision = 30530
        ERRDateToDoubleConversion = 30532
        ERRDoubleToDateConversion = 30533
        ERRZeroDivide = 30542
        ERRTryAndOnErrorDoNotMix = 30544
        ERRPropertyAccessIgnored = 30545
        ERRInterfaceNoDefault1 = 30547
        ERRInvalidAssemblyAttribute1 = 30548
        ERRInvalidModuleAttribute1 = 30549
        ERRAmbiguousInUnnamedNamespace1 = 30554
        ERRDefaultMemberNotProperty1 = 30555
        ERRAmbiguousInNamespace2 = 30560
        ERRAmbiguousInImports2 = 30561
        ERRAmbiguousInModules2 = 30562

        ' ERRAmbiguousInApplicationObject2 = 30563 ' comment out in Dev10
        ERRArrayInitializerTooFewDimensions = 30565

        ERRArrayInitializerTooManyDimensions = 30566
        ERRInitializerTooFewElements1 = 30567
        ERRInitializerTooManyElements1 = 30568
        ERRNewOnAbstractClass = 30569
        ERRDuplicateNamedImportAlias1 = 30572
        ERRDuplicatePrefix = 30573
        ERRStrictDisallowsLateBinding = 30574

        ' ERRPropertyMemberSyntax = 30576  unused in Roslyn
        ERRAddressOfOperandNotMethod = 30577

        ERREndExternalSource = 30578
        ERRExpectedEndExternalSource = 30579
        ERRNestedExternalSource = 30580
        ERRAddressOfNotDelegate1 = 30581
        ERRSyncLockRequiresReferenceType1 = 30582
        ERRMethodAlreadyImplemented2 = 30583
        ERRDuplicateInInherits1 = 30584
        ERRNamedParamArrayArgument = 30587
        ERROmittedParamArrayArgument = 30588
        ERRParamArrayArgumentMismatch = 30589
        ERREventNotFound1 = 30590

        'ERRNoDefaultSource = 30591
        ERRModuleCantUseVariableSpecifier1 = 30593

        ERRSharedEventNeedsSharedHandler = 30594
        ERRExpectedMinus = 30601
        ERRInterfaceMemberSyntax = 30602
        ERRInvInsideInterface = 30603
        ERRInvInsideEndsInterface = 30604
        ERRBadFlagsInNotInheritableClass1 = 30607
        ERRUnimplementedMustOverride = 30609 ' substituted into ERRBaseOnlyClassesMustBeExplicit2
        ERRBaseOnlyClassesMustBeExplicit2 = 30610
        ERRNegativeArraySize = 30611
        ERRMyClassAbstractCall1 = 30614
        ERREndDisallowedInDllProjects = 30615
        ERRBlockLocalShadowing1 = 30616
        ERRModuleNotAtNamespace = 30617
        ERRNamespaceNotAtNamespace = 30618
        ERRInvInsideEndsEnum = 30619
        ERRInvalidOptionStrict = 30620
        ERREndStructureNoStructure = 30621
        ERREndModuleNoModule = 30622
        ERREndNamespaceNoNamespace = 30623
        ERRExpectedEndStructure = 30624
        ERRExpectedEndModule = 30625
        ERRExpectedEndNamespace = 30626
        ERROptionStmtWrongOrder = 30627
        ERRStructCantInherit = 30628
        ERRNewInStruct = 30629
        ERRInvalidEndGet = 30630
        ERRMissingEndGet = 30631
        ERRInvalidEndSet = 30632
        ERRMissingEndSet = 30633
        ERRInvInsideEndsProperty = 30634
        ERRDuplicateWriteabilityCategoryUsed = 30635
        ERRExpectedGreater = 30636
        ERRAttributeStmtWrongOrder = 30637
        ERRNoExplicitArraySizes = 30638
        ERRBadPropertyFlags1 = 30639
        ERRInvalidOptionExplicit = 30640
        ERRMultipleParameterSpecifiers = 30641
        ERRMultipleOptionalParameterSpecifiers = 30642
        ERRUnsupportedProperty1 = 30643
        ERRInvalidOptionalParameterUsage1 = 30645
        ERRReturnFromNonFunction = 30647
        ERRUnterminatedStringLiteral = 30648
        ERRUnsupportedType1 = 30649
        ERRInvalidEnumBase = 30650
        ERRByRefIllegal1 = 30651

        ''' If you make any change involving these errors, such as creating more specific versions for use
        ''' in other contexts, please make sure to appropriately modify Bindable::ResolveOverloadingShouldSkipBadMember
        ERRUnreferencedAssembly3 = 30652

        ERRUnreferencedModule3 = 30653

        ERRReturnWithoutValue = 30654

        ' ERRCantLoadStdLibrary1 = 30655   Roslyn doesn't use special messages when well-known assemblies cannot be loaded.
        ERRUnsupportedField1 = 30656

        ERRUnsupportedMethod1 = 30657
        ERRNoNonIndexProperty1 = 30658
        ERRBadAttributePropertyType1 = 30659
        ERRLocalsCannotHaveAttributes = 30660
        ERRPropertyOrFieldNotDefined1 = 30661
        ERRInvalidAttributeUsage2 = 30662
        ERRInvalidMultipleAttributeUsage1 = 30663
        ERRCantThrowNonException = 30665
        ERRMustBeInCatchToRethrow = 30666
        ERRParamArrayMustBeByVal = 30667
        ERRUseOfObsoleteSymbol2 = 30668
        ERRRedimNoSizes = 30670
        ERRInitWithMultipleDeclarators = 30671
        ERRInitWithExplicitArraySizes = 30672
        ERREndSyncLockNoSyncLock = 30674
        ERRExpectedEndSyncLock = 30675
        ERRNameNotEvent2 = 30676
        ERRAddOrRemoveHandlerEvent = 30677
        ERRUnrecognizedEnd = 30678

        ERRArrayInitForNonArray2 = 30679

        ERREndRegionNoRegion = 30680
        ERRExpectedEndRegion = 30681
        ERRInheritsStmtWrongOrder = 30683
        ERRAmbiguousAcrossInterfaces3 = 30685
        ERRDefaultPropertyAmbiguousAcrossInterfaces4 = 30686
        ERRInterfaceEventCantUse1 = 30688
        ERRExecutableAsDeclaration = 30689
        ERRStructureNoDefault1 = 30690

        ' ERRTypeMemberAsExpression2 = 30691 Now giving BC30109
        ERRMustShadow2 = 30695

        'ERROverloadWithOptionalTypes2 = 30696
        ERROverrideWithOptionalTypes2 = 30697

        'ERRUnableToGetTempPath = 30698
        'ERRNameNotDeclaredDebug1 = 30699
        ''' This error should never be seen.
        'ERRNoSideEffects = 30700
        'ERRInvalidNothing = 30701
        'ERRIndexOutOfRange1 = 30702
        'ERRRuntimeException2 = 30703
        'ERRRuntimeException = 30704
        'ERRObjectReferenceIsNothing1 = 30705
        ''' This error should never be seen.
        'ERRExpressionNotValidInEE = 30706
        'ERRUnableToEvaluateExpression = 30707
        'ERRUnableToEvaluateLoops = 30708
        'ERRNoDimsInDebugger = 30709
        ERRExpectedEndOfExpression = 30710

        'ERRSetValueNotAllowedOnNonLeafFrame = 30711
        'ERRUnableToClassInformation1 = 30712
        'ERRNoExitInDebugger = 30713
        'ERRNoResumeInDebugger = 30714
        'ERRNoCatchInDebugger = 30715
        'ERRNoFinallyInDebugger = 30716
        'ERRNoTryInDebugger = 30717
        'ERRNoSelectInDebugger = 30718
        'ERRNoCaseInDebugger = 30719
        'ERRNoOnErrorInDebugger = 30720
        'ERREvaluationAborted = 30721
        'ERREvaluationTimeout = 30722
        'ERREvaluationNoReturnValue = 30723
        'ERRNoErrorStatementInDebugger = 30724
        'ERRNoThrowStatementInDebugger = 30725
        'ERRNoWithContextInDebugger = 30726
        ERRStructsCannotHandleEvents = 30728

        ERROverridesImpliesOverridable = 30730

        'ERRNoAddressOfInDebugger = 30731
        'ERREvaluationOfWebMethods = 30732
        ERRLocalNamedSameAsParam1 = 30734

        ERRModuleCantUseTypeSpecifier1 = 30735

        'ERREvaluationBadStartPoint = 30736
        ERRInValidSubMainsFound1 = 30737

        ERRMoreThanOneValidMainWasFound2 = 30738

        'ERRNoRaiseEventOfInDebugger = 30739
        'ERRInvalidCast2 = 30741
        ERRCannotConvertValue2 = 30742

        'ERRArrayElementIsNothing = 30744
        'ERRInternalCompilerError = 30747
        'ERRInvalidCast1 = 30748
        'ERRUnableToGetValue = 30749
        'ERRUnableToLoadType1 = 30750
        'ERRUnableToGetTypeInformationFor1 = 30751
        ERROnErrorInSyncLock = 30752

        ERRNarrowingConversionCollection2 = 30753
        ERRGotoIntoTryHandler = 30754
        ERRGotoIntoSyncLock = 30755
        ERRGotoIntoWith = 30756
        ERRGotoIntoFor = 30757
        ERRBadAttributeNonPublicConstructor = 30758

        'ERRArrayElementIsNothing1 = 30759
        'ERRObjectReferenceIsNothing = 30760
        ' ERRStarliteDisallowsLateBinding = 30762
        ' ERRStarliteBadDeclareFlags = 30763
        ' ERRNoStarliteOverloadResolution = 30764
        'ERRNoSupportFileIOKeywords1 = 30766
        ' ERRNoSupportGetStatement = 30767 - star-lite error message
        ' ERRNoSupportLineKeyword = 30768      cut from Roslyn
        ' ERRStarliteDisallowsEndStatement = 30769 cut from Roslyn
        ERRDefaultEventNotFound1 = 30770

        ERRInvalidNonSerializedUsage = 30772

        'ERRNoContinueInDebugger = 30780
        ERRExpectedContinueKind = 30781

        ERRContinueDoNotWithinDo = 30782
        ERRContinueForNotWithinFor = 30783
        ERRContinueWhileNotWithinWhile = 30784
        ERRDuplicateParameterSpecifier = 30785
        ERRModuleCantUseDLLDeclareSpecifier1 = 30786
        ERRStructCantUseDLLDeclareSpecifier1 = 30791
        ERRTryCastOfValueType1 = 30792
        ERRTryCastOfUnconstrainedTypeParam1 = 30793
        ERRAmbiguousDelegateBinding2 = 30794
        ERRSharedStructMemberCannotSpecifyNew = 30795
        ERRGenericSubMainsFound1 = 30796
        ERRGeneralProjectImportsError3 = 30797
        ERRInvalidTypeForAliasesImport2 = 30798

        ERRUnsupportedConstant2 = 30799

        ERRObsoleteArgumentsNeedParens = 30800
        ERRObsoleteLineNumbersAreLabels = 30801
        ERRObsoleteStructureNotType = 30802

        'ERRObsoleteDecimalNotCurrency = 30803 cut from Roslyn
        ERRObsoleteObjectNotVariant = 30804

        'ERRObsoleteArrayBounds = 30805    unused in Roslyn
        ERRObsoleteLetSetNotNeeded = 30807

        ERRObsoletePropertyGetLetSet = 30808
        ERRObsoleteWhileWend = 30809

        'ERRObsoleteStaticMethod = 30810   cut from Roslyn
        ERRObsoleteRedimAs = 30811

        ERRObsoleteOptionalWithoutValue = 30812
        ERRObsoleteGosub = 30814

        'ERRObsoleteFileIOKeywords1 = 30815    cut from Roslyn
        'ERRObsoleteDebugKeyword1 = 30816      cut from Roslyn
        ERRObsoleteOnGotoGosub = 30817

        'ERRObsoleteMathCompatKeywords1 = 30818    cut from Roslyn
        'ERRObsoleteMathKeywords2 = 30819          cut from Roslyn
        'ERRObsoleteLsetKeyword1 = 30820           cut from Roslyn
        'ERRObsoleteRsetKeyword1 = 30821           cut from Roslyn
        'ERRObsoleteNullKeyword1 = 30822           cut from Roslyn
        'ERRObsoleteEmptyKeyword1 = 30823          cut from Roslyn
        ERRObsoleteEndIf = 30826

        ERRObsoleteExponent = 30827
        ERRObsoleteAsAny = 30828
        ERRObsoleteGetStatement = 30829

        'ERRObsoleteLineKeyword = 30830            cut from Roslyn
        ERROverrideWithArrayVsParamArray2 = 30906

        ''' CONSIDER improve this error message
        ERRCircularBaseDependencies4 = 30907

        ERRNestedBase2 = 30908
        ERRAccessMismatchOutsideAssembly4 = 30909
        ERRInheritanceAccessMismatchOutside3 = 30910
        ERRUseOfObsoletePropertyAccessor3 = 30911
        ERRUseOfObsoletePropertyAccessor2 = 30912
        ERRAccessMismatchImplementedEvent6 = 30914
        ERRAccessMismatchImplementedEvent4 = 30915
        ERRInheritanceCycleInImportedType1 = 30916
        ERRNoNonObsoleteConstructorOnBase3 = 30917
        ERRNoNonObsoleteConstructorOnBase4 = 30918
        ERRRequiredNonObsoleteNewCall3 = 30919
        ERRRequiredNonObsoleteNewCall4 = 30920
        ERRInheritsTypeArgAccessMismatch7 = 30921
        ERRInheritsTypeArgAccessMismatchOutside5 = 30922

        'ERRAccessMismatchTypeArgImplEvent7 = 30923    unused in Roslyn
        'ERRAccessMismatchTypeArgImplEvent5 = 30924    unused in Roslyn
        ERRPartialTypeAccessMismatch3 = 30925

        ERRPartialTypeBadMustInherit1 = 30926
        ERRMustOverOnNotInheritPartClsMem1 = 30927
        ERRBaseMismatchForPartialClass3 = 30928
        ERRPartialTypeTypeParamNameMismatch3 = 30931
        ERRPartialTypeConstraintMismatch1 = 30932
        ERRLateBoundOverloadInterfaceCall1 = 30933
        ERRRequiredAttributeConstConversion2 = 30934
        ERRAmbiguousOverrides3 = 30935
        ERROverriddenCandidate1 = 30936
        ERRAmbiguousImplements3 = 30937
        ERRAddressOfNotCreatableDelegate1 = 30939

        'ERRReturnFromEventMethod = 30940  unused in Roslyn
        'ERRBadEmptyStructWithCustomEvent1 = 30941
        ERRComClassGenericMethod = 30943

        ERRSyntaxInCastOp = 30944

        'ERRUnimplementedBadMemberEvent = 30945    Cut in Roslyn
        'ERREvaluationStopRequested = 30946
        'ERREvaluationSuspendRequested = 30947
        'ERREvaluationUnscheduledFiber = 30948
        ERRArrayInitializerForNonConstDim = 30949

        ERRDelegateBindingFailure3 = 30950

        'ERRDelegateBindingTypeInferenceFails2 = 30952
        'ERRConstraintViolationError1 = 30953
        'ERRConstraintsFailedForInferredArgs2 = 30954
        'ERRTypeMismatchDLLProjectMix6 = 30955
        'ERREvaluationPriorTimeout = 30957
        'ERREvaluationENCObjectOutdated = 30958
        ' Obsolete ERRTypeRefFromMetadataToVBUndef = 30960
        'ERRTypeMismatchMixedDLLs6 = 30961
        'ERRReferencedAssemblyCausesCycle3 = 30962
        'ERRAssemblyRefAssembly2 = 30963
        'ERRAssemblyRefProject2 = 30964
        'ERRProjectRefAssembly2 = 30965
        'ERRProjectRefProject2 = 30966
        'ERRReferencedAssembliesAmbiguous4 = 30967
        'ERRReferencedAssembliesAmbiguous6 = 30968
        'ERRReferencedProjectsAmbiguous4 = 30969
        'ERRGeneralErrorMixedDLLs5 = 30970
        'ERRGeneralErrorDLLProjectMix5 = 30971
        ERRStructLayoutAttributeNotAllowed = 30972

        'ERRClassNotLoadedDuringDebugging = 30973
        'ERRUnableToEvaluateComment = 30974
        'ERRForIndexInUse = 30975
        'ERRNextForMismatch = 30976
        ERRIterationVariableShadowLocal1 = 30978

        ERRInvalidOptionInfer = 30979
        ERRCircularInference1 = 30980
        ERRInAccessibleOverridingMethod5 = 30981
        ERRNoSuitableWidestType1 = 30982
        ERRAmbiguousWidestType3 = 30983
        ERRExpectedAssignmentOperatorInInit = 30984
        ERRExpectedQualifiedNameInInit = 30985
        ERRExpectedLbrace = 30987
        ERRUnrecognizedTypeOrWith = 30988
        ERRDuplicateAggrMemberInit1 = 30989
        ERRNonFieldPropertyAggrMemberInit1 = 30990
        ERRSharedMemberAggrMemberInit1 = 30991
        ERRParameterizedPropertyInAggrInit1 = 30992
        ERRNoZeroCountArgumentInitCandidates1 = 30993
        ERRAggrInitInvalidForObject = 30994

        'ERRBadWithRefInConstExpr = 30995
        ERRInitializerExpected = 30996

        ERRLineContWithCommentOrNoPrecSpace = 30999

        ' ERRMemberNotFoundForNoPia = 31000    not used in Roslyn. This looks to be a VB EE message
        ERRInvInsideEnum = 31001

        ERRInvInsideBlock = 31002
        ERRUnexpectedExpressionStatement = 31003
        ERRWinRTEventWithoutDelegate = 31004
        ERRSecurityCriticalAsyncInClassOrStruct = 31005
        ERRSecurityCriticalAsync = 31006
        ERRBadModuleFile1 = 31007
        ERRBadRefLib1 = 31011

        'ERRUnableToLoadDll1 = 31013
        'ERRBadDllEntrypoint2 = 31014
        'ERRBadOutputFile1 = 31019
        'ERRBadOutputStream = 31020
        'ERRDeadBackgroundThread = 31021
        'ERRXMLParserError = 31023
        'ERRUnableToCreateMetaDataAPI = 31024
        'ERRUnableToOpenFile1 = 31027
        ERREventHandlerSignatureIncompatible2 = 31029

        ERRConditionalCompilationConstantNotValid = 31030

        'ERRProjectCCError0 = 31031
        ERRInterfaceImplementedTwice1 = 31033

        ERRInterfaceNotImplemented1 = 31035
        ERRAmbiguousImplementsMember3 = 31040

        'ERRBadInterfaceMember = 31041
        ERRImplementsOnNew = 31042

        ERRArrayInitInStruct = 31043
        ERREventTypeNotDelegate = 31044
        ERRProtectedTypeOutsideClass = 31047
        ERRDefaultPropertyWithNoParams = 31048
        ERRInitializerInStruct = 31049
        ERRDuplicateImport1 = 31051
        ERRBadModuleFlags1 = 31052
        ERRImplementsStmtWrongOrder = 31053
        ERRMemberConflictWithSynth4 = 31058
        ERRSynthMemberClashesWithSynth7 = 31059
        ERRSynthMemberClashesWithMember5 = 31060
        ERRMemberClashesWithSynth6 = 31061
        ERRSetHasOnlyOneParam = 31063
        ERRSetValueNotPropertyType = 31064
        ERRSetHasToBeByVal1 = 31065
        ERRStructureCantUseProtected = 31067
        ERRBadInterfaceDelegateSpecifier1 = 31068
        ERRBadInterfaceEnumSpecifier1 = 31069
        ERRBadInterfaceClassSpecifier1 = 31070
        ERRBadInterfaceStructSpecifier1 = 31071

        'ERRWarningTreatedAsError = 31072
        'ERRDelegateConstructorMissing1 = 31074    unused in Roslyn
        ERRUseOfObsoleteSymbolNoMessage1 = 31075

        ERRMetaDataIsNotAssembly = 31076
        ERRMetaDataIsNotModule = 31077
        ERRReferenceComparison3 = 31080
        ERRCatchVariableNotLocal1 = 31082
        ERRModuleMemberCantImplement = 31083
        ERREventDelegatesCantBeFunctions = 31084
        ERRInvalidDate = 31085
        ERRCantOverride4 = 31086
        ERRCantSpecifyArraysOnBoth = 31087
        ERRNotOverridableRequiresOverrides = 31088
        ERRPrivateTypeOutsideType = 31089
        ERRTypeRefResolutionError3 = 31091
        ERRParamArrayWrongType = 31092
        ERRCoClassMissing2 = 31094
        ERRInvalidMeReference = 31095
        ERRInvalidImplicitMeReference = 31096
        ERRRuntimeMemberNotFound2 = 31097

        'ERRRuntimeClassNotFound1 = 31098
        ERRBadPropertyAccessorFlags = 31099

        ERRBadPropertyAccessorFlagsRestrict = 31100
        ERROnlyOneAccessorForGetSet = 31101
        ERRNoAccessibleSet = 31102
        ERRNoAccessibleGet = 31103
        ERRWriteOnlyNoAccessorFlag = 31104
        ERRReadOnlyNoAccessorFlag = 31105
        ERRBadPropertyAccessorFlags1 = 31106
        ERRBadPropertyAccessorFlags2 = 31107
        ERRBadPropertyAccessorFlags3 = 31108
        ERRInAccessibleCoClass3 = 31109
        ERRMissingValuesForArraysInApplAttrs = 31110
        ERRExitEventMemberNotInvalid = 31111
        ERRInvInsideEndsEvent = 31112

        'ERREventMemberSyntax = 31113  abandoned per KevinH analysis. Roslyn bug 1637
        ERRMissingEndEvent = 31114

        ERRMissingEndAddHandler = 31115
        ERRMissingEndRemoveHandler = 31116
        ERRMissingEndRaiseEvent = 31117

        'ERREndAddHandlerNotAtLineStart = 31118
        'ERREndRemoveHandlerNotAtLineStart = 31119
        'ERREndRaiseEventNotAtLineStart = 31120
        ERRCustomEventInvInInterface = 31121

        ERRCustomEventRequiresAs = 31122
        ERRInvalidEndEvent = 31123
        ERRInvalidEndAddHandler = 31124
        ERRInvalidEndRemoveHandler = 31125
        ERRInvalidEndRaiseEvent = 31126
        ERRDuplicateAddHandlerDef = 31127
        ERRDuplicateRemoveHandlerDef = 31128
        ERRDuplicateRaiseEventDef = 31129
        ERRMissingAddHandlerDef1 = 31130
        ERRMissingRemoveHandlerDef1 = 31131
        ERRMissingRaiseEventDef1 = 31132
        ERREventAddRemoveHasOnlyOneParam = 31133
        ERREventAddRemoveByrefParamIllegal = 31134
        ERRSpecifiersInvOnEventMethod = 31135
        ERRAddRemoveParamNotEventType = 31136
        ERRRaiseEventShapeMismatch1 = 31137
        ERREventMethodOptionalParamIllegal1 = 31138
        ERRCantReferToMyGroupInsideGroupType1 = 31139
        ERRInvalidUseOfCustomModifier = 31140
        ERRInvalidOptionStrictCustom = 31141
        ERRObsoleteInvalidOnEventMember = 31142
        ERRDelegateBindingIncompatible2 = 31143
        ERRExpectedXmlName = 31146
        ERRUndefinedXmlPrefix = 31148
        ERRDuplicateXmlAttribute = 31149
        ERRMismatchedXmlEndTag = 31150
        ERRMissingXmlEndTag = 31151
        ERRReservedXmlPrefix = 31152
        ERRMissingVersionInXmlDecl = 31153
        ERRIllegalAttributeInXmlDecl = 31154
        ERRQuotedEmbeddedExpression = 31155
        ERRVersionMustBeFirstInXmlDecl = 31156
        ERRAttributeOrder = 31157

        'ERRUnexpectedXmlName = 31158
        ERRExpectedXmlEndEmbedded = 31159

        ERRExpectedXmlEndPI = 31160
        ERRExpectedXmlEndComment = 31161
        ERRExpectedXmlEndCData = 31162
        ERRExpectedSQuote = 31163
        ERRExpectedQuote = 31164
        ERRExpectedLT = 31165
        ERRStartAttributeValue = 31166
        ERRExpectedDiv = 31167
        ERRNoXmlAxesLateBinding = 31168
        ERRIllegalXmlStartNameChar = 31169
        ERRIllegalXmlNameChar = 31170
        ERRIllegalXmlCommentChar = 31171
        ERREmbeddedExpression = 31172
        ERRExpectedXmlWhiteSpace = 31173
        ERRIllegalProcessingInstructionName = 31174
        ERRDTDNotSupported = 31175

        'ERRIllegalXmlChar = 31176     unused in Dev10
        ERRIllegalXmlWhiteSpace = 31177

        ERRExpectedSColon = 31178
        ERRExpectedXmlBeginEmbedded = 31179
        ERRXmlEntityReference = 31180
        ERRInvalidAttributeValue1 = 31181
        ERRInvalidAttributeValue2 = 31182
        ERRReservedXmlNamespace = 31183
        ERRIllegalDefaultNamespace = 31184

        'ERRRequireAggregateInitializationImpl = 31185
        ERRQualifiedNameNotAllowed = 31186

        ERRExpectedXmlns = 31187

        'ERRDefaultNamespaceNotSupported = 31188 Not reported by Dev10.
        ERRIllegalXmlnsPrefix = 31189

        ERRXmlFeaturesNotAvailable = 31190

        'ERRUnableToEmbedUacManifest = 31191   now reporting ErrorCreatingWin32ResourceFile
        ERRUnableToReadUacManifest2 = 31192

        'ERRUseValueForXmlExpression3 = 31193 ' Replaced by WRNUseValueForXmlExpression3
        ERRTypeMismatchForXml3 = 31194

        ERRBinaryOperandsForXml4 = 31195

        'ERRXmlFeaturesNotAvailableDebugger = 31196
        ERRFullWidthAsXmlDelimiter = 31197

        'ERRXmlRequiresParens = 31198 No Longer Reported. Removed per 926946.
        ERRXmlEndCDataNotAllowedInContent = 31198

        'ERRUacALink3Missing = 31199               not used in Roslyn.
        'ERRXmlFeaturesNotSupported = 31200        not detected by the Roslyn compiler
        ERREventImplRemoveHandlerParamWrong = 31201

        ERRMixingWinRTAndNETEvents = 31202
        ERRAddParamWrongForWinRT = 31203
        ERRRemoveParamWrongForWinRT = 31204
        ERRReImplementingWinRTInterface5 = 31205
        ERRReImplementingWinRTInterface4 = 31206
        ERRXmlEndElementNoMatchingStart = 31207
        ERRUndefinedTypeOrNamespace1 = 31208
        ERRBadInterfaceInterfaceSpecifier1 = 31209
        ERRTypeClashesWithVbCoreType4 = 31210
        ERRSecurityAttributeMissingAction = 31211
        ERRSecurityAttributeInvalidAction = 31212
        ERRSecurityAttributeInvalidActionAssembly = 31213
        ERRSecurityAttributeInvalidActionTypeOrMethod = 31214
        ERRPrincipalPermissionInvalidAction = 31215
        ERRPermissionSetAttributeInvalidFile = 31216
        ERRPermissionSetAttributeFileReadError = 31217
        ERRExpectedWarningKeyword = 31218
        ERRInvalidHashAlgorithmName = 31219

        ''' NOTE: If you add any new errors that may be attached to a symbol during meta-import when it is marked as bad,
        '''      particularly if it applies to method symbols, please appropriately modify Bindable::ResolveOverloadingShouldSkipBadMember.
        '''      Failure to do so may break customer code.
        ''' AVAILABLE                             31220-31390

        ERRInvalidSubsystemVersion = 31391
        ERRLibAnycpu32bitPreferredConflict = 31392
        ERRRestrictedAccess = 31393
        ERRRestrictedConversion1 = 31394
        ERRNoTypecharInLabel = 31395
        ERRRestrictedType1 = 31396
        ERRNoTypecharInAlias = 31398
        ERRNoAccessibleConstructorOnBase = 31399
        ERRBadStaticLocalInStruct = 31400
        ERRDuplicateLocalStatic1 = 31401
        ERRImportAliasConflictsWithType2 = 31403
        ERRCantShadowAMustOverride1 = 31404

        'ERROptionalsCantBeStructs = 31405
        ERRMultipleEventImplMismatch3 = 31407

        ERRBadSpecifierCombo2 = 31408
        ERRMustBeOverloads2 = 31409

        'ERRCantOverloadOnMultipleInheritance = 31410
        ERRMustOverridesInClass1 = 31411

        ERRHandlesSyntaxInClass = 31412
        ERRSynthMemberShadowsMustOverride5 = 31413

        'ERRCantImplementNonVirtual3 = 31415   unused in Roslyn
        ' ERRMemberShadowsSynthMustOverride5 = 31416   unused in Roslyn
        ERRCannotOverrideInAccessibleMember = 31417

        ERRHandlesSyntaxInModule = 31418
        ERRIsNotOpRequiresReferenceTypes1 = 31419
        ERRClashWithReservedEnumMember1 = 31420
        ERRMultiplyDefinedEnumMember2 = 31421
        ERRBadUseOfVoid = 31422
        ERREventImplMismatch5 = 31423
        ERRForwardedTypeUnavailable3 = 31424
        ERRTypeFwdCycle2 = 31425
        ERRBadTypeInCCExpression = 31426
        ERRBadCCExpression = 31427
        ERRVoidArrayDisallowed = 31428
        ERRMetadataMembersAmbiguous3 = 31429
        ERRTypeOfExprAlwaysFalse2 = 31430
        ERROnlyPrivatePartialMethods1 = 31431
        ERRPartialMethodsMustBePrivate = 31432
        ERROnlyOnePartialMethodAllowed2 = 31433
        ERROnlyOneImplementingMethodAllowed3 = 31434
        ERRPartialMethodMustBeEmpty = 31435
        ERRPartialMethodsMustBeSub1 = 31437
        ERRPartialMethodGenericConstraints2 = 31438
        ERRPartialDeclarationImplements1 = 31439
        ERRNoPartialMethodInAddressOf1 = 31440
        ERRImplementationMustBePrivate2 = 31441
        ERRPartialMethodParamNamesMustMatch3 = 31442
        ERRPartialMethodTypeParamNameMismatch3 = 31443
        ERRPropertyDoesntImplementAllAccessors = 31444
        ERRInvalidAttributeUsageOnAccessor = 31445
        ERRNestedTypeInInheritsClause2 = 31446
        ERRTypeInItsInheritsClause1 = 31447
        ERRBaseTypeReferences2 = 31448
        ERRIllegalBaseTypeReferences3 = 31449
        ERRInvalidCoClass1 = 31450
        ERRInvalidOutputName = 31451
        ERRInvalidFileAlignment = 31452
        ERRInvalidDebugInformationFormat = 31453

        ''' NOTE: If you add any new errors that may be attached to a symbol during meta-import when it is marked as bad,
        '''      particularly if it applies to method symbols, please appropriately modify Bindable::ResolveOverloadingShouldSkipBadMember.
        '''      Failure to do so may break customer code.

        ''' AVAILABLE                             31451 - 31497
        ERRConstantStringTooLong = 31498

        ERRMustInheritEventNotOverridden = 31499
        ERRBadAttributeSharedProperty1 = 31500
        ERRBadAttributeReadOnlyProperty1 = 31501
        ERRDuplicateResourceName1 = 31502
        ERRAttributeMustBeClassNotStruct1 = 31503
        ERRAttributeMustInheritSysAttr = 31504

        'ERRAttributeMustHaveAttrUsageAttr = 31505  unused in Roslyn.
        ERRAttributeCannotBeAbstract = 31506

        ' ERRAttributeCannotHaveMustOverride = 31507 - reported by dev10 but error is redundant. ERRAttributeCannotBeAbstract covers this case.
        'ERRCantFindCORSystemDirectory = 31508
        ERRUnableToOpenResourceFile1 = 31509

        'ERRBadAttributeConstField1 = 31510
        ERRBadAttributeNonPublicProperty1 = 31511

        ERRSTAThreadAndMTAThread0 = 31512
        'ERRSTAThreadAndMTAThread1 = 31513

        ''' If you make any change involving this error, such as creating a more specific version for use
        ''' in a particular context, please make sure to appropriately modify Bindable::ResolveOverloadingShouldSkipBadMember
        ERRIndirectUnreferencedAssembly4 = 31515

        ERRBadAttributeNonPublicType1 = 31516
        ERRBadAttributeNonPublicContType2 = 31517

        'ERRAlinkManifestFilepathTooLong = 31518     '  this scenario reports a more generic error
        ERRBadMetaDataReference1 = 31519

        ' ERRErrorApplyingSecurityAttribute1 = 31520 ' we're now reporting more detailed diagnostics: ERRSecurityAttributeMissingAction,  ERRSecurityAttributeInvalidAction, ERRSecurityAttributeInvalidActionAssembly or ERRSecurityAttributeInvalidActionTypeOrMethod
        'ERRDuplicateModuleAttribute1 = 31521
        ERRDllImportOnNonEmptySubOrFunction = 31522

        ERRDllImportNotLegalOnDeclare = 31523
        ERRDllImportNotLegalOnGetOrSet = 31524

        'ERRTypeImportedFromDiffAssemVersions3 = 31525
        ERRDllImportOnGenericSubOrFunction = 31526

        ERRComClassOnGeneric = 31527

        ''' If you make any change involving this error, such as creating a more specific version for use
        ''' in a particular context, please make sure to appropriately modify Bindable::ResolveOverloadingShouldSkipBadMember
        'ERRIndirectUnreferencedAssembly3 = 31528

        ERRDllImportOnInstanceMethod = 31529
        ERRDllImportOnInterfaceMethod = 31530
        ERRDllImportNotLegalOnEventMethod = 31531

        ''' If you make any change involving these errors, such as creating more specific versions for use
        ''' in other contexts, please make sure to appropriately modify Bindable::ResolveOverloadingShouldSkipBadMember
        'ERRIndirectUnreferencedProject3 = 31532
        'ERRIndirectUnreferencedProject2 = 31533

        ERRFriendAssemblyBadArguments = 31534
        ERRFriendAssemblyStrongNameRequired = 31535
        'ERRFriendAssemblyRejectBinding = 31536    EDMAURER This has been replaced with two, more specific errors ERRFriendRefNotEqualToThis and ERRFriendRefSigningMismatch.

        ERRFriendAssemblyNameInvalid = 31537
        ERRFriendAssemblyBadAccessOverride2 = 31538

        ERRAbsentReferenceToPIA1 = 31539

        'ERRCorlibMissingPIAClasses1 = 31540       EDMAURER Roslyn uses the ordinary missing required type message
        ERRCannotLinkClassWithNoPIA1 = 31541

        ERRInvalidStructMemberNoPIA1 = 31542
        ERRNoPIAAttributeMissing2 = 31543

        ERRNestedGlobalNamespace = 31544

        'ERRNewCoClassNoPIA = 31545                EDMAURER Roslyn gives 31541
        'ERREventNoPIANoDispID = 31546
        'ERREventNoPIANoGuid1 = 31547
        'ERREventNoPIANoComEventInterface1 = 31548
        ERRPIAHasNoAssemblyGuid1 = 31549

        'ERRStructureExplicitFieldLacksOffset = 31550
        'ERRCannotLinkEventInterfaceWithNoPIA1 = 31551
        ERRDuplicateLocalTypes3 = 31552

        ERRPIAHasNoTypeLibAttribute1 = 31553

        ' ERRNoPiaEventsMissingSystemCore = 31554      use ordinary missing required type
        ' ERRSourceInterfaceMustExist = 31555
        ERRSourceInterfaceMustBeInterface = 31556

        ERREventNoPIANoBackingMember = 31557
        ERRNestedInteropType = 31558       ' used to be ERRInvalidInteropType
        ERRIsNestedIn2 = 31559
        ERRLocalTypeNameClash2 = 31560
        ERRInteropMethodWithBody1 = 31561

        ERRUseOfLocalBeforeDeclaration1 = 32000
        ERRUseOfKeywordFromModule1 = 32001

        'ERRUseOfKeywordOutsideClass1 = 32002
        'ERRSymbolFromUnreferencedProject3 = 32004
        ERRBogusWithinLineIf = 32005

        ERRCharToIntegralTypeMismatch1 = 32006
        ERRIntegralToCharTypeMismatch1 = 32007
        ERRNoDirectDelegateConstruction1 = 32008
        ERRMethodMustBeFirstStatementOnLine = 32009
        ERRAttrAssignmentNotFieldOrProp1 = 32010
        ERRStrictDisallowsObjectComparison1 = 32013
        ERRNoConstituentArraySizes = 32014
        ERRFileAttributeNotAssemblyOrModule = 32015
        ERRFunctionResultCannotBeIndexed1 = 32016
        ERRArgumentSyntax = 32017
        ERRExpectedResumeOrGoto = 32019
        ERRExpectedAssignmentOperator = 32020
        ERRNamedArgAlsoOmitted2 = 32021
        ERRCannotCallEvent1 = 32022
        ERRForEachCollectionDesignPattern1 = 32023
        ERRDefaultValueForNonOptionalParam = 32024

        ' ERRRegionWithinMethod = 32025    removed this limitation in Roslyn
        'ERRSpecifiersInvalidOnNamespace = 32026   abandoned, now giving 'Specifiers and attributes are not valid on this statement.'
        ERRExpectedDotAfterMyBase = 32027

        ERRExpectedDotAfterMyClass = 32028
        ERRStrictArgumentCopyBackNarrowing3 = 32029
        ERRLbElseifAfterElse = 32030

        'ERREndSubNotAtLineStart = 32031
        'ERREndFunctionNotAtLineStart = 32032
        'ERREndGetNotAtLineStart = 32033
        'ERREndSetNotAtLineStart = 32034
        ERRStandaloneAttribute = 32035

        ERRNoUniqueConstructorOnBase2 = 32036
        ERRExtraNextVariable = 32037
        ERRRequiredNewCallTooMany2 = 32038
        ERRForCtlVarArraySizesSpecified = 32039
        ERRBadFlagsOnNewOverloads = 32040
        ERRTypeCharOnGenericParam = 32041
        ERRTooFewGenericArguments1 = 32042
        ERRTooManyGenericArguments1 = 32043
        ERRGenericConstraintNotSatisfied2 = 32044
        ERRTypeOrMemberNotGeneric1 = 32045
        ERRNewIfNullOnGenericParam = 32046
        ERRMultipleClassConstraints1 = 32047
        ERRConstNotClassInterfaceOrTypeParam1 = 32048
        ERRDuplicateTypeParamName1 = 32049
        ERRUnboundTypeParam2 = 32050
        ERRIsOperatorGenericParam1 = 32052
        ERRArgumentCopyBackNarrowing3 = 32053
        ERRShadowingGenericParamWithMember1 = 32054
        ERRGenericParamBase2 = 32055
        ERRImplementsGenericParam = 32056

        'ERRExpressionCannotBeGeneric1 = 32058 unused in Roslyn
        ERROnlyNullLowerBound = 32059

        ERRClassConstraintNotInheritable1 = 32060
        ERRConstraintIsRestrictedType1 = 32061
        ERRGenericParamsOnInvalidMember = 32065
        ERRGenericArgsOnAttributeSpecifier = 32066
        ERRAttrCannotBeGenerics = 32067
        ERRBadStaticLocalInGenericMethod = 32068
        ERRSyntMemberShadowsGenericParam3 = 32070
        ERRConstraintAlreadyExists1 = 32071
        ERRInterfacePossiblyImplTwice2 = 32072
        ERRModulesCannotBeGeneric = 32073
        ERRGenericClassCannotInheritAttr = 32074
        ERRDeclaresCantBeInGeneric = 32075

        'ERRGenericTypeRequiresTypeArgs1 = 32076
        ERROverrideWithConstraintMismatch2 = 32077

        ERRImplementsWithConstraintMismatch3 = 32078
        ERROpenTypeDisallowed = 32079
        ERRHandlesInvalidOnGenericMethod = 32080
        ERRMultipleNewConstraints = 32081
        ERRMustInheritForNewConstraint2 = 32082
        ERRNoSuitableNewForNewConstraint2 = 32083
        ERRBadGenericParamForNewConstraint2 = 32084
        ERRNewArgsDisallowedForTypeParam = 32085
        ERRDuplicateRawGenericTypeImport1 = 32086
        ERRNoTypeArgumentCountOverloadCand1 = 32087
        ERRTypeArgsUnexpected = 32088
        ERRNameSameAsMethodTypeParam1 = 32089
        ERRTypeParamNameFunctionNameCollision = 32090

        'ERROverloadsMayUnify2 = 32091 unused in Roslyn
        ERRBadConstraintSyntax = 32092

        ERROfExpected = 32093
        ERRArrayOfRawGenericInvalid = 32095
        ERRForEachAmbiguousIEnumerable1 = 32096
        ERRIsNotOperatorGenericParam1 = 32097
        ERRTypeParamQualifierDisallowed = 32098
        ERRTypeParamMissingCommaOrRParen = 32099
        ERRTypeParamMissingAsCommaOrRParen = 32100
        ERRMultipleReferenceConstraints = 32101
        ERRMultipleValueConstraints = 32102
        ERRNewAndValueConstraintsCombined = 32103
        ERRRefAndValueConstraintsCombined = 32104
        ERRBadTypeArgForStructConstraint2 = 32105
        ERRBadTypeArgForRefConstraint2 = 32106
        ERRRefAndClassTypeConstrCombined = 32107
        ERRValueAndClassTypeConstrCombined = 32108
        ERRConstraintClashIndirectIndirect4 = 32109
        ERRConstraintClashDirectIndirect3 = 32110
        ERRConstraintClashIndirectDirect3 = 32111
        ERRConstraintCycleLink2 = 32112
        ERRConstraintCycle2 = 32113
        ERRTypeParamWithStructConstAsConst = 32114
        ERRNullableDisallowedForStructConstr1 = 32115

        'ERRNoAccessibleNonGeneric1 = 32117
        'ERRNoAccessibleGeneric1 = 32118
        ERRConflictingDirectConstraints3 = 32119

        ERRInterfaceUnifiesWithInterface2 = 32120
        ERRBaseUnifiesWithInterfaces3 = 32121
        ERRInterfaceBaseUnifiesWithBase4 = 32122
        ERRInterfaceUnifiesWithBase3 = 32123

        ERROptionalsCantBeStructGenericParams = 32124 'TODO: remove

        'ERRInterfaceMethodImplsUnify3 = 32125
        ERRAddressOfNullableMethod = 32126

        ERRIsOperatorNullable1 = 32127
        ERRIsNotOperatorNullable1 = 32128
        'ERRNullableOnEnum = 32129
        'ERRNoNullableType = 32130 unused in Roslyn

        ERRClassInheritsBaseUnifiesWithInterfaces3 = 32131
        ERRClassInheritsInterfaceBaseUnifiesWithBase4 = 32132
        ERRClassInheritsInterfaceUnifiesWithBase3 = 32133

        ERRShadowingTypeOutsideClass1 = 32200
        ERRPropertySetParamCollisionWithValue = 32201
        'ERREventNameTooLong = 32204 ' Deprecated in favor of ERRTooLongMetadataName
        'ERRWithEventsNameTooLong = 32205 ' Deprecated in favor of ERRTooLongMetadataName

        ERRSxSIndirectRefHigherThanDirectRef3 = 32207
        ERRDuplicateReference2 = 32208

        'ERRSxSLowerVerIndirectRefNotEmitted4 = 32209  not used in Roslyn
        ERRDuplicateReferenceStrong = 32210

        ERRIllegalCallOrIndex = 32303
        ERRConflictDefaultPropertyAttribute = 32304

        'ERRClassCannotCreated = 32400

        ERRBadAttributeUuid2 = 32500
        ERRComClassAndReservedAttribute1 = 32501
        ERRComClassRequiresPublicClass2 = 32504
        ERRComClassReservedDispIdZero1 = 32505
        ERRComClassReservedDispId1 = 32506
        ERRComClassDuplicateGuids1 = 32507
        ERRComClassCantBeAbstract0 = 32508
        ERRComClassRequiresPublicClass1 = 32509
        'ERRDefaultCharSetAttributeNotSupported = 32510

        ERRUnknownOperator = 33000
        ERRDuplicateConversionCategoryUsed = 33001
        ERROperatorNotOverloadable = 33002
        ERRInvalidHandles = 33003
        ERRInvalidImplements = 33004
        ERREndOperatorExpected = 33005
        ERREndOperatorNotAtLineStart = 33006
        ERRInvalidEndOperator = 33007
        ERRExitOperatorNotValid = 33008
        ERRParamArrayIllegal1 = 33009
        ERROptionalIllegal1 = 33010
        ERROperatorMustBePublic = 33011
        ERROperatorMustBeShared = 33012
        ERRBadOperatorFlags1 = 33013
        ERROneParameterRequired1 = 33014
        ERRTwoParametersRequired1 = 33015
        ERROneOrTwoParametersRequired1 = 33016
        ERRConvMustBeWideningOrNarrowing = 33017
        ERROperatorDeclaredInModule = 33018
        ERRInvalidSpecifierOnNonConversion1 = 33019
        ERRUnaryParamMustBeContainingType1 = 33020
        ERRBinaryParamMustBeContainingType1 = 33021
        ERRConvParamMustBeContainingType1 = 33022
        ERROperatorRequiresBoolReturnType1 = 33023
        ERRConversionToSameType = 33024
        ERRConversionToInterfaceType = 33025
        ERRConversionToBaseType = 33026
        ERRConversionToDerivedType = 33027
        ERRConversionToObject = 33028
        ERRConversionFromInterfaceType = 33029
        ERRConversionFromBaseType = 33030
        ERRConversionFromDerivedType = 33031
        ERRConversionFromObject = 33032
        ERRMatchingOperatorExpected2 = 33033
        ERRUnacceptableLogicalOperator3 = 33034
        ERRConditionOperatorRequired3 = 33035
        ERRCopyBackTypeMismatch3 = 33037
        ERRForLoopOperatorRequired2 = 33038
        ERRUnacceptableForLoopOperator2 = 33039
        ERRUnacceptableForLoopRelOperator2 = 33040
        ERROperatorRequiresIntegerParameter1 = 33041

        ERRCantSpecifyNullableOnBoth = 33100
        ERRBadTypeArgForStructConstraintNull = 33101
        ERRCantSpecifyArrayAndNullableOnBoth = 33102
        ERRCantSpecifyTypeCharacterOnIIF = 33103
        ERRIllegalOperandInIIFCount = 33104
        ERRIllegalOperandInIIFName = 33105
        ERRIllegalOperandInIIFConversion = 33106
        ERRIllegalCondTypeInIIF = 33107
        ERRCantCallIIF = 33108
        ERRCantSpecifyAsNewAndNullable = 33109
        ERRIllegalOperandInIIFConversion2 = 33110
        ERRBadNullTypeInCCExpression = 33111
        ERRNullableImplicit = 33112

        ''' NOTE: If you add any new errors that may be attached to a symbol during meta-import when it is marked as bad,
        '''      particularly if it applies to method symbols, please appropriately modify Bindable::ResolveOverloadingShouldSkipBadMember.
        '''      Failure to do so may break customer code.
        ''' AVAILABLE                             33113 - 34999

        ERRMissingRuntimeHelper = 35000

        'ERRNoStdModuleAttribute = 35001 ' Note: we're now reporting a use site error in this case.
        'ERRNoOptionTextAttribute = 35002
        ERRDuplicateResourceFileName1 = 35003

        ERRExpectedDotAfterGlobalNameSpace = 36000
        ERRNoGlobalExpectedIdentifier = 36001
        ERRNoGlobalInHandles = 36002
        ERRElseIfNoMatchingIf = 36005
        ERRBadAttributeConstructor2 = 36006
        ERREndUsingWithoutUsing = 36007
        ERRExpectedEndUsing = 36008
        ERRGotoIntoUsing = 36009
        ERRUsingRequiresDisposePattern = 36010
        ERRUsingResourceVarNeedsInitializer = 36011
        ERRUsingResourceVarCantBeArray = 36012
        ERROnErrorInUsing = 36013
        ERRPropertyNameConflictInMyCollection = 36015
        ERRInvalidImplicitVar = 36016

        ERRObjectInitializerRequiresFieldName = 36530
        ERRExpectedFrom = 36531
        ERRLambdaBindingMismatch1 = 36532
        ERRCannotLiftByRefParamQuery1 = 36533
        ERRExpressionTreeNotSupported = 36534
        ERRCannotLiftStructureMeQuery = 36535
        ERRInferringNonArrayType1 = 36536
        ERRByRefParamInExpressionTree = 36538
        'ERRObjectInitializerBadValue = 36543

        ''' If you change this message, make sure to change message for QueryDuplicateAnonTypeMemberName1 as well!
        ERRDuplicateAnonTypeMemberName1 = 36547

        ERRBadAnonymousTypeForExprTree = 36548
        ERRCannotLiftAnonymousType1 = 36549

        ERRExtensionOnlyAllowedOnModuleSubOrFunction = 36550
        ERRExtensionMethodNotInModule = 36551
        ERRExtensionMethodNoParams = 36552
        ERRExtensionMethodOptionalFirstArg = 36553
        ERRExtensionMethodParamArrayFirstArg = 36554

        ''' If you change this message, make sure to change message for  QueryAnonymousTypeFieldNameInference as well!
        'ERRBadOrCircularInitializerReference = 36555
        ERRAnonymousTypeFieldNameInference = 36556

        ERRNameNotMemberOfAnonymousType2 = 36557
        ERRExtensionAttributeInvalid = 36558
        ERRAnonymousTypePropertyOutOfOrder1 = 36559

        ''' If you change this message, make sure to change message for  QueryAnonymousTypeDisallowsTypeChar as well!
        ERRAnonymousTypeDisallowsTypeChar = 36560

        ERRExtensionMethodUncallable1 = 36561
        ERRExtensionMethodOverloadCandidate3 = 36562
        ERRDelegateBindingMismatch = 36563
        ERRDelegateBindingTypeInferenceFails = 36564
        ERRTooManyArgs = 36565
        ERRNamedArgAlsoOmitted1 = 36566
        ERRNamedArgUsedTwice1 = 36567
        ERRNamedParamNotFound1 = 36568
        ERROmittedArgument1 = 36569
        ERRUnboundTypeParam1 = 36572
        ERRExtensionMethodOverloadCandidate2 = 36573
        ERRAnonymousTypeNeedField = 36574
        ERRAnonymousTypeNameWithoutPeriod = 36575
        ERRAnonymousTypeExpectedIdentifier = 36576

        'ERRNoAnonymousTypeInitializersInDebugger = 36577
        'ERRTooFewGenericArguments = 36578
        'ERRTooManyGenericArguments = 36579
        'ERRDelegateBindingMismatch3_3 = 36580 unused in Roslyn
        'ERRDelegateBindingTypeInferenceFails3 = 36581
        ERRTooManyArgs2 = 36582

        ERRNamedArgAlsoOmitted3 = 36583
        ERRNamedArgUsedTwice3 = 36584
        ERRNamedParamNotFound3 = 36585
        ERROmittedArgument3 = 36586
        ERRUnboundTypeParam3 = 36589
        ERRTooFewGenericArguments2 = 36590
        ERRTooManyGenericArguments2 = 36591

        ERRExpectedInOrEq = 36592
        ERRExpectedQueryableSource = 36593
        ERRQueryOperatorNotFound = 36594

        ERRCannotUseOnErrorGotoWithClosure = 36595
        ERRCannotGotoNonScopeBlocksWithClosure = 36597
        ERRCannotLiftRestrictedTypeQuery = 36598

        ERRQueryAnonymousTypeFieldNameInference = 36599
        ERRQueryDuplicateAnonTypeMemberName1 = 36600
        ERRQueryAnonymousTypeDisallowsTypeChar = 36601
        ERRReadOnlyInClosure = 36602
        ERRExprTreeNoMultiDimArrayCreation = 36603
        ERRExprTreeNoLateBind = 36604

        ERRExpectedBy = 36605
        ERRQueryInvalidControlVariableName1 = 36606

        ERRExpectedIn = 36607

        'ERRQueryStartsWithLet = 36608
        'ERRNoQueryExpressionsInDebugger = 36609
        ERRQueryNameNotDeclared = 36610

        ''' Available 36611

        ERRNestedFunctionArgumentNarrowing3 = 36612

        ''' If you change this message, make sure to change message for  QueryAnonTypeFieldXMLNameInference as well!
        ERRAnonTypeFieldXMLNameInference = 36613

        ERRQueryAnonTypeFieldXMLNameInference = 36614

        ERRExpectedInto = 36615

        'ERRAggregateStartsWithLet = 36616
        ERRTypeCharOnAggregation = 36617

        ERRExpectedOn = 36618
        ERRExpectedEquals = 36619
        ERRExpectedAnd = 36620
        ERREqualsTypeMismatch = 36621
        ERREqualsOperandIsBad = 36622

        ''' see 30581 (lambda version of AddressOf)
        ERRLambdaNotDelegate1 = 36625

        ''' see 30939 (lambda version of AddressOf)
        ERRLambdaNotCreatableDelegate1 = 36626

        'ERRNoLambdaExpressionsInDebugger = 36627
        ERRCannotInferNullableForVariable1 = 36628

        ERRNullableTypeInferenceNotSupported = 36629
        ERRExpectedJoin = 36631
        ERRNullableParameterMustSpecifyType = 36632
        ERRIterationVariableShadowLocal2 = 36633
        ERRLambdasCannotHaveAttributes = 36634
        ERRLambdaInSelectCaseExpr = 36635
        ERRAddressOfInSelectCaseExpr = 36636
        ERRNullableCharNotSupported = 36637

        ''' The follow error messages are paired with other query specific messages above.  Please
        ''' make sure to keep the two in sync
        ERRCannotLiftStructureMeLambda = 36638

        ERRCannotLiftByRefParamLambda1 = 36639
        ERRCannotLiftRestrictedTypeLambda = 36640

        ERRLambdaParamShadowLocal1 = 36641
        ERRStrictDisallowImplicitObjectLambda = 36642
        ERRCantSpecifyParamsOnLambdaParamNoType = 36643

        ERRTypeInferenceFailure1 = 36644
        ERRTypeInferenceFailure2 = 36645
        ERRTypeInferenceFailure3 = 36646
        ERRTypeInferenceFailureNoExplicit1 = 36647
        ERRTypeInferenceFailureNoExplicit2 = 36648
        ERRTypeInferenceFailureNoExplicit3 = 36649

        ERRTypeInferenceFailureAmbiguous1 = 36650
        ERRTypeInferenceFailureAmbiguous2 = 36651
        ERRTypeInferenceFailureAmbiguous3 = 36652
        ERRTypeInferenceFailureNoExplicitAmbiguous1 = 36653
        ERRTypeInferenceFailureNoExplicitAmbiguous2 = 36654
        ERRTypeInferenceFailureNoExplicitAmbiguous3 = 36655

        ERRTypeInferenceFailureNoBest1 = 36656
        ERRTypeInferenceFailureNoBest2 = 36657
        ERRTypeInferenceFailureNoBest3 = 36658
        ERRTypeInferenceFailureNoExplicitNoBest1 = 36659
        ERRTypeInferenceFailureNoExplicitNoBest2 = 36660
        ERRTypeInferenceFailureNoExplicitNoBest3 = 36661

        ERRDelegateBindingMismatchStrictOff2 = 36663
        'ERRTooDeepNestingOfParensInLambdaParam = 36664 - No Longer Reported. Removed per 926942

        ' ERRInaccessibleReturnTypeOfSymbol1 = 36665
        ERRInaccessibleReturnTypeOfMember2 = 36666

        ERRLocalNamedSameAsParamInLambda1 = 36667
        ERRMultilineLambdasCannotContainOnError = 36668

        'ERRBranchOutOfMultilineLambda = 36669 obsolete - was not even reported in Dev10 any more.
        ERRLambdaBindingMismatch2 = 36670

        'ERRMultilineLambdaShadowLocal1 = 36671 'unused in Roslyn
        ERRStaticInLambda = 36672

        ERRMultilineLambdaMissingSub = 36673
        ERRMultilineLambdaMissingFunction = 36674
        ERRStatementLambdaInExpressionTree = 36675

        ''' ERRStrictDisallowsImplicitLambda                 = 36676
        ''' replaced by LambdaNoType and LambdaNoTypeObjectDisallowed and LambdaTooManyTypesObjectDisallowed
        ERRAttributeOnLambdaReturnType = 36677

        ERRExpectedIdentifierOrGroup = 36707
        ERRUnexpectedGroup = 36708
        ERRDelegateBindingMismatchStrictOff3 = 36709
        ERRDelegateBindingIncompatible3 = 36710
        ERRArgumentNarrowing2 = 36711
        ERROverloadCandidate1 = 36712
        ERRAutoPropertyInitializedInStructure = 36713
        ERRInitializedExpandedProperty = 36714
        'ERRNewExpandedProperty = 36715 'unused in Roslyn

        ERRLanguageVersion = 36716
        ERRArrayInitNoType = 36717
        ERRNotACollection1 = 36718
        ERRNoAddMethod1 = 36719
        ERRCantCombineInitializers = 36720
        ERREmptyAggregateInitializer = 36721

        ERRVarianceDisallowedHere = 36722
        ERRVarianceInterfaceNesting = 36723
        ERRVarianceOutParamDisallowed1 = 36724
        ERRVarianceInParamDisallowed1 = 36725
        ERRVarianceOutParamDisallowedForGeneric3 = 36726
        ERRVarianceInParamDisallowedForGeneric3 = 36727
        ERRVarianceOutParamDisallowedHere2 = 36728
        ERRVarianceInParamDisallowedHere2 = 36729
        ERRVarianceOutParamDisallowedHereForGeneric4 = 36730
        ERRVarianceInParamDisallowedHereForGeneric4 = 36731
        ERRVarianceTypeDisallowed2 = 36732
        ERRVarianceTypeDisallowedForGeneric4 = 36733
        ERRLambdaTooManyTypesObjectDisallowed = 36734
        ERRVarianceTypeDisallowedHere3 = 36735
        ERRVarianceTypeDisallowedHereForGeneric5 = 36736
        ERRAmbiguousCastConversion2 = 36737
        ERRVariancePreventsSynthesizedEvents2 = 36738
        ERRNestingViolatesCLS1 = 36739
        ERRVarianceOutNullableDisallowed2 = 36740
        ERRVarianceInNullableDisallowed2 = 36741
        ERRVarianceOutByValDisallowed1 = 36742
        ERRVarianceInReturnDisallowed1 = 36743
        ERRVarianceOutConstraintDisallowed1 = 36744
        ERRVarianceInReadOnlyPropertyDisallowed1 = 36745
        ERRVarianceOutWriteOnlyPropertyDisallowed1 = 36746
        ERRVarianceOutPropertyDisallowed1 = 36747
        ERRVarianceInPropertyDisallowed1 = 36748
        ERRVarianceOutByRefDisallowed1 = 36749
        ERRVarianceInByRefDisallowed1 = 36750
        ERRLambdaNoType = 36751

        ''' ERRNoReturnStatementsForMultilineLambda  = 36752
        ''' replaced by LambdaNoType and LambdaNoTypeObjectDisallowed
        'ERRCollectionInitializerArity2 = 36753
        ERRVarianceConversionFailedOut6 = 36754

        ERRVarianceConversionFailedIn6 = 36755
        ERRVarianceIEnumerableSuggestion3 = 36756
        ERRVarianceConversionFailedTryOut4 = 36757
        ERRVarianceConversionFailedTryIn4 = 36758
        ERRAutoPropertyCantHaveParams = 36759
        ERRIdentityDirectCastForFloat = 36760

        ERRTypeDisallowsElements = 36807
        ERRTypeDisallowsAttributes = 36808
        ERRTypeDisallowsDescendants = 36809
        'ERRXmlSchemaCompileError = 36810

        ERRTypeOrMemberNotGeneric2 = 36907
        ERRExtensionMethodCannotBeLateBound = 36908
        ERRTypeInferenceArrayRankMismatch1 = 36909

        ERRQueryStrictDisallowImplicitObject = 36910
        ERRIfNoType = 36911
        ERRIfNoTypeObjectDisallowed = 36912
        ERRIfTooManyTypesObjectDisallowed = 36913
        ERRArrayInitNoTypeObjectDisallowed = 36914
        ERRArrayInitTooManyTypesObjectDisallowed = 36915
        ERRLambdaNoTypeObjectDisallowed = 36916
        ERROverloadsModifierInModule = 36917
        ERRSubRequiresSingleStatement = 36918
        ERRSubDisallowsStatement = 36919
        ERRSubRequiresParenthesesLParen = 36920
        ERRSubRequiresParenthesesDot = 36921
        ERRSubRequiresParenthesesBang = 36922
        ERRCannotEmbedInterfaceWithGeneric = 36923
        ERRCannotUseGenericTypeAcrossAssemblyBoundaries = 36924
        ERRCannotUseGenericBaseTypeAcrossAssemblyBoundaries = 36925
        ERRBadAsyncByRefParam = 36926
        ERRBadIteratorByRefParam = 36927

        'ERRBadAsyncExpressionLambda = 36928 'unused in Roslyn
        ERRBadAsyncInQuery = 36929

        ERRBadGetAwaiterMethod1 = 36930

        'ERRExpressionTreeContainsAwait = 36931
        ERRRestrictedResumableType1 = 36932

        ERRBadAwaitNothing = 36933
        ERRAsyncSubMain = 36934
        ERRPartialMethodsMustNotBeAsync1 = 36935
        ERRInvalidAsyncIteratorModifiers = 36936
        ERRBadAwaitNotInAsyncMethodOrLambda = 36937
        ERRBadIteratorReturn = 36938
        ERRBadYieldInTryHandler = 36939
        ERRBadYieldInNonIteratorMethod = 36940

        ''' unused 36941
        ERRBadReturnValueInIterator = 36942

        ERRBadAwaitInTryHandler = 36943

        'ERRBadAwaitObject = 36944 'unused in Roslyn
        ERRBadAsyncReturn = 36945

        ERRBadResumableAccessReturnVariable = 36946
        ERRBadIteratorExpressionLambda = 36947

        'ERRAwaitLibraryMissing = 36948
        'ERRAwaitPattern1 = 36949
        ERRConstructorAsync = 36950

        ERRInvalidLambdaModifier = 36951
        ERRReturnFromNonGenericTaskAsync = 36952
        'ERRBadAutoPropertyFlags1 = 36953 'unused in Roslyn

        ERRBadOverloadCandidates2 = 36954
        ERRBadStaticInitializerInResumable = 36955
        ERRResumablesCannotContainOnError = 36956
        ERRFriendRefNotEqualToThis = 36957
        ERRFriendRefSigningMismatch = 36958
        ERRFailureSigningAssembly = 36960
        ERRSignButNoPrivateKey = 36961
        ERRInvalidVersionFormat = 36962

        ERRExpectedSingleScript = 36963
        ERRReferenceDirectiveOnlyAllowedInScripts = 36964
        ERRNamespaceNotAllowedInScript = 36965
        ERRKeywordNotAllowedInScript = 36966

        ERRReservedAssemblyName = 36968

        ERRConstructorCannotBeDeclaredPartial = 36969
        ERRModuleEmitFailure = 36970

        ERRParameterNotValidForType = 36971
        ERRMarshalUnmanagedTypeNotValidForFields = 36972
        ERRMarshalUnmanagedTypeOnlyValidForFields = 36973
        ERRAttributeParameterRequired1 = 36974
        ERRAttributeParameterRequired2 = 36975
        ERRInvalidVersionFormat2 = 36976
        ERRInvalidAssemblyCultureForExe = 36977
        ERRInvalidMultipleAttributeUsageInNetModule2 = 36978
        ERRSecurityAttributeInvalidTarget = 36979

        ERRPublicKeyFileFailure = 36980
        ERRPublicKeyContainerFailure = 36981

        ERRInvalidAssemblyCulture = 36982
        ERREncUpdateFailedMissingAttribute = 36983

        ERRCantAwaitAsyncSub1 = 37001
        ERRResumableLambdaInExpressionTree = 37050
        ERRDllImportOnResumableMethod = 37051
        ERRCannotLiftRestrictedTypeResumable1 = 37052
        ERRBadIsCompletedOnCompletedGetResult2 = 37053
        ERRSynchronizedAsyncMethod = 37054
        ERRBadAsyncReturnOperand1 = 37055
        ERRDoesntImplementAwaitInterface2 = 37056
        ERRBadAwaitInNonAsyncMethod = 37057
        ERRBadAwaitInNonAsyncVoidMethod = 37058
        ERRBadAwaitInNonAsyncLambda = 37059
        ERRLoopControlMustNotAwait = 37060

        ERRMyGroupCollectionAttributeCycle = 37201
        ERRLiteralExpected = 37202
        ERRPartialMethodDefaultParameterValueMismatch2 = 37203
        ERRPartialMethodParamArrayMismatch2 = 37204

        ERRNetModuleNameMismatch = 37205
        ERRBadModuleName = 37206
        ERRCmdOptionConflictsSource = 37207
        ERRTypeForwardedToMultipleAssemblies = 37208
        ERRInvalidSignaturePublicKey = 37209
        ERRCollisionWithPublicTypeInModule = 37210
        ERRExportedTypeConflictsWithDeclaration = 37211
        ERRExportedTypesConflict = 37212
        ERRAgnosticToMachineModule = 37213
        ERRConflictingMachineModule = 37214
        ERRCryptoHashFailed = 37215
        ERRCantHaveWin32ResAndManifest = 37216

        ERRForwardedTypeConflictsWithDeclaration = 37217
        ERRForwardedTypeConflictsWithExportedType = 37218
        ERRForwardedTypesConflict = 37219

        ERRTooLongMetadataName = 37220
        ERRMissingNetModuleReference = 37221
        ERRUnsupportedModule1 = 37222
        ERRUnsupportedEvent1 = 37223
        ERRNetModuleNameMustBeUnique = 37224
        ERRPDBWritingFailed = 37225
        ERRParamDefaultValueDiffersFromAttribute = 37226
        ERRResourceInModule = 37227
        ERRFieldHasMultipleDistinctConstantValues = 37228

        ERRAmbiguousInNamespaces2 = 37229
        ERREncNoPIAReference = 37230
        ERRLinkedNetmoduleMetadataMustProvideFullPEImage = 37231
        ERRCantReadRulesetFile = 37232
        ERRMetadataReferencesNotSupported = 37233
        ERRPlatformDoesntSupport = 37234
        ERRCantUseRequiredAttribute = 37235
        ERREncodinglessSyntaxTree = 37236

        ERRInvalidFormatSpecifier = 37237

        ERRCannotBeMadeNullable1 = 37238
        ERRBadConditionalWithRef = 37239
        ERRNullPropagatingOpInExpressionTree = 37240
        ERRTooLongOrComplexExpression = 37241

        ERRBadPdbData = 37242
        ERRAutoPropertyCantBeWriteOnly = 37243

        ERRExpressionDoesntHaveName = 37244
        ERRInvalidNameOfSubExpression = 37245
        ERRMethodTypeArgsUnexpected = 37246
        ERRInReferencedAssembly = 37247
        ERREncReferenceToAddedMember = 37248
        ERRInterpolationFormatWhitespace = 37249
        ERRInterpolationAlignmentOutOfRange = 37250
        ERRInterpolatedStringFactoryError = 37251
        ERRDebugEntryPointNotSourceMethodDefinition = 37252
        ERRInvalidPathMap = 37253
        ERRPublicSignNoKey = 37254
        ERRTooManyUserStrings = 37255
        ERRPeWritingFailure = 37256

        ERROptionMustBeAbsolutePath = 37257
        ERRDocFileGen = 37258

        ERRTupleTooFewElements = 37259
        ERRTupleReservedElementNameAnyPosition = 37260
        ERRTupleReservedElementName = 37261
        ERRTupleDuplicateElementName = 37262

        ERRRefReturningCallInExpressionTree = 37263

        ERRSourceLinkRequiresPdb = 37264
        ERRCannotEmbedWithoutPdb = 37265

        ERRInvalidInstrumentationKind = 37266

        ERRValueTupleTypeRefResolutionError1 = 37267

        ERRTupleElementNamesAttributeMissing = 37268
        ERRExplicitTupleElementNamesAttribute = 37269
        ERRTupleLiteralDisallowsTypeChar = 37270

        ERRDuplicateProcDefWithDifferentTupleNames2 = 37271
        ERRInterfaceImplementedTwiceWithDifferentTupleNames2 = 37272
        ERRInterfaceImplementedTwiceWithDifferentTupleNames3 = 37273
        ERRInterfaceImplementedTwiceWithDifferentTupleNamesReverse3 = 37274
        ERRInterfaceImplementedTwiceWithDifferentTupleNames4 = 37275

        ERRInterfaceInheritedTwiceWithDifferentTupleNames2 = 37276
        ERRInterfaceInheritedTwiceWithDifferentTupleNames3 = 37277
        ERRInterfaceInheritedTwiceWithDifferentTupleNamesReverse3 = 37278
        ERRInterfaceInheritedTwiceWithDifferentTupleNames4 = 37279

        ERRNewWithTupleTypeSyntax = 37280
        ERRPredefinedValueTupleTypeMustBeStruct = 37281
        ERRPublicSignNetModule = 37282
        ERRBadAssemblyName = 37283

        ERRMergeConflictMarkerEncountered = 37284

        ERRBadSourceCodeKind = 37285
        ERRBadDocumentationMode = 37286
        ERRBadLanguageVersion = 37287
        ERRInvalidPreprocessorConstantType = 37288
        ERRTupleInferredNamesNotAvailable = 37289
        ERRInvalidDebugInfo = 37290

        ERRNoRefOutWhenRefOnly = 37300
        ERRNoNetModuleOutputWhenRefOutOrRefOnly = 37301

        ERRBadNonTrailingNamedArgument = 37302
        ERRExpectedNamedArgumentInAttributeList = 37303
        ERRNamedArgumentSpecificationBeforeFixedArgumentInLateboundInvocation = 37304

        ERRValueTupleResolutionAmbiguous3 = 37305

        ''' WARNINGS BEGIN HERE
        WRNUseOfObsoleteSymbol2 = 40000

        WRNInvalidOverrideDueToTupleNames2 = 40001
        WRNMustOverloadBase4 = 40003
        WRNOverrideType5 = 40004
        WRNMustOverride2 = 40005
        WRNDefaultnessShadowed4 = 40007
        WRNUseOfObsoleteSymbolNoMessage1 = 40008
        WRNAssemblyGeneration0 = 40009
        WRNAssemblyGeneration1 = 40010
        WRNComClassNoMembers1 = 40011
        WRNSynthMemberShadowsMember5 = 40012
        WRNMemberShadowsSynthMember6 = 40014
        WRNSynthMemberShadowsSynthMember7 = 40018
        WRNUseOfObsoletePropertyAccessor3 = 40019
        WRNUseOfObsoletePropertyAccessor2 = 40020

        ' WRNMemberShadowsMemberInModule5 = 40021      ' no repro in legacy test, most probably not reachable. Unused in Roslyn.
        ' WRNSynthMemberShadowsMemberInModule5 = 40022 ' no repro in legacy test, most probably not reachable. Unused in Roslyn.
        ' WRNMemberShadowsSynthMemberInModule6 = 40023 ' no repro in legacy test, most probably not reachable. Unused in Roslyn.
        ' WRNSynthMemberShadowsSynthMemberMod7 = 40024 ' no repro in legacy test, most probably not reachable. Unused in Roslyn.
        WRNFieldNotCLSCompliant1 = 40025

        WRNBaseClassNotCLSCompliant2 = 40026
        WRNProcTypeNotCLSCompliant1 = 40027
        WRNParamNotCLSCompliant1 = 40028
        WRNInheritedInterfaceNotCLSCompliant2 = 40029
        WRNCLSMemberInNonCLSType3 = 40030
        WRNNameNotCLSCompliant1 = 40031
        WRNEnumUnderlyingTypeNotCLS1 = 40032
        WRNNonCLSMemberInCLSInterface1 = 40033
        WRNNonCLSMustOverrideInCLSType1 = 40034
        WRNArrayOverloadsNonCLS2 = 40035
        WRNRootNamespaceNotCLSCompliant1 = 40038
        WRNRootNamespaceNotCLSCompliant2 = 40039
        WRNGenericConstraintNotCLSCompliant1 = 40040
        WRNTypeNotCLSCompliant1 = 40041
        WRNOptionalValueNotCLSCompliant1 = 40042
        WRNCLSAttrInvalidOnGetSet = 40043
        WRNTypeConflictButMerged6 = 40046

        ' WRNTypeConflictButMerged7 = 40047  ' deprecated
        WRNShadowingGenericParamWithParam1 = 40048

        WRNCannotFindStandardLibrary1 = 40049
        WRNEventDelegateTypeNotCLSCompliant2 = 40050
        WRNDebuggerHiddenIgnoredOnProperties = 40051
        WRNSelectCaseInvalidRange = 40052
        WRNCLSEventMethodInNonCLSType3 = 40053
        WRNExpectedInitComponentCall2 = 40054
        WRNNamespaceCaseMismatch3 = 40055
        WRNUndefinedOrEmptyNamespaceOrClass1 = 40056
        WRNUndefinedOrEmptyProjectNamespaceOrClass1 = 40057

        'WRNInterfacesWithNoPIAMustHaveGuid1 = 40058 ' Not reported by Dev11.
        WRNIndirectRefToLinkedAssembly2 = 40059

        WRNDelaySignButNoKey = 40060

        WRNUnimplementedCommandLineSwitch = 40998

        ' WRNDuplicateAssemblyAttribute1 = 41000   'unused in Roslyn
        WRNNoNonObsoleteConstructorOnBase3 = 41001

        WRNNoNonObsoleteConstructorOnBase4 = 41002
        WRNRequiredNonObsoleteNewCall3 = 41003
        WRNRequiredNonObsoleteNewCall4 = 41004
        WRNMissingAsClauseinOperator = 41005

        WRNConstraintsFailedForInferredArgs2 = 41006
        WRNConditionalNotValidOnFunction = 41007
        WRNUseSwitchInsteadOfAttribute = 41008
        WRNTupleLiteralNameMismatch = 41009

        ''' AVAILABLE                             41010 - 41199
        WRNReferencedAssemblyDoesNotHaveStrongName = 41997

        WRNRecursiveAddHandlerCall = 41998
        WRNImplicitConversionCopyBack = 41999
        WRNMustShadowOnMultipleInheritance2 = 42000

        ' WRNObsoleteClassInitialize = 42001     ' deprecated
        ' WRNObsoleteClassTerminate = 42002      ' deprecated
        WRNRecursiveOperatorCall = 42004

        ' WRNIndirectlyImplementedBaseMember5 = 42014 ' deprecated
        ' WRNImplementedBaseMember4 = 42015 ' deprecated

        WRNImplicitConversionSubst1 = 42016 ' populated by 42350/42332/42336/42337/42338/42339/42340
        WRNLateBindingResolution = 42017
        WRNObjectMath1 = 42018
        WRNObjectMath2 = 42019
        WRNObjectAssumedVar1 = 42020  ' populated by 42111/42346
        WRNObjectAssumed1 = 42021  ' populated by 42347/41005/42341/42342/42344/42345/42334/42343
        WRNObjectAssumedProperty1 = 42022  ' populated by 42348

        ''' AVAILABLE                             42023

        WRNUnusedLocal = 42024
        WRNSharedMemberThroughInstance = 42025
        WRNRecursivePropertyCall = 42026

        WRNOverlappingCatch = 42029
        WRNDefAsgUseNullRefByRef = 42030
        WRNDuplicateCatch = 42031
        WRNObjectMath1Not = 42032

        WRNBadChecksumValExtChecksum = 42033
        WRNMultipleDeclFileExtChecksum = 42034
        WRNBadGUIDFormatExtChecksum = 42035
        WRNObjectMathSelectCase = 42036
        WRNEqualToLiteralNothing = 42037
        WRNNotEqualToLiteralNothing = 42038

        ''' AVAILABLE                             42039 - 42098
        WRNUnusedLocalConst = 42099

        ''' UNAVAILABLE                           42100
        WRNComClassInterfaceShadows5 = 42101

        WRNComClassPropertySetObject1 = 42102

        ''' only reference types are considered for definite assignment.
        ''' DefAsg's are all under VBadvanced
        WRNDefAsgUseNullRef = 42104

        WRNDefAsgNoRetValFuncRef1 = 42105
        WRNDefAsgNoRetValOpRef1 = 42106
        WRNDefAsgNoRetValPropRef1 = 42107
        WRNDefAsgUseNullRefByRefStr = 42108
        WRNDefAsgUseNullRefStr = 42109

        ' WRNFieldInForNotExplicit = 42110       'unused in Roslyn
        WRNStaticLocalNoInference = 42111

        ''' AVAILABLE                             42112 - 42202
        ' WRNSxSHigherIndirectRefEmitted4 = 42203    'unused in Roslyn
        ' WRNReferencedAssembliesAmbiguous6 = 42204  'unused in Roslyn
        ' WRNReferencedAssembliesAmbiguous4 = 42205  'unused in Roslyn
        ' WRNMaximumNumberOfWarnings = 42206     'unused in Roslyn
        WRNInvalidAssemblyName = 42207

        ''' AVAILABLE                             42209 - 42299
        WRNXMLDocBadXMLLine = 42300

        WRNXMLDocMoreThanOneCommentBlock = 42301
        WRNXMLDocNotFirstOnLine = 42302
        WRNXMLDocInsideMethod = 42303
        WRNXMLDocParseError1 = 42304
        WRNXMLDocDuplicateXMLNode1 = 42305
        WRNXMLDocIllegalTagOnElement2 = 42306
        WRNXMLDocBadParamTag2 = 42307
        WRNXMLDocParamTagWithoutName = 42308
        WRNXMLDocCrefAttributeNotFound1 = 42309
        WRNXMLMissingFileOrPathAttribute1 = 42310
        WRNXMLCannotWriteToXMLDocFile2 = 42311
        WRNXMLDocWithoutLanguageElement = 42312
        WRNXMLDocReturnsOnWriteOnlyProperty = 42313
        WRNXMLDocOnAPartialType = 42314
        WRNXMLDocReturnsOnADeclareSub = 42315
        WRNXMLDocStartTagWithNoEndTag = 42316
        WRNXMLDocBadGenericParamTag2 = 42317
        WRNXMLDocGenericParamTagWithoutName = 42318
        WRNXMLDocExceptionTagWithoutCRef = 42319
        WRNXMLDocInvalidXMLFragment = 42320
        WRNXMLDocBadFormedXML = 42321
        WRNInterfaceConversion2 = 42322
        WRNLiftControlVariableLambda = 42324

        ' 42325 unused, was abandoned, now used in unit test "EnsureLegacyWarningsAreMaintained". Please update test if you are going to use this number.
        WRNLambdaPassedToRemoveHandler = 42326

        WRNLiftControlVariableQuery = 42327
        WRNRelDelegatePassedToRemoveHandler = 42328
        ' WRNQueryMissingAsClauseinVarDecl = 42329  ' unused in Roslyn.

        ' WRNLiftUsingVariableInLambda1 = 42330     ' unused in Roslyn.
        ' WRNLiftUsingVariableInQuery1 = 42331      ' unused in Roslyn.
        WRNAmbiguousCastConversion2 = 42332         ' substitutes into 42016

        WRNVarianceDeclarationAmbiguous3 = 42333
        WRNArrayInitNoTypeObjectAssumed = 42334
        WRNTypeInferenceAssumed3 = 42335
        WRNVarianceConversionFailedOut6 = 42336 ' substitutes into 42016
        WRNVarianceConversionFailedIn6 = 42337 ' substitutes into 42016
        WRNVarianceIEnumerableSuggestion3 = 42338 ' substitutes into 42016
        WRNVarianceConversionFailedTryOut4 = 42339 ' substitutes into 42016
        WRNVarianceConversionFailedTryIn4 = 42340 ' substitutes into 42016
        WRNIfNoTypeObjectAssumed = 42341
        WRNIfTooManyTypesObjectAssumed = 42342
        WRNArrayInitTooManyTypesObjectAssumed = 42343
        WRNLambdaNoTypeObjectAssumed = 42344
        WRNLambdaTooManyTypesObjectAssumed = 42345
        WRNMissingAsClauseinVarDecl = 42346
        WRNMissingAsClauseinFunction = 42347
        WRNMissingAsClauseinProperty = 42348

        WRNObsoleteIdentityDirectCastForValueType = 42349
        WRNImplicitConversion2 = 42350 ' substitutes into 42016

        WRNMutableStructureInUsing = 42351
        WRNMutableGenericStructureInUsing = 42352

        WRNDefAsgNoRetValFuncVal1 = 42353
        WRNDefAsgNoRetValOpVal1 = 42354
        WRNDefAsgNoRetValPropVal1 = 42355
        WRNAsyncLacksAwaits = 42356
        WRNAsyncSubCouldBeFunction = 42357
        WRNUnobservedAwaitableExpression = 42358
        WRNUnobservedAwaitableDelegate = 42359
        WRNPrefixAndXmlnsLocalName = 42360
        WRNUseValueForXmlExpression3 = 42361 ' Replaces ERRUseValueForXmlExpression3

        'WRNPDBConstantStringValueTooLong = 42363  we gave up on this warning. See comments in commonCompilation.Emit()
        WRNReturnTypeAttributeOnWriteOnlyProperty = 42364

        ''' AVAILABLE 42365

        WRNInvalidVersionFormat = 42366
        WRNMainIgnored = 42367
        WRNEmptyPrefixAndXmlnsLocalName = 42368

        WRNDefAsgNoRetValWinRtEventVal1 = 42369

        WRNAssemblyAttributeFromModuleIsOverridden = 42370
        WRNRefCultureMismatch = 42371
        WRNConflictingMachineAssembly = 42372

        WRNPdbLocalNameTooLong = 42373
        WRNPdbUsingNameTooLong = 42374

        WRNXMLDocCrefToTypeParameter = 42375

        WRNAnalyzerCannotBeCreated = 42376
        WRNNoAnalyzerInAssembly = 42377
        WRNUnableToLoadAnalyzer = 42378

        WRNAttributeIgnoredWhenPublicSigning = 42379
        WRNExperimental = 42380

        ''' AVAILABLE                             42381 - 49998
        ERRWRNNextAvailable = 42381

        ''' HIDDENS AND INFOS BEGIN HERE
        HDNUnusedImportClause = 50000

        HDNUnusedImportStatement = 50001
        INFUnableToLoadSomeTypesInAnalyzer = 50002

        ''' AVAILABLE                             50003 - 54999

        ' Adding diagnostic arguments from resx file
        IDSProjectSettingsLocationName = 56000

        IDSFunctionReturnType = 56001
        IDSTheSystemCannotFindThePathSpecified = 56002

        ' available: 56003
        IDSMSGADDMODULE = 56004

        IDSMSGADDLINKREFERENCE = 56005
        IDSMSGADDREFERENCE = 56006
        IDSLogoLine1 = 56007
        IDSLogoLine2 = 56008
        IDSVBCHelp = 56009
        IDSLangVersions = 56010
        IDSToolName = 56011

        ' Feature codes
        FEATUREAutoProperties

        FEATURELineContinuation
        FEATUREStatementLambdas
        FEATURECoContraVariance
        FEATURECollectionInitializers
        FEATURESubLambdas
        FEATUREArrayLiterals
        FEATUREAsyncExpressions
        FEATUREIterators
        FEATUREGlobalNamespace
        FEATURENullPropagatingOperator
        FEATURENameOfExpressions
        FEATUREReadonlyAutoProperties
        FEATURERegionsEverywhere
        FEATUREMultilineStringLiterals
        FEATURECObjInAttributeArguments
        FEATURELineContinuationComments
        FEATURETypeOfIsNot
        FEATUREYearFirstDateLiterals
        FEATUREWarningDirectives
        FEATUREPartialModules
        FEATUREPartialInterfaces
        FEATUREImplementingReadonlyOrWriteonlyPropertyWithReadwrite
        FEATUREDigitSeparators
        FEATUREBinaryLiterals
        FEATURETuples
        FEATURELeadingDigitSeparator
        FEATUREPrivateProtected
        FEATUREInterpolatedStrings
    End Enum

End Namespace
