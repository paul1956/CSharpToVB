---
languages:
- vb
products:
- dotnet-core
- windows
page_type: Code Converter
name: "CSharpToVB (Visual Basic)"
description: "C# to VB Converter"
---
[![Build status badge](https://github.com/paul1956/CSharpToVB/workflows/CI/badge.svg)](https://github.com/paul1956/CSharpToVB/workflows/CI/badge.svg "Build status")

# C# to VB.Net Converter

## Requires
- Visual Studio 2019 Version 16.9 Preview 4 to support Visual Basic Code Generation
- Visual Studio 2019 Version 16.8 support can be found in branch "Support16.8"
- .NET Core 5.0 to use the converter

## License
- MIT

## Technologies
  - Windows Forms
  - dotnet-core

## Topics
- Code Converter

## Updated
- 2/6/2022

## Description
This application will convert C# to VB while trying to preserve comments, formatting and as many Directives
as possible. It converts the True side of conditional directives, the False side is not converted. After
conversion it tries to compile the code. If a file can't be compiled it can be add to an ignore list so you can continue to process other files.

You can convert text by typing or pasting in into the left Window, a file, project or folder from the menu's.

The Tests under "TestCompile" are designed to translate the C# code in Roslyn and uses
GetRoslynRootDirectory to find it. Results of folder conversion are stored in with the same directory
structure with the root directory renamed to _VB. The original directories are unchanged.

This version required at least Visual Studio 2019 version 16.3.0 to compile the code it produces because it uses the new Visual Basic feature _ ' Comment to preserve
most comments and formatting. To build the converter application, Visual Studio 2019 Version 16.11.9 or Visual Studio 2022 and
.Net Core 6.0

Opening the resulting Visual Basic files in Visual Studio will further improve the formatting.

The last five files compiled are saved in an MRU list and you can save the edited Source windows into
a Snippet and reload it. This is useful when you are debugging and want to focus on the lines that
are a problem. You can search Input or Output buffers and hide the search options by clicking on the X.
You can stop folder conversion with the Stop button. You can restart from where you left off by selecting the option
"Start Conversion from last file" or start at the beginning by deselecting this option, if this version is
selected and you switch to convert a new folder the application will immediately return done and you will
need to deselect the option. You can show or hide line numbers from the View Menu.

Work to be done:
There are several Stop statements for debugging, and if you continue past them reasonable things will
happen but a better translation is possible.

## Important Notes
https://dotnetfeed.blob.core.windows.net/dotnet-core/index.json is added to NuGet Package Source to find
Microsoft.Dotnet.XUnitExtensions which provides ConditionalFact and PlatformSpecific attributes used for testing.

Any changes to CSharpToVB application needs to change the version information in the project file **AND** in MyProject/AssemblyInfo.vb until Visual Studio is able to general file automatically for a WinForms application.

## What's New in this release
  New in 5.0.6.5
  Update Icons
  New in 5.0.6.4/5.0.2.9
  Update NuGet Packages and use IsKind instead of Kind
  New in 5.0.6.3/5.0.2.8
  Fix Cy.yml file to use .Net 6.0
  Fix Form1.Designer to set Me.TSFindLookInComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
  and remove setting in Form1


  New in 5.0.6.1/5.0.2.7
  Fix Crash in VisitEventDeclaration also preserve comments in Event Handlers

  New in 5.0.6.1/5.0.2.6
  Fix for #80 

  New in 5.0.6.1/5.0.2.4
  Fix search dropdown to not use Simple to prevent toolstrip from growing

  New in 5.0.6.0/5.0.2.4
  Update to use .Net 6.0
  Fix Issue #81

  New in 5.0.4.0/5.0.2.2
  - Mostly cleanup

  New in 5.0.4.0/5.0.2.1
  - Do a better job at not renaming Types
  - Local Types that conflict with Type names in Libraries will not be renamed

  New in 5.0.4.0/5.0.2.0
  - Redesign of Event handling
  - Stopped renaming of object initializers

  New in 5.0.4.0/5.0.1.22
  - Fix issue #76
  - Major improvement is project and solution conversion
  - Major improvement in custom event and invoke event handling


  New in 5.0.3.5/5.0.1.21
  - Fixed issue #77
  - Small workaround for "path"

  New in 5.0.3.5/5.0.1.20
  - Fixed issue #78

  New in 5.0.3.5/5.0.1.19
  - Add support for Tuple ImplicitElementAccess
  - Fix new icon sizes

  New in 5.0.3.4/5.0.1.18
  - Change logic for update checking
  - Better support dark mode with transparent icons

  New in 5.0.3.3/5.0.1.18
  - Improved code that checks for updates and make it Async

  New in 5.0.3.2/5.0.1.18
  - Improved fix for issue #75

  New in 5.0.3.2/5.0.1.16
  - Fixes issue #74 conversion of "this" in Sub New for Class

  New in 5.0.3.2/5.0.1.15
  - Add Update not available Icon

  New in 5.0.3.1/5.0.1.15
  - Change Interlocked.Increment to Threading.Interlocked.Increment to partly address issue #72
  - Improve support for Dark Mode

  New in 5.0.3.0/5.0.1.14
  - Major improvement in colorize performance
  - Improvement in StopButton responsiveness
  - Remove unused parameters

  New in 5.0.2.13/5.0.1.14
  - Improve conversion of dynamic types
  - Improve converting += of strings to &=
  - When pasting into conversion buffers respect colorize settings

  New in 5.0.2.12/5.0.1.13
  - Add support for Code Colorization on Paste

  New in 5.0.2.11/5.0.1.13
  - Improve editing experience, remove old Color Mode code

  New in 5.0.2.10/5.0.1.13
  - Fix crash when coloring code with no failures

  New in 5.0.2.9/5.0.1.13
  - Very preliminary Dark Mode support based on work by @DualBrain

  New in 5.0.2.8/5.0.1.13
  - Fix duplicate last statement when Switch expression is converted
  - Update Code Coverage which is now almost 95%
  - Correct spelling errors and eliminate more unused code
  - Fix Spelling errors in variable names and comments

  New in 5.0.2.7/5.0.1.12
  - Fix Search to correctly put found text at top of visible window.
  - Speed up Trivia Conversion.
  - Improve Handling of Conditional Directives, using Comments where necessary 

  New in 5.0.2.6/5.0.1.11
 - Fix conversion of Lambda functions with multiple discard parameters
 - Update Roslyn Tests to only run if Environment variable "EnableRoslynTests" is set to "True"

  New in 5.0.2.6/5.0.1.10
 - Fix conversion of Lambda functions with discard parameter

  New in 5.0.2.6/5.0.1.9
 - Fix issue #72

  New in 5.0.2.6/5.0.1.8
  - Fix messages for issue #70

  New in 5.0.2.5/5.0.1.8
  - Fix version checking to correctly use Version.Parse and logic for detecting Generated Code 

  New in 5.0.2.4/5.0.1.8
  - Improve update needed checker logic 

  New in 5.0.2.3/5.0.1.8
  - Remove duplicate #endif trivia and converted comment

  New in 5.0.2.3/5.0.1.7
  - Improve handling of empty CSS.BlockSyntax
  - Add update code coverage showing increase in Method coverage to 90%

  New in 5.0.2.3/5.0.1.6
  - Fix Attributes in Enum list

  New in 5.0.2.3/5.0.1.5
  - Ignore CA1826 for OrDefault extensions
  - Update IntPtr Arrays in TypeSyntax
  - Prevent application crash when compiler crashes

  New in 5.0.2.2/5.0.1.4
  - Remove ~50 redundant test
  - Support IntPtr Arrays
  - Fix Interfaces properties that contain VB reserved names

  New in 5.0.2.2/5.0.1.3
  - Make sure MethodStatements end in EOL
  - Use ProfessionalRender for ContextStrip. MenuStrip and ToolStrip
  - Don't convert .cs files that really contain VB code.
  - Handle multi-line Attributes with comments

  New in 5.0.2.0/5.0.1.1
  - Update default Font Setting for conversion buffers
  - Add helper for removing Option statements from result.

  New in 5.0.1.5/5.0.1.0
  - Fix comments in ConditionalExpressions
  - Support additional places where directives are not allowed
  - Don't allow TopLevelStatement(TLS) conversion when buffer does not contain TLS and only allow snippet conversion for TLS's.
  - Add support for DLL definitions done as local functions

  New in 5.0.1.4/5.0.0.29
  - Improve InitializerExpressionSyntax conversion

  New in 5.0.1.4/5.0.0.28
  - Fixed Issues/67
  - Fixed Sub Main removal in top level code

  New in 5.0.1.3/5.0.0.27
  - General code cleanup around Theme support

  New in 5.0.1.1/5.0.0.27
  - Drop ReadOnly when converting Struct
  - Fix conversion of non value Nullable Types

  New in 5.0.1.1/5.0.0.25
  - Add support for exact translation of Top Level Statements, this could be improved by deleting extra lines in output
  - Add theme support for Light and Dark Mode

  New in 5.0.0.29/5.0.0.24
  - Improve edit experience of Colorized buffers

  New in 5.0.0.28/5.0.0.24
  - Set edit buffers to read-only instead of Disabled when colorizing

  New in 5.0.0.27/5.0.0.24
  - Improve Colorization to handle Regex and SubStrings, a new function AdjustAdditiveSpans was added to merger the new spans it would use a good design review.

  New in 5.0.0.25/5.0.0.24
  - Fix Cref conversions
  - Add more Stops before throw to help debug
  - Change color from Yellow to Purple for String Escape

  New in 5.0.0.25/5.0.0.23
  - Fix issue #63

  New in 5.0.0.25/5.0.0.22
  - Improve ++ and -- handling and use more existing keywords and tokens

  New in 5.0.0.25/5.0.0.21
  - Implement ToString for Records

  New in 5.0.0.25/5.0.0.20
  - Add support for foreach variable with variable is a Tuple
  - Fix custom events to include RaiseEvent
  - Fix when Namespace and Class names are the same just differing by case

  New in 5.0.0.25/5.0.0.19
  - Clean up Tuple Deconstruction
  - Optimize some trivia by creating them at startup
  - Fixes issues #56-58 and #61

  New in 5.0.0.25/5.0.0.18
  - Fix removing ? from Nullable Strings and Objects
  - 

  New in 5.0.0.24/5.0.0.17
  - Improve support C# 9.0 WithExpressions, including inherited Records
  - Cleanup and organize VisualBasicSyntaxFactory 

  New in 5.0.0.23/5.0.0.17
  - Improve support C# 9.0 WithExpressions
  - Add first unit test for WithExpressions

  New in 5.0.0.23/5.0.0.16
  - Support C# 9.0 WithExpressions

  New in 5.0.0.23/5.0.0.15

- Application UI Changes
    - Fix Copy of items from File List in main UI using keyboard

- Converter Changes
  - Add initial support for ImplicitObjectCreationExpression new()
  - Add support for "Using" local declarations from C# 8
  - Support Directives in Class header including implements, inherits, Directives are moved to end but logic is maintained 

***Not all cases may be covered for new features so please provide feedback***

  New in 5.0.0.22/5.0.0.14
  - Fix translation of Nullable Strings and some complex strings
  - Fix Roslyn Test where a .cs file only has VB code

  New in 5.0.0.22/5.0.0.13
  - Fix ArrowExpression conversion that were sometime being converted to Functions instead of Subs when returning void

  New in 5.0.0.22/5.0.0.12
  - Add limited support for init accessors

  New in 5.0.0.22/5.0.0.11
  - Fix crash if MRU file is deleted
  - Run VB Code Cleanup and fix Name format violations 

  New in 5.0.0.21/5.0.0.10
  - Improve and simplify update notification handling

  New in 5.0.0.20/5.0.0.10
  - Convert notify to StatusStrip Notify icon.

  New in 5.0.0.19/5.0.0.10
  - Fixed formatting in ReadMe.MD file
  - Add ability to check for newer Master versions on GitHub automatically on startup and manually under Help

  New in 5.0.0.18/5.0.0.10
  - Fixed Issue #55 Editing keys now work in edit buffers in addition to Edit and Context Menus
  - Fixed Issue #54 Guard against crash when LineNumbersForRichTextBox.Parent is nothing
  - Fixed Issue #53 Create a discard function when a C# discard assign (_) is converted to __ in VB
  - Fixed Issue #51 scoping of nested Blocks

  New in 5.0.0.17/5.0.0.9
  - Fixed Issue #52 Merged fix by @elGuille-info to handle "Add Public to Sub Main when declared in non static class in C#"

  New in 5.0.0.17/5.0.0.8
  - Fixed crash when trying to set controls in StartUp Form from MyApplication_Startup
  - Change default for editor buffers font to Consolas, added support for user to change font to anything they want, and save changes
  - Better handle Dim X = New Y to use "As" more often

  New in 5.0.0.16/5.0.0.7
  - Fix issue where progress reporting was not happening
  - Use TextRenderer where easy to make text more readable

  New in 5.0.0.15/5.0.0.7
  - Fix corner case with converting Dictionary types
  - Add support for display of Shortcut keys on Edit menu items
  - Update formating to comply with .editorConfig

  New in 5.0.0.14/5.0.0.6
  - Fix Equals to Convert Record to Class (may not be complete yet)
  - Fix For Each where ElementType was not always used for loop variable type
  - Cleanup more style violations

  New in 5.0.0.14/5.0.0.5
  - Add Equals to Convert Record to Class (may not be complete yet)

  New in 5.0.0.14/5.0.0.4
  - Convert Record to Class (may not be complete yet)

  New in 5.0.0.12/5.0.0.3
  - No logic changes, extensive formatting changes to support camelCase for locals

  New in 5.0.0.11/5.0.0.2
  - Fix Yield as Reserved word
  - Formatting around New and Dim statement initializer starting on new line

  New in 5.0.0.11/5.0.0.1
  - Fix Await Statements to not do temp assignments
  - Fix add and remove handler for SimpleMemberAccessExpressions 

  In Process
  - Simplify Trivia restructuring removing duplicate code
  - Finish supporting skipped test
  - Improvements in Formating long binary, octal and hex numbers as strings using _ separator

## Code Coverage
```
+------------------+-------+--------+--------+
| Module           | Line  | Branch | Method |
+------------------+-------+--------+--------+
| VB.CodeConverter | 85.3% | 78.04% | 94.81% |
+------------------+-------+--------+--------+

+---------+-------+--------+--------+
|         | Line  | Branch | Method |
+---------+-------+--------+--------+
| Total   | 85.3% | 78.04% | 94.81% |
+---------+-------+--------+--------+
| Average | 85.3% | 78.04% | 94.81% |
+---------+-------+--------+--------+
```