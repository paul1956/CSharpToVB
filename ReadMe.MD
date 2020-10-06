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
# C# to VB.Net Converter

## Requires

- Visual Studio 2019 Version 16.5.0 Preview 1.0 or later to edit the GUI in the solution designer and you need to enable the designer in the Visual Studio Preview. Go to Tools > Options > Environment > Preview Features and select the Use the preview Windows Forms designer for .NET Core application option. 
- Visual Studio 2019 version 16.3.0 to compile the code it produces
- .NET Core 3.1 to use the converter

## License
- MIT

## Technologies
  - Windows Forms
  - dotnet-core

## Topics
- Code Converter

## Updated
- 09/28/2020

## Description
This application will convert C# to VB while trying to preserve comments, formatting and as many Directives
as possible. It converts the True side of conditional directives, the False side is not converted. After
conversion it tries to compile the code. If a file can't be compiled it can be add to an ignore list so you can continue to process other files.

You can convert text by typing or pasting in into the left Window, a file, project or folder from the menu's.

The Tests under "TestCompile" are designed to translate the C# code in Roslyn and uses
GetRoslynRootDirectory to find it. Results of folder conversion are stored in with the same directory
structure with the root directory renamed to _VB. The original directories are unchanged.

This version required at least Visual Studio 2019 version 16.3.0 to compile the code it produces because it uses the new Visual Basic feature _ ' Comment to preserve
most comments and formatting. To build the converter application, Visual Studio 16.5.0 Preview 1.0 and .Net Core 3.1.

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
  New in 4.6.0.9/4.0.0.7
  - Use new ToolStripTextProgressBar

  New in 4.6.0.8/4.0.0.7
  - Fix nameColon name getting changed
  - Add support back for Line Number Display which was deleted by Visual Studio
  - Add support to not fail on 'Default Constraint'
  - Add support for Implicit Object Creation Expression 
  - Fix TextColor in "Find What" ComboBox
  - Add limited support for Patterns

  New in 4.6.0.7
  - Select All support added to Editing

  New in 4.6.0.6
  - Many small improvements in ToolString Find including placeholder text
  - Lots of reorganization moved everything into folders, Moved all helpers out of  Form1

  New in 4.6.0.5/4.0.0.5
  - While feature wise this is a very minor update a lot of code has been changed to clean up duplication
      - Remove old Implements translation Code
  - Adjust Find Toolbar item positions for easier use
  - Better organize main Form1 code
  - Fix more corner cases for Implements
  - Make Public Function Friend where possible
  - Complete redo of "As Type" support for var, it is a lot of text processing and needs a lot of cleanup and performance work and removal of duplicate code.
      - Add support for NullableTypes
      - Better Array support

  New in 4.6.0.4/4.0.0.4
  - Improve Menu placement and internal naming
  - Improve Char integer math

  New in 4.6.0.3/4.0.0.3
  - Allow Paste of Plain Text
  - Performance improvement in converting Trivia
  - Fix most failed tests around implements
  - More improvements in Interface support

  New in 4.6.0.2/4.0.0.2
  - Minor code cleanup for Core 5.0 Compatibility

  New in 4.6.0.1/4.0.0.1
  - New improved Find Toolbar
  - Minor fix for duplicate trivia around synthesized imports that C# doesn't need but VB does

  New in 4.6.0.0/4.0.0.0
  - Added or updated 100's of tests (10 skipped and will fail)
  - Removed redundant tests
  - Fixed Event Conversion
  - Fixed Invoking New InvocationExpressions expressions
  - Improvements in merging and preserving trivia
  - Improvement in Reference =
  - Some File menu improvements
  - Improve compatibility with Core 5.0
  - Fix translation of + Operator for String
  - Replace List(Of SyntaxTrivia) with SyntaxTriviaList which caused a lot of changes
  - Preserve Banner on top of file
  - Preserve C# numeric literal suffix characters
  - Fixed math issue with -9223372036854775808 needs to be translated to -9223372036854775808UL
  - Fixed Issue with When clause in the Case Label

  In Process
  - Simplify Trivia restructuring removing duplicate code
  - Finish supporting skipped tests
  - Improvements in Formating long binary, octal and hex numbers as strings using _ separator

## Code Coverage
```
+------------------+--------+--------+--------+
| Module           | Line   | Branch | Method |
+------------------+--------+--------+--------+
| VB.CodeConverter | 79.65% | 73.84% | 90.28% |
+------------------+--------+--------+--------+

+---------+--------+--------+--------+
|         | Line   | Branch | Method |
+---------+--------+--------+--------+
| Total   | 79.65% | 73.84% | 90.28% |
+---------+--------+--------+--------+
| Average | 79.65% | 73.84% | 90.28% |
+---------+--------+--------+--------+
```