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

- Visual Studio 2019 Version 16.8.0
- .NET Core 5.0 to use the converter

## License
- MIT

## Technologies
  - Windows Forms
  - dotnet-core

## Topics
- Code Converter

## Updated
- 11/12/2020

## Description
This application will convert C# to VB while trying to preserve comments, formatting and as many Directives
as possible. It converts the True side of conditional directives, the False side is not converted. After
conversion it tries to compile the code. If a file can't be compiled it can be add to an ignore list so you can continue to process other files.

You can convert text by typing or pasting in into the left Window, a file, project or folder from the menu's.

The Tests under "TestCompile" are designed to translate the C# code in Roslyn and uses
GetRoslynRootDirectory to find it. Results of folder conversion are stored in with the same directory
structure with the root directory renamed to _VB. The original directories are unchanged.

This version required at least Visual Studio 2019 version 16.3.0 to compile the code it produces because it uses the new Visual Basic feature _ ' Comment to preserve
most comments and formatting. To build the converter application, Visual Studio 16.8.0 and .Net Core 5.0

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
+-----------------------+--------+--------+--------+
| Module                | Line   | Branch | Method |
+-----------------------+--------+--------+--------+
| VB.CodeConverter      | 68.46% | 62.29% | 84.99% |
+-----------------------+--------+--------+--------+

+---------+--------+--------+--------+
|         | Line   | Branch | Method |
+---------+--------+--------+--------+
| Total   | 68.46% | 62.29% | 84.99% |
+---------+--------+--------+--------+
| Average | 68.46% | 62.29% | 84.99% |
+---------+--------+--------+--------+
```