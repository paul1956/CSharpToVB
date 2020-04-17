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
- 04/12/2020

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
Handle .DLL's from C# project file
There are several Stop statements for debugging, and if you continue past them reasonable things will
happen but a better translation is possible.


It will try to create a VB project File from the original C# project file.

https://dotnetfeed.blob.core.windows.net/dotnet-core/index.json is added to NuGet Package Source to find
Microsoft.Dotnet.XUnitExtensions which provides ConditionalFact and PlatformSpecific attributes used for testing.

  New in Release 4.3.0.4
  - Minor cleanup to StartNextInstance

  New in Release 4.3.0.3
  - Fix #45 missing () when Properties are called
  - Upgrade Buildalyzer

  New in Release 4.3.0.2
  - Fix #14 formatting of Array Literals

  New in Release 4.3.0.1
  - Minor fix for Deconstruction inside ArrowExpressionClauseSyntax
  - Add comment to show original code where directives are removed in ArrowExpressionClauseSyntax

  New in Release 4.3.0.0
  - Major rewrite of ArrowExpressionClauseSyntax handling
  - Fix issue #24 You can now specify VB Options that are included in converted file and what values the Options are
  - Fixes changing Options included in converted file reflected without restarting application
  - Fix issues #36, #37, #38, #39, #40, #41, $43, #44 and #46 Issue details can be found here https://github.com/paul1956/CSharpToVB/issues

  New in Release 4.2.10.1
  - Minor formatting updates

 New in Release 4.2.10.0
 - Improve Startup Screen
 - Make single instance (great example of doing in Core using NamedPipes)

 New in Release 4.2.9.0
- Allow user to specify what Visual Basic Options are included in converted file. Tests will continue to use existing behavior.
- Options are specified under Advanced Options menu and are saved per user.
- About Box has been modified to show CodeConverter version information
- Update <RepositoryType>git</RepositoryType> in project files

New in Release 4.2.8.0
- Fix conversion of x ??= y to use x = If(x, y)
- If you just type a random Snippet that does not look like a program this version will not try compile it, and will be able to convert anything that can be converted inside a function.
- Fix version display in splash screen
- Add ability to open some default browsers at the GitHub issues page for this repo. Currently only iE, Chrome and Edge are supported, if you would like additional browsers supported open an issue with browser ProgID from registry and full path.
- Fix case when handling
- Fix coalescing assignment operator handling
- Fix formatting around deleted break statements
 
New in Release 4.2.7.0
- Added Tests for 3 major Core project types and fixed bugs found in project translation

New in Release 4.2.6.0
- Restructure Convert Project to enable better error handling, better progress messages and prepare for Convert Solution

New in Release 4.2.5.0
- Double clicking on Error List line will scroll VB buffer to the error
- Issue #29 closed, at the end of a project conversion the root of the VB project will open in explorer

New in Release 4.2.4.0
- Improved status is given during Convert Project during the long startup delay
- Warning is given for non-SDK projects but the C# files are converted
- Issues #27,  #28, #30, #32, #33, #34, #36 are closed
- Issue #37 closed, double clicking on C# file is list will open it and converted VB file


## Code Coverage
```
+------------------+--------+--------+--------+
| Module           | Line   | Branch | Method |
+------------------+--------+--------+--------+
| VB.CodeConverter | 79.19% | 72.5%  | 86.61% |
+------------------+--------+--------+--------+

+---------+--------+--------+--------+
|         | Line   | Branch | Method |
+---------+--------+--------+--------+
| Total   | 79.19% | 72.5%  | 86.61% |
+---------+--------+--------+--------+
| Average | 79.19% | 72.5%  | 86.61% |
+---------+--------+--------+--------+
```