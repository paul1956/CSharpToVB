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
- 05/19/2020

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


It will try to create a VB project File from the original C# project file.

https://dotnetfeed.blob.core.windows.net/dotnet-core/index.json is added to NuGet Package Source to find
Microsoft.Dotnet.XUnitExtensions which provides ConditionalFact and PlatformSpecific attributes used for testing.
  New in 4.4.2.5
  - Update Form1.Designer.VB with changes from new WinForms designer

  New in 4.4.2.4
  - Improved File Processed and File Error ListBoxes to only enable when there is something in them

  New in 4.4.2.3/Converter 3.4.2.8
  - Add TODO Check to converted local functions as comment
  - Add TODO around Inherits with directive conditional parameters
  - Fixed Interface translation
  - Fix Clear Highlighted Text algorithm to put cursor in correct place
  - Fix search history list management
  - Fix Tuple Assignment
  - Improve comment handling for Switch statement
  - Remove unused code

  New in 4.4.2.2/Converter 3.4.2.6
  - Improvement in exception messages 

  New in 4.4.2.1/Converter 3.4.2.5
  - Update NuGet packages
  - Dispose more stuff and add 'Shared' to keep analyzers happy

  New in 4.4.2.0/Converter 3.4.2.4
  - Complete rewrite of Find, made it a floating tool window, added support for match whole word and match case
  - Added a 5 memory temporary buffer for text search terms
  - TODO save settings and history for Find tool window
  - General code cleanup, added support for Firebox browser

  New in 4.4.1.3/Converter 3.4.2.3
  - Clear status line at start of conversions
  - Improve handling of Array Types and Collection Initializers

  New in 4.4.1.2/Converter 3.4.2.2
  - Improve handling of "Is Type" to handle value types

  New in 4.4.1.0/Converter 3.4.2.1
  - Remove most _Renamed name changes, if you still find ant please open issue
  - Correct handling of MyBase
  - Improve formatting of parameters
  - Fix type for Sub Lambda expressions

  New in 4.4.1.1/Converter 3.4.1.1
  - Fix formatting for Optional and ParamArray parameters

  New in Release 4.4.0.4/Converter 3.4.0.3
  - Update to use new single-instance design
  - Improve menu names, add some titles
  - Update About Box information and all dialog box titles 
  - Improvements in Local Function handling, including nested local functions 
  - Fix formatting of arguments of so they are not left justified.

  New in Release 4.4.0.2/Converter 3.4.0.0
  - No logic changes, update and correct license banners and remove unused C# code.


  New in Release 4.4.0.1/Converter 3.4.0.0
  - All the changes are in the converter which now handles many local functions.
  - Please provide feedback if you have a specific local function that is not correctly converted.
  - I want to thank Joseph Musser @jnm2 for help with the algorithm that figures out where to patch in the lambda function that simulate the C# local function. 

  New in Release 4.4.0.0
  - Convert Solutions and better conversions of Projects
  - There is a prompt to select which framework to use during conversion for project with more then one framework, under Visual Studio all frameworks will be converted but only the last will be saved for each project
  - Under Visual Studio as Frameworks will be converted but just the last will be saved
  - Improve Index Extension to allow MoveNext
  - Remove unused code
  - Added ability to save last project and solution on File menu
  - Current status of project/solution conversion and Framework shown on status bar

  New in Release 4.3.2.0
  - Implement For Each with Index Extension and using it where possible
  - Catch ArgumentException where expected and allow throw other exceptions
  - Start changing locals to begin with lowercase character(s)
  - Fix long outstanding issue where ignored files were being written to output directory as empty files and total files was calculated correctly.
  - Fix deconstruction tuple elements when elements are constructed on left of equals for example (EvalStackFrame frameOpt, IOperation operationOpt) = _evalStack.Pop();
  - Correctly Autonomous Types when converting Projects

  New in Release 4.3.1.1
  - Spelling corrections

  New in Release 4.3.1.0
  - Support double-click in file list Window to open C# and VB File for Folder Convert in addition to Project Convert 

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
- Add ability to open some default browsers at the GitHub issues page for this Repo. Currently only iE, Chrome and Edge are supported, if you would like additional browsers supported open an issue with browser ProgID from registry and full path.
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
| VB.CodeConverter | 79.16% | 72.86% | 87.45% |
+------------------+--------+--------+--------+

+---------+--------+--------+--------+
|         | Line   | Branch | Method |
+---------+--------+--------+--------+
| Total   | 79.16% | 72.86% | 87.45% |
+---------+--------+--------+--------+
| Average | 79.16% | 72.86% | 87.45% |
+---------+--------+--------+--------+
```