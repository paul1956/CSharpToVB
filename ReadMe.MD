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
- 12/14/2019

## Description
This application will convert C# to VB while trying to preserve comments, formatting and as many Directives
as possible. It converts the True side of conditional directives, the False side is not converted. After
conversion it tries to compile the code, there is a list of ignored errors that can be modified. If a file
can't be compiled it can be add to an ignore list so you can continue to process other files.

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

New is Release 4.0

- The program does not deal well with bad C# code so there is now a new feature to add a fixed code fragment to make a fragment into a valid C# program
- The Code Fragment can be edition under Options/Advanced Options
- Ignoring errors that broke translated code have been removed, every valid program should compile without errors
- Translation of #pragma disable/enable errors has been removed they the original pragma is now translated and then commented out.
- After translating a folder the total time is displayed in the status bar
- Code coverage is only done on CodeConverter Project.
- Translated Attributes will end in a line continuation, Visual Studio can remove the unnecessary ones using code cleanup.

## Code Coverage
```
+------------------+--------+--------+--------+
| Module           | Line   | Branch | Method |
+------------------+--------+--------+--------+
| VB.CodeConverter | 79.6%  | 72.37% | 87.91% |
+------------------+--------+--------+--------+

+---------+--------+--------+--------+
|         | Line   | Branch | Method |
+---------+--------+--------+--------+
| Total   | 79.6%  | 72.37% | 87.91% |
+---------+--------+--------+--------+
| Average | 79.6%  | 72.37% | 87.91% |
+---------+--------+--------+--------+
```