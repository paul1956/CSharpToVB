This application will convert C# to VB while trying to preserve comments, formatting and as many Directives
as possible. It converts the True side of conditional directives, the False side is not converted. After
conversion it tries to compile the code, there is a list of ignored errors that can be modified. If I file
can't be compiled it can be add to an ignore list.

You can convert text by typing in into the left Window, a file, project or folder from the menu's.

The Tests under "TestCompile" are designed to translate the C# code in Roslyn and uses
GetRoslynRootDirectory to find it. Results of folder conversion are stored in with the same directory
structure with the root directory renamed to _VB. The original directories are unchanged.

This version required at least Visual Studio 2019 version 16.2.0 Preview 3.0 to compile the code it produces uses the new Visual Basic feature _ ' Comment to preserve
most comments and formatting.

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
Create a VB project File from the original C# project file, which should be easier under Core 3.0 since
it is almost empty.
There are several Stop statements for debugging, and if you continue past them reasonable things will
happen but a better translation is possible.
The program does not deal well with bad C# code.