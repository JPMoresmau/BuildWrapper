# BuildWrapper

[![Build Status](https://travis-ci.org/JPMoresmau/BuildWrapper.svg?branch=master)](https://travis-ci.org/JPMoresmau/BuildWrapper)

BuildWrapper is currently NOT maintained anymore. Feel free to fork and take up maintainership!

BuildWrapper is a program designed to help an IDE deal with a Haskell project. It combines several tools under a simple API:

- Cabal to configure and build the project. BuildWrapper works best on project that provide a cabal file.
- GHC to load a particular file or buffer and generate a typechecked AST
- Haskell-src-exts to parse a particular file and generate an outline

BuildWrapper provides a library and an executable. Intended usage is mostly through the executable. This is how it's used by EclipseFP, the set of Haskell development plugins for the Eclipse IDE.
This executable is *short-lived*, it is not a running server as scion (the project BuildWrapper is an evolution of) or scion-browser. You launch it with parameters, it perform some work, and returns a JSON result.
BuildWrapper uses a temporary work folder inside the project to store both a copy of the source files and the result of its operations. In an IDE setting, the content of the temporary folder may contain files representing unsaved data, which allow BuildWrapper to use Cabal and file based operations regardless.


You can run `buildwrapper --help` to get a feel for the different options you can call buildwrapper with. You can also run EclipseFP with the debug mode preference on to see the BuildWrapper interaction in an Eclipse console view.

## Troubleshooting
BuildWrapper may fail/crash if cabal-install was built against a different version of Cabal than the version of Cabal that BuildWrapper was built against.

## Generic options
These options are available to all commands.

- tempfolder: the name of the temporary folder, relative to the location of the project cabal file. Usually .dist-buildwrapper
- cabalpath: the location of the cabal executable
- cabalfile: the location of the project cabal file
- cabalflags: the flags to pass to cabal when configuring
- logcabal: should the call to the cabal executable be logged?

## synchronize
Synchronize ensures that all the files in the temporary work folder represent the up to date version of the source files. It returns the list of files actually copied from the main folder to the work folder.

- force: true/false: copies files even if destination (in temporary folder) is newer

Example: `buildwrapper synchronize --force=false --tempfolder=.dist-buildwrapper --cabalpath=/home/myuser/.cabal/bin/cabal --cabalfile=/home/myuser/myproject/myproject.cabal --cabalflags= --logcabal=true`


## synchronize1
Synchronizes only one file.

- file: the relative path of the file to synchronize (relative to the project root)
- force: true/false: copies files even if destination (in temporary folder) is newer

## write
Updates the content of the file in the work folder. Note that an external tool could also write directly in the work folder.

- file: the relative path of the file to update (relative to the project root)
- contents: the contents to write

## configure
Runs cabal configure on the project. This command usually is not needed, as the build command will trigger a configure if needed.
Returns the errors encountered, if any.

- verbosity: the verbosity of Cabal output
- cabaltarget: Source|Target: use the original cabal file or the one in the work folder

## build
Runs cabal build on the project.
Returns the errors encountered, if any, and the files processed during that build.

- verbosity: the verbosity of Cabal output
- cabaltarget: Source|Target: use the original cabal file or the one in the work folder
- output: true|false: should we actually run the linker and generates output

Example: `buildwrapper build --output=true --cabaltarget=Source --tempfolder=.dist-buildwrapper --cabalpath=/home/myuser/.cabal/bin/cabal --cabalfile=/home/myuser/myproject/myproject.cabal --cabalflags= --logcabal=true`


## build1
Build one file using the GHC API. BuildWrapper takes care of calling the API with the proper flags from the cabal file.
Returns the errors encountered during the build, if any.
The AST and the build flags used are stored in a hidden file alongside the source file in the work folder. This file, with the .bwinfo extension, is plain JSON and can be parsed by an external tool if need be.

- file: the relative path of the file to update (relative to the project root)

## getbuildflags
Returns the build flags use to build a particular file

- file: the relative path of the file to update (relative to the project root)

## outline
Returns an outline of the file: the top level declarations, the import and export statements. This is generated using haskell-src-exts so the file does not need to be correct in respect to the typechecker, but needs to be valid Haskell syntax. If need be, the file is pre-processed by cpp2hs.

- file: the relative path of the file to update (relative to the project root)

## tokentypes
Returns a collection of lexer tokens for a particular file. The tokens have a type assigned to them (documentation, symbols, etc). This is used to provide syntax coloring in an IDE. If need be, the file is pre-processed and the preprocessor tokens are returned in the collection too.

- file: the relative path of the file to update (relative to the project root)

## occurrences
Find all occurrences of the given text in lexer tokens. Only fully matching tokens are retrieved.

- file: the relative path of the file to update (relative to the project root)
- token: the token text to search for

## thingatpoint
Returns the object found at a particular point in a source. Information can include a name, a module, a type, a haddock type code.
This uses the generated .bwinfo file to perform the search so does not invoke the GHC API unless necessary (.bwinfo file missing or older than source)

- file: the relative path of the file to update (relative to the project root)
- line: the line to look at
- column: the column to look at

## namesinscope
Returns the list of names in scope (GHC API call)

- file: the relative path of the file to update (relative to the project root)

## locals
Returns the list of names defined locally to a point in the source (inside the function, say)

- file: the relative path of the file to update (relative to the project root)
- sline: the start line of the defining scope
- scolumn: the start column of the defining scope
- eline: the end line of the defining scope
- ecolumn: the end column of the defining scope

## cleanimports
Returns the cleaned imports line: the location and import text for the minimal required imports

- file: the relative path of the file to update (relative to the project root)

## dependencies
Returns the list of all package dependencies for all cabal components in the cabal file, with the package database they are registered in

## components
Returns the list of all components of the cabal file (executables, library, test suites)
