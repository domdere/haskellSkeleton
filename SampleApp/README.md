# TestMain

Serves as a test executable to demonstrate how to build an **Haskell** executable,

## CMakeLists.txt

The most important step is adding the executable to the overall project:

    ADD_HASKELL_EXECUTABLE_TARGET ( testMain
        "Main.hs" )

Specify the license file to use, `cmake` will tell your build system to look in the `Licenses` directory for it:

    ADD_LICENSE_TARGET ( testMain "MIT-LICENSE" )

If your executable will require linking to a shared library built from a library in another language *that is part of the codebase for this project*, then let `cmake` know that that library is required to be placed in the same directory as the final binary (for distribution) for it to run with a line like this:

    INSTALL_CPP_RELOCATABLE_LIBRARY ( "testMain" "TestCLib" )

When adding dependencies for the building of the binary itself, you have to add it to the target `<executable target name>-executable` instead of just `<executable target name>` (Sorry, its convoluted I know):

    ADD_DEPENDENCIES ( testMain-executable TestLib TestFFI )

## README.md

Each executable requires a `README.md` file present in its directory (like this one), which will get placed in the same directory as the binary output for distribution (it will be renamed as `README`)

## package.json

A file called `package.json` is required in the folder too, `build-scripts/generateCabal.py` will use this file to generate the `.cabal` file to build the executable, its contents are like so (*comments will have to be removed*):


    {
        "name": "testMain", # the final name of the binary
        "version": "0.1.0.0", # current version
        "synopsis": "testproj", # description for the executable in case you wish to upload it to hackage
        "homepage": "http://nothing.com",
        "license": "MIT", # license description
        "author": "Author",
        "maintainer": "author@email.com",
        "category": "Text",
        "build-type": "Simple",
        "main-is": "Main.hs",
        "extra-lib-dirs": ["../TestCLib"], # In the case of the this example project, linking to external shared libs is required, this list tells them where to look for shared libs
        "extra-libraries": ["TestCLib"],   # Ditto, but it tells them which libraries within the above directories to look for
        "build-depends": "base ==4.6.*"    # comma seperated list of dependencies, as you use non-standard libs (that are available on hackage), 
                                           # you will have to add hackage package names here, or the application will not build at all, 
                                           # it will be useful for people installing your application/library from hackage too
    }
