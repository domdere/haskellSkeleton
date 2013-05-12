# Adding C Libraries to the Project

Say you have written up a C library in your project, using the Haskell Macros here,
you can get it linking to your main executable with the following steps:

For the sake of argument, lets assume the C Library is named `TestCLib` (and its in a directory with the same name)

-   Make sure the library is added as a *shared* library in the `CMakeLists.txt` file for the library, for e.g:

        ADD_LIBRARY( TestCLib SHARED header.h src.c)

-   Add `TestCLib` as a dependency for any haskell library/executable that uses it directly, for e.g:

        ADD_DEPENDENCIES ( TestHaskellLibrary TestCLib )

-   If the name of the project fo rthe executable using it is `TestMain`, then in `TestMain/package.json`, make sure you have:
    -   `../TestCLib` in `extra-lib-dirs` - The path relative to `TestMain/` where the library is (technically its the path relative to where `TestMain/` is in the build directory, which is where the built library will end up, but the relevent directory structure here will be mimiced from the source directory anyway)
    -   `TestCLib` in `extra-libraries` - This will Eventually tell cabal to include `libTestCLib.so` in the list of files for the linker to look through when it finds functions it cant link.

-   In the `TestMain/CMakeLists.txt` add a line like this:

        INSTALL_CPP_RELOCATABLE_LIBRARY( "TestMain" "TestCLib")

    To make deployment easier, the binary will have the `.` or `$ORIGIN` path added to its `RPATH`.  So  no matter where you put the binary, it will look in its own directory for any libraries it cant find in the standard system locations, this target will make sure the built library file for `TestCLib` will be copied to the same directory as the built binary so that deployment is simple (just bundle the contents of that directory) and the binary will work as soon as the target is built for debugging/testing and what not.
