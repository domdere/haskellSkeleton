# Haskell Skeleton

A starting point for a Haskell project

The **CMake** scripts use **Cabal** to build the Haskell executables and libraries but allow for **C/C++** projects to be part of the same build process

## Intended Features

-    Easy integration of **C/C++** libraries:
    -    The build system should be able to handle both the compilation and linking of **C/C++** and **Haskell** libraries that are included in the project
-    Easy integration of **Python** Libraries:
    -    The build system should be able to handle the byte code compilation of **Python** modules and embedding of that byte code and the **Python** interpreter in the final binary.

## Disclaimer

The **CMake** scripts have gotten far more convoluted than they probably have to be, they work though, perhaps someone else can get some better ideas from them.

Apart from the ability to add other CMake targets to your build, this doesnt offer much more than just adding a `c-sources` list to your `cabal` file.  Perhaps once it can embed python code it will have something to offer that will make the trouble worthwhile
