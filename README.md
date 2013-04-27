# Haskell Skeleton

A starting point for a Haskell project

## Intended Features

* Easy integration of C/C++ libraries:
    * When you are finding that you are dealing with so much data that you are getting Stack Overflow exceptions and you can't structure your functions/algorithms to avoid it, then you can implement it as a procedural function in a C library and the build files will be able to build it and link it into the final binary.  Keep the C functions pure.
    * Sometimes it is imperative to create objects that do I/O and keep internal state and as such are not pure (for instance a Resource Loader that caches resources.  You can implement them in a C++ library and the build system should be able to integrate them into your binary