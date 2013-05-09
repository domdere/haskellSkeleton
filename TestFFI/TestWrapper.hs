{-# LANGUAGE ForeignFunctionInterface #-}
module TestFFI.TestWrapper(hello_world) where

import Foreign
import Foreign.C.Types


foreign import ccall "TestClib/test.h hello_world"
    c_hello_world :: IO ()

hello_world :: IO ()
hello_world = c_hello_world
