{-# LANGUAGE ForeignFunctionInterface #-}

module TestFFI where

import Foreign
import Foreign.C.Types

--newType TestConstantClass = TestConstantClassConstructor { testConstant :: CInt }
--    deriving (Eq, Show)

#{enum TestConstantClass, TestConstantClassConstructor
, testValue = TEST_CONST
, testValue2 = TEST_CONST_2
}

#include "TestCLib/test.h"
