ADD_HASKELL_EXECUTABLE_TARGET ( testMain
    "main.hs" )

ADD_LICENCE_TARGET (testMain "MIT-LICENSE")

INSTALL_CPP_RELOCATABLE_LIBRARY ( "testMain" "TestCLib" )

ADD_DEPENDENCIES ( testMain-executable TestLib TestFFI )
