ADD_HASKELL_LIBRARY ( TestFFI
    "ConstBindings.hsc"
    "TestWrapper.hs")

ADD_DEPENDENCIES ( TestFFI
    TestCLib )

ADD_HSC2HS_TARGET ( "TestFFI" "ConstBindings" ) 
