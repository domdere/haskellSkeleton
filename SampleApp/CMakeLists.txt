ADD_HASKELL_EXECUTABLE_TARGET ( sampleApp
    "CommandLineOptions.hs"
    "Main.hs" )

ADD_LICENCE_TARGET (sampleApp "MIT-LICENSE")

INSTALL_CPP_RELOCATABLE_LIBRARY ( "sampleApp" "TestCLib" )

ADD_DEPENDENCIES ( sampleApp-executable 
    Logging
    LogOptions
    TestLib
    TestFFI )
