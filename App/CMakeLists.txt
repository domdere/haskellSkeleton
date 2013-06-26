ADD_HASKELL_EXECUTABLE_TARGET ( app
    "Main.hs"
    "AppState.hs" )

ADD_LICENCE_TARGET (app "MIT-LICENSE")

#INSTALL_CPP_RELOCATABLE_LIBRARY ( "app" "Blah" )

ADD_DEPENDENCIES ( app-executable
    Routes
    Handlers )
