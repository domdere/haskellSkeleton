OPTION ( HASKELL_MACRO_DEBUG "Debug for Haskell Project Macros" )

MACRO ( GET_EXECUTABLE_NAME input outputVar)
    IF ( WIN32 )
        IF ( HASKELL_MACRO_DEBUG )
            MESSAGE ( STATUS "Executable: ${input}.exe" )
        ENDIF ( HASKELL_MACRO_DEBUG )
        SET ( ${outputVar} "${input}.exe" )
    ELSE ( WIN32 )
        IF ( HASKELL_MACRO_DEBUG )
            MESSAGE ( STATUS "Executable: ${input}" )
        ENDIF ( HASKELL_MACRO_DEBUG )
        SET ( ${outputVar} "${input}" )
    ENDIF ( WIN32 )
ENDMACRO ( GET_EXECUTABLE_NAME )

MACRO ( GET_STATIC_LIBRARY_NAME input outputVar)
    IF ( WIN32 )
        SET ( ${outputVar} "${input}.lib" )
    ELSE ( WIN32 )
        SET ( ${outputVar} "${input}.a" )
    ENDIF ( WIN32 )
ENDMACRO ( GET_STATIC_LIBRARY_NAME )

MACRO ( GET_RELOCATABLE_LIBRARY_NAME input outputVar)
    IF ( WIN32 )
        SET ( ${outputVar} "${input}.dll" )
    ELSE ( WIN32 )
        SET ( ${outputVar} "${input}.so" )
    ENDIF ( WIN32 )
ENDMACRO ( GET_RELOCATABLE_LIBRARY_NAME )

MACRO ( ADD_SRC_PATH var )

    SET (${var} )

    FOREACH ( srcFile ${ARGN} )
        SET (${var} ${var} "${ROOT_SRC_DIR}/${srcFile}")
    ENDFOREACH ( srcFile srcFiles )

ENDMACRO ( ADD_SRC_PATH var )

MACRO ( ADD_BIN_PATH var )

    SET (${var} )

    FOREACH ( srcFile ${ARGN} )
        SET (${var} ${var} "${ROOT_BIN_DIR}/${srcFile}")
    ENDFOREACH ( srcFile srcFiles )

ENDMACRO ( ADD_BIN_PATH var )


MACRO ( BUILD_WITH_CABAL projectName projectOutput )

    ADD_SRC_PATH ( SRC_DEPENDS ${ARGN})

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Setting up cabal target:" )
        MESSAGE ( STATUS "  --Project Name: ${projectName}" )
        MESSAGE ( STATUS "  --Target Output: ${projectOutput}" )
        MESSAGE ( STATUS "  --Dependencies: ${ARGN}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    ADD_CUSTOM_COMMAND (
        OUTPUT ${projectOutput}
        COMMAND "${CABAL_EXECUTABLE}"
        ARGS "install"
            "--prefix" "${ROOT_BIN_DIR}/${projectName}"
        DEPENDS ${ARGN}
        WORKING_DIRECTORY "${ROOT_SRC_DIR}/${projectName}" )

    SET ( SRC_DEPENDS )

ENDMACRO ( BUILD_WITH_CABAL )

MACRO ( ADD_HASKELL_EXECUTABLE_TARGET projectName )

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Attempting to add executable project: ${projectName}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    GET_EXECUTABLE_NAME ( "${ROOT_BIN_DIR}/${projectName}" ${projectName}_EXECUTABLE )

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Trying to create rule to make ${${projectName}_EXECUTABLE}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    BUILD_WITH_CABAL ( ${projectName} 
        ${${projectName}_EXECUTABLE} 
        ${ARGN} )

    ADD_CUSTOM_TARGET ( ${projectName} ALL DEPENDS
        "${${projectName}_EXECUTABLE}" )

ENDMACRO ( ADD_HASKELL_EXECUTABLE_TARGET )

MACRO ( HASKELL_LIBRARY srcFiles )
ENDMACRO ( HASKELL_LIBRARY srcFiles )

