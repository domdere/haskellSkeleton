MACRO ( GET_EXECUTABLE_NAME input outputVar)
    IF ( WIN32 )
        SET ( ${outputVar} "${input}.exe" )
    ELSE ( WIN32 )
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

MACRO ( BUILD_WITH_CABAL projectName projectOutput )

    ADD_SRC_PATH ( SRC_DEPENDS ${ARGN})

    ADD_CUSTOM_COMMAND (
        OUTPUT ${projectOutput}
        COMMAND "${CABAL_EXECUTABLE}"
        ARGS "install"
            "--prefix" "${ROOT_BIN_DIR}/${projectName}"
        DEPENDS ${SRC_DEPENDS},
        WORKING_DIRECTORY "${ROOT_SRC_DIR}/${projectName}" )

    SET ( SRC_DEPENDS )

ENDMACRO ( BUILD_WITH_CABAL )

MACRO ( ADD_HASKELL_EXECTUABLE_TARGET projectName )

    

ENDMACRO ( ADD_HASKELL_EXECTUABLE_TARGET )

MACRO ( HASKELL_LIBRARY srcFiles )
ENDMACRO ( HASKELL_LIBRARY srcFiles )

