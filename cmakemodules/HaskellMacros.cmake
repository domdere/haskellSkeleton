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

MACRO ( ADD_PATH pathStr var )

    SET (${var} )

    FOREACH ( srcFile ${ARGN} )
        SET (${var} ${${var}} "${pathStr}/${srcFile}")
    ENDFOREACH ( srcFile srcFiles )

ENDMACRO ( ADD_PATH var )

MACRO ( ADD_SRC_PATH var )

    SET (${var} )

    FOREACH ( srcFile ${ARGN} )
        SET (${var} ${${var}} "${ROOT_SRC_DIR}/${srcFile}")
    ENDFOREACH ( srcFile srcFiles )

ENDMACRO ( ADD_SRC_PATH var )

MACRO ( ADD_BIN_PATH var )

    SET (${var} )

    FOREACH ( srcFile ${ARGN} )
        SET (${var} ${var} "${ROOT_BIN_DIR}/${srcFile}")
    ENDFOREACH ( srcFile srcFiles )

ENDMACRO ( ADD_BIN_PATH var )


MACRO ( BUILD_WITH_CABAL projectName projectOutput )

    ADD_PATH ( "${ROOT_BIN_DIR}/${projectName}" SRC_DEPENDS ${ARGN})

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Setting up cabal target:" )
        MESSAGE ( STATUS "  --Project Name: ${projectName}" )
        MESSAGE ( STATUS "  --Target Output: ${projectOutput}" )
        MESSAGE ( STATUS "  --Dependencies: ${SRC_DEPENDS}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    ADD_CUSTOM_COMMAND (
        OUTPUT ${projectOutput}
        COMMAND "${CABAL_EXECUTABLE}"
        ARGS "install"
            "--prefix" "${ROOT_BIN_DIR}/${projectName}"
        DEPENDS "${SRC_DEPENDS}"
        WORKING_DIRECTORY "${ROOT_BIN_DIR}/${projectName}" )
    SET ( SRC_DEPENDS )

ENDMACRO ( BUILD_WITH_CABAL )

MACRO ( COPY_FILES src dest )

    ADD_CUSTOM_COMMAND (
        OUTPUT "${dest}/${filename}"
        COMMAND "cp"
        ARGS "-v"
            "${ARGN}" "${dest}"
        DEPENDS "${ARGN}"
        WORKING_DIRECTORY "${src}" )

ENDMACRO ( COPY_FILES src dest )

MACRO ( COPY_FILES_SRC_TO_BIN projectName)

    ADD_SRC_PATH ( ${projectName}_WITH_SRC_PATH )
    
    FOREACH (filename ${ARGN} )

        IF ( HASKELL_MACRO_DEBUG )
            MESSAGE ( STATUS "Adding target to create ${ROOT_BIN_DIR}/${projectName}/${filename} from ${ROOT_SRC_DIR}/${projectName}/${filename}" )
        ENDIF ( HASKELL_MACRO_DEBUG )

        ADD_CUSTOM_COMMAND (
            OUTPUT "${ROOT_BIN_DIR}/${projectName}/${filename}"
            COMMAND "cp"
            ARGS "-v"
                "${filename}" "${ROOT_BIN_DIR}/${projectName}/${filename}"
            DEPENDS "${ROOT_SRC_DIR}/${projectName}/${filename}"
            WORKING_DIRECTORY "${ROOT_SRC_DIR}/${projectName}" )

    ENDFOREACH (filename ${ARGN} )

ENDMACRO ( COPY_FILES_SRC_TO_BIN projectName)

MACRO ( ADD_DOCUMENTATION_TARGET projectName )

    ADD_CUSTOM_TARGET (
        ${projectName}-doc
        COMMAND "${HADDOCK_EXECUTABLE}"
            "-h"
            -o "${ROOT_BIN_DIR}/${projectName}/doc/"
            "${projectName}/main.hs"
        DEPENDS ${projectName}
        WORKING_DIRECTORY "${ROOT_BIN_DIR}" )

ENDMACRO ( ADD_DOCUMENTATION_TARGET projectName )


MACRO ( ADD_HASKELL_EXECUTABLE_TARGET projectName )

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Attempting to add executable project: ${projectName}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    GET_EXECUTABLE_NAME ( "${ROOT_BIN_DIR}/${projectName}/bin/${projectName}" ${projectName}_EXECUTABLE )

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Trying to create rule to make ${${projectName}_EXECUTABLE}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    ADD_SRC_PATH ( ${projectName}_SOURCES ${ARGN} )

    COPY_FILES_SRC_TO_BIN ( ${projectName} ${ARGN} )

    CABAL_TARGET ( ${projectName} )

    BUILD_WITH_CABAL ( ${projectName} 
        ${${projectName}_EXECUTABLE}
        ${ARGN} )

    ADD_CUSTOM_TARGET ( ${projectName} ALL 
        DEPENDS "${${projectName}_EXECUTABLE}"
            "${ROOT_BIN_DIR}/${projectName}/${projectName}.cabal" )

    ADD_README_TARGET ( ${projectName} )

    ADD_DOCUMENTATION_TARGET ( ${projectName} )

ENDMACRO ( ADD_HASKELL_EXECUTABLE_TARGET )

MACRO ( ADD_HASKELL_LIBRARY projectName )
    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Attempting to add haskell library/module: ${projectName}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    ADD_PATH ( "${ROOT_BIN_DIR}/${projectName}" ${projectName}_DEPENDS ${ARGN})

    COPY_FILES_SRC_TO_BIN ( ${projectName} ${ARGN} )

    ADD_CUSTOM_TARGET ( ${projectName} ALL 
        DEPENDS "${${projectName}_DEPENDS}" )

ENDMACRO ( ADD_HASKELL_LIBRARY projectNames )

MACRO ( CABAL_TARGET projectName )

    ADD_CUSTOM_COMMAND ( 
        OUTPUT "${ROOT_BIN_DIR}/${projectName}/${projectName}.cabal"
        COMMAND "${PYTHON_EXECUTABLE}"
        ARGS "generateCabal.py"
            "${ROOT_SRC_DIR}/${projectName}/package.json"
            "${ROOT_BIN_DIR}/${projectName}/${projectName}.cabal"
        DEPENDS "${ROOT_SRC_DIR}/build-scripts/generateCabal.py"
        WORKING_DIRECTORY "${ROOT_SRC_DIR}/build-scripts/" )

ENDMACRO ( CABAL_TARGET projectName )

MACRO ( ADD_LICENCE_TARGET projectName license )

    ADD_CUSTOM_COMMAND (
        OUTPUT "${ROOT_BIN_DIR}/${projectName}/LICENSE"
        COMMAND "cp"
        ARGS "-v"
            "${license}" "${ROOT_BIN_DIR}/${projectName}/LICENSE"
        DEPENDS "${ROOT_SRC_DIR}/Licences/${license}"
        WORKING_DIRECTORY "${ROOT_SRC_DIR}/Licences" )

    ADD_CUSTOM_TARGET ( ${projectName}-license ALL 
        DEPENDS "${ROOT_BIN_DIR}/${projectName}/LICENSE" )

    ADD_DEPENDENCIES ( ${projectName} ${projectName}-license )


ENDMACRO ( ADD_LICENCE_TARGET projectName license )

MACRO ( ADD_README_TARGET projectName )

    ADD_CUSTOM_COMMAND (
        OUTPUT "${ROOT_BIN_DIR}/${projectName}/README"
        COMMAND "cp"
        ARGS "-v"
            "README" "${ROOT_BIN_DIR}/${projectName}/README"
        DEPENDS "${ROOT_SRC_DIR}/${projectName}/README"
        WORKING_DIRECTORY "${ROOT_SRC_DIR}/${projectName}" )

    ADD_CUSTOM_TARGET ( ${projectName}-readme ALL 
        DEPENDS "${ROOT_BIN_DIR}/${projectName}/README" )

    ADD_DEPENDENCIES ( ${projectName} ${projectName}-readme )

ENDMACRO ( ADD_README_TARGET projectName )
