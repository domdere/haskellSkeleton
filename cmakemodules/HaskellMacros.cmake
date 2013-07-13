OPTION ( HASKELL_MACRO_DEBUG "Debug for Haskell Project Macros" OFF )
OPTION ( ENABLE_HASKELL_PROFILING "Set to true to add profiling bookkeeping to the binaries" OFF )
OPTION ( ENABLE_HASKELL_PARALLEL_PROFILING "Set to true to add bookkeeping for profiling parallelism to the binaries" OFF )

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
        SET ( ${outputVar} "lib${input}.dll" )
    ELSE ( WIN32 )
        SET ( ${outputVar} "lib${input}.so" )
    ENDIF ( WIN32 )
ENDMACRO ( GET_RELOCATABLE_LIBRARY_NAME )

MACRO ( ADD_PATH pathStr var )

    SET ( ${var} )

    FOREACH ( srcFile ${ARGN} )
        SET (${var} ${${var}} "${pathStr}/${srcFile}")
    ENDFOREACH ( srcFile srcFiles )

ENDMACRO ( ADD_PATH var )

MACRO ( ADD_SRC_PATH var )

    SET (${var} )

    FOREACH ( srcFile ${ARGN} )
        SET (${var} ${${var}} "${CMAKE_CURRENT_SOURCE_DIR}/${srcFile}")
    ENDFOREACH ( srcFile srcFiles )

ENDMACRO ( ADD_SRC_PATH var )

MACRO ( ADD_BIN_PATH var )

    SET (${var} )

    FOREACH ( srcFile ${ARGN} )
        SET (${var} ${var} "${CMAKE_CURRENT_BINARY_DIR}/${srcFile}")
    ENDFOREACH ( srcFile srcFiles )

ENDMACRO ( ADD_BIN_PATH var )


MACRO ( BUILD_WITH_CABAL projectName projectOutput )

    ADD_PATH ( "${CMAKE_CURRENT_BINARY_DIR}" SRC_DEPENDS ${ARGN})

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Setting up cabal target:" )
        MESSAGE ( STATUS "  --Project Name: ${projectName}" )
        MESSAGE ( STATUS "  --Target Output: ${projectOutput}" )
        MESSAGE ( STATUS "  --Dependencies: ${SRC_DEPENDS}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    IF ( ENABLE_HASKELL_PROFILING )
        SET ( ${projectName}_PROFILE_FLAG "--enable-executable-profiling" )
    ENDIF ( ENABLE_HASKELL_PROFILING )

    ADD_CUSTOM_TARGET ( ${projectName}-preprocessing
        DEPENDS ${SRC_DEPENDS} )

    ADD_CUSTOM_COMMAND (
        OUTPUT ${projectOutput}
        COMMAND "${CABAL_EXECUTABLE}"
        ARGS "install"
            ${${projectName}_PROFILE_FLAG}
            "--prefix" "${CMAKE_CURRENT_BINARY_DIR}"
        DEPENDS ${projectName}-preprocessing "${CMAKE_CURRENT_BINARY_DIR}/${projectName}.cabal"
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" )
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
            MESSAGE ( STATUS "Adding target to create ${CMAKE_CURRENT_BINARY_DIR}/${filename} from ${CMAKE_CURRENT_SOURCE_DIR}/${filename}" )
        ENDIF ( HASKELL_MACRO_DEBUG )

        ADD_CUSTOM_COMMAND (
            OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${filename}"
            COMMAND "cp"
            ARGS "-v"
                "${filename}" "${CMAKE_CURRENT_BINARY_DIR}/${filename}"
            DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${filename}"
            WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" )

    ENDFOREACH (filename ${ARGN} )

ENDMACRO ( COPY_FILES_SRC_TO_BIN projectName)

MACRO ( ADD_DOCUMENTATION_TARGET projectName )

    ADD_CUSTOM_TARGET (
        ${projectName}-doc
        COMMAND "${HADDOCK_EXECUTABLE}"
            "-h"
            -o "${CMAKE_CURRENT_BINARY_DIR}/doc/"
            "Main.hs"
        DEPENDS ${projectName}
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" )

ENDMACRO ( ADD_DOCUMENTATION_TARGET projectName )


MACRO ( ADD_HASKELL_EXECUTABLE_TARGET projectName )

    CABAL_TARGET ( ${projectName} )

    ADD_HASKELL_EXECUTABLE_TARGET_WITH_CABAL ( ${projectName} ${ARGN})
    
ENDMACRO ( ADD_HASKELL_EXECUTABLE_TARGET )

MACRO ( ADD_HASKELL_EXECUTABLE_TARGET_WITH_CABAL projectName )

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Attempting to add executable project: ${projectName}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    GET_EXECUTABLE_NAME ( "${CMAKE_CURRENT_BINARY_DIR}/bin/${projectName}" ${projectName}_EXECUTABLE )

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Trying to create rule to make ${${projectName}_EXECUTABLE}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    ADD_SRC_PATH ( ${projectName}_SOURCES ${ARGN} )

    COPY_FILES_SRC_TO_BIN ( ${projectName} ${ARGN} )

    BUILD_WITH_CABAL ( ${projectName} 
        ${${projectName}_EXECUTABLE}
        ${ARGN} )

    ADD_CUSTOM_TARGET ( ${projectName}-executable ALL 
        DEPENDS "${${projectName}_EXECUTABLE}" )

    ADD_CUSTOM_TARGET ( ${projectName} ALL 
        DEPENDS "${projectName}-executable" )

    ADD_README_TARGET ( ${projectName} )

    ADD_DOCUMENTATION_TARGET ( ${projectName} )

ENDMACRO ( ADD_HASKELL_EXECUTABLE_TARGET_WITH_CABAL )


MACRO ( ADD_HASKELL_LIBRARY projectName )
    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "Attempting to add haskell library/module: ${projectName}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    ADD_PATH ( "${CMAKE_CURRENT_BINARY_DIR}" ${projectName}_DEPENDS ${ARGN})

    IF ( HASKELL_MACRO_DEBUG )
        MESSAGE ( STATUS "File Dependencies for ${projectName}: ${${projectName}_DEPENDS}" )
    ENDIF ( HASKELL_MACRO_DEBUG )

    COPY_FILES_SRC_TO_BIN ( ${projectName} ${ARGN} )

    ADD_CUSTOM_TARGET ( ${projectName} ALL 
        DEPENDS ${${projectName}_DEPENDS} )

ENDMACRO ( ADD_HASKELL_LIBRARY projectNames )

MACRO ( CABAL_TARGET projectName )

    IF ( UNIX )
        # Tells the binary to search in its own directory for shared libs
        SET ( ${projectName}_SYSTEMOPT "--unix" )
    ENDIF ( UNIX )

    IF ( ENABLE_HASKELL_PARALLEL_PROFILING )
        SET ( ${projectName}_PARALLEL_PROFILE_FLAG "--profiling" )
    ENDIF ( ENABLE_HASKELL_PARALLEL_PROFILING )

    IF ( ${CMAKE_BUILD_TYPE} STREQUAL "Release" OR ${CMAKE_BUILD_TYPE} STREQUAL "RelWithDebInfo" )
        # Tells the binary to search in its own directory for shared libs
        SET ( ${projectName}_RELEASE_FLAG "--release" )
    ENDIF ( ${CMAKE_BUILD_TYPE} STREQUAL "Release" OR ${CMAKE_BUILD_TYPE} STREQUAL "RelWithDebInfo" )

    ADD_CUSTOM_COMMAND ( 
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${projectName}.cabal"
        COMMAND "${PYTHON_EXECUTABLE}"
        ARGS "generateCabal.py"
            "${${projectName}_SYSTEMOPT}"
            "${${projectName}_RELEASE_FLAG}"
            "${${projectName}_PARALLEL_PROFILE_FLAG}"
            "--extra-libraries=\"${${projectName}_EXTRA_LIBS}\""
            "--extra-lib-dirs=\"${${projectName}_EXTRA_LIB_DIRS}\""
            "${CMAKE_CURRENT_SOURCE_DIR}/package.json"
            "${CMAKE_CURRENT_BINARY_DIR}/${projectName}.cabal"
        DEPENDS "${ROOT_SRC_DIR}/build-scripts/generateCabal.py"
            "${CMAKE_CURRENT_SOURCE_DIR}/package.json"
        WORKING_DIRECTORY "${ROOT_SRC_DIR}/build-scripts/" )

ENDMACRO ( CABAL_TARGET projectName )

MACRO ( ADD_LICENCE_TARGET projectName license )

    ADD_CUSTOM_COMMAND (
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/LICENSE"
        COMMAND "cp"
        ARGS "-v"
            "${license}" "${CMAKE_CURRENT_BINARY_DIR}/LICENSE"
        DEPENDS "${ROOT_SRC_DIR}/Licences/${license}"
        WORKING_DIRECTORY "${ROOT_SRC_DIR}/Licences" 
        COMMENT "Installing LICENSE file for ${projectName}" )

    ADD_CUSTOM_TARGET ( ${projectName}-license ALL 
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/LICENSE" )

    ADD_DEPENDENCIES ( ${projectName}-executable ${projectName}-license )


ENDMACRO ( ADD_LICENCE_TARGET projectName license )

MACRO ( ADD_README_TARGET projectName )

    ADD_CUSTOM_COMMAND (
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/README"
        COMMAND "cp"
        ARGS "-v"
            "README.md" "${CMAKE_CURRENT_BINARY_DIR}/README"
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/README.md"
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        COMMENT "Installing README file for ${projectName}" )

    ADD_CUSTOM_TARGET ( ${projectName}-readme ALL 
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/README" )

    ADD_DEPENDENCIES ( ${projectName}-executable ${projectName}-readme )

ENDMACRO ( ADD_README_TARGET projectName )

MACRO ( ADD_HSC2HS_TARGET projectName hscFileSansExtension )

    ADD_CUSTOM_COMMAND (
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${hscFileSansExtension}.hs"
        COMMAND "${HSC2HS_EXECUTABLE}"
        ARGS "-I${ROOT_SRC_DIR}" 
            "-I${BINDINGS_DSL_INCLUDE}"
            "-o" "${CMAKE_CURRENT_BINARY_DIR}/${hscFileSansExtension}.hs"
            "${projectName}/${hscFileSansExtension}.hsc"
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${hscFileSansExtension}.hsc"
        WORKING_DIRECTORY "${ROOT_BIN_DIR}" 
        COMMENT "Preprocessing ${hscFileSansExtension}.hsc -> ${hscFileSansExtension}.hs" )

    ADD_CUSTOM_TARGET ( ${projectName}-${hscFileSansExtension} ALL 
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${hscFileSansExtension}.hs" )

    ADD_DEPENDENCIES ( ${projectName} 
        ${projectName}-${hscFileSansExtension} )

ENDMACRO ( ADD_HSC2HS_TARGET projectName hscFileSansExtension )

# For libraries that are part of the same project only...
MACRO ( INSTALL_CPP_RELOCATABLE_LIBRARY projectName library )

    GET_RELOCATABLE_LIBRARY_NAME( ${library} ${library}_NAME )
    GET_EXECUTABLE_NAME ( "${CMAKE_CURRENT_BINARY_DIR}/bin/${projectName}" ${projectName}_EXECUTABLE )

    ADD_CUSTOM_COMMAND (
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/bin/${${library}_NAME}"
        COMMAND "cp"
        ARGS "-v"
            "${ROOT_BIN_DIR}/${library}/${${library}_NAME}" "${CMAKE_CURRENT_BINARY_DIR}/bin/${${library}_NAME}"
        DEPENDS ${library} "${${projectName}_EXECUTABLE}"
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        COMMENT "Installing library ${${library}_NAME} for ${projectName}" )

    ADD_CUSTOM_TARGET ( ${projectName}-${library} ALL 
        DEPENDS ${projectName}-executable
            "${CMAKE_CURRENT_BINARY_DIR}/bin/${${library}_NAME}" )

    ADD_DEPENDENCIES ( ${projectName} 
        ${projectName}-${library} )

ENDMACRO ( INSTALL_CPP_RELOCATABLE_LIBRARY projectName library )

MACRO ( ADD_HASKELL_EXECUTABLE_DEPENDENCIES projectName )
    
    ADD_DEPENDENCIES ( ${projectName}-executable ${ARGN} )

ENDMACRO ( ADD_HASKELL_EXECUTABLE_DEPENDENCIES projectName )
