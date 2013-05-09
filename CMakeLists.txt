CMAKE_MINIMUM_REQUIRED(  VERSION 2.8 )

# Change this project name to whatever you desire, will require that you go through
# and refactor all instances of PROJECTNAME_SOURCE_DIR and PROJECTNAME_BINARY_DIR though

PROJECT ( PROJECTNAME )

MESSAGE ( "*** Source directory detected as: ${PROJECTNAME_SOURCE_DIR}" )
MESSAGE ( "*** Build directory detected as: ${PROJECTNAME_BINARY_DIR}" ) 

SET ( ROOT_SRC_DIR "${PROJECTNAME_SOURCE_DIR}" )
SET ( ROOT_BIN_DIR "${PROJECTNAME_BINARY_DIR}" )

SET ( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${ROOT_SRC_DIR}/cmakemodules/") 

# valid values for CMAKE_BUILD_TYPE:
# -DCMAKE_BUILD_TYPE:STRING=Release
# -DCMAKE_BUILD_TYPE:STRING=Debug

IF ( NOT CMAKE_BUILD_TYPE )
    MESSAGE ( STATUS "Build Type: Unset, assuming Debug" )
    SET (CMAKE_BUILD_TYPE "Debug" )
ELSE ( NOT CMAKE_BUILD_TYPE )
    MESSAGE ( STATUS "Build Type: ${CMAKE_BUILD_TYPE}" )
ENDIF ( NOT CMAKE_BUILD_TYPE )

INCLUDE ( CheckTypeSize )

SET ( CMAKE_REQUIRED_FLAGS "${CMAKE_CXX_FLAGS}" )

CHECK_TYPE_SIZE ( "void*" PTR_SIZE )

IF ( PTR_SIZE EQUAL 4 )
    SET ( IS32BIT TRUE )
    SET ( BITNESS "32-Bit" )
ELSE ( PTR_SIZE EQUAL 4 )
    SET ( IS32BIT FALSE )
    SET ( BITNESS "64-Bit" )
ENDIF ( PTR_SIZE EQUAL 4 )

MESSAGE ( STATUS "${BITNESS} system detected" )

MESSAGE ( STATUS "System Name: ${CMAKE_SYSTEM_NAME}" )

MACRO ( ADD_SUBPROJECT directory )
    MESSAGE ( STATUS "Adding subproject: ${directory}" )
    ADD_SUBDIRECTORY ( ${directory} )
ENDMACRO ( ADD_SUBPROJECT directory )

SET ( DESIRED_BOOST_VERSION "1_49_0" )

MACRO ( DISPLAY_BOOST_LIB_INFO )
    FOREACH ( component ${ARGN} )
        STRING ( TOUPPER ${component} upperComponent )
        
        IF ( Boost_${upperComponent}_FOUND )
            MESSAGE ( STATUS "${component} libs found at: ${Boost_${upperComponent}_LIBRARY}" )
            MESSAGE ( STATUS "${component} debug libs found at: ${Boost_${upperComponent}_LIBRARY_DEBUG}" )
            MESSAGE ( STATUS "${component} release libs found at: ${Boost_${upperComponent}_LIBRARY_RELEASE}" )
        ELSE ( Boost_${upperComponent}_FOUND )
            MESSAGE ( ERROR " ${component} libs NOT found!" )
        ENDIF ( Boost_${upperComponent}_FOUND )
    ENDFOREACH ( component ${ARGN} )
ENDMACRO ( DISPLAY_BOOST_LIB_INFO )

MACRO ( DISPLAY_PYTHON_INTERP_INFO )

    IF ( PYTHONINTERP_FOUND )
        MESSAGE ( STATUS "Python ${PYTHON_VERSION_STRING} Interpreter was found at ${PYTHON_EXECUTABLE}" )
    ELSE ( PYTHONINTERP_FOUND )
        MESSAGE ( ERROR "Python Interpreter was NOT found, the build process requires it." ) 
    ENDIF ( PYTHONINTERP_FOUND )

ENDMACRO ( DISPLAY_PYTHON_INTERP_INFO )

MACRO ( DISPLAY_HASKELL_INFO )

    IF ( GHC_FOUND )
        MESSAGE ( STATUS "Haskell Compiler (version ${GHC_VERSION}) was found at: ${GHC_EXECUTABLE}" )
    ELSE ( GHC_FOUND )
        MESSAGE ( STATUS "Haskell Compiler was NOT Found!" )
    ENDIF ( GHC_FOUND )

    IF ( GHCI_FOUND )
        MESSAGE ( STATUS "Haskell Interpreter (version ${GHCI_VERSION}) was found at: ${GHCI_EXECUTABLE}" )
    ELSE ( GHCI_FOUND )
        MESSAGE ( STATUS "Haskell Interpreter was NOT Found!" )
    ENDIF ( GHCI_FOUND )

    IF ( CABAL_FOUND )
        MESSAGE ( STATUS "Cabal (version ${CABAL_VERSION}) was found at: ${CABAL_EXECUTABLE}" )
    ELSE ( CABAL_FOUND )
        MESSAGE ( STATUS "Cabal was NOT Found!" )
    ENDIF ( CABAL_FOUND )

    IF ( HADDOCK_FOUND )
        MESSAGE ( STATUS "Haddock (version ${HADDOCK_VERSION}) was found at: ${HADDOCK_EXECUTABLE}" )
    ELSE ( HADDOCK_FOUND )
        MESSAGE ( STATUS "Haddock was NOT Found!" )
    ENDIF ( HADDOCK_FOUND )

    IF ( HSC2HS_FOUND )
        MESSAGE ( STATUS "hsc2hs (version ${HSC2HS_VERSION}) was found at: ${HSC2HS_EXECUTABLE}" )
    ELSE ( HSC2HS_FOUND )
        MESSAGE ( STATUS "hsc2hs was NOT Found!" )
    ENDIF ( HSC2HS_FOUND )


ENDMACRO ( DISPLAY_HASKELL_INFO )

MACRO (DISPLAY_GIT_INFO )
    IF ( GIT_FOUND )
        MESSAGE ( STATUS "Git Vers.${GIT_VERSION_STRING} was found at: ${GIT_EXECUTABLE}" )
    ELSE ( GIT_FOUND )
        MESSAGE ( ERROR "Git was not found, its required for versioning" )
    ENDIF ( GIT_FOUND )
ENDMACRO (DISPLAY_GIT_INFO )

IF ( WIN32 )
    # need these for windows
    SET (MiscLib winmm.lib comctl32.lib)

    # need to set any special search paths for boost etc...?
ENDIF ( WIN32 )

IF ( ${CMAKE_SYSTEM_NAME} STREQUAL "Linux" )
    MESSAGE ( STATUS "Linux detected, telling compiler to link in pthreads..." )
    SET ( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread" )
ENDIF ( ${CMAKE_SYSTEM_NAME} STREQUAL "Linux" )
 

MESSAGE ( STATUS "Searching for third party libraries...." )

MESSAGE ( STATUS "Any find package CMake modules you wish to use can go in: ${CMAKE_MODULE_PATH}" )

MESSAGE ( STATUS "Boost: " )
# Disable auto linking, its a bit of a pain in windows.
ADD_DEFINITIONS ( -DBOOST_ALL_NO_LIB )

# want to use static boost libs?
IF (WIN32)
    SET ( Boost_USE_STATIC_LIBS ON )
ENDIF (WIN32)

IF ( CMAKE_COMPILER_IS_GNUCXX )
    SET ( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall" )
ENDIF ( CMAKE_COMPILER_IS_GNUCXX )

FIND_PACKAGE ( Boost REQUIRED COMPONENTS date_time thread unit_test_framework program_options)

IF ( Boost_FOUND )
    MESSAGE ( STATUS "Boost includes were found at: ${Boost_INCLUDE_DIR}" )
    MESSAGE ( STATUS "Boost libs were found at: ${Boost_LIBRARY_DIRS}" )
    MESSAGE ( STATUS "" )

DISPLAY_BOOST_LIB_INFO ( date_time thread unit_test_framework program_options )

ELSE ( Boost_FOUND )
    MESSAGE ( ERROR " Boost libs were NOT found!" )
ENDIF ( Boost_FOUND )

FIND_PACKAGE ( PythonInterp )

DISPLAY_PYTHON_INTERP_INFO ()

FIND_PACKAGE ( Haskell )

DISPLAY_HASKELL_INFO ()

OPTION ( USE_GIT_FOR_VERSIONING "Set to true if building from a Git repo so git versioning info can be provided, switch off if building from an exported archive" ON)

IF ( USE_GIT_FOR_VERSIONING )
    FIND_PACKAGE( Git )
    
    # I'll leave the macro in just in case, but the find package for git actually displays the same info
    # macro below displays...
    #DISPLAY_GIT_INFO()
ENDIF ( USE_GIT_FOR_VERSIONING )

# Main include directories:
INCLUDE_DIRECTORIES (
    ${ROOT_SRC_DIR}
    ${ROOT_BIN_DIR}
    ${Boost_INCLUDE_DIR} )

# LINK_DIRECTORIES is not necessary, use TARGET_LINK_DIRECTORIES with
# the absolute paths to the libs instead
# LINK_DIRECTORIES (
#    ${Boost_LIBRARY_DIRS}
#    ${Poco_LIBRARY_DIR} )

INCLUDE( HaskellMacros )

# Sub Projects:
#ADD_SUBPROJECT(sampleBinaryProject)
ADD_SUBPROJECT(testMain)
ADD_SUBPROJECT(TestLib)
ADD_SUBPROJECT(TestCLib)
ADD_SUBPROJECT(TestFFI)
