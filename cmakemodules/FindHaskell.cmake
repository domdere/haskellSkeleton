# Locate the Haskell Platform, so far only looks for ghc and ghci
# Only supports Linux at the moment
# This module defines
#  GHC_FOUND, if false, do not try to link to Log4cplus
#  GHCI_FOUND, if false, do not try to link to Log4cplus
#
#  GHC_EXECUTABLE
#  GHC_VERSION
#  GHC_VERSION_MAJOR
#  GHC_VERSION_MINOR
#  GHC_VERSION_PATCH
#  GHCI_EXECUTABLE
#  GHCI_VERSION
#  GHCI_VERSION_MAJOR
#  GHCI_VERSION_MINOR
#  GHCI_VERSION_PATCH
#  CABAL_EXECUTION
#  CABAL_FOUND
#  CABAL_VERSION
#  CABAL_VERSION_MAJOR
#  CABAL_VERSION_MINOR
#  CABAL_VERSION_PATCH
#  HADDOCK_FOUND
#  HADDOCK_VERSION
#  HADDOCK_VERSION_MAJOR
#  HADDOCK_VERSION_MINOR
#  HADDOCK_VERSION_PATCH

FIND_PROGRAM(GHC_EXECUTABLE ghc
  HINTS
  $ENV{HASKELL_DIR}
  #PATH_SUFFIXES bin
  PATHS
  /usr/bin
)

FIND_PROGRAM(GHCI_EXECUTABLE ghci
  HINTS
  $ENV{HASKELL_DIR}
  #PATH_SUFFIXES bin
  PATHS
  /usr/bin
)

FIND_PROGRAM(CABAL_EXECUTABLE cabal
  HINTS
  $ENV{HASKELL_DIR}
  #PATH_SUFFIXES bin
  PATHS
  /usr/bin
)

FIND_PROGRAM(HADDOCK_EXECUTABLE haddock
  HINTS
  $ENV{HASKELL_DIR}
  #PATH_SUFFIXES bin
  PATHS
  /usr/bin
)



OPTION (HASKELL_DEBUG "Debug mode for the CMake FindHaskell module." OFF)

IF ( HASKELL_DEBUG )
    MESSAGE ( STATUS "Haskell Compiler: ${GHC_EXECUTABLE}" )
    MESSAGE ( STATUS "Haskell Interpreter: ${GHCI_EXECUTABLE}" )
    MESSAGE ( STATUS "Cabal Build System: ${CABAL_EXECUTABLE}" )
    MESSAGE ( STATUS "Haddock Documentation Generation System: ${HADDOCK_EXECUTABLE}" )
ENDIF ( HASKELL_DEBUG )

INCLUDE( FindPackageHandleStandardArgs )
FIND_PACKAGE_HANDLE_STANDARD_ARGS( GHC DEFAULT_MSG GHC_EXECUTABLE )
FIND_PACKAGE_HANDLE_STANDARD_ARGS( GHCI DEFAULT_MSG GHCI_EXECUTABLE )
FIND_PACKAGE_HANDLE_STANDARD_ARGS( CABAL DEFAULT_MSG CABAL_EXECUTABLE )
FIND_PACKAGE_HANDLE_STANDARD_ARGS( HADDOCK DEFAULT_MSG HADDOCK_EXECUTABLE )

MARK_AS_ADVANCED( GHC_EXECUTABLE GHCI_EXECUTABLE CABAL_EXECUTABLE HADDOCK_EXECUTABLE )

# if either of them were found, determine the version, 
# (ideally they ought to be the same...)

MACRO (GET_HASKELL_VERSION haskell )
    EXECUTE_PROCESS (
        COMMAND "${${haskell}_EXECUTABLE}" --version
        OUTPUT_VARIABLE  ${haskell}_VERSION__
        RESULT_VARIABLE  ${haskell}_VERSION_RESULT__
        ERROR_QUIET)

    IF ( NOT ${haskell}_VERSION_RESULT__)
        # if the result was False/0 it means there was no error
        # and we have the version output in ${HASKELL}_VERSION__
        # and we just have to parse it out.

        IF ( HASKELL_DEBUG )
            MESSAGE ( STATUS "${${haskell}_EXECUTABLE} --version -> '${${haskell}_VERSION__}'" )
        ENDIF ( HASKELL_DEBUG )
         
        STRING ( REGEX REPLACE "^The Glorious Glasgow Haskell Compilation System, version ([0-9]+)\\.[0-9]+\\.[0-9]+.*" "\\1" ${haskell}_VERSION_MAJOR "${${haskell}_VERSION__}" )

        STRING ( REGEX REPLACE "^The Glorious Glasgow Haskell Compilation System, version [0-9]+\\.([0-9]+)\\.[0-9]+.*\\n" "\\1" ${haskell}_VERSION_MINOR "${${haskell}_VERSION__}" )

        STRING ( REGEX REPLACE "^The Glorious Glasgow Haskell Compilation System, version [0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" ${haskell}_VERSION_PATCH "${${haskell}_VERSION__}" )

        STRING ( REGEX REPLACE "^The Glorious Glasgow Haskell Compilation System, version ([\\.0-9^\\n]+).*" "\\1" ${haskell}_VERSION "${${haskell}_VERSION__}" )

    ENDIF ( NOT ${haskell}_VERSION_RESULT__)

ENDMACRO (GET_HASKELL_VERSION )

MACRO (GET_CABAL_VERSION )
    EXECUTE_PROCESS (
        COMMAND "${CABAL_EXECUTABLE}" --version
        OUTPUT_VARIABLE  CABAL_VERSION__
        RESULT_VARIABLE  CABAL_VERSION_RESULT__
        ERROR_QUIET )

    IF ( NOT CABAL_VERSION_RESULT__)
        # if the result was False/0 it means there was no error
        # and we have the version output in ${HASKELL}_VERSION__
        # and we just have to parse it out.

        IF ( HASKELL_DEBUG )
            MESSAGE ( STATUS "${CABAL_EXECUTABLE} --version -> '${CABAL_VERSION__}'" )
        ENDIF ( HASKELL_DEBUG )
         
        STRING ( REGEX REPLACE ".*using version ([0-9]+)\\.[0-9]+\\.[0-9]+ of the Cabal library.*" "\\1" CABAL_VERSION_MAJOR "${CABAL_VERSION__}" )

        STRING ( REGEX REPLACE ".*using version [0-9]+\\.([0-9]+)\\.[0-9]+ of the Cabal library.*" "\\1" CABAL_VERSION_MINOR "${CABAL_VERSION__}" )

        STRING ( REGEX REPLACE ".*using version [0-9]+\\.[0-9]+\\.([0-9]+) of the Cabal library.*" "\\1" CABAL_VERSION_PATCH "${CABAL_VERSION__}" )

        STRING ( REGEX REPLACE ".*using version ([\\.0-9^\\n]+) of the Cabal library.*" "\\1" CABAL_VERSION "${CABAL_VERSION__}" )

    ENDIF ( NOT CABAL_VERSION_RESULT__)

ENDMACRO (GET_CABAL_VERSION )

MACRO (GET_HADDOCK_VERSION )
    EXECUTE_PROCESS (
        COMMAND "${HADDOCK_EXECUTABLE}" --version
        OUTPUT_VARIABLE  HADDOCK_VERSION__
        RESULT_VARIABLE  HADDOCK_VERSION_RESULT__
        ERROR_QUIET )

    IF ( NOT HADDOCK_VERSION_RESULT__)
        # if the result was False/0 it means there was no error
        # and we have the version output in HADDOCK_VERSION__
        # and we just have to parse it out.

        IF ( HASKELL_DEBUG )
            MESSAGE ( STATUS "${HADDOCK_EXECUTABLE} --version -> '${HADDOCK_VERSION__}'" )
        ENDIF ( HASKELL_DEBUG )
         
        STRING ( REGEX REPLACE "^Haddock version ([0-9]+)\\.[0-9]+\\.[0-9]+.*" "\\1" HADDOCK_VERSION_MAJOR "${HADDOCK_VERSION__}" )

        STRING ( REGEX REPLACE "^Haddock version [0-9]+\\.([0-9]+)\\.[0-9]+.*" "\\1" HADDOCK_VERSION_MINOR "${HADDOCK_VERSION__}" )

        STRING ( REGEX REPLACE "^Haddock version [0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" HADDOCK_VERSION_PATCH "${HADDOCK_VERSION__}" )

        STRING ( REGEX REPLACE "^Haddock version ([\\.0-9^\\n]+).*" "\\1" HADDOCK_VERSION "${HADDOCK_VERSION__}" )

    ENDIF ( NOT HADDOCK_VERSION_RESULT__)

ENDMACRO (GET_HADDOCK_VERSION )


IF ( GHC_FOUND )
    IF ( HASKELL_DEBUG )
        MESSAGE ( STATUS "Attempting to find version for ghc.." )
    ENDIF ( HASKELL_DEBUG )
    
    GET_HASKELL_VERSION("GHC")

    IF ( GHC_VERSION )
        MESSAGE ( STATUS "ghc version determined to be: ${GHC_VERSION}" )
    ENDIF ( GHC_VERSION )
ENDIF ( GHC_FOUND )

IF ( GHCI_FOUND )
    IF ( HASKELL_DEBUG )
        MESSAGE ( STATUS "Attempting to find version for ghci.." )
    ENDIF ( HASKELL_DEBUG )
    
    GET_HASKELL_VERSION("GHCI")

    IF ( GHCI_VERSION )
        MESSAGE ( STATUS "ghci version determined to be: ${GHCI_VERSION}" )
    ENDIF ( GHCI_VERSION )
ENDIF ( GHCI_FOUND )

IF ( CABAL_FOUND )
    IF ( HASKELL_DEBUG )
        MESSAGE ( STATUS "Attempting to find version for cabal.." )
    ENDIF ( HASKELL_DEBUG )

    GET_CABAL_VERSION()

    IF ( CABAL_VERSION )
        MESSAGE ( STATUS "cabal version determined to be: ${CABAL_VERSION}")
    ENDIF ( CABAL_VERSION )

ENDIF ( CABAL_FOUND )

IF ( HADDOCK_FOUND )
    IF ( HASKELL_DEBUG )
        MESSAGE ( STATUS "Attempting to find version for haddock.." )
    ENDIF ( HASKELL_DEBUG )

    GET_HADDOCK_VERSION()

    IF ( HADDOCK_VERSION )
        MESSAGE ( STATUS "haddock version determined to be: ${HADDOCK_VERSION}")
    ENDIF ( HADDOCK_VERSION )

ENDIF ( HADDOCK_FOUND )
