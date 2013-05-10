#!/usr/bin/python
import sys
sys.path.append('../python_modules/')

import json
import logging

from optparse import OptionGroup

import appUtils

def usageString():
    return '%prog <json config path> <output path for cabal file>'

def versionString():
    return '%prog: 1.0'

def appSetOpts(parser):
    
    cabalOptions = OptionGroup(
        parser, 
        'Cabal Option Options')

    cabalOptions.add_option('--additional-includes', type='string', dest='includes', default='',
        help='add additional include libraries, colon delimited', 
        metavar='LIBRARIES')

    cabalOptions.add_option('--additional-include-dirs', type='string', dest='includeDirs', default='',
        help='add additional include dirs, colon delimited', 
        metavar='LIBRARIES')

    parser.add_option_group(cabalOptions) 

    return


def generateCabalString(
    cfg,
    options):

    includeDirs = '.:..'
    linkDirs = ''
    linkLibs = ''

    cfgHasLinkDirs = len(cfg['link-library-dirs']) > 0
    optionsHaveIncludeDirs = len(options.includeDirs) > 0

    if cfgHasLinkDirs or optionsHaveIncludeDirs:
        if cfgHasLinkDirs:
            linkDirs += ':'.join(cfg['link-library-dirs'])

        if optionsHaveIncludeDirs:
            includeDirs = includeDirs + ':' + options.includeDirs

    cfgHasIncludeLibs = len(cfg['link-libraries']) > 0
    optionsHaveIncludeLibs = len(options.includes) > 0

    if cfgHasIncludeLibs or optionsHaveIncludeLibs:

        if cfgHasIncludeLibs:
            linkLibs += ':'.join(cfg['link-libraries'])

        if optionsHaveIncludeLibs:
            if cfgHasIncludeLibs:
                linkLibs += ':' 
            
            linkLibs += options.includes

    optionStr = '-i%s -L%s -l%s' % (includeDirs, linkDirs, linkLibs)

    result = '-- Initial testMain.cabal generated by cabal init.  For further\n\
-- documentation, see http://haskell.org/cabal/users-guide/\n\
\n\
-- The name of the package.\n\
name:                ' + cfg['name'] + '\n\
\n\
-- The package version.  See the Haskell package versioning policy (PVP)\n\
-- for standards guiding when and how versions should be incremented.\n\
-- http://www.haskell.org/haskellwiki/Package_versioning_policy\n\
-- PVP summary:      +-+------- breaking API changes\n\
--                   | | +----- non-breaking API additions\n\
--                   | | | +--- code changes with no API change\n\
version:             ' + cfg['version'] + '\n\
\n\
-- A short (one-line) description of the package.\n\
synopsis:            ' + cfg['synopsis'] + '\n\
\n\
-- A longer description of the package.\n\
-- description:         \n\
\n\
-- URL for the project homepage or repository.\n\
homepage:            ' + cfg['homepage'] + '\n\
\n\
-- The license under which the package is released.\n\
license:             ' + cfg['license'] + '\n\
\n\
-- The file containing the license text.\n\
license-file:        LICENSE\n\
\n\
-- The package author(s).\n\
author:              ' + cfg['author'] + '\n\
\n\
-- An email address to which users can send suggestions, bug reports, and \n\
-- patches.\n\
maintainer:          ' + cfg['maintainer'] + '\n\
\n\
-- A copyright notice.\n\
-- copyright:           \n\
\n\
category:            ' + cfg['category'] + '\n\
\n\
build-type:          ' + cfg['build-type'] + '\n\
\n\
-- Constraint on the version of Cabal needed to build this package.\n\
cabal-version:       >=1.8\n\
\n\
\n\
executable ' + cfg['name'] + '\n\
  -- .hs or .lhs file containing the Main module.\n\
  main-is: ' + cfg['main-is'] + '            \n\
 \n\
  ghc-options: ' + optionStr + ' \n\
  -- Modules included in this executable, other than Main.\n\
  -- other-modules:       \n\
  \n\
  -- Other library packages from which modules are imported.\n\
  build-depends:       ' + cfg['build-depends']
  
    return result

def main(options, args, parser):

    if len(args) != 2:
        parser.error(
            'Expected 2 arguments, got %d instead', 
            len(args))

    jsonConfig = args[0]
    outputPath = args[1]

    logger = logging.getLogger(__name__)

    cfg = None

    try:
        with open(jsonConfig, 'r') as jsonFile:
            cfgString = jsonFile.read()

            logger.debug('%s', cfgString)

            cfg = json.loads(cfgString)

    except IOError as fileError:
        logger.critical('Error loading JSON config: %s', fileError)
        return
    
    cabalString = generateCabalString(
        cfg,
        options)

    try:
        with open(outputPath, 'w') as outFile:
            outFile.write(cabalString)

    except IOError as fileError:
        logger.critical(
            'Failed to write to output file: %s', 
            fileError)
        return 

    return

if __name__ == '__main__':
    appUtils.main(usageString, versionString, appSetOpts, main)
