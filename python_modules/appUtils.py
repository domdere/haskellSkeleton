import logUtils

from contextlib import closing
from optparse import OptionParser

def setopts(parser):
    parser.add_option_group(
        logUtils.makeLogOpts(parser))

    return

def main(
    usageMethod, 
    versionMethod, 
    setAppOpts, 
    mainMethod):

    parser = OptionParser(
        usage=usageMethod(),
        version=versionMethod())

    setopts(parser)
    setAppOpts(parser)

    (options, args) = parser.parse_args()

    if not logUtils.isLogLevelValid(
        options.loglevel):

        parser.error(
            "Invalid options for --loglevel: %s" % (
                options.loglevel))

    with closing(
        logUtils.initialiseLogging(
            logUtils.getLoggingConfig(options))):

        mainMethod(options, args, parser)

    return
