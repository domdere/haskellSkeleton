from optparse import OptionParser, OptionGroup
import logging
import multiprocessing
import sys
import traceback

# this library a bunch of things that are now in the std 'logging' library in Python 3.3 onwards
# in python 3.3+ you just have to import logging.handlers instead
from logutils.queue import QueueHandler, QueueListener

# allows access from all modules in a process to the logging queue so that they may spawn process of their own.
global logQueue
global logConfig

def makeLogOpts(parser):
    logOpts = OptionGroup(parser, "Logging Options")

    logOpts.add_option('--logfile', type='string', dest='logfile',
        help='If set will activate logging to FILENAME',
        metavar='FILENAME')

    logOpts.add_option('--no-console-logging', action='store_true', dest='consoleSuppressed',
        help='IFF logging to a file is activated AND this flag is set, logging to stderr will be suppressed')


    logOpts.add_option('--loglevel', type='string', dest='loglevel', default='INFO',
        help='Set the loglevel, one of INFO, DEBUG, WARN, ERROR or CRITICAL. [default=%default]',
        metavar='LOGLEVEL')

    logOpts.add_option('--sync-logging', action='store_true', dest='syncLogging', default=False,
        help='Logging IO takes place in a single process that all handled log messages get sent to, if this is set, all logging happens synchronously in process')

    return logOpts

def getLoggingConfig(options):
    return LoggingConfig(
        options.logfile,
        options.loglevel,
        options.consoleSuppressed,
        options.syncLogging)

def isLogLevelValid(logStr):
    if logStr == 'INFO' or logStr == 'WARN' or logStr == 'DEBUG' or logStr == 'ERROR' or logStr == 'CRITICAL':
        return True
    else:
        return False

def getLogLevel(logStr):
    if logStr == 'INFO':
        return logging.INFO

    if logStr == 'WARN':
        return logging.WARNING

    if logStr == 'DEBUG':
        return logging.DEBUG

    if logStr == 'ERROR':
        return logging.ERROR

    if logStr == 'CRITICAL':
        return logging.CRITICAL

class LoggingConfig:
    def __init__(self, logfile, loglevel, consoleSuppressed, syncLogging):
        self.logfile = logfile
        self.loglevel = loglevel
        self.consoleSuppressed = consoleSuppressed
        self.syncLogging = syncLogging

        return

def loggingProcessMain(config, queue):
    configureLoggingProcess(config, queue)
    
    # taken from the logging cookbook in the std python doco:
    # http://docs.python.org/3.3/howto/logging-cookbook.html#logging-to-a-single-file-from-multiple-processes

    logger = logging.getLogger('LoggerProcess')

    logger.info('Asynchronous Log Process Started')

    while True:
        try:
            record = queue.get()
            if record == None:
                logger.info('Log Processing Exiting cleanly...')
                # None is used as a sentinel to tell the process to quit
                break

            logger = logging.getLogger(record.name)
            # dont check the log level, it should have been checked on the client side to reduce the
            # overhead of transmission
            logger.handle(record) 
        except KeyboardInterrupt:
            # detected an interrupt, go back to waiting though and wait for the main process to call the loggers back in,
            # other processes may still want to log stuff while shutting down.
            logging.info('Keyboard interrupt detected, the logger process will stay up until the main process calls it in to allow other processes to log during shutdown')
            continue
        except SystemExit:
            raise
        except:
            print('Problem with Logging Process:')
            traceback.print_exc()
    return

class AsyncLoggingAppContext:
    def __init__(self, config):
        self.__config = config
        self.__logQueue = multiprocessing.Queue(-1)
        self.__logProcess = multiprocessing.Process(target=loggingProcessMain, args=(config, self.__logQueue))
        return

    def start(self):
        self.__logProcess.start()

        configureWorkerProcess(self.__config, self.__logQueue)

        return

    def stop(self):
        self.__logQueue.put_nowait(None)
        self.__logProcess.join()
        return

    def close(self):
        self.stop()

# this is the trivial counterpart to the previous class.
class SyncLoggingAppContext:
    def __init__(self, config):
        self.__config = config
        return

    def start(self):
        configureLoggingProcess(self.__config, None)
        return

    def close(self):
        # no clean up to do for the synchronous case
        return 

def configureLoggingProcess(config, queue):
    global logQueue
    global logConfig

    logQueue = queue
    logConfig = config

    formatString = '%(asctime)s: %(name)s (%(levelname)s) [%(process)d:%(thread)d]: %(message)s'

    if config.logfile != None:
        logging.basicConfig(
            filename=config.logfile,
            format=formatString,
            level=getLogLevel(config.loglevel))

        if not config.consoleSuppressed:
            consoleHandler = logging.StreamHandler()
            consoleHandler.setLevel(getLogLevel(config.loglevel))
            consoleHandler.setFormatter(logging.Formatter(formatString))
            logging.getLogger().addHandler(consoleHandler)

    else:
        logging.basicConfig(
            format=formatString,
            level=getLogLevel(config.loglevel))

    logging.captureWarnings(True)

    return

def configureWorkerProcess(config, queue):
    global logQueue
    global logConfig

    logQueue = queue
    logConfig = config

    rootLogger = logging.getLogger()
    # clear the handlers:
    # from the python docs: http://docs.python.org/2/library/multiprocessing.html
    #   "Note that on Windows child processes will only inherit the level of the parent process's logger, any other customisation of the logger will not be inherited"
    #   So on windows the [root] logger will start off with no handlers, while on Unix it inherits the Handler from the main process (hence in a multiprocess situation
    #   if the handlers are not cleared, each log line in the child process will be duplicated 
    #   hence just clear the handlers to be safe.
    rootLogger.handlers = []
    rootLogger.addHandler(QueueHandler(queue))    
    rootLogger.setLevel(getLogLevel(config.loglevel))

    return

def initialiseLogging(config):
    context = None

    if config.syncLogging:
        context = SyncLoggingAppContext(config)
        
    else:
        context = AsyncLoggingAppContext(config)

    context.start()

    return context
