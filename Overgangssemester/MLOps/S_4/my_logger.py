from loguru import logger
import sys

logger.add("logger_file.log", format="{time} {level} {message}", level="DEBUG", rotation="100 MB")

# logger.remove() # Remove the default logger
# logger.add(sys.stdout, level="WARNING") # Add a new logger with WARNING level

# debug
logger.debug("That's it, beautiful and simple logging!")
# info
logger.info("This is an informational message.")
# warning
logger.warning("This is a warning message.")
# critical
logger.critical("This is a critical message.")


