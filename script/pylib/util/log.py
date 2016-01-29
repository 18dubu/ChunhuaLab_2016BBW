# logging utlities

import logging

# define the script global logger
def strd_logger (name):
	log = logging.getLogger (name)
	log.setLevel (logging.INFO)
	formatter = logging.Formatter('[%(asctime)s %(levelname)s] %(message)s', "%Y-%m-%d %H:%M:%S")
	handler = logging.StreamHandler()
	handler.setFormatter(formatter)
	log.addHandler(handler)
	return log
