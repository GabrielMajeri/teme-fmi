# TCP client
import socket
import logging
import time
import sys

logging.basicConfig(format = u'[LINE:%(lineno)d]# %(levelname)-8s [%(asctime)s]  %(message)s', level = logging.NOTSET)

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM, proto=socket.IPPROTO_TCP)

port = 10000
adresa = 'rt1'
server_address = (adresa, port)

try:
    logging.info('Handshake cu %s', str(server_address))
    sock.connect(server_address)
    time.sleep(1)
    sock.send(b'1')

finally:
    logging.info('closing socket')
    sock.close()
