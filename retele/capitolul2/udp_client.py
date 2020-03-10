import socket

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

port = 10000
adresa = 'localhost'
server_address = (adresa, port)

sent = sock.sendto('mesaj'.encode('utf-8'), server_address)
data, server = sock.recvfrom(4096)

print(data, server)

sock.close()
