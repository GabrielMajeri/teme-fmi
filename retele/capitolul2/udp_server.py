import socket

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

port = 10000
adresa = 'localhost'
server_address = (adresa, port)
sock.bind(server_address)

data, address = sock.recvfrom(4096)

print(data, address)

sent = sock.sendto(data, address)

sock.close()
