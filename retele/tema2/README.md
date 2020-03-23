# Tema 2 de la laboratorul de rețele

## Exerciții HTTP

### DNS-over-HTTPS

Modulul `doh.py` definește o funcție care utilizează serviciul de DNS-over-HTTPS
de la Cloudflare pentru a afla adresa IP a unui domeniu.

![Adresa DNS pentru fmi.unibuc.ro](http/1-doh/dns.png)

### Servirea cererilor de tip `GET` prin Flask

Am rulat Flask în container, și am deschis pagina în browser.

![Pagina primită în browser](http/2-get/localhost.png)
![Mesajele de pe server](http/2-get/flask.png)

### Cereri de tip `POST`

Am modificat server-ul să ridice la pătrat numărul primit ca JSON prin POST.

![Cerere `POST` prin `curl`](http/3-post/post.png)
![Ridicarea la pătrat pe rețea](http/3-post/squared.png)

### `httpbin`

Am scris un script de Python care face cereri HTTP cu diferite verbe.

![Cereri diverse la httpbin](http/4-httpbin/httpbin.png)

## Exerciții UDP

### Server și client pe aceeași mașină

Am rulat client-ul și server-ul de UDP pe același calculator.

![Server și client care comunică peste UDP](udp/1-comunicare.png)

### Server și client în containere diferite

Am modificat client-ul și server-ul ca să poată rula fiecare din container-ul lui.

![Server și client în containere diferite](udp/3-peste-bridge.png)

### Comunicare UDP vizualizată prin `tcpdump`

![Un mesaj transmis înainte și înapoi prin UDP](udp/6-tcp-dump.png)

### Accesez server-ul de UDP din container de pe host

Am modificat `docker-compose.yml` ca să pot accesa port-ul `10000` de pe
host.

![Un mesaj transmis de pe host pe container și înapoi](udp/7-local.png)

## Exerciții TCP

Exercițiile 1-3 au mers la UDP.

### Mesaj transmis prin TCP

![Transmiterea unui mesaj de la client la server prin TCP](tcp/4-comunicare.png)

### 3-way Handshake

Am modificat client-ul să transmită un singur byte, și server-ul să accepte un singur byte și să nu-l retransmită.
Mai jos sunt pachete schimbate între client și server.

```
IP 172.21.0.2.59128 > 172.21.0.3.10000: Flags [S], seq 2472923734, win 64240, options [mss 1460,sackOK,TS val 3722954303 ecr 0,nop,wscale 7], length 0
IP 172.21.0.3.10000 > 172.21.0.2.59128: Flags [S.], seq 3967639458, ack 2472923735, win 65160, options [mss 1460,sackOK,TS val 4044896210 ecr 3722954303,nop,wscale 7], length 0
IP 172.21.0.2.59128 > 172.21.0.3.10000: Flags [.], ack 3967639459, win 502, options [nop,nop,TS val 3722954303 ecr 4044896210], length 0
IP 172.21.0.2.59128 > 172.21.0.3.10000: Flags [P.], seq 2472923735:2472923736, ack 3967639459, win 502, options [nop,nop,TS val 3722955304 ecr 4044896210], length 1
IP 172.21.0.3.10000 > 172.21.0.2.59128: Flags [.], ack 2472923736, win 510, options [nop,nop,TS val 4044897211 ecr 3722955304], length 0
IP 172.21.0.2.59128 > 172.21.0.3.10000: Flags [F.], seq 2472923736, ack 3967639459, win 502, options [nop,nop,TS val 3722955304 ecr 4044897211], length 0
IP 172.21.0.3.10000 > 172.21.0.2.59128: Flags [.], ack 2472923737, win 510, options [nop,nop,TS val 4044897251 ecr 3722955304], length 0
IP 172.21.0.3.10000 > 172.21.0.2.59128: Flags [F.], seq 3967639459, ack 2472923737, win 510, options [nop,nop,TS val 4044898212 ecr 3722955304], length 0
IP 172.21.0.2.59128 > 172.21.0.3.10000: Flags [.], ack 3967639460, win 502, options [nop,nop,TS val 3722956305 ecr 4044898212], length 0
```
