# Tema 2 de la laboratorul de rețele

## Exerciții HTTP

### DNS-over-HTTPS

Modulul `doh.py` definește o funcție care utilizează serviciul de DNS-over-HTTPS
de la Cloudflare pentru a afla adresa IP a unui domeniu.

![Adresa DNS pentru fmi.unibuc.ro](http/1-doh/dns.png)

### Servirea cererilor de tip `GET` prin Flask

![Pagina primită în browser](http/2-get/localhost.png)
![Mesajele de pe server](http/2-get/flask.png)

### Cereri de tip `POST`

![Cerere `POST` prin `curl`](http/3-post/post.png)
![Ridicarea la pătrat pe rețea](http/3-post/squared.png)

### `httpbin`

![Cereri diverse la httpbin](http/4-httpbin/httpbin.png)
