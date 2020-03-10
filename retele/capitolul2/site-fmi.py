# Acest program se conectează la site-ul FMI,
# de unde extrage și printează maxima zilei.

from bs4 import BeautifulSoup
import requests

headers = {
    "Accept": "text/html",
    "Accept-Language": "en-US,en",
    "Cookie": "__utmc=177244722",
    "Host": "fmi.unibuc.ro",
    "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.79 Safari/537.36"
}

response = requests.get("http://fmi.unibuc.ro/ro")
#print (response.text[:200])

supa = BeautifulSoup(response.text, features='html.parser')

div = supa.find('div', {'class': 'cst2'})
paragraph = div.find('p')

print(paragraph.text)
