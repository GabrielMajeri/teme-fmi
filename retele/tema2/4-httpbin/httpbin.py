import requests

base_url = 'https://httpbin.org/'
headers = {'Content-Type': 'application/json'}

res = requests.get(f'{base_url}/get', headers=headers)
print('GET:', res.json())

res = requests.post(f'{base_url}/post', headers=headers, data="hello")
print('POST:', res.json())

# delay în secunde
delay = 2
res = requests.put(f'{base_url}/delay/{delay}', headers=headers)
print('POST:', res.json())

res = requests.get(f'{base_url}/redirect-to')
print('DELETE:', res.text)

res = requests.patch(f'{base_url}/anything', data="something")
print('PATCH:', res.json())
