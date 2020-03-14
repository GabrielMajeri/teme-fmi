import requests
import json

number = 15

url = 'http://rt1:8001/post'

headers = {'Content-Type': 'application/json'}
data = json.dumps({'value': number})
res = requests.post(url, headers=headers, data=data)

res_data = res.json()
print(res_data)

squared = res_data['squared_value']

print(number, "squared is", squared)
