from flask import Flask, jsonify
from flask import request

app = Flask(__name__)


@app.route('/')
def hello():
    return "Hello World!"


'''
This method expects a json content.
Use header: 'Content-Type: application/json'
'''
@app.route('/post', methods=['POST'])
def post_method():
    req = request.get_json()
    value = req['value']
    return jsonify({'squared_value': value ** 2})


@app.route('/<name>')
def hello_name(name):
    return "Hello {}!".format(name)


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8001)
