from flask import Flask, json

companies = [{"date": [1,2,3,4,5]}]

api = Flask(__name__)

@api.route('/test', methods=['GET'])
def get_companies():
    return json.dumps(companies)

if __name__ == '__main__':
    api.run()
