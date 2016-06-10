#!/usr/bin/env python3

import requests
import sys
import json

BASE_DOMAIN = 'agora'
BASE_URL = 'https://%s/elections/api/election/' % BASE_DOMAIN
BASE_OLD_URL = 'https://%s/elections-old/api/election/' % BASE_DOMAIN
DIRECTOR = 'auth1'
AUTHORITIES = ['auth2']
headers = {'Accept': '*/*', 'Content-Type': 'application/json'}

eid = sys.argv[1]
print(requests.post(BASE_URL + eid + "/stop", headers=headers, verify=False).text)
