#!/usr/bin/env python3

import requests
import sys
import json

BASE_DOMAIN = 'agora'
BASE_URL = 'https://%s/elections/api/election/' % BASE_DOMAIN
BASE_OLD_URL = 'https://%s/elections-old/api/election/' % BASE_DOMAIN
DIRECTOR = 'auth1'
AUTHORITIES = ['auth2']
headers = {'Content-Type': 'application/json'}

eid = sys.argv[1]
data = requests.get(BASE_OLD_URL + eid, headers=headers, verify=False).json()['payload']
election = json.dumps({
    "id": data['configuration']["id"],
    "layout": data['configuration']['layout'],
    "director": DIRECTOR,
    "authorities": AUTHORITIES,
    "title": data['configuration']["title"],
    "description": data['configuration']["description"],
    "questions": data['configuration']["questions"],
    "start_date": "2016-01-05T11:11:11.111",
    "end_date": "2016-01-05T11:11:11.111",
    "presentation":
    {
        "share_text": "",
        "theme": "default",
        "urls": [],
        "theme_css": ""
    },
    "real": True
})
print("\ndata to be sent: %s\n" % election)
print(requests.post(BASE_URL + eid, headers=headers, data=election, verify=False).text)
