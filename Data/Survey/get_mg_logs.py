#!/usr/bin/env python3
import mail_config
import requests
import json

def get_bounces():
    return requests.get(
        'https://api.mailgun.net/v3/{0}/bounces'.format(mail_config.MG_DOMAIN),
        auth=('api', mail_config.MG_API_KEY),
        params={'limit': 1000})

def get_unsubscribes():
    return requests.get(
        'https://api.mailgun.net/v3/{0}/unsubscribes'.format(mail_config.MG_DOMAIN),
        auth=('api', mail_config.MG_API_KEY),
        params={'limit': 1000})

all_unsubs = json.loads(get_unsubscribes().text)

print(all_unsubs['items'])
