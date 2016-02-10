#!/usr/bin/env python3
# There are some errant obscure character codes in the JSON file. This tiny
# script clears them out.

with open('../data_raw/NGO lists/Raw/un-icso.json', 'r') as f:
    original = f.read()
    fixed = original.replace('\u001e', ' ').replace('\u000b', '\\r\\n').replace('\u0001', '')

with open('../data_raw/NGO lists/Raw/un-icso-clean.json', 'w') as f:
    f.write(fixed)
