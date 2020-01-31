#!/usr/bin/python3

import json
import sys
import urllib.request

# Get version from PsNR/DESCRIPTION at github
description = urllib.request.urlopen(r'https://raw.githubusercontent.com/UUPharmacometrics/PsNR/master/DESCRIPTION')
for line in description:
    s = line.decode('utf-8')
    if s.startswith('Version:'):
        version = s[9:-1]
        break

# Get sha of latest commit to master at github
commit = urllib.request.urlopen(r'https://api.github.com/repos/UUPharmacometrics/PsNR/commits/master')
commit = commit.read().decode('utf-8')
commit = json.loads(commit)
sha = commit['sha']

# Get renv.lock from PsNR/renv.lock at github
lock = urllib.request.urlopen(r'https://raw.githubusercontent.com/UUPharmacometrics/PsNR/master/renv.lock')
contents = []
for line in lock:
    s = line.decode('utf-8')
    contents.append(s)

# Write renv.lock with added entry for PsNR
with open('PsN-Source/renv.lock', 'w') as fh:
    lines = [
        '    "PsNR": {\n',
        '      "Package": "PsNR",\n',
        '      "Version": "' + version + '",\n',
        '      "Source": "GitHub",\n',
        '      "RemoteType": "github",\n',
        '      "RemoteHost": "api.github.com",\n',
        '      "RemoteRepo": "PsNR",\n',
        '      "RemoteUsername": "UUPharmacometrics",\n',
        '      "RemoteRef": "master",\n',
        '      "RemoteSha": "' + sha + '"\n',
        '    }\n']

    new_contents = contents[:-3] + [contents[-3].rstrip() + ',\n'] + lines + contents[-2:]
    fh.writelines(new_contents)
