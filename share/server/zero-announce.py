#!/usr/bin/env python
import pybonjour
import time
sdRef = pybonjour.DNSServiceRegister(
    name="couchdb",
    regtype="_couchdb._tcp", 
    port = 5984)

try:
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        pass
finally:
    sdRef.close()
