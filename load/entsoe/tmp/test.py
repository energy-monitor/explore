import requests
import json

keycloak_url = "https://keycloak.tp.entsoe.eu/realms/tp/protocol/openid-connect/token"
fms_url = "https://fms.tp.entsoe.eu/downloadFileContent"

username = "blublap"
password = "xxx"

client_id = "tp-fms-public"

# Get token request
payload = {
    "client_id": client_id,
    "grant_type": "password",
    "username": username,
    "password": password
}

headers = {
    'Content-Type': 'application/x-www-form-urlencoded'
}

# Download file request 
response = requests.post(keycloak_url, headers=headers, data=payload)
access_token = response.json().get("access_token")



payload = json.dumps({
  "folder": "/TP_export/ActualTotalLoad_6.1.A/",
  "filename": "2014_12_ActualTotalLoad_6.1.A.csv",
#   "lastUpdateTimestamp": "2021-06-02T10:38:15.909Z",
  "topLevelFolder": "TP_export",
  "downloadAsZip": False
})
headers = {
  'Authorization': f'Bearer {access_token}',
  'Content-Type': 'application/json'
}

response = requests.request("POST", fms_url, headers=headers, data=payload)

with open("downloaded_file.csv", "w", encoding="utf-8", newline="") as f:
    f.write(response.text)

print("File saved as downloaded_file.csv")