import requests
import pprint
import json

##Search options
# search: terms to search for (applies to name, filing, text)
# filter: filter returned search terms (applies to year, month, day, quarter, and filing)
#         (eq, ne, gt, lt, ge, le)
# select: specifies which fields should be returned from the search
# index: which index of documents to search (options at the moment are: jsonindex, findox-index)

def search(terms, select="*", filterterm="", index="jsonindex"):
    searchAddr = "https://rdablobdev.search.windows.net/indexes/"+index+"/docs/search?api-version=2017-11-11"
    header = {"Content-Type":"application/json","Accept":"application/json","api-key":"D8124037D23E0DB470E5F6A16A577549"} #query key
    payload = {"search":terms,"filter":filterterm,"select":select}

    r = requests.post(searchAddr, json=payload, headers=header)

    pyobj = json.loads(r.text)

    if r.status_code == 200:
        for each in pyobj["value"]:
            print(json.dumps(each,indent=2,separators=(',',':')))
    else:
        print("Error Code: ",json.dumps(pyobj["error"]["code"],indent=2,separators=(',',':')))
        print("Error message: ",json.dumps(pyobj["error"]["message"],indent=2,separators=(',',':')))
