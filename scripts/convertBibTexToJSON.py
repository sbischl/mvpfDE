# requires bibjson.py and bibtex2bibjson.py from https://github.com/internaut/bibtex2bibjson in thirdparty subfolder
# requires bibtexparser, install with pip install bibtexparser

# The script needs to be called from the root directory of the project. Going onto scripts and calling it then results in pathlib not finding the literature.bib file

# This script reads the literature.bib file from the project root, converts it into json and places the json file into the /web/data folder

from thirdparty.bibjson import *
import pathlib
import json

literature_bib_string = (pathlib.Path(__file__).parents[1] / 'literature.bib').read_text(encoding="utf-8")
collection = collection_from_bibtex_str(literature_bib_string, collection = "literature")
collection_with_bibtex_as_key = {}
for entry in collection["records"]:
    collection_with_bibtex_as_key[entry["id"]] = entry
    # Remove unnecessary keys
    collection_with_bibtex_as_key[entry["id"]].pop("citekey", None)
    collection_with_bibtex_as_key[entry["id"]].pop("collection", None)
    collection_with_bibtex_as_key[entry["id"]].pop("abstract", None)
    collection_with_bibtex_as_key[entry["id"]].pop("ENTRYTYPE", None)
    collection_with_bibtex_as_key[entry["id"]].pop("ID", None)
    collection_with_bibtex_as_key[entry["id"]].pop("id", None)

with open(pathlib.Path(__file__).parents[1] / 'web' / 'data' / 'literature.json', 'w') as f:
    json.dump(collection_with_bibtex_as_key, f, indent = True)