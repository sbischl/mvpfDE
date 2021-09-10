# requires bibjson.py and bibtex2bibjson.py from https://github.com/internaut/bibtex2bibjson in thirdparty subfolder
# requires bibtexparser, install with pip install bibtexparser

# This script reads the literature.bib file from the project root, converts it into json and places the json file into the /web/data folder

from thirdparty.bibjson import *
import pathlib
import json

literature_bib_string = (pathlib.Path(__file__).parents[1] / 'literature.bib').read_text(encoding="utf-8")
collection = collection_from_bibtex_str(literature_bib_string, collection = "literature")

with open(pathlib.Path(__file__).parents[1] / 'web' / 'data' / 'literature.json', 'w') as f:
    json.dump(collection, f, indent = True)