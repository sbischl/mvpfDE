# This script adds translations and fixes small things that are not possible or I don't know how to fix in the bs4:bookdown page.

import pathlib
import re

# Path to where the bookdown translator creataes the html files. When calling serve_book it goes to the _book subfolder of the bookdown folder
directory = (pathlib.Path(__file__).parents[1] / 'bookdown' / '_book')
#.read_text(encoding="utf-8")
#print(literature_bib_string)
for file in directory.glob('*.html'):
    content = file.read_text(encoding="utf-8")
    #----------------
    # Regex Replace
    #----------------

    # Make search German
    # <input id="search" class="form-control" type="search" placeholder="Search" aria-label="Search">
    content = re.sub("(?<=placeholder=\")Search", "Suchen", content)
    content = re.sub(r"(?<=<h2>)Table of contents", "Inhaltsverzeichnis", content)
    content = re.sub("View book source (?=<i class=\"fab fa-github\">)", "Projekt-Quellcode", content)
    content = re.sub(r"(?<=<h2>)On this page", "Auf dieser Seite", content)
    content = re.sub("View source (?=<i class=\"fab fa-github\">)", "Seiten-Quellcode", content)
    content = re.sub("href=\"https://github.com/sbischl/mvpfde/blob/master/", "href=\"https://github.com/sbischl/mvpfDE/tree/master/bookdown/", content)
    content = re.sub("<li><a id=\"book-edit\" href=\".*\">Edit this page <i class=\"fab fa-github\"></i></a></li>", "", content)
    content = re.sub(r"<footer ((.|\n)*)</footer>", "", content)
    #print(file)
    file.write_text(content, encoding="utf-8")
    # Regex Delete