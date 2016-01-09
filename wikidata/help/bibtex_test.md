First, let's just test using code blocks:

~~~ {.haskell .numberLines}
  qsort []     = []
  qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
                 qsort (filter (>= x) xs) 
~~~

Now, the BibtexGitit renders bibliography data inside a code block using citeproc-hs.
  The data can come from two sources:  it can be included inline
  like so

~~~ {.bib}
@article {Wiles95,
    AUTHOR = {Wiles, Andrew},
     TITLE = {Modular elliptic curves and Fermats last theorem},
   JOURNAL = {Ann. of Math. (2)},
  FJOURNAL = {Annals of Mathematics. Second Series},
    VOLUME = {141},
      YEAR = {1995},
    NUMBER = {3},
     PAGES = {443--551},
      ISSN = {0003-486X},
     CODEN = {ANMAAH},
   MRCLASS = {11G05 (11D41 11F11 11F80 11G18)},
  MRNUMBER = {MR1333035 (96d:11071)},
MRREVIEWER = {Karl Rubin},
       DOI = {10.2307/2118559},
       URL = {http://dx.doi.org/10.2307/2118559},
}
~~~

The ability to read the data from a file does not work in the current version:

~~~ {.bib file="testbibtex2"}
Wiles95
Erdos90
~~~

  When reading from a file, the block contains the keys to display from the file
  with keys seperated by spaces or newlines.  If no keys are provided the entire
  file is displayed.

* Each bib entry is labeled in HTML by the key.  So you can link to an entry
  from anywhere else in gitit by using [display text about Wiles95](bibtex_test#Wiles95).  Currently
  this only works with HTML.

* If the entry contains a url this plugin adds a link.  You can edit the url
  to point to a file inside gitit or to an external site.  Essentially, everything
  in the URL is put into the parens following a link, i.e. the added link is
  [article](text from URL) so any link type supported by pandoc can be stuck
  into the URL of the reference.

* This plugin no longer uses the citeproc-hs package to parse and render the data. 

