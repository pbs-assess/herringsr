# Clean up the directory after building the document (Windows version)

del sr.aux
del sr.knit.md
del sr.log
del sr.pdf
del sr.Rmd
del sr.tex
del sr.upa
del sr.utf8.md

rmdir /S /Q csas-style
rmdir /S /Q knitr-figs-pdf
rmdir /S /Q _book
