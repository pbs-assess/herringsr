# Clean up the directory after building the document (Windows version)

del HerringSR.aux
del HerringSR.knit.md
del HerringSR.log
del HerringSR.pdf
del HerringSR.Rmd
del HerringSR.tex
del HerringSR.upa
del HerringSR.utf8.md

rmdir /S /Q csas-style
rmdir /S /Q knitr-figs-pdf
rmdir /S /Q _book
