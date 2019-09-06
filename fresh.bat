# Clean up the directory after a building the document (Windows version)

del sr.log
del sr.Rmd
del sr.tex
del sr.upa

rmdir /S /Q csas-style
rmdir /S /Q knitr-figs-pdf
