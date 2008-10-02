#!/bin/sh

rm -rf hwriter.Rcheck .Rhistory ..Rcheck example-hwriter.html inst/doc/hwriter.pdf hwriter*.tar.gz

R CMD build .
R CMD check hwriter*.tar.gz
rm -rf hwriter.Rcheck
scp hwriter*.tar.gz lobster.ebi.ac.uk:~/public_html/hwriter/
R CMD INSTALL . 
echo "library(hwriter) ; example(hwriter) ; file.copy(file.path(tempdir(),'example-hwriter.html'),'.',overwrite=TRUE)" | R --no-save --vanilla
scp example-hwriter.html lobster.ebi.ac.uk:~/public_html/hwriter/index.html

rm -rf hwriter.Rcheck .Rhistory ..Rcheck example-hwriter.html inst/doc/hwriter.pdf
