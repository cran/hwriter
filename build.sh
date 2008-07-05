#!/bin/sh

R CMD build .
R CMD check hwriter*.tar.gz
rm -rf hwriter.Rcheck
scp hwriter*.tar.gz lobster:~/public_html/hwriter/
R CMD INSTALL . 
echo "library(hwriter) ; example(hwriter) ; file.copy(file.path(tempdir(),'example-hwriter.html'),'.',overwrite=TRUE)" | R --no-save --vanilla
scp example-hwriter.html lobster:~/public_html/hwriter/index.html
rm -rf example-hwriter.html
rm -rf hwriter*.tar.gz
rm -rf inst/doc/hwriter.pdf
