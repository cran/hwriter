#!/bin/sh

rm -rf hwriter.Rcheck .Rhistory ..Rcheck example-hwriter.html inst/doc/hwriter.pdf hwriter*.tar.gz article

R CMD build .
R CMD check hwriter*.tar.gz
rm -rf hwriter.Rcheck

R CMD INSTALL . 
echo "library(hwriter) ; example(hwriter) ; file.copy(file.path(tempdir(),'example-hwriter.html'),'.',overwrite=TRUE)" | R --no-save --vanilla

rm -rf hwriter.Rcheck .Rhistory ..Rcheck inst/doc/hwriter.pdf
