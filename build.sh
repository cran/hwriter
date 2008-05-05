#!/bin/sh

R CMD build .
R CMD check hwriter*.tar.gz
rm -rf .Rcheck ..Rcheck hwriter.Rcheck
scp hwriter*.tar.gz lobster:~/public_html/hwriter/
R CMD INSTALL . 
echo 'library(hwriter) ; example(hwriter)' | R --no-save --vanilla
scp example-hwriter.html lobster:~/public_html/hwriter/index.html
scp iris*.jpg volcano.png lobster:~/public_html/hwriter/
rm -rf example-hwriter.html iris*.jpg volcano.png
rm -rf hwriter*.tar.gz
rm -rf Rplots.ps inst/doc/hwriter.pdf
