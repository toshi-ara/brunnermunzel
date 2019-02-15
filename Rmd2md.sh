#!/bin/sh

LANG=C Rscript -e 'knitr::knit("README.Rmd")'

