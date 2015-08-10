#!/bin/bash

echo "Rendering the file ..."
Rscript --vanilla -e "rmarkdown::render('notes.Rmd', 'pdf_document', quiet = TRUE)"
