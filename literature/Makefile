default:
	@echo "Rendering the document into PDF format ...\n"
	@Rscript --vanilla -e "rmarkdown::render('notes.Rmd', 'pdf_document', quiet = TRUE)"
	@echo "... done!"

watch:
	@echo "Watching ..."
	@fswatch notes.Rmd | xargs -n1 ./compile.sh
