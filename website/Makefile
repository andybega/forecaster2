
.PHONY: build
build: 
	Rscript -e "rmarkdown::render_site()"
	open _site/index.html

.PHONY: clean
clean: 
	rm -rf _site

.PHONY: update
update: 
	Rscript -e "source('_data/update-inputs.R')"
	Rscript -e "source('_data/prepare-data.R')"


#_site/%.html: %.Rmd
#  Rscript -e "rmarkdown::render_site()"
