
.PHONY: build
build: 
	Rscript -e "rmarkdown::render_site()"

.PHONY: clean
clean: 
	rm -rf _site

.PHONY: update
update: 
	Rscript -e "source('_data/update-inputs.R')"
	Rscript -e "source('_data/prepare-data.R')"

.PHONY: deploy
deploy:
	cp -r ./_site/ ../../predictiveheuristics.com/public_html/forecasts

.PHONY: open
open:
	open _site/index.html

#_site/%.html: %.Rmd
#  Rscript -e "rmarkdown::render_site()"
