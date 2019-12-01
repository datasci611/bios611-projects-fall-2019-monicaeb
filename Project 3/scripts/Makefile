.PHONY: proj3
proj3:
	docker image build -t proj3 .
	python wrangling.ipynb
	Rscript plots.R
	Rscript -e "require(rmarkdown) p3-report-umd.rmd