hw5.html: hw5.Rmd data/sheetz/sheetz_geo_df.rds data/wawa/wawa_geo_df.rds
	Rscript -e "library(rmarkdown); render('hw5.Rmd')"

data/sheetz/sheetz_geo_df.rds: parse_sheetz.R data/sheetz/sheetz_data.rds
	Rscript parse_sheetz.R

data/wawa/wawa_geo_df.rds: parse_wawa.R data/wawa/wawa_data.rds
	Rscript parse_wawa.R

data/sheetz/sheetz_data.rds: get_sheetz.R
	mkdir -p data/sheetz; Rscript $<

data/wawa/wawa_data.rds: get_wawa.R
	mkdir -p data/wawa; Rscript $<

.PHONY: clean_html clean_data
clean_html:
	rm hw5.html
	
clean_data:
	rm -rf data/
