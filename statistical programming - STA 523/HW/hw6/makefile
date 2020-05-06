hw6.html: hw6.Rmd darksky.rds darksky2.rds cbs2017.rds test.rds
	clean_html; clean_data; Rscript -e "library(rmarkdown); render('hw6.Rmd')"
	
cbs2017.rds: get_cbs.R
	Rscript get_cbs.R
	
test.rds: get_cbs.R
	Rscript get_cbs.R

darksky.rds: darksky_data.R
	Rscript darksky_data.R
	
darksky2.rds: darksky_data.R
	Rscript darksky_data.R
	
.PHONY: clean_html clean_data
clean_html:
	rm hw6.html
	
clean_data:
	rm -rf hw6_files/; rm -rf cbs_byte-me.csv; rm -rf cbs2017.rds; rm -rf test.rds; rm -rf darksky2.rds; rm -rf darksky.rds