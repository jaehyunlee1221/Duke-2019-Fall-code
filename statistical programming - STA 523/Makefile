.PHONY : all
	all:20191015.html

20191015.html: 20191015.Rmd data/ok_tor.rds data/school_districts.rds data/fire_stations.rds
	Rscript -e "library(rmarkdown); render('20191015.Rmd')"

data/ok_tor.rds: R/get_tornadoes.R
	cd R; Rscript get_tornadoes.R

data/school_districts.rds: R/get_school_district.R
	cd R; Rscript get_school_district.R

data/fire_stations.rds: R/get_fire_stations.R
	cd R; Rscript get_fire_stations.R


