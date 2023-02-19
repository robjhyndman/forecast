# Makefile for generating R packages.
# 2017 Rob J Hyndman
#
# Assumes Makefile is in top folder of package

PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

default: build

check:
	Rscript -e "rcmdcheck::rcmdcheck()"

build:
	-rm -f rstudio
	-Rscript -e "devtools::build(args = c('--compact-vignettes=both'))"

install:
	R CMD INSTALL .

clean:
	-rm -f ../$(PKG_NAME)_*.tar.gz
	-rm -r -f man/*.Rd
	-rm -r -f NAMESPACE

docs:
	Rscript -e "roxygen2::roxygenize()"

revdep:
	Rscript -e "revdepcheck::revdep_check(num_workers=3)"

release:
	-rm -f rstudio
	-Rscript -e "devtools::submit_cran(args = c('--compact-vignettes=both'))"
