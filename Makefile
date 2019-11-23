# Makefile for generating R packages.
# 2017 Rob J Hyndman
#
# Assumes Makefile is in top folder of package

PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)


all: install

check:
	Rscript -e "devtools::check(document=TRUE)"

build:
	Rscript -e "devtools::build()"

install:
	R CMD INSTALL .

clean:
	-rm -f ../$(PKG_NAME)_*.tar.gz
	-rm -r -f man/*.Rd
	-rm -r -f NAMESPACE

docs:
	Rscript -e "roxygen2::roxygenize()"

pkgdown:
	Rscript -e "pkgdown::build_site()"

revdep:
	Rscript -e "revdepcheck::revdep_check(num_workers=3)"
