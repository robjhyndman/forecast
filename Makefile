# Makefile for generating R packages.
# 2017 Rob J Hyndman
#
# Assumes Makefile is in top folder of package

PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

default: build

build:
	Rscript -e "devtools::build(args = c('--compact-vignettes=both'))"

check:
	Rscript -e "try(devtools::check('.'), silent=FALSE)"

clean:
	rm -f ../$(PKG_NAME)_*.tar.gz
	rm -r -f man/*.Rd
	rm -r -f NAMESPACE
	rm -f .Rhistory
	rm -f *.RData
	rm -f *.Rproj
	rm -rf .Rproj.user

coverage: ## get test coverage
	Rscript -e "devtools::test_coverage('.')"

create:
	Rscript -e "pak::pak(c('devtools', 'usethis', 'pkgdown', 'rmarkdown', 'rcmdcheck', 'roxygen2', 'testthat'))"
	Rscript -e "pak::pkg_install('r-lib/revdepcheck')"
	Rscript -e "usethis::create_package(path = getwd(), rstudio = FALSE)"

docs:
	Rscript -e "roxygen2::roxygenize()"

install:
	R CMD INSTALL .

pkgdown:
	Rscript -e "pkgdown::build_site('.')"

revdep:
	Rscript -e "revdepcheck::revdep_check(num_workers=3)"

release:
	Rscript -e "devtools::submit_cran(args = c('--compact-vignettes=both'))"

test:
	Rscript -e "devtools::test('.')"
