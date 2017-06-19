install:
	Rscript -e "rt::rmake()"

build:
	Rscript -e "rt::rbuild()"

check:
	Rscript -e "rt::rcheck()"

docs:
	Rscript -e "rt::rdoc()"

pkgdown:
	mv vignettes /tmp/; Rscript -e "rt::rpkgdown()"; mv /tmp/vignettes .

winbuild:
	Rscript -e "rt::rwinbuild(devel=TRUE)"

all: install