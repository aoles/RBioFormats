PKGNAME := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
PKGVER := $(shell grep '^Version:' DESCRIPTION | sed -E 's/^Version:[[:space:]]+//')
PKGDIR = $(shell basename $(dir $(realpath $(firstword $(MAKEFILE_LIST)))))

BFVER := $(shell grep '^BioFormats:' DESCRIPTION | sed -E 's/^BioFormats:[[:space:]]+//')

RSCRIPT = Rscript --vanilla

document:
	${RSCRIPT} -e "devtools::document()"

ant:
	cd java; ant

build:
	cd ..; R CMD build ${PKGDIR}

install: build
	R CMD INSTALL ../${PKGNAME}_${PKGVER}.tar.gz

check: build
	export R_CHECK_ENVIRON=/Users/oles/.bioc_check.Renviron
	cd ..; R CMD check ${PKGNAME}_${PKGVER}.tar.gz

bioccheck: check
	cd ..; ${RSCRIPT} -e "BiocCheck::BiocCheck('${PKGNAME}_${PKGVER}.tar.gz', 'new-package'=TRUE)"

vignette: install
	cd vignettes; R CMD Sweave ${PKGNAME}.Rmd

update: check vignette

upgrade: ant update

commit:
	git commit -am "Update to BioFormats ${BFVER}"; git tag Bio-Formats_${BFVER}

push: commit
	git push upstream
	git push origin; git push origin --tags

publish: push

.PHONY: document ant build install check vignette update commit tag push publish
