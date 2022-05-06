PKGNAME := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
PKGVER := $(shell grep '^Version:' DESCRIPTION | sed -E 's/^Version:[[:space:]]+//')
PKGDIR = $(shell basename $(dir $(realpath $(firstword $(MAKEFILE_LIST)))))

BFVER := $(shell grep '^BioFormats:' DESCRIPTION | sed -E 's/^BioFormats:[[:space:]]+//')

RSCRIPT = Rscript --vanilla

document:
	${RSCRIPT} -e "devtools::document()"

ant:
	cd java; ant

build: ant
	cd ..; R CMD build ${PKGDIR}

install: build
	R CMD INSTALL ../${PKGNAME}_${PKGVER}.tar.gz

check: build
	cd ..; R CMD check --no-manual ${PKGNAME}_${PKGVER}.tar.gz

vignette: install
	cd vignettes; R CMD Sweave ${PKGNAME}.Rmd

update: check vignette

commit:
	git commit -am "Update to BioFormats ${BFVER}"

tag: commit
	git tag Bio-Formats_${BFVER}

push: tag
	git push --tags

publish: push

.PHONY: document ant build install check vignette update commit tag push publish
