.onAttach <- function(lib, pkg) {
  msg <- sprintf("BioFormats library version %s", BioFormats.version())
  packageStartupMessage(msg)
}

.onLoad <- function(lib, pkg) {
  ## check whether called on a source package directory which has a different
  ## path to .jar files compared to the installed package (this workaround is
  ## needed, e.g., to run devtools::test)
  pkg_dir <- file.path(lib, pkg)
  installed <- getwd() != pkg_dir

  jar_dir <-
    if (installed)
      file.path(pkg_dir, "java")
    else
      jar_dir = file.path(pkg_dir, "inst", "java")

  tryCatch(bf_jar <- .download_bioformats(pkg_dir),
           error = function(e)
             stop("failed to download Bio-Formats Java library.\n  Check your internet connection and try again.", call.=FALSE)
           )

  jars =
    if (installed)
      ""
    else
      list.files(jar_dir, pattern = ".*\\.jar", full.names = TRUE)

  .jpackage(pkg, lib.loc = lib, morePaths = c(jars, bf_jar))

  FormatTools <<- J("loci.formats.FormatTools")
}

.download_bioformats <- function(pkg_dir, verbose = TRUE){
	bf_url <- .bioformats_jar_url(pkg_dir)

	bfc <- .get_cache()
	rid <- bfcquery(bfc, "bioformats_package.jar", "rname")$rid
	if (!length(rid)) {
		if( verbose )
			message( "Downloading BioFormats JAR file" )
		rid <- names(bfcadd(bfc, "bioformats_package.jar", bf_url))
	}
	if (isTRUE(bfcneedsupdate(bfc, rid)))
		bfcdownload(bfc, rid)

	bf_jar <- bfcrpath(bfc, rids = rid)

	if (!.verify_md5sum(bf_url, bf_jar))
		stop("failed to download Bio-Formats Java library.", call. = FALSE)

	bf_jar
}


.bioformats_jar_url <- function (pkg_dir) {
	url_template <- "https://downloads.openmicroscopy.org/bio-formats/%s/artifacts/%s"
	ver <- read.dcf(file.path(pkg_dir, "DESCRIPTION"), "BioFormats")
	jar <- "bioformats_package.jar"
	sprintf(url_template, ver, jar)
}

.get_cache <- function() {
	cache <- tools::R_user_dir("RBioFormats", which = "cache")
	BiocFileCache::BiocFileCache(cache, ask = FALSE)
}

.verify_md5sum <- function(bf_url, bf_jar) {
	md5_file <-  suppressWarnings(
		tryCatch(readLines(paste(bf_url, "md5", sep=".")), error = function(e) "")
	)

	if (nchar(md5_file)==0L)
		return(FALSE)

	md5_remote <- sub("([0-9a-z]+).*", "\\1", md5_file)
	md5_local <- tools::md5sum(bf_jar)

	return(md5_local == md5_remote)
}
