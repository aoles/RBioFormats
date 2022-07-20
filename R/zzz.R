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

.download_bioformats <- function(pkg_dir){
  bf_url <- .bioformats_jar_url(pkg_dir)
  bf_jar <- .bioformats_jar_dst()

  if ( !file.exists(bf_jar) || !.verify_md5sum(bf_url, bf_jar) )
    utils::download.file(bf_url, bf_jar, mode = "wb", quiet = FALSE)

  bf_jar
}

.bioformats_jar_url <- function (pkg_dir) {
  url_template <- "https://downloads.openmicroscopy.org/bio-formats/%s/artifacts/%s"
  ver <- read.dcf(file.path(pkg_dir, "DESCRIPTION"), "BioFormats")
  jar <- "bioformats_package.jar"
  sprintf(url_template, ver, jar)
}

.bioformats_jar_dst <- function() {
  jar_filename <- "bioformats_package.jar"
  cache_dir <- tools::R_user_dir("RBioFormats", which = "cache")
  if (!dir.exists(cache_dir))
    dir.create(cache_dir, recursive = TRUE)
  file.path(cache_dir, jar_filename)
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
