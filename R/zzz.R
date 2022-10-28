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

  bf_ver <- read.dcf(file.path(pkg_dir, "DESCRIPTION"), "BioFormats")
  bf_jar <- .bioformats_jar_dst(bf_ver)

  .get_bioformats(bf_ver, bf_jar)

  jars =
    if (installed)
      ""
    else
      list.files(jar_dir, pattern = ".*\\.jar", full.names = TRUE)

  .jpackage(pkg, lib.loc = lib, morePaths = c(jars, bf_jar))

  .init_formattools()
}

.get_bioformats <- function(ver, bf_jar){
  bf_url <- .bioformats_jar_url(ver)

  if ( !file.exists(bf_jar) ) {
    # Use 100s instead of default of 60s for timing out the file download
    tryCatch(utils::download.file(bf_url, bf_jar, mode = "wb", quiet = FALSE),
             timeout = max(100, getOption("timeout")),
             error = function(e) {
               file.remove(bf_jar)
               stop(
                 "Failed to download Bio-Formats Java library.\n  Check your internet connection and try again. Consider setting the environment variable R_DEFAULT_INTERNET_TIMEOUT to a value higher than 100.",
                 call.=FALSE)
             }
    )
  }

}

.bioformats_jar_url <- function (ver) {
  url_template <- "https://downloads.openmicroscopy.org/bio-formats/%s/artifacts/%s"
  jar <- "bioformats_package.jar"
  sprintf(url_template, ver, jar)
}

.bioformats_jar_dst <- function(ver) {
  jar_filename <- sprintf("bioformats_package_%s.jar", ver)
  cache_dir <- tools::R_user_dir("RBioFormats", which = "cache")
  if (!dir.exists(cache_dir))
    dir.create(cache_dir, recursive = TRUE)
  file.path(cache_dir, jar_filename)
}

.init_formattools <- function() {
  FormatTools <- J("loci.formats.FormatTools")
  package_environment <- parent.env(environment())
  assign("FormatTools", FormatTools, envir = package_environment)
}
