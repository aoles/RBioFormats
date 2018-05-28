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
  
  tryCatch(download_bioformats(pkg_dir, jar_dir),
           error = function(e) 
             stop("failed to download Bio-Formats Java library.\n  Check your internet connection and try again.", call.=FALSE)
           )
  
  jars =
    if (installed)
      "" 
    else
      list.files(jar_dir, pattern = ".*\\.jar", full.names = TRUE)
  
  .jpackage(pkg, lib.loc = lib, morePaths = jars)
  
  msg <- sprintf("BioFormats library version %s", BioFormats.version())
  packageStartupMessage(msg)
  
  FormatTools <<- J("loci.formats.FormatTools")
}

download_bioformats <- function (pkg_dir, jar_dir) {
  ver <- read.dcf(file.path(pkg_dir, "DESCRIPTION"), "BioFormats")
  jar <- "bioformats_package.jar"
  url_template <- "https://downloads.openmicroscopy.org/bio-formats/%s/artifacts/%s"
  jar_url <- sprintf(url_template, ver, jar)
  jar_dst <- file.path(jar_dir, jar)
  
  if ( file.exists(jar_dst) ) {
    md5_local <- tools::md5sum(jar_dst)
    md5_file <- readLines(paste(jar_url, "md5", sep="."))
    md5_remote <- sub("([0-9a-z]+).*", "\\1", md5_file)    
    if ( md5_local == md5_remote )
      return(FALSE)
  }
  
  utils::download.file(jar_url, jar_dst, quiet=FALSE)
  
  return(TRUE)
}
