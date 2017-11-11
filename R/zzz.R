.onLoad <- function(lib, pkg) {
  ## check whether called on a source package directory which has a different
  ## path to .jar files compared to the installed package (this workaround is
  ## needed, e.g., to run devtools::test)
  pkg_dir = file.path(lib, pkg)
  if (getwd() == pkg_dir)
    jars = list.files(file.path(pkg_dir, "inst", "java"), pattern = ".*\\.jar", full.names = TRUE)
  else
    jars = ""
  
  .jpackage(pkg, lib.loc = lib, morePaths = jars)
  
  FormatTools <<- J("loci.formats.FormatTools")
}
