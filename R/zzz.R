.onLoad <- function(lib, pkg) {
  .jpackage(pkg, lib.loc = lib)
  
  ## workaround needed to run devtools::test() as it is executed under the 
  ## source directory structure which has a different path to .jar files
  ## compared to the installed package
  .jaddClassPath(dir(file.path(getwd(), "inst", "java"), full.names = TRUE))
  
  FormatTools <<- J("loci.formats.FormatTools")
}
