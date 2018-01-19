#' Bio-Formats Version Number
#'
#' Provides the version of the Bio-Formats library embedded in the package.
#' @export
#' @template author
BioFormats.version = function() .jfield("loci/formats/FormatTools", "S", "VERSION")

#' Bio-Formats FormatTools Class
#' 
#' A utility class for format reader and writer implementations.
#' @examples 
#' # List available pixel types
#' sapply(0:7, FormatTools$getPixelTypeString)
#' @template author
#' @export
## assigned in .onLoad after Java Virtual Machine is initialized
FormatTools = NULL

#' Java Memory Settings
#' 
#' Information about the Java heap space usage.
#' 
#' @param units Units to return the size in: "k", "m" or "g"
#' @return The maximum amount of memory that the JVM will attempt to use,
#'   measured in \code{units}.
#' @examples 
#' \dontrun{
#' ## assign 4 gigabytes of heap space to the Java environment.
#' options( java.parameters = "-Xmx4g" )
#' library( "RBioFormats" )
#' 
#' checkJavaMemory()
#' }
#' @template author
#' @export
checkJavaMemory = function(units = "m") {
  units = tolower(units)[1L]
  
  runtime = .jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
  memory = .jcall(runtime, "J", "maxMemory")
  
  pow = switch (units, k = 1, m = 2, g = 3, 0)
  
  memory / 2^(10*pow)
}
