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
