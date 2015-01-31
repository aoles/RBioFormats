#' Generate Test Images
#' 
#' Create a mock image of specific parameters for testing.
#' 
#' Generates mock files of specific size or pixel type containing gradient 
#' images. The desired parameters can be provided as key value pairs to the 
#' \code{mockFile} function. For a list of available parameters see below.
#' 
#' @param name File name.
#' @param ... File parameters; available parameters are listed below.
#' 
#' @section Parameters:
#' \tabular{lll}{
#'   \strong{Name} \tab \strong{Default} \tab \strong{Description} \cr
#'   sizeX \tab 512 \tab horizontal size in pixels \cr
#'   sizeY \tab 512 \tab vertical size in pixels \cr
#'   sizeZ \tab 1 \tab number of Z sections \cr
#'   sizeC \tab 1 \tab number of channels \cr
#'   sizeT \tab 1 \tab number of time points \cr
#'   pixelType \tab uint8 \tab string specifying pixel type: int8, uint8, int16, uint16, int32, uint32, float, double \cr
#'   bitsPerPixel \tab 0 \tab number of valid bits (<= number of bits implied by pixel type) \cr
#'   rgb \tab 1 \tab number of channels that are merged together \cr
#'   dimOrder \tab XYZCT \tab string describing dimension order \cr
#'   orderCertain \tab true \tab \cr
#'   little \tab true \tab whether or not the pixel data should be little-endian \cr
#'   interleaved \tab true \tab whether or not merged channels are interleaved \cr
#'   indexed \tab false \tab whether or not a color lookup table is present \cr
#'   falseColor \tab false \tab whether or not the color lookup table is just for making the image look pretty \cr
#'   series \tab 1 \tab number of series (Images) \cr
#'   lutLength \tab 3 \tab number of entries in the color lookup table \cr
#' }
#'   
#' @template author
#' @examples
#' f = mockFile(sizeX=256, sizeY=256)
#' img = read.image(f)
#' img
#' 
#' @export
mockFile = function(name = "mockfile", ...) {
  args = list(...)
  paste0(name, "&", paste(names(args), tolower(args), sep = "=", collapse = "&"), ".fake")
}
