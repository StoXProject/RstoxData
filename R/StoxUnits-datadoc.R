#' Stox units
#'
#' Standard units used in Stox
#'
#' @docType data
#'
#' @usage data(StoxUnits)
#'
#' @format A \code{\link[data.table]{data.table}} with the following columns:
#'  \describe{
#'   \item{quantity}{The quantity the units apply to. Units can be converted to other units of the same quantity}
#'   \item{symbol}{Symbol or shorthand notation for the unit}
#'   \item{name}{Name of the unit}
#'   \item{conversion}{Conversion factor. All units of the same quantity are expressed relative to common base unit whos value for conversion is 1.}
#'  }
#'
#' @keywords datasets
#'
#' @examples
#' data(StoxUnits)
#' 
"StoxUnits"

