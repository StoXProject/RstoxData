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
#'   \item{id}{unique identifier for unit}
#'   \item{quantity}{The quantity the units apply to. Units can be converted to other units of the same quantity}
#'   \item{symbol}{Symbol or shorthand notation for the unit. Unique within quantity}
#'   \item{shortname}{Short name of the unit, intended for plotting and menu options}
#'   \item{name}{Name of the unit}
#'   \item{conversion}{Conversion factor. All units of the same quantity are expressed relative to common base unit whos value for conversion is 1.}
#'  }
#'  
#'  The 'id', 'quantity', and 'conversion' defines a unit. 
#'  'symbol', shortname' and 'name' should are unique for a given quantity
#'  
#' @seealso \code{\link[RstoxData]{setUnit}}, \code{\link[RstoxData]{getUnit}}, \code{\link[RstoxData]{findUnit}}, and \code{\link[RstoxData]{getUnitOptions}}
#'
#' @keywords datasets
#'
#' @examples
#' data(StoxUnits)
#' 
"StoxUnits"

