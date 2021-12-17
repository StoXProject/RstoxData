#' convert units
#' @description 
#'  Express values in a different unit.
#' @details 
#'  The argument 'conversionTable' defines valid units and their symbols.
#'  This defaults to \code{\link[RstoxData]{StoxUnits}}.
#' @param value to be expressed in different unit
#' @param unit symbol of unit for value
#' @param desired symbol of unit desired after conversion
#' @param conversionTable formatted as \code{\link[RstoxData]{StoxUnits}}
#' @return value expressed in 'desired' unit.
#' @example 
#'  convertUnits(c(1,2), "km", "nmi")
#'  convertUnits(10, "t", "g")
#' @export
convertUnits <- function(value, unit, desired, conversionTable=StoxUnits){
  
  if (!(unit %in% conversionTable$symbol)){
    stop(paste("Symbol", unit, "not found in 'conversionTable'"))
  }
  if (!(desired %in% conversionTable$symbol)){
    stop(paste("Symbol", desired, "not found in 'conversionTable'"))
  }
  if (conversionTable$quantity[conversionTable$symbol==unit] != conversionTable$quantity[conversionTable$symbol==desired]){
    stop(paste(unit, "and", desired, "are of different quantities."))
  }
  
  facUnit <- conversionTable$conversion[conversionTable$symbol == unit]
  facDesired <- conversionTable$conversion[conversionTable$symbol == desired]
  
  conversion = facUnit / facDesired
  
  return(value*conversion)
  
}