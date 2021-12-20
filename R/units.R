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
#' @examples
#'  convertUnits(c(1,2), "km", "nmi")
#'  convertUnits(10, "t", "g")
#' @export
convertUnits <- function(value, unit, desired, conversionTable=RstoxData::StoxUnits){
  
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

#' Set units of value
#' @description 
#'  Set unit of a value in accordance with StoX convention (adds the attribute 'unit').
#'  Converts unit if already set.
#' @details 
#'  The argument 'conversionTable' defines valid units and their symbols.
#'  This defaults to \code{\link[RstoxData]{StoxUnits}}.
#' @param value value to set unit for.
#' @param desired the desired unit for the column
#' @param conversionTable formatted as \code{\link[RstoxData]{StoxUnits}}
#' @return converted value with the attribute 'Unit' set / altered.
#' @examples 
#'  dt <- data.table::data.table(weight=c(1000,1200))
#'  dt$weight <- setDtUnit(dt$weight, "g")
#'  print(dt$weight)
#'  dt$weight <- setDtUnit(dt$weight, "kg")
#'  print(dt$weight)
#' @export
setUnit <- function(value, desired, conversionTable=RstoxData::StoxUnits){
  
  if (!(desired %in% conversionTable$symbol)){
    stop(paste("Symbol", desired, "not found in 'conversionTable'"))
  }
  
  if (!is.null(attr(value, "unit"))){
    value <- convertUnits(value, attr(value, "unit"), desired, conversionTable)
  }
  
  attr(value, "unit") <- desired
  
  return(value)

}

#' Get unit for value
#' @description 
#'  Get unit of a value in accordance with StoX convention.
#' @param value value to read unit from
#' @param property the property of the unit that is to be returned (e.g. 'symbol' or 'name')
#' @return the unit of value. <NA> if unit is not set
#' @examples 
#'  dt <- data.table::data.table(weight=c(1000,1200))
#'  dt$weight <- setDtUnit(dt$weight, "g")
#'  print(getUnit(dt), "name")
#' @export
getUnit <- function(value, property=c("symbol", "name"), conversionTable=RstoxData::StoxUnits){
  if (is.null(attr(value, "unit"))){
    return(as.character(NA))
  }
  
  property <- match.arg(property)
  
  unit <- attr(value, "unit")
  if (!(unit %in% conversionTable$symbol)){
    stop(paste("Symbol", unit, "not found in 'conversionTable'"))
  }
  
  return(conversionTable[[property]][conversionTable$symbol==unit])
  
}