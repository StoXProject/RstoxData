#' convert units
#' @description 
#'  Express values in a different unit.
#' @details 
#'  The argument 'conversionTable' defines valid units and their symbols.
#'  This defaults to \code{\link[RstoxData]{StoxUnits}}.
#' @param value to be expressed in different unit
#' @param unit id of unit for value
#' @param desired id of unit desired after conversion
#' @param conversionTable formatted as \code{\link[RstoxData]{StoxUnits}}
#' @return value expressed in 'desired' unit.
#' @examples 
#'  convertUnits(c(1,2), "km", "nmi")
#'  convertUnits(10, "t", "g")
#' @noRd
convertUnits <- function(value, unit, desired, conversionTable=RstoxData::StoxUnits){
  
  if (!(unit %in% conversionTable$id)){
    stop(paste(unit, "not found in 'conversionTable'"))
  }
  if (!(desired %in% conversionTable$id)){
    stop(paste(desired, "not found in 'conversionTable'"))
  }
  if (conversionTable$quantity[conversionTable$id==unit] != conversionTable$quantity[conversionTable$id==desired]){
    stop(paste(unit, "and", desired, "are of different quantities."))
  }
  
  facUnit <- conversionTable$conversion[conversionTable$id == unit]
  facDesired <- conversionTable$conversion[conversionTable$id == desired]
  
  conversion = facUnit / facDesired
  
  return(value*conversion)
  
}

#' Look up unit
#' @description 
#'  Find the unit identifier given quantity and either symbol, shortname or name
#' @details 
#'  This is useful for looking up correct unit identifiers from user input.
#'  Provide either symbol, shortname or name, do not provide more than one.
#'  
#'  The argument 'conversionTable' defines valid units and their symbols.
#'  This defaults to \code{\link[RstoxData]{StoxUnits}}.
#' @param quantity The quantity to look up units for
#' @param shortname shortname of the desired unit
#' @param symbol Symbol of the desired unit
#' @param name name of the desired unit
#' @param unitTable formatted as \code{\link[RstoxData]{StoxUnits}}
#' @return valid id in unitTable
#' @seealso \code{\link[RstoxData]{getUnitOptions}} for listing all valid options for a quantity.
#' @examples 
#'  findUnit("mass", getUnitOptions("mass")[1])
#'  findUnit("length", "m")
#' @export
findUnit <- function(quantity, shortname=NULL, symbol=NULL, name=NULL, unitTable=RstoxData::StoxUnits){
  if (!(quantity %in% unitTable$quantity)){
    stop(paste(quantity, "is not a valid quantity."))
  }
  if (sum(c(is.null(symbol), is.null(shortname), is.null(name))) != 2){
    stop("Provide either 'symbol', 'shortname' or 'name'.")
  }
  
  if (!is.null(symbol)){
    filt <- unitTable$symbol==symbol & unitTable$quantity==quantity
    if (sum(filt)==0){
      stop(paste(symbol, "is not a valid symbol for quantity:", quantity))
    }
    stopifnot(sum(filt)==1)
    return(unitTable$id[filt])
  }
  if (!is.null(shortname)){
    filt <- unitTable$shortname==shortname & unitTable$quantity==quantity
    if (sum(filt)==0){
      stop(paste(shortname, "is not a valid shortname for quantity:", quantity))
    }
    stopifnot(sum(filt)==1)
    return(unitTable$id[filt])
  }
  if (!is.null(name)){
    filt <- unitTable$name==name & unitTable$quantity==quantity
    if (sum(filt)==0){
      stop(paste(name, "is not a valid name for quantity:", quantity))
    }
    stopifnot(sum(filt)==1)
    return(unitTable$id[filt])
  }
  
}

#' Set units of value
#' @description 
#'  Set unit of a value in accordance with StoX convention (adds the attribute 'stoxUnit').
#'  Converts unit if already set.
#' @details 
#'  The argument 'conversionTable' defines valid units and their symbols.
#'  This defaults to \code{\link[RstoxData]{StoxUnits}}.
#'
#'  Providing id as NA removes unit from value
#'
#' @param value value to set unit for.
#' @param id id of the desired unit for the column
#' @param conversionTable formatted as \code{\link[RstoxData]{StoxUnits}}
#' @param assertNew if True an error is raised if the unit is already set for 'value'
#' @return converted value with the attribute 'Unit' set / altered.
#' @examples 
#'  dt <- data.table::data.table(weight=c(1000,1200))
#'  dt$weight <- setUnit(dt$weight, "mass-g")
#'  print(dt$weight)
#'  dt$weight <- setUnit(dt$weight, "mass-kg")
#'  print(dt$weight)
#' @export
setUnit <- function(value, id, conversionTable=RstoxData::StoxUnits, assertNew=FALSE){
  
  if (is.na(id)){
    if (!is.null(attr(value, "stoxUnit"))){
      attr(value, "stoxUnit") <- NULL      
    }
    return(value)
  }
  
  if (!(id %in% conversionTable$id)){
    stop(paste("Symbol", id, "not found in 'conversionTable'"))
  }
  
  if (!is.null(attr(value, "stoxUnit"))){
    if (assertNew){
      stop("Unit is already set.")
    }
    value <- convertUnits(value, attr(value, "stoxUnit"), id, conversionTable)
  }
  
  attr(value, "stoxUnit") <- id
  
  return(value)

}

#' Get unit for value
#' @description 
#'  Get unit id, shortname, symbol or name of a value in accordance with StoX convention.
#' @param value value to read unit from
#' @param property the property of the unit that is to be returned (e.g. 'symbol' or 'name')
#' @param unitTable formatted as \code{\link[RstoxData]{StoxUnits}}
#' @return the unit of value. <NA> if unit is not set
#' @examples 
#'  dt <- data.table::data.table(weight=c(1000,1200))
#'  dt$weight <- setUnit(dt$weight, "mass-g")
#'  print(getUnit(dt$weight, "name"))
#' @export
getUnit <- function(value, property=c("id", "shortname", "symbol", "name"), unitTable=RstoxData::StoxUnits){
  if (is.null(attr(value, "stoxUnit"))){
    return(as.character(NA))
  }
  
  property <- match_arg_informative(property)
  
  unit <- attr(value, "stoxUnit")
  if (!(unit %in% unitTable$id)){
    stop(paste(unit, "not found in 'conversionTable'"))
  }
  
  return(unitTable[[property]][unitTable$id==unit])
  
}

#' Does the input have unit?
#' @description 
#'  Get unit id, shortname, symbol or name of a value in accordance with StoX convention.
#' @inheritParams getUnit
#' @return TRUE if the \code{value} has unit, FALSE if not.
#' @examples 
#'  dt <- data.table::data.table(weight=c(1000,1200))
#'  dt$weight <- setUnit(dt$weight, "mass-g")
#'  print(hasUnit(dt$weight))
#'  print(hasUnit(1))
#' @export
hasUnit <- function(value, property=c("id", "shortname", "symbol", "name"), unitTable=RstoxData::StoxUnits){
	unit <- getUnit(value, property=property, unitTable=unitTable)
	!is.na(unit)
}

#' Get available units
#' @description 
#'  Get the unit shortnames, symbols or names that are available for a given quantity.
#'  The available quantities and units are defined by the argument unitTable
#'  which defaults to RstoxData::StoxUnits
#'  
#'  This is useful for providing user options, and the options can be limited to a suitable range based on 'conversionRange'
#'  
#' @param quantity quantity to get units for, such as 'mass', 'length' etc.
#' @param property the property of the unit that is to be returned (e.g. 'symbol' or 'shortname')
#' @param unitTable formatted as \code{\link[RstoxData]{StoxUnits}}
#' @param conversionRange pair of numeric values providing the lower and upper bound of which values for unitTable$conversion should be provided
#' @return a character vector with available unit symbols.
#' @examples 
#'  print(getUnitOptions("mass"))
#'  print(getUnitOptions("mass", conversionRange=c(1e-3,1e3)))
#' @export
getUnitOptions <- function(quantity, property=c("shortname", "symbol", "name"), unitTable=RstoxData::StoxUnits, conversionRange=NULL){
  
  property <- match_arg_informative(property)
  
  if (!(quantity %in% unitTable$quantity)){
    stop(paste(quantity, "is not a valid quantity."))
  }
  
  if (!is.null(conversionRange)){
    unitTable <- unitTable[unitTable$quantity==quantity,]
    minC <- min(conversionRange)
    maxC <- max(conversionRange)
    unitTable <- unitTable[unitTable$conversion >= minC & unitTable$conversion <= maxC,]
    
    if (nrow(unitTable) == 0){
      stop("No units found in the requested conversion range.")
    }
  }
  
  return(unitTable[[property]][unitTable$quantity==quantity])
  
}

#' Get available units
#' @description 
#'  Get the unit symbmols that are available for a given quantity.
#'  The available quntities and units are defined by the argument unitTable
#'  which defaults to RstoxData::StoxUnits
#' @param quantity quantity to get units for, such as 'mass', 'length' etc.
#' @param unitTable formatted as \code{\link[RstoxData]{StoxUnits}}
#' @return a character vector with availble unit symbols.
#' @examples 
#'  print(getUnitOptions("mass"))
#' @export
getUnitOptions <- function(quantity, unitTable=RstoxData::StoxUnits){
  
  if (!(quantity %in% unitTable$quantity)){
    stop(paste(quantity, "is not a valid quantity."))
  }
  
  return(unitTable$symbol[unitTable$quantity==quantity])
  
}

#' Get available units
#' @description 
#'  Get the unit shortnames, symbols or names that are available for a given quantity.
#'  The available quantities and units are defined by the argument unitTable
#'  which defaults to RstoxData::StoxUnits
#'  
#' @param quantity quantity to get units for, such as 'mass', 'length' etc.
#' @param property the property of the unit that is to be returned (e.g. 'symbol' or 'shortname')
#' @param unitTable formatted as \code{\link[RstoxData]{StoxUnits}}
#' @param conversionRange pair of numeric values providing the lower and upper bound of which values for unitTable$conversion should be provided
#' @return a character vector with available unit symbols.
#' @examples 
#'  print(getUnitOptions("mass"))
#'  print(getUnitOptions("mass", conversionRange=c(1e-3,1e3)))
#' @export
getUnitOptions <- function(quantity, property=c("shortname", "symbol", "name"), unitTable=RstoxData::StoxUnits, conversionRange=NULL){
  
  property <- match_arg_informative(property)
  
  if (!(quantity %in% unitTable$quantity)){
    stop(paste(quantity, "is not a valid quantity."))
  }
  
  if (!is.null(conversionRange)){
    unitTable <- unitTable[unitTable$quantity==quantity,]
    minC <- min(conversionRange)
    maxC <- max(conversionRange)
    unitTable <- unitTable[unitTable$conversion >= minC & unitTable$conversion <= maxC,]
    
    if (nrow(unitTable) == 0){
      stop("No units found in the requested conversion range.")
    }
  }
  
  return(unitTable[[property]][unitTable$quantity==quantity])
  
}