#
# Run this script to regnerate the RstoxData resource 'StoxUnits'
# If the format is extended, update the documentation in R/Stox-Units-datadoc.R
#
# Units should be considered defined by quantity, id, and conversion. 
# Changing any of these for a unit breaches backwards compatibility.
# symbol, shortname and name are auxiliary information. 
# These could be changed or clarified, but must be kept unique for a given quantity.

StoxUnits <- data.table::data.table(id=character(),
                                    quantity=character(),
                                    symbol=character(),
                                    shortname=character(),
                                    name=character(),
                                    conversion=numeric())

StoxUnits <- rbind(StoxUnits, list("mass-g", "mass", "g", "g", "gram", 1e-3))
StoxUnits <- rbind(StoxUnits, list("mass-kg", "mass", "kg", "kg", "kilogram", 1))
StoxUnits <- rbind(StoxUnits, list("mass-ton", "mass", "t", "ton", "metric ton", 1e3))
StoxUnits <- rbind(StoxUnits, list("mass-kt", "mass", "kt", "kiloton", "kiloton", 1e6))
StoxUnits <- rbind(StoxUnits, list("length-mm", "length", "mm", "mm", "millimeter", 1e-3))
StoxUnits <- rbind(StoxUnits, list("length-cm", "length", "cm","cm", "centimeter", 1e-2))
StoxUnits <- rbind(StoxUnits, list("length-m", "length", "m", "m", "meter", 1))
StoxUnits <- rbind(StoxUnits, list("length-km", "length", "km", "km", "kilometer", 1e3))
StoxUnits <- rbind(StoxUnits, list("length-nmi", "length", "M", "nmi", "nautical mile", 1852)) #M is hydrographic standard
StoxUnits <- rbind(StoxUnits, list("cardinality-N", "cardinality", "N", "individuals", "individuals", 1))
StoxUnits <- rbind(StoxUnits, list("cardinality-kN", "cardinality", "kN", "10^3 individuals", "thousand individuals", 1e3))
StoxUnits <- rbind(StoxUnits, list("cardinality-MN", "cardinality", "MN", "10^6 individuals", "million individuals", 1e6))
StoxUnits <- rbind(StoxUnits, list("fraction-decimal", "fraction", "0.", "fraction", "decimal", 1))
StoxUnits <- rbind(StoxUnits, list("fraction-percent", "fraction", "%", "%", "percent", 1e-2))
# treat age separate from time, as it is not a fixed ratio to actual time units
StoxUnits <- rbind(StoxUnits, list("age-year", "age", "y", "year", "year", 1))
StoxUnits <- rbind(StoxUnits, list("time-s", "time", "s", "second", "second", 1))

stopifnot(!any(duplicated(paste(StoxUnits$id))))

StoxUnits <- StoxUnits[,c("id", "conversion", "quantity", "symbol", "shortname", "name")]

usethis::use_data(StoxUnits, overwrite = T)

