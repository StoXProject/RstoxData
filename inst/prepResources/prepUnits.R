#
# Run this script to regnerate the RstoxData resource 'StoxUnits'
# If the format is extended, update the documentation in R/Stox-Units-datadoc.R
#

StoxUnits <- data.table::data.table(quantity=character(),
                                    symbol=character(),
                                    name=character(),
                                    conversion=numeric())

StoxUnits <- rbind(StoxUnits, list("mass", "g", "gram", 1e-3))
StoxUnits <- rbind(StoxUnits, list("mass", "kg", "kilogram", 1))
StoxUnits <- rbind(StoxUnits, list("mass", "t", "metric ton", 1e3))
StoxUnits <- rbind(StoxUnits, list("mass", "kt", "kiloton", 1e6))
StoxUnits <- rbind(StoxUnits, list("length", "mm", "millimeter", 1e-3))
StoxUnits <- rbind(StoxUnits, list("length", "cm", "centimeter", 1e-2))
StoxUnits <- rbind(StoxUnits, list("length", "m", "meter", 1))
StoxUnits <- rbind(StoxUnits, list("length", "km", "kilometer", 1e3))
StoxUnits <- rbind(StoxUnits, list("length", "nmi", "nautical mile", 1852))
StoxUnits <- rbind(StoxUnits, list("cardinality", "i", "individuals", 1))
StoxUnits <- rbind(StoxUnits, list("cardinality", "ki", "thousand individuals", 1e3))
StoxUnits <- rbind(StoxUnits, list("cardinality", "Mi", "million individuals", 1e6))

stopifnot(!any(duplicated(StoxUnits$symbol)))

usethis::use_data(StoxUnits, overwrite = T)

