#
# Run this script to regnerate the RstoxData resource 'StoxUnits'
# If the format is extended, update the documentation in R/Stox-Units-datadoc.R
#
# Units should be considered defined by quantity, id, and conversion. 
# Changing any of these for a unit breaches backwards compatibility wrp to stored units.
# symbol, shortname and name are auxiliary information. 
# Changes to any of these may break backwards compatibility for code that lookip unit by name, symbol or shortname
# 
# Typically, the user "sees" shortname, symbol and name.
# 


setwd(file.path(getwd(), "RstoxData"))
setwd(file.path(getwd(), "data-raw"))

StoxUnits <- data.table::data.table(id=character(),
                                    quantity=character(),
                                    symbol=character(),
                                    shortname=character(),
                                    name=character(),
                                    conversion=numeric())

StoxUnits <- rbind(StoxUnits, list("mass-g", "mass", "g", "g", "gram", 1e-3))
StoxUnits <- rbind(StoxUnits, list("mass-kg", "mass", "kg", "kg", "kilogram", 1))
StoxUnits <- rbind(StoxUnits, list("mass-ton", "mass", "t", "ton", "metric ton", 1e3))
StoxUnits <- rbind(StoxUnits, list("mass-kt", "mass", "kt", "kiloton", "metric kiloton", 1e6))
StoxUnits <- rbind(StoxUnits, list("mass-Mt", "mass", "Mt", "megaton", "metric megaton", 1e9))

StoxUnits <- rbind(StoxUnits, list("length-mm", "length", "mm", "mm", "millimeter", 1e-3))
StoxUnits <- rbind(StoxUnits, list("length-cm", "length", "cm","cm", "centimeter", 1e-2))
StoxUnits <- rbind(StoxUnits, list("length-m", "length", "m", "m", "meter", 1))
StoxUnits <- rbind(StoxUnits, list("length-km", "length", "km", "km", "kilometer", 1e3))
StoxUnits <- rbind(StoxUnits, list("length-nmi", "length", "M", "nmi", "nautical mile", 1852)) #M is hydrographic standard

StoxUnits <- rbind(StoxUnits, list("cardinality-N", "cardinality", "N", "individuals", "individuals", 1))
StoxUnits <- rbind(StoxUnits, list("cardinality-kN", "cardinality", "kN", "10^3 individuals", "thousand individuals", 1e3))
StoxUnits <- rbind(StoxUnits, list("cardinality-MN", "cardinality", "MN", "10^6 individuals", "million individuals", 1e6))
StoxUnits <- rbind(StoxUnits, list("cardinality-GN", "cardinality", "GN", "10^9 individuals", "billion individuals", 1e9))

StoxUnits <- rbind(StoxUnits, list("area_number_density-N/nmi^2", "area_number_density", "N/nmi^2", "individuals/nmi^2", "individuals per square nautical mile", 1))
StoxUnits <- rbind(StoxUnits, list("area_number_density-kN/nmi^2", "area_number_density", "kN/nmi^2", "10^3 individuals/nmi^2", "thousand individuals per square nautical mile", 1e3))
StoxUnits <- rbind(StoxUnits, list("area_number_density-MN/nmi^2", "area_number_density", "MN/nmi^2", "10^6 individuals/nmi^2", "million individuals per square nautical mile", 1e6))

StoxUnits <- rbind(StoxUnits, list("NASC-m^2/nmi^2", "NASC", "m^2/nmi^2", "m^2/nmi^2", "square meter backscattering cross-section per square nautical mile surveyed area", 1))

StoxUnits <- rbind(StoxUnits, list("fraction-decimal", "fraction", "0.", "fraction", "decimal", 1))
StoxUnits <- rbind(StoxUnits, list("fraction-percent", "fraction", "%", "%", "percent", 1e-2))

StoxUnits <- rbind(StoxUnits, list("frequency-Hz", "frequency", "Hz", "Hz", "hertz", 1))
StoxUnits <- rbind(StoxUnits, list("frequency-KHz", "frequency", "kHz", "kHz", "kilohertz", 1000))

# treat age separate from time, as it is not a fixed ratio to actual time units
StoxUnits <- rbind(StoxUnits, list("age-year", "age", "y", "year", "year", 1))
StoxUnits <- rbind(StoxUnits, list("time-s", "time", "s", "s", "second", 1))
StoxUnits <- rbind(StoxUnits, list("datetime-ISO8601", "datetime", "ISO8601", "ISO8601", "ISO8601", 1))

# Degrees:
StoxUnits <- rbind(StoxUnits, list("longitude-degree_east", "angle", "°N", "degree east", "WGS84 degree east", 1))
StoxUnits <- rbind(StoxUnits, list("latitude-degree_north", "angle", "°E", "degree north", "WGS84 degree north", 1))
StoxUnits <- rbind(StoxUnits, list("angle-degree", "angle", "°", "degree", "degree", 1))


stopifnot(!any(duplicated(paste(StoxUnits$id))))

StoxUnits <- StoxUnits[,c("id", "conversion", "quantity", "symbol", "shortname", "name")]

usethis::use_data(StoxUnits, overwrite = T)

