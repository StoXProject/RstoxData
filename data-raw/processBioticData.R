library(data.table)
library(usethis)

# setwd(file.path(getwd(), "RstoxData", "data-raw"))

# Read Data file and translation table
stoxBioticObject <- list()
stoxBioticObject$tableMapList <- list()
stoxBioticObject$complexMaps <- list()
stoxBioticObject$convertLenRes <- list()
stoxBioticObject$convertLen <- list()
stoxBioticObject$convertWt <- list()
stoxBioticObject$borrowVariables <- list()


##### NMDBioticv3.1 #####
stoxBioticObject$indageHeadersList[["nmdbioticv3.1"]] <- c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "specimenid")

## Format: {source variable, target keyname}
stoxBioticObject$tableKeyList[["nmdbioticv3.1"]] <- list(
                 Cruise = list(c("cruise", "missiontype", "startyear", "platform", "missionnumber"), "CruiseKey"),
                 Station = list("station", "StationKey"),
                 Haul = list("serialnumber", "HaulKey"),
                 SpeciesCategory = list(c("commonname", "catchcategory", "aphia", "scientificname"), "SpeciesCategoryKey"),
                 #Sample = list("catchpartnumber", "SampleKey"),
                 Sample = list("catchsampleid", "SampleKey"),
                 Individual = list("specimenid", "IndividualKey"),
                 SubIndividual = list("preysampleid", "SubIndividualKey")
                )
#stoxBioticObject$tableMapList[["nmdbioticv3.1"]] <- list(list("mission", "Cruise"), list("individual", "Individual"), list("prey", "SubIndividual"))
stoxBioticObject$tableMapList[["nmdbioticv3.1"]] <- list(list("mission", "Cruise"), list("prey", "SubIndividual"))
stoxBioticObject$complexMaps[["nmdbioticv3.1"]] <- fread("stox-translate-nmdbioticv3.1.csv", stringsAsFactors=FALSE)
## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv3.1"]] <- function(x) {
	z <- c(0.001, 0.005, 0.01, 0.03, 0.05, 0.0005, 0.0001, 0.0001, 0.002, 0.003, 0.02, 0.2) * 100
	names(z) <- seq_len(12)
	return(z[x])
}
stoxBioticObject$convertLen[["nmdbioticv3.1"]] <- NULL
stoxBioticObject$convertWt[["nmdbioticv3.1"]] <- NULL
stoxBioticObject$borrowVariables[["nmdbioticv3.1"]] <- list(
	list(
		variable = "lengthmeasurement", 
		source = "Sample", 
		target = "Individual"
	), 
	list(
		variable = c("stationstopdate", "stationstoptime", "stationstartdate", "stationstarttime"), 
		source = "Station", 
		target = "Haul"
	)#, 
	#list(
	#	variable = "age", 
	#	source = "SubIndividual", 
	#	target = "Individual"
	#)
)


##### NMDBioticv3 #####
stoxBioticObject$indageHeadersList[["nmdbioticv3"]] <- c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "specimenid")

## Format: {source variable, target keyname}  
stoxBioticObject$tableKeyList[["nmdbioticv3"]] <- list(
                 Cruise = list(c("cruise", "missiontype", "startyear", "platform", "missionnumber"), "CruiseKey"), 
                 Station = list("station", "StationKey"),
                 Haul = list("serialnumber", "HaulKey"),
                 SpeciesCategory = list(c("commonname", "catchcategory", "aphia", "scientificname"), "SpeciesCategoryKey"),
                 #Sample = list("catchpartnumber", "SampleKey"),
                 Sample = list("catchsampleid", "SampleKey"),
                 Individual = list("specimenid", "IndividualKey"),
                 SubIndividual = list("preysampleid", "SubIndividualKey")
                )
#stoxBioticObject$tableMapList[["nmdbioticv3"]] <- list(list("mission", "Cruise"), list("individual", "Individual"), list("prey", "SubIndividual")) 
stoxBioticObject$tableMapList[["nmdbioticv3"]] <- list(list("mission", "Cruise"), list("prey", "SubIndividual")) 
stoxBioticObject$complexMaps[["nmdbioticv3"]] <- fread("stox-translate-nmdbioticv3.csv", stringsAsFactors=FALSE)
## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv3"]] <- function(x) {
	z <- c(0.001, 0.005, 0.01, 0.03, 0.05, 0.0005, 0.0001, 0.0001, 0.002, 0.003, 0.02, 0.2) * 100
	names(z) <- seq_len(12)
	return(z[x])
}
stoxBioticObject$convertLen[["nmdbioticv3"]] <- NULL
stoxBioticObject$convertWt[["nmdbioticv3"]] <- NULL
stoxBioticObject$borrowVariables[["nmdbioticv3"]] <- list(
	list(
		variable = "lengthmeasurement", 
		source = "Sample", 
		target = "Individual"
	), 
	list(
		variable = c("stationstopdate", "stationstoptime", "stationstartdate", "stationstarttime"), 
		source = "Station", 
		target = "Haul"
	)#, 
	#list(
	#	variable = "age", 
	#	source = "SubIndividual", 
	#	target = "Individual"
	#)
)


##### NMDBioticv1.4 #####
stoxBioticObject$indageHeadersList[["nmdbioticv1.4"]] <- NULL

## Format: {source variable, target keyname}
stoxBioticObject$tableKeyList[["nmdbioticv1.4"]] <- list(
                 Cruise = list(c("cruise", "missiontype", "year", "platform", "missionnumber"), "CruiseKey"),
                 Station = list("station", "StationKey"),
                 Haul = list("serialno", "HaulKey"),
                 SpeciesCategory = list(c("noname", "species", "aphia", "group"), "SpeciesCategoryKey"),
                 Sample = list("samplenumber", "SampleKey"),
                 Individual = list("specimenno", "IndividualKey"),
                 SubIndividual = list("fishno", "SubIndividualKey")
                )
#stoxBioticObject$tableMapList[["nmdbioticv1.4"]] <- list(list("mission", "Cruise"), list("individual", "Individual"), list("prey", "SubIndividual"))
stoxBioticObject$tableMapList[["nmdbioticv1.4"]] <- list(list("mission", "Cruise"), list("prey", "SubIndividual"))
stoxBioticObject$complexMaps[["nmdbioticv1.4"]] <- fread("stox-translate-nmdbioticv1.4.csv", stringsAsFactors=FALSE)
## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv1.4"]] <- function(x) {
	z <- c(0.001, 0.005, 0.01, 0.03, 0.05, 0.0005, 0.0001, 0.0001, 0.002, 0.003, 0.02, 0.2) * 100
	names(z) <- seq_len(12)
	return(z[x])
}
stoxBioticObject$convertLen[["nmdbioticv1.4"]] <- NULL
stoxBioticObject$convertWt[["nmdbioticv1.4"]] <- NULL
stoxBioticObject$borrowVariables[["nmdbioticv1.4"]] <- list(
	list(
		variable = "lengthmeasurement", 
		source = "Sample", 
		target = "Individual"
	), 
	list(
		variable = c("stopdate.fishstation", "stoptime", "startdate.fishstation", "starttime"), 
		source = "Station", 
		target = "Haul"
	)#, 
	#list(
	#	variable = "age", 
	#	source = "SubIndividual", 
	#	target = "Individual"
	#)
)


##### NMDBioticv1.1 #####
stoxBioticObject$indageHeadersList[["nmdbioticv1.1"]] <- stoxBioticObject$indageHeadersList[["nmdbioticv1.4"]]

## Format: {source variable, target keyname}
stoxBioticObject$tableKeyList[["nmdbioticv1.1"]] <- stoxBioticObject$tableKeyList[["nmdbioticv1.4"]]
stoxBioticObject$tableMapList[["nmdbioticv1.1"]] <- stoxBioticObject$tableMapList[["nmdbioticv1.4"]]
stoxBioticObject$complexMaps[["nmdbioticv1.1"]] <- fread("stox-translate-nmdbioticv1.1.csv", stringsAsFactors=FALSE)
## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv1.1"]] <- stoxBioticObject$convertLenRes[["nmdbioticv1.4"]]
stoxBioticObject$convertLen[["nmdbioticv1.1"]] <- stoxBioticObject$convertLen[["nmdbioticv1.4"]]
stoxBioticObject$convertWt[["nmdbioticv1.1"]] <- stoxBioticObject$convertWt[["nmdbioticv1.4"]]
stoxBioticObject$borrowVariables[["nmdbioticv1.1"]] <- stoxBioticObject$borrowVariables[["nmdbioticv1.4"]]






##### icesBiotic #####
stoxBioticObject$indageHeadersList[["icesBiotic"]] <- NULL

## Format: {source variable, target keyname}
stoxBioticObject$tableKeyList[["icesBiotic"]] <- list(
                 Cruise = list("LocalID", "CruiseKey"),
                 Station = list("StationName", "StationKey"),
                 Haul = list("Number", "HaulKey"),
                 SpeciesCategory = list("SpeciesCode", "SpeciesCategoryKey"),
                 Sample = list("SpeciesCategory", "SampleKey"),
                 Individual = list("FishID", "IndividualKey")
                )
stoxBioticObject$tableMapList[["icesBiotic"]] <- list(list("Cruise", "Cruise"), list("Biology", "Individual"))
stoxBioticObject$complexMaps[["icesBiotic"]] <- fread("stox-translate-icesBiotic.csv", stringsAsFactors=FALSE)

## Length Res conversion
stoxBioticObject$convertLenRes[["icesBiotic"]] <- function(resName) {
	resNumeric_cm <- c(1, 0.5, 0.1)
	names(resNumeric_cm) <- c("cm", "halfcm", "mm")
	# Assume that the length resolution  is already translated to actual values:
    #xx <- tail(unlist(strsplit(x, "[_]")), 1)
    return(resNumeric_cm[resName])
}

## Length conversion
stoxBioticObject$convertLen[["icesBiotic"]] <- function(inputUnit, outputUnit) {

	# Define units
	# mm and halfcm are reported in mm as per http://vocab.ices.dk/?ref=1486:
	weightFactor <- c(mm = 10, halfcm = 10, cm = 1)
	outputFactor <- c(mm = 10, cm = 1)
	conversionTable <- outer(
    	1 / weightFactor, 
    	1 / weightFactor
    )
    
    unitFactor <- conversionTable[cbind(inputUnit, outputUnit)]
    
    return(unitFactor)
}

## Weight conversion
stoxBioticObject$convertWt[["icesBiotic"]] <- function(inputUnit, outputUnit) {

    # Define units
    weightFactor <- c(gr = 1000, kg = 1)
	conversionTable <- outer(
		1 / weightFactor, 
		weightFactor
	)
	
	unitFactor <- conversionTable[inputUnit, outputUnit]
	
	return(unitFactor)
}

# Universal second phase conversion
stoxBioticObject$convertTable <- fread("stox-biotic-final-phase.csv", stringsAsFactors=FALSE)


use_data(stoxBioticObject, overwrite = TRUE)

