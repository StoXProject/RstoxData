library(data.table)
library(usethis)

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
                 Sample = list("catchpartnumber", "SampleKey"),
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
                 Sample = list("catchpartnumber", "SampleKey"),
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
	)#, 
	#list(
	#	variable = "age", 
	#	source = "SubIndividual", 
	#	target = "Individual"
	#)
)


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
stoxBioticObject$convertLenRes[["icesBiotic"]] <- function(x) {
	z <- c(1, 0.5, 0.1)
	names(z) <- c("cm", "halfcm", "mm")
    xx <- tail(unlist(strsplit(x, "[_]")), 1)
	return(z[xx])
}

## Length conversion
stoxBioticObject$convertLen[["icesBiotic"]] <- function(x, y) {

    # Get units
    xx <- unlist(lapply(strsplit(x, "[_]"), tail, 1))

    if(y == "cm") {
        z <- c(1, 0.01, 0.01)
    } else if (y == "mm") {
        z <- c(100, 1, 1)
    } else {
        print("Invalid length conversion!")
        return(NA)
    }

    names(z) <- c("cm", "halfcm", "mm")
    return(z[xx])
}

## Weight conversion
stoxBioticObject$convertWt[["icesBiotic"]] <- function(x, y) {

    # Get units
    xx <- unlist(lapply(strsplit(x, "[_]"), tail, 1))

    if(y == "kg") {
        z <- c(1, 0.001)
    } else if (y == "gr") {
        z <- c(1000, 1)
    } else {
        print("Invalid length conversion!")
        return(NA)
    }

    names(z) <- c("kg", "gr")
    return(z[xx])
}

# Universal second phase conversion
stoxBioticObject$convertTable <- fread("stox-biotic-final-phase.csv", stringsAsFactors=FALSE)


use_data(stoxBioticObject, overwrite = TRUE)

