# data-raw/process.R
# XSD data pre-processing

library(xml2)
library(usethis)

setwd(file.path(getwd(), "RstoxData"))

source("R/xsdUtils.R")

xsdFiles <- list.files("data-raw/", pattern="*.xsd",  full.names = TRUE)

xsdObjects <- lapply(xsdFiles, createXsdObject)

names(xsdObjects) <- basename(xsdFiles)

# Result ordering
xsdObjects[["nmdbioticv1.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv1.1.xsd"]]$tableOrder <- c("missions", "mission",  "missionlog", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv1.2.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv1.3.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv1.4.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv3.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylengthfrequencytable", "copepodedevstagefrequencytable", "tag")
xsdObjects[["nmdbioticv3.1.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylengthfrequencytable", "copepodedevstagefrequencytable", "tag")


xsdObjects[["nmdechosounderv1.xsd"]]$tableOrder <- c("echosounder_dataset", "distance_list", "distance", "frequency", "ch_type", "sa_by_acocat", "sa", "acocat_list", "acocat")
xsdObjects[["landingerv2.xsd"]]$tableOrder <- c("Landingsdata", "Seddellinje", "Art", "Produkt", "Dellanding", "Redskap", "Kvote", "Mottakendefartøy", "Fartøy", "Fisker", "Fangstdata", "Produksjon", "Mottaker", "Salgslagdata")

xsdObjects[["icesAcoustic.xsd"]]$tableOrder <- c("Acoustic", "Instrument", "Calibration", "DataAcquisition", "DataProcessing", "Cruise", "Survey", "Log", "Sample", "Data")
xsdObjects[["icesBiotic.xsd"]]$tableOrder <- c("Biotic", "Cruise", "Survey", "Haul", "Catch", "Biology")

use_data(xsdObjects, overwrite = TRUE)

library(data.table)
library(usethis)


setwd(file.path(getwd(), "data-raw"))

# Read Data file and translation table
stoxBioticObject <- list()
stoxBioticObject$tableMapList <- list()
stoxBioticObject$complexMaps <- list()
stoxBioticObject$convertLenRes <- list()
stoxBioticObject$convertLen <- list()
stoxBioticObject$convertWt <- list()
# It was discussed to always compensate for fishingdepthcount, but this needs to be a separate function:
#stoxBioticObject$getEffectiveTowDistance_fishingdepthcount <- list()
stoxBioticObject$borrowVariables <- list()


# Define the keys on each level, to add to the complexMaps:
StoxBioticLevels <- c("Cruise", "Station", "Haul", "SpeciesCategory", "Sample", "Individual")
StoxBioticLevelsSansCruise <- setdiff(StoxBioticLevels, "Cruise")
StoxBioticLevelsNumeric <- structure(seq_along(StoxBioticLevels), names = StoxBioticLevels)
StoxBioticLevelsNumericSansCruise <- StoxBioticLevelsNumeric[names(StoxBioticLevelsNumeric) != "Cruise"]
NMDLevels = c("fishstation", "fishstation", "catchsample", "catchsample", "individual")


keysForComplexMaps <- data.table::data.table(
	level = rep(NMDLevels, StoxBioticLevelsNumericSansCruise), 
	variable = paste0(
		StoxBioticLevels[unlist(lapply(StoxBioticLevelsNumericSansCruise, sequence))], 
		"Key"
	), 
	target = rep(StoxBioticLevelsSansCruise, StoxBioticLevelsNumericSansCruise), 
	iskey = "Y"
)

getIndividualComplexMap <- function(NMDBioticTableName, NMDBioticFormat) {
	NMDBioticFormat.xsd <- paste0(NMDBioticFormat, ".xsd")
	complexMap <- data.table::data.table(
		format = NMDBioticFormat, 
		# The agedetermination has been merged into the individual table, so we can hard code to "individual":
		#level = NMDBioticTableName, 
		level = "individual", 
		variable = xsdObjects[[NMDBioticFormat.xsd]]$tableHeaders[[NMDBioticTableName]], 
		target = "Individual", 
		iskey = ""
	)
	
	# Skip the variables from the higher tables:
	atIndividual <- which(xsdObjects[[NMDBioticFormat.xsd]]$tableOrder == "individual")
	tableJustBeforeIndividual <- xsdObjects[[NMDBioticFormat.xsd]]$tableOrder[atIndividual - 1]
	numberOfVariablesToRemoveAtStart <- xsdObjects[[NMDBioticFormat.xsd]]$prefixLens[tableJustBeforeIndividual]
	
	
	complexMap <- complexMap[-seq_len(numberOfVariablesToRemoveAtStart), ]
	
	return(complexMap)
}


readComplexMap <- function(
	NMDBioticFormat, 
	keysForComplexMaps, 
	#lowestTable = FALSE
	SplitTableAllocation = c("Default", "Lowest", "Highest")
) {
	
	# Read complex maps, and add Keys and variables from individual and agedetermination:
	NMDBioticFormat.csv <- paste0(NMDBioticFormat, ".csv")
	
	# Read the appropriate mapping file:
	SplitTableAllocation <- match.arg(SplitTableAllocation)
	if(SplitTableAllocation == "Default") {
		file <- paste0("stox-translate-", NMDBioticFormat.csv)
	}
	else if(SplitTableAllocation == "Lowest") {
		file <- paste0("stox-translate-lowestTable-", NMDBioticFormat.csv)
	}
	else if(SplitTableAllocation == "Highest") {
		file <- paste0("stox-translate-highestTable-", NMDBioticFormat.csv)
	}
	
	complexMaps <- rbind(
		data.table::fread(file), 
		getIndividualComplexMap("individual", NMDBioticFormat), 
		getIndividualComplexMap("agedetermination", NMDBioticFormat), 
		getIndividualComplexMap("tag", NMDBioticFormat)
	)
	if(any(duplicated(complexMaps$variable))) {
		complexMaps[duplicated(variable), variable := paste(variable, level, sep = ".")]
	}
	
	# Add keys:
	complexMaps <- rbind(
		data.table::data.table(
			format = NMDBioticFormat, 
			keysForComplexMaps
		), 
		complexMaps
	)
	
	return(complexMaps)
}

convertLenRes_NMDBiotic <- function(x) {
	z <- c(0.001, 0.005, 0.01, 0.03, 0.05, 0.0005, 0.0001, 0.0001, 0.002, 0.003, 0.02, 0.2) * 100
	names(z) <- seq_len(12)
	return(z[x])
}

getCatchFractionWeight_NMDBiotic3 <- function(catchweight, catchproducttype, catchcategory) {
	hasInvalid <- !catchproducttype %in% 1 & !is.na(catchweight)
	if(any(hasInvalid)) {
		affectedSpecies <- unique(catchcategory[hasInvalid])
		invalid <- setdiff(unique(catchproducttype), 1)
		warning("StoX: There are catchproducttype that are not 1 (", paste(invalid, collapse = ", "), "), but with non-missing catchweight. This results in missing CatchFractionWeight in StoxBiotic() for the following catchcategory: ", paste(affectedSpecies, collapse = ", "), ".")
	}
	ifelse(catchproducttype %in% 1, catchweight, NA_real_)
}
getCatchFractionWeight_NMDBiotic1 <- function(weight, producttype, species) {
	hasInvalid <- !producttype %in% 1 & !is.na(weight)
	if(any(hasInvalid)) {
		affectedSpecies <- unique(species[hasInvalid])
		invalid <- setdiff(unique(producttype), 1)
		warning("StoX: There are producttype that are not 1 (", paste(invalid, collapse = ", "), "), but with non-missing weight (in the catchsample table). This results in missing CatchFractionWeight in StoxBiotic() for the following species: ", paste(affectedSpecies, collapse = ", "), ".")
	}
	ifelse(producttype %in% 1, weight, NA_real_)
}

getSampleWeight_NMDBiotic3 <- function(lengthsampleweight, sampleproducttype, catchcategory) {
	hasInvalid <- !sampleproducttype %in% 1 & !is.na(lengthsampleweight)
	if(any(hasInvalid)) {
		affectedSpecies <- unique(catchcategory[hasInvalid])
		invalid <- setdiff(unique(sampleproducttype), 1)
		warning("StoX: There are sampleproducttype that are not 1 (", paste(invalid, collapse = ", "), "), but with non-missing lengthsampleweight. This results in missing SampleWeight in StoxBiotic() for the following catchcategory: ", paste(affectedSpecies, collapse = ", "), ".")
	}
	ifelse(sampleproducttype %in% 1, lengthsampleweight, NA_real_)
}
getSampleWeight_NMDBiotic1 <- function(lengthsampleweight, sampleproducttype, species) {
	hasInvalid <- !sampleproducttype %in% 1 & !is.na(lengthsampleweight)
	if(any(hasInvalid)) {
		affectedSpecies <- unique(species[hasInvalid])
		invalid <- setdiff(unique(sampleproducttype), 1)
		warning("StoX: There are sampleproducttype that are not 1 (", paste(invalid, collapse = ", "), "), but with non-missing lengthsampleweight. This results in missing SampleWeight in StoxBiotic() for the following species: ", paste(affectedSpecies, collapse = ", "), ".")
	}
	ifelse(sampleproducttype %in% 1, lengthsampleweight, NA_real_)
}

getIndividualRoundWeight_NMDBiotic3 <- function(individualweight, individualproducttype, catchcategory) {
	hasInvalid <- !individualproducttype %in% 1 & !is.na(individualweight)
	if(any(hasInvalid)) {
		affectedSpecies <- unique(catchcategory[hasInvalid])
		invalid <- setdiff(unique(individualproducttype), 1)
		warning("StoX: There are individualproducttype that are not 1 (", paste(invalid, collapse = ", "), "), but with non-missing individualweight. This results in missing IndividualRoundWeight in StoxBiotic() for the following catchcategory: ", paste(affectedSpecies, collapse = ", "), ".")
	}
	ifelse(individualproducttype %in% 1, individualweight * 1000, NA_real_)
}
getIndividualRoundWeight_NMDBiotic1 <- function(weight.individual, producttype.individual, species) {
	hasInvalid <- !producttype.individual %in% 1 & !is.na(weight.individual)
	if(any(hasInvalid)) {
		affectedSpecies <- unique(species[hasInvalid])
		invalid <- setdiff(unique(producttype.individual), 1)
		warning("StoX: There are producttype.individual that are not 1 (", paste(invalid, collapse = ", "), "), but with non-missing weight (in the individual table). This results in missing IndividualRoundWeight in StoxBiotic() for the following species: ", paste(affectedSpecies, collapse = ", "), ".")
	}
	ifelse(producttype.individual %in% 1, weight.individual * 1000, NA_real_)
}

getIndividualTotalLength_NMDBiotic3 <- function(length, lengthmeasurement, catchcategory) {
	hasInvalid <- !lengthmeasurement %in% 'E' & !is.na(length)
	if(any(hasInvalid)) {
		affectedSpecies <- unique(catchcategory[hasInvalid])
		invalid <- setdiff(unique(lengthmeasurement), 'E')
		warning("StoX: There are lengthmeasurement that are not 'E' (", paste(invalid, collapse = ", "), "), but with non-missing length. This results in missing IndividualTotalLength in StoxBiotic() for the following catchcategory: ", paste(affectedSpecies, collapse = ", "), ".")
	}
	ifelse(lengthmeasurement %in% 'E', length * 100, NA_real_)
}
getIndividualTotalLength_NMDBiotic1 <- function(length, lengthmeasurement, species) {
	hasInvalid <- !lengthmeasurement %in% 'E' & !is.na(length)
	if(any(hasInvalid)) {
		affectedSpecies <- unique(species[hasInvalid])
		invalid <- setdiff(unique(lengthmeasurement), 'E')
		warning("StoX: There are lengthmeasurement that are not 'E' (", paste(invalid, collapse = ", "), "), but with non-missing length. This results in missing IndividualTotalLength in StoxBiotic() for the following species: ", paste(affectedSpecies, collapse = ", "), ".")
	}
	ifelse(lengthmeasurement %in% 'E', length * 100, NA_real_)
}


getBottomDepth_NMDBiotic <- function(bottomdepthstart, bottomdepthstop) {
	hasInvalid <- is.na(bottomdepthstart) | is.na(bottomdepthstop)
	if(any(hasInvalid)) {
		warning("StoX: The BottomDepth is calculated as the average of bottomdepthstart and bottomdepthstop from NMDBiotic. At least one of these are missing (NA) in ", sum(hasInvalid), " out of ", length(hasInvalid), " stations. RedefineStoxBiotic can be used to define the BottomDepth as e.g. bottomdepthstart in the case that this is present.")
	}
	ifelse(hasInvalid, NA_real_, (bottomdepthstart + bottomdepthstop) / 2)
}


getDateTime_NMDBiotic1 <- function(startdate.fishstation, starttime) {
	
	DateTime <- as.POSIXct(paste0(startdate.fishstation, starttime), format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
	
	return(DateTime)
}



getDateTime_NMDBiotic3 <- function(stationstartdate, stationstarttime) {
	
	DateTime <- as.POSIXct(paste0(stationstartdate, stationstarttime), format = "%Y-%m-%dZ%H:%M:%OSZ", tz = "UTC")
	
	return(DateTime)
}


getDateTime_ICESBiotic <- function(StartTime) {
	
	# Try the two allowed time formats of the ICESBiotic:
	allowedTimeFormatsICESBiotic <- c(
		"%Y-%m-%dT%H:%M", 
		"%Y-%m-%d %H:%M"
	)
	
	areNotNAs <- !is.na(StartTime)
	
	DateTime <- NULL
	for(format in allowedTimeFormatsICESBiotic) {
		if(!length(DateTime) || !all(!is.na(DateTime[areNotNAs]))) {
			DateTime <- as.POSIXct(StartTime, tz = "UTC", format = format)
		}
	}
	
	return(DateTime)
}






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

# Read complex maps, and add Keys and variables from individual and agedetermination:
stoxBioticObject$complexMaps[["nmdbioticv3.1"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv3.1", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = FALSE
	SplitTableAllocation = "Default"
)
stoxBioticObject$complexMaps_lowestTable[["nmdbioticv3.1"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv3.1", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = TRUE
	SplitTableAllocation = "Lowest"
)
stoxBioticObject$complexMaps_highestTable[["nmdbioticv3.1"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv3.1", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = TRUE
	SplitTableAllocation = "Highest"
)




## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv3.1"]] <- convertLenRes_NMDBiotic
stoxBioticObject$convertLen[["nmdbioticv3.1"]] <- NULL
stoxBioticObject$convertWt[["nmdbioticv3.1"]] <- NULL
stoxBioticObject$getCatchFractionWeight[["nmdbioticv3.1"]] <- getCatchFractionWeight_NMDBiotic3
stoxBioticObject$getSampleWeight[["nmdbioticv3.1"]] <- getSampleWeight_NMDBiotic3
stoxBioticObject$getIndividualRoundWeight[["nmdbioticv3.1"]] <- getIndividualRoundWeight_NMDBiotic3
stoxBioticObject$getIndividualTotalLength[["nmdbioticv3.1"]] <- getIndividualTotalLength_NMDBiotic3
stoxBioticObject$getBottomDepth[["nmdbioticv3.1"]] <- getBottomDepth_NMDBiotic

# DateTime of NMDBiotic3.1:
stoxBioticObject$getDateTime[["nmdbioticv3.1"]] <- getDateTime_NMDBiotic3

# It was discussed to always compensate for fishingdepthcount, but this needs to be a separate function:
#stoxBioticObject$getEffectiveTowDistance_fishingdepthcount[["nmdbioticv3.1"]] <- function(distance, fishingdepthcount) {
#	fishingdepthcount[is.na(fishingdepthcount)] <-  1
#	EffectiveTowDistance <- distance / fishingdepthcount
#	return(EffectiveTowDistance)
#}
stoxBioticObject$borrowVariables[["nmdbioticv3.1"]] <- list(
	list(
		variable = "lengthmeasurement", 
		source = "Sample", 
		target = "Individual"
	), 
	# Borrow species in order to merge in age-data to Individuals. Species must be borrowed in two steps, SpeciesCategory -> Sample and Sample -> Individual, to preserve uniqueness:
	list(
		variable = "catchcategory", 
		source = "SpeciesCategory", 
		target = "Sample"
	), 
	list(
		variable = "catchcategory", 
		source = "Sample", 
		target = "Individual"
	)
	# This (copying c("stationstopdate", "stationstoptime", "stationstartdate", "stationstarttime")) was used when trying to implement TowDuration, which was later abandoned:
	#, 
	#list(
	#	variable = c("stationstopdate", "stationstoptime", "stationstartdate", "stationstarttime"), 
	#	source = "Station", 
	#	target = "Haul"
	
	#)#, 
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

# Read complex maps, and add Keys and variables from individual and agedetermination:
stoxBioticObject$complexMaps[["nmdbioticv3"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv3", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = FALSE
	SplitTableAllocation = "Default"
)
stoxBioticObject$complexMaps_lowestTable[["nmdbioticv3"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv3", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = TRUE
	SplitTableAllocation = "Lowest"
)
stoxBioticObject$complexMaps_highestTable[["nmdbioticv3"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv3", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = TRUE
	SplitTableAllocation = "Highest"
)

## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv3"]] <- convertLenRes_NMDBiotic
stoxBioticObject$convertLen[["nmdbioticv3"]] <- NULL
stoxBioticObject$convertWt[["nmdbioticv3"]] <- NULL
stoxBioticObject$getCatchFractionWeight[["nmdbioticv3"]] <- getCatchFractionWeight_NMDBiotic3
stoxBioticObject$getSampleWeight[["nmdbioticv3"]] <- getSampleWeight_NMDBiotic3
stoxBioticObject$getIndividualRoundWeight[["nmdbioticv3"]] <- getIndividualRoundWeight_NMDBiotic3
stoxBioticObject$getIndividualTotalLength[["nmdbioticv3"]] <- getIndividualTotalLength_NMDBiotic3
stoxBioticObject$getBottomDepth[["nmdbioticv3"]] <- getBottomDepth_NMDBiotic

# DateTime of NMDBiotic3.0:
stoxBioticObject$getDateTime[["nmdbioticv3"]] <- getDateTime_NMDBiotic3

# It was discussed to always compensate for fishingdepthcount, but this needs to be a separate function:
#stoxBioticObject$getEffectiveTowDistance_fishingdepthcount[["nmdbioticv3"]] <- stoxBioticObject$getEffectiveTowDistance_fishingdepthcount[["nmdbioticv3.1"]]
stoxBioticObject$borrowVariables[["nmdbioticv3"]] <- stoxBioticObject$borrowVariables[["nmdbioticv3.1"]]
	# This (copying c("stationstopdate", "stationstoptime", "stationstartdate", "stationstarttime")) was used when trying to implement TowDuration, which was later abandoned:
	#, 
	#list(
	#	variable = c("stationstopdate", "stationstoptime", "stationstartdate", "stationstarttime"), 
	#	source = "Station", 
	#	target = "Haul"
	#)#, 
	
	#list(
	#	variable = "age", 
	#	source = "SubIndividual", 
	#	target = "Individual"
	#)
#)



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

# Read complex maps, and add Keys and variables from individual and agedetermination:
stoxBioticObject$complexMaps[["nmdbioticv1.4"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv1.4", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = FALSE
	SplitTableAllocation = "Default"
)
stoxBioticObject$complexMaps_lowestTable[["nmdbioticv1.4"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv1.4", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = TRUE
	SplitTableAllocation = "Lowest"
)
stoxBioticObject$complexMaps_highestTable[["nmdbioticv1.4"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv1.4", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = TRUE
	SplitTableAllocation = "Highest"
)

## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv1.4"]] <- convertLenRes_NMDBiotic
stoxBioticObject$convertLen[["nmdbioticv1.4"]] <- NULL
stoxBioticObject$convertWt[["nmdbioticv1.4"]] <- NULL
# It was discussed to always compensate for fishingdepthcount, but this needs to be a separate function:
#stoxBioticObject$getEffectiveTowDistance_fishingdepthcount[["nmdbioticv1.4"]] <- function(distance, fishingdepthmax) {
#	if(any(fishingdepthmax > 9000)) {
#		"StoX: Detected fishingdepthmax > 9000, which has historically been used to indicate trawling at multiple depths. This is not supported by StoX. The data must be converted to NMDBiotic >= 3, and fishingdepthcount, fishingdepthmin and fishingdepthmax interpreted from the codes defined by IMR, in case you need EffectiveTowDistance to be compensated for trawling at multiple depths."
#	}
#	return(distance)
#}
stoxBioticObject$getCatchFractionWeight[["nmdbioticv1.4"]] <- getCatchFractionWeight_NMDBiotic1
stoxBioticObject$getSampleWeight[["nmdbioticv1.4"]] <- getSampleWeight_NMDBiotic1
stoxBioticObject$getIndividualRoundWeight[["nmdbioticv1.4"]] <- getIndividualRoundWeight_NMDBiotic1
stoxBioticObject$getIndividualTotalLength[["nmdbioticv1.4"]] <- getIndividualTotalLength_NMDBiotic1
stoxBioticObject$getBottomDepth[["nmdbioticv1.4"]] <- getBottomDepth_NMDBiotic

# DateTime of NMDBiotic1.4:
stoxBioticObject$getDateTime[["nmdbioticv1.4"]] <- getDateTime_NMDBiotic1

stoxBioticObject$borrowVariables[["nmdbioticv1.4"]] <- list(
	list(
		variable = "lengthmeasurement", 
		source = "Sample", 
		target = "Individual"
	), 
	# Borrow species in order to merge in age-data to Individuals. Species must be borrowed in two steps, SpeciesCategory -> Sample and Sample -> Individual, to preserve uniqueness:
	list(
		variable = "species", 
		source = "SpeciesCategory", 
		target = "Sample"
	), 
	list(
		variable = "species", 
		source = "Sample", 
		target = "Individual"
	)
	# This (copying c("stationstopdate", "stationstoptime", "stationstartdate", "stationstarttime")) was used when trying to implement TowDuration, which was later abandoned:#, 
	#list(
	#	variable = c("stopdate.fishstation", "stoptime", "startdate.fishstation", "starttime"), 
	#	source = "Station", 
	#	target = "Haul"
	#)#, 

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

# Read complex maps, and add Keys and variables from individual and agedetermination:
stoxBioticObject$complexMaps[["nmdbioticv1.1"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv1.1", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = FALSE
	SplitTableAllocation = "Default"
)
stoxBioticObject$complexMaps_lowestTable[["nmdbioticv1.1"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv1.1", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = TRUE
	SplitTableAllocation = "Lowest"
)
stoxBioticObject$complexMaps_highestTable[["nmdbioticv1.1"]] <- readComplexMap(
	NMDBioticFormat = "nmdbioticv1.1", 
	keysForComplexMaps = keysForComplexMaps, 
	#lowestTable = TRUE
	SplitTableAllocation = "Highest"
)

## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv1.1"]] <- stoxBioticObject$convertLenRes[["nmdbioticv1.4"]]
stoxBioticObject$convertLen[["nmdbioticv1.1"]] <- stoxBioticObject$convertLen[["nmdbioticv1.4"]]
stoxBioticObject$convertWt[["nmdbioticv1.1"]] <- stoxBioticObject$convertWt[["nmdbioticv1.4"]]
stoxBioticObject$getCatchFractionWeight[["nmdbioticv1.1"]] <- getCatchFractionWeight_NMDBiotic1
stoxBioticObject$getSampleWeight[["nmdbioticv1.1"]] <- getSampleWeight_NMDBiotic1
stoxBioticObject$getIndividualRoundWeight[["nmdbioticv1.1"]] <- getIndividualRoundWeight_NMDBiotic1
stoxBioticObject$getIndividualTotalLength[["nmdbioticv1.1"]] <- getIndividualTotalLength_NMDBiotic1
stoxBioticObject$getBottomDepth[["nmdbioticv1.1"]] <- getBottomDepth_NMDBiotic

# DateTime of NMDBiotic1.1:
stoxBioticObject$getDateTime[["nmdbioticv1.1"]] <- getDateTime_NMDBiotic1

# It was discussed to always compensate for fishingdepthcount, but this needs to be a separate function:
#stoxBioticObject$getEffectiveTowDistance_fishingdepthcount[["nmdbioticv1.1"]] <- stoxBioticObject$getEffectiveTowDistance_fishingdepthcount[["nmdbioticv1.4"]]
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
stoxBioticObject$complexMaps_lowestTable[["icesBiotic"]] <- fread("stox-translate-lowestTable-icesBiotic.csv", stringsAsFactors=FALSE) 
stoxBioticObject$complexMaps_highestTable[["icesBiotic"]] <- fread("stox-translate-highestTable-icesBiotic.csv", stringsAsFactors=FALSE)

# DateTime of ICESBiotic:
stoxBioticObject$getDateTime[["icesBiotic"]] <- getDateTime_ICESBiotic

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
    	1 / outputFactor
    )
	
	# Keep NAs:
	conversionFactor <- rep(NA_real_, length(inputUnit))
	inputUnit_isNA <- is.na(inputUnit)
	# Get the conversion factor:
	conversionFactor[!inputUnit_isNA] <- conversionTable[cbind(inputUnit[!inputUnit_isNA], outputUnit)]
    
    return(conversionFactor)
}

## Weight conversion
stoxBioticObject$convertWt[["icesBiotic"]] <- function(inputUnit, outputUnit) {

	# Define units
    weightFactor <- c(gr = 1000, kg = 1)
	conversionTable <- outer(
		1 / weightFactor, 
		weightFactor
	)
	
	# Keep NAs:
	conversionFactor <- rep(NA_real_, length(inputUnit))
	inputUnit_isNA <- is.na(inputUnit)
	# Get the conversion factor:
	conversionFactor[!inputUnit_isNA] <- conversionTable[cbind(inputUnit[!inputUnit_isNA], outputUnit)]
	
	return(conversionFactor)
}

# Universal second phase conversion
stoxBioticObject$convertTable <- data.table::fread("stox-biotic-final-phase.csv")


use_data(stoxBioticObject, overwrite = TRUE)

setwd("..")
setwd("..")

