library(data.table)
library(usethis)

# Read Data file and translation table
stoxBioticObject <- list()
stoxBioticObject$tableMapList <- list()
stoxBioticObject$complexMaps <- list()
stoxBioticObject$convertLenRes <- list()

# NMDBioticv3
stoxBioticObject$indageHeadersList[["nmdbioticv3"]] <- c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "specimenid")

## Format: {source variable, target keyname}  
stoxBioticObject$tableKeyList[["nmdbioticv3"]] <- list(
                 list(c("cruise", "missiontype", "startyear", "platform", "missionnumber"), "CruiseKey"), 
                 list("station", "StationKey"),
                 list("serialnumber", "HaulKey"),
                 list(c("commonname", "catchcategory", "aphia", "scientificname"), "SpeciesCategoryKey"),
                 list("catchpartnumber", "SampleKey"),
                 list("specimenid", "IndividualKey"),
                 list("preysampleid", "SubIndividualKey")
                )
stoxBioticObject$tableMapList[["nmdbioticv3"]] <- list(list("mission", "Cruise"), list("individual", "Individual"), list("prey", "SubIndividual")) 
stoxBioticObject$complexMaps[["nmdbioticv3"]] <- fread("stox-translate-nmdbioticv3.csv", stringsAsFactors=FALSE)
## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv3"]] <- function(x) {
	z <- c(0.001, 0.005, 0.01, 0.03, 0.05, 0.0005, 0.0001, 0.0001, 0.002, 0.003, 0.02, 0.2) * 100
	names(z) <- seq_len(12)
	return(z[x])
}

# NMDBioticv1.4
stoxBioticObject$indageHeadersList[["nmdbioticv1.4"]] <- c("missiontype", "year", "platform", "missionnumber", "serialno", "samplenumber", "specimenno")

## Format: {source variable, target keyname}
stoxBioticObject$tableKeyList[["nmdbioticv1.4"]] <- list(
                 list(c("cruise", "missiontype", "year", "platform", "missionnumber"), "CruiseKey"),
                 list("station", "StationKey"),
                 list("serialno", "HaulKey"),
                 list(c("noname", "species", "aphia", "group"), "SpeciesCategoryKey"),
                 list("samplenumber", "SampleKey"),
                 list("specimenno", "IndividualKey"),
                 list("fishno", "SubIndividualKey")
                )
stoxBioticObject$tableMapList[["nmdbioticv1.4"]] <- list(list("mission", "Cruise"), list("individual", "Individual"), list("prey", "SubIndividual"))
stoxBioticObject$complexMaps[["nmdbioticv1.4"]] <- fread("stox-translate-nmdbioticv1.4.csv", stringsAsFactors=FALSE)
## Length conversion
stoxBioticObject$convertLenRes[["nmdbioticv1.4"]] <- function(x) {
	z <- c(0.001, 0.005, 0.01, 0.03, 0.05, 0.0005, 0.0001, 0.0001, 0.002, 0.003, 0.02, 0.2) * 100
	names(z) <- seq_len(12)
	return(z[x])
}

# Universal second phase conversion
stoxBioticObject$convertTable <- fread("stox-biotic-final-phase.csv", stringsAsFactors=FALSE)


use_data(stoxBioticObject, overwrite = TRUE)
