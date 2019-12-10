library(data.table)
library(usethis)

# Read Data file and translation table
stoxBioticObject <- list()

# NMDBioticv3
stoxBioticObject$indageHeadersList[["nmdbioticv3"]] <- c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "specimenid")

## Format: {source variable, target keyname}  
stoxBioticObject$tableKeyList[["nmdbioticv3"]] <- list(
                 list("cruise", "CruiseKey"), 
                 list("serialnumber", "StationKey"),
                 list("gear", "HaulKey"),
                 list(c("commonname", "catchcategory", "aphia", "scientificname"), "SpeciesCategoryKey"),
                 list("catchpartnumber", "SampleKey"),
                 list("specimenid", "IndividualKey"),
                 list("preysampleid", "SubIndividualKey")
                )
stoxBioticObject$tableMapList[["nmdbioticv3"]] <- list(list("mission", "Cruise"), list("individual", "Individual"), list("prey", "SubIndividual")) 
stoxBioticObject$complexMaps[["nmdbioticv3"]] <- fread("stox-translate.csv", stringsAsFactors=FALSE)

# Universal second phase conversion
stoxBioticObject$convertTable <- fread("stox-biotic-final-phase.csv", stringsAsFactors=FALSE)


use_data(stoxBioticObject, overwrite = TRUE)
