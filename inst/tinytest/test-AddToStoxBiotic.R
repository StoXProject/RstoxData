# AddToStoxBiotic
testOneFormat <- function(formatFile, variablesToAdd) {
	example <- system.file("testresources", formatFile, package="RstoxData")
	
	b <- RstoxData::ReadBiotic(example)
	suppressWarnings(s <- RstoxData::StoxBiotic(b))
	
	a <- RstoxData::AddToStoxBiotic(
		StoxBioticData = s, 
		BioticData = b, 
		VariableNames = unlist(variablesToAdd), 
		#AddToLowestTable = FALSE
		SplitTableAllocation = "Default"
	)
	aLowest <- RstoxData::AddToStoxBiotic(
		StoxBioticData = s, 
		BioticData = b, 
		VariableNames = unlist(variablesToAdd), 
		#AddToLowestTable = TRUE
		SplitTableAllocation = "Lowest"
	)
	
	valid0a <- all(unlist(mapply("%in%", variablesToAdd, lapply(a[names(variablesToAdd)], names))))
	variablesToAddLowest <- list(
		Haul = unlist(variablesToAdd[c("Station", "Haul")]), 
		Sample = unlist(variablesToAdd[c("SpeciesCategory", "Sample")])
	)
	valid0b <- all(unlist(mapply("%in%", variablesToAddLowest, lapply(aLowest[names(variablesToAddLowest)], names))))
	valid1 <- all(unlist(variablesToAdd[c("Station", "Haul")]) %in% names(aLowest$Haul))
	valid2 <- all(unlist(variablesToAdd[c("SpeciesCategory", "Sample")]) %in% names(aLowest$Sample))
	valid3 <- !any(unlist(variablesToAdd) %in% names(aLowest$Station))
	valid4 <- !any(unlist(variablesToAdd) %in% names(aLowest$SpeciesCategory))
	
	noDuplicatedStationKey <- !any(duplicated(a[[names(variablesToAdd)[1]]], by = "StationKey"))
	
	out <- list(
		valid0a = valid0a, 
		valid0b = valid0b, 
		valid1 = valid1, 
		valid2 = valid2, 
		valid3 = valid3, 
		valid4 = valid4, 
		noDuplicatedStationKey = noDuplicatedStationKey
	)
	
	return(out)
}

# NMDBiotic v3.1:
variablesToAdd_3.1 <- list(
	Station = c("serialnumber", "area"), # To Station
	Haul = c("fishingdepthstart", "vesselspeed"), # To Haul
	SpeciesCategory = c("scientificname", "foreignobject"), # To SpeciesCategory
	Sample = c("group", "agingstructure") # To Sample
)
expect_true(
	all(
		testOneFormat(
			formatFile = "biotic3.1_example.xml", 
			variablesToAdd = variablesToAdd_3.1
		)
	)
)

# NMDBiotic v3:
variablesToAdd_3 <- list(
	Station = c("serialnumber", "area"), # To Station
	Haul = c("fishingdepthstart", "vesselspeed"), # To Haul
	SpeciesCategory = c("scientificname", "foreignobject"), # To SpeciesCategory
	Sample = c("group", "agingstructure") # To Sample
)
expect_true(
	all(
		testOneFormat(
			formatFile = "biotic_v3_example.xml", 
			variablesToAdd = variablesToAdd_3
		)
	)
)

# NMDBiotic v1.4:
variablesToAdd_1.4 <- list(
	Station = c("serialno", "area"), # To Station
	Haul = c("fishingdepthmin", "doorspread"), # To Haul
	SpeciesCategory = c("noname", "foreignobject"), # To SpeciesCategory
	Sample = c("group", "agingstructure") # To Sample
)
expect_true(
	all(
		testOneFormat(
			formatFile = "biotic_v1.4_example.xml", 
			variablesToAdd = variablesToAdd_1.4
		)
	)
) 

# ICESBiotic:
variablesToAdd_ICES <- list(
	Station = c("TowDirection", "StationName"), # To Station
	Haul = c("MinTrawlDepth", "Gear"), # To Haul
	SpeciesCategory = c("SpeciesValidity", "SpeciesSex"), # To SpeciesCategory
	Sample = c("SpeciesCategoryWeight", "LengthType") # To Sample
)
expect_true(
	all(
		testOneFormat(
			formatFile = "ICES_Biotic_1.xml", 
			variablesToAdd = variablesToAdd_ICES
		)
	)
) 

# Multiple tags should give NA tagid and tagtype
# Read NMDBiotic data and add repeated tags:
bioticFilePath <- system.file("testresources", "biotic3.1_example.xml", package = "RstoxData")
NMDBioticData <- RstoxData::ReadBiotic(bioticFilePath)

# Add fictive tags:
keys <- c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "specimenid")
keysOfLastIndividual <- utils::tail(NMDBioticData[[1]]$individual, 1)[, ..keys]
NMDBioticData[[1]]$tag <- cbind(keysOfLastIndividual, tagid = c(1,2), tagtype = NA)
StoxBioticData <- RstoxData::StoxBiotic(NMDBioticData)
StoxBioticDataAdded <- RstoxData::AddToStoxBiotic(BioticData = NMDBioticData, StoxBioticData = StoxBioticData, VariableNames = "tagid")

expect_true(all(is.na(StoxBioticDataAdded$Individual$tagid)))

NMDBioticData[[1]]$tag <- cbind(keysOfLastIndividual, tagid = c(2), tagtype = NA)
StoxBioticDataAdded <- RstoxData::AddToStoxBiotic(BioticData = NMDBioticData, StoxBioticData = StoxBioticData, VariableNames = "tagid")

expect_equal(StoxBioticDataAdded$Individual$tagid, c(rep(NA, 3), 2))


