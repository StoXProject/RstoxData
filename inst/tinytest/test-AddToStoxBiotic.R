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
	
	valid0 <- all(unlist(mapply("%in%", variablesToAdd[names(variablesToAdd)], lapply(a[names(variablesToAdd)], names))))
	valid1 <- all(unlist(variablesToAdd[c("Station", "Haul")]) %in% names(aLowest$Haul))
	valid2 <- all(unlist(variablesToAdd[c("SpeciesCategory", "Sample")]) %in% names(aLowest$Sample))
	valid3 <- !any(unlist(variablesToAdd) %in% names(aLowest$Station))
	valid4 <- !any(unlist(variablesToAdd) %in% names(aLowest$SpeciesCategory))
	
	noDuplicatedStationKey <- !any(duplicated(a[[names(variablesToAdd)[1]]], by = "StationKey"))
	
	out <- list(
		valid0 = valid0, 
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

