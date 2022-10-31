#' Convert BioticData to StoxBioticData
#'
#' @inheritParams ModelData
#' @inheritParams general_arguments
#' 
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
#' 
#' @seealso The definition of the \code{\link[=StoxBioticFormat]{StoxBiotic format}} and \code{\link{generalSamplingHierarhcy}}.
#'
#' @export
#' 
StoxBiotic <- function(BioticData) {
	# Convert from BioticData to the general sampling hierarchy:
	GeneralSamplingHierarchy <- BioticData2GeneralSamplingHierarchy(BioticData, NumberOfCores = 1L)
	
	# Extract the StoxBiotic data and rbind across files:
	StoxBioticData <- GeneralSamplingHierarchy2StoxBiotic(GeneralSamplingHierarchy, NumberOfCores = 1L)
	
	# Remove rows of duplicated keys:
	#StoxBioticData <- removeRowsOfDuplicatedKeysFromStoxBioticData(StoxBioticData)
	StoxBioticData <- removeRowsOfDuplicatedKeys(
		StoxData = StoxBioticData, 
		stoxDataFormat = "Biotic"
	)
	
	# Order rows:
	orderRowsByKeys(StoxBioticData)
	
	# Ensure that the numeric values are rounded to the defined number of digits:
	#setRstoxPrecisionLevel(StoxBioticData)
	
	return(StoxBioticData)
}


# Function to convert each element (representing input files) a BioticData object to the general sampling hierarchy:
BioticData2GeneralSamplingHierarchy <- function(
	BioticData, 
	NumberOfCores = 1L, 
	#AddToLowestTable = FALSE, 
	SplitTableAllocation = c("Default", "Lowest", "Highest")
) {
	
	# Make a copy, since we are modifying things by reference:
	BioticDataCopy <- data.table::copy(BioticData)
    
	# Run the first phase possibly on several cores:
	lapplyOnCores(
		BioticDataCopy, 
		FUN = StoxBiotic_firstPhase, 
		NumberOfCores = NumberOfCores,
		#AddToLowestTable = AddToLowestTable
		SplitTableAllocation = SplitTableAllocation
	)
}

# Function to convert rbind :
GeneralSamplingHierarchy2StoxBiotic <- function(GeneralSamplingHierarchy, NumberOfCores = integer()) {
    
	# Run the second phase possibly on several cores:
	StoxBioticData <- lapplyOnCores(
		GeneralSamplingHierarchy, 
		FUN = StoxBiotic_secondPhase, 
		NumberOfCores = NumberOfCores
	)
	
	# Rbind for each StoxBiotic table:
	StoxBioticData <- rbindlist_StoxFormat(StoxBioticData)
    
	return(StoxBioticData)
}


# Function to rbind each table of the StoX format. The input is a list of either StoxBiotic or StoxAcoustic formats (not mixed in the same list):
rbindlist_StoxFormat <- function(x) {
	# Rbind for each StoX format table:
	tableNames <- names(x[[1]])
	x <- lapply(
		tableNames, 
		function(name) data.table::rbindlist(lapply(x, "[[", name))
	)
	names(x) <- tableNames
	return(x)
}


# Function to convert the data read from one type of biotic data into the general sampling hierarchy for biotic data defined by StoX:
#' @importFrom data.table .N setindexv
firstPhase <- function(
	data, 
	datatype, 
	stoxBioticObject, 
	#AddToLowestTable = FALSE
	SplitTableAllocation = c("Default", "Lowest", "Highest")
) {
    
	# Getting data for the datatype
    tableKey <- stoxBioticObject$tableKeyList[[datatype]]
    tableMap <- stoxBioticObject$tableMapList[[datatype]]
    indageHeaders <- stoxBioticObject$indageHeadersList[[datatype]]
    
    # 1. Merge: a) Cruise number with everything b) age reading and individual (specific to data source type)
    if( datatype %in% c("nmdbioticv3", "nmdbioticv3.1") ) {
    	
        ## If preferredagereading in indivdiual is NA, use 1 as the preferred reading
        data$individual[,preferredagereading:= ifelse(is.na(preferredagereading), 1, preferredagereading)]
        ## Merge individual and age
    	data$individual <- merge(data$individual, data$agedetermination, by.x=c(indageHeaders, "preferredagereading"), by.y=c(indageHeaders, "agedeterminationid"), all.x=TRUE)
    	
    	# Merge in the tag:
    	data$individual <- mergeByIntersect(data$individual, data$tag, all.x=TRUE)
    	
    	## Merge individual and tag
    	#data$individual <- merge(data$individual, data$tag, by.x=indageHeaders, by.y=indageHeaders, all.x=TRUE)
    	
    	## Cascading merge tables
        toMerge <- c("mission", "fishstation", "catchsample", "individual", "prey")
        data <- mergeDataTables(data, toMerge)
    } 
    else if(datatype %in% c("nmdbioticv1.1", "nmdbioticv1.4")) {

	    ## Merge individual and age
	    indageHeaders <- intersect(names(data$agedetermination), names(data$individual))
	    data$individual[,preferredagereading:= 1]
	    data$individual <- merge(data$individual, data$agedetermination, by.x=c(indageHeaders, "preferredagereading"), by.y=c(indageHeaders, "no"), all.x=TRUE)

	    ## Cascading merge tables
	    toMerge <- c("mission", "fishstation", "catchsample", "individual")

	    # Use custom suffixes as it contains duplicate header name between tables
	    data <- mergeDataTables(data, toMerge)
	  } 
    else if(datatype == "icesBiotic") {

    	# Merge Cruise and Survey name
	    data[["Cruise"]][, Survey := tail(data[["Survey"]], 1)]
    	
    	# Apply translations defined in the table 'vocabulary':
    	if(length(data$vocabulary)) {
    		tablesToTranslate <- setdiff(names(data), c("metadata", "vocabulary"))
    		tablesToTranslate <- tablesToTranslate[sapply(data[tablesToTranslate], nrow) > 0]
    		
    		vocabulary <- findVariablesMathcinigVocabulary(
    			vocabulary = data$vocabulary, 
    			data = data[tablesToTranslate]
    		)
    		# Uniqueify since some columns (keys) are present in several tables:
    		vocabulary <- unique(vocabulary)
    		
    		translateVariables(
    			data = data[tablesToTranslate], 
    			TranslationDefinition = "FunctionInput",
    			Translation = vocabulary, 
    			translate.keys = TRUE
    		)
    	}
    	
    	## Cascading merge tables
	    toMerge <- c("Cruise", "Haul", "Catch")

	    # Use custom suffixes as it contains duplicate header name between tables
	    data <- mergeDataTables(data, toMerge)
	    
	    # Merge Biology manually as there is no obvious key relation with Catch table
	    toAdd <- c("WeightUnit", "LengthCode", "LengthClass")
	    setnames(data$Biology, toAdd, paste0(toAdd, ".Biology"))

	    # Set intersection column
	    byVars <- intersect(names(data$Catch), names(data$Biology))
	    
	    # Get row count
	    nrowC <- nrow(data$Biology)
	    
	    # Match keys of Catch and Biotic and obtain the indices in the Biology table for each row of the Catch table: 
	    keyColumns <- intersect(names(data$Catch), names(data$Biology))
	    keysCatchPasted <- data$Catch[, do.call(paste0, .SD), .SDcols = keyColumns]
	    keysBiologyPasted <- data$Biology[, do.call(paste0, .SD), .SDcols = keyColumns]
	    BiologyIndex <- split(seq_along(keysBiologyPasted), match(keysBiologyPasted, keysCatchPasted))
	    catchIndicesInBiology <- as.list(double(length(keysCatchPasted)))
	    catchIndicesInBiology[as.numeric(names(BiologyIndex))] <- BiologyIndex
	    
	    # All individuals stored in the Biology table have weight measured:
	    data$Biology[, WeightMeasurement := TRUE]
	    
	    # Add the number of sampled individuals in the Biology table for each combination of the keys of the Catch table:
	    data$Biology[, NumberOfSampledIndividuals := .N, by = c(byVars, "LengthCode.Biology", "LengthClass.Biology")]
	    # Merge this into the Catch table:
	    byVarX <- c(byVars, "LengthCode", "LengthClass")
	    byVarY <- c(byVars, "LengthCode.Biology", "LengthClass.Biology")
	    data$Catch <- merge(
	    	data$Catch, 
	    	unique(data$Biology[, c(byVarY, "NumberOfSampledIndividuals"), with=FALSE]), 
	    	by.x = byVarX, 
	    	by.y = byVarY, 
	    	all.x = TRUE, 
	    	sort = FALSE
	    )
	    
	    # Translate NAs to 0, and calculate also the number of individuals to generate from average weigth:
	    data$Catch[is.na(NumberOfSampledIndividuals), NumberOfSampledIndividuals := 0]
	    data$Catch[, NumberOfIndividualsToGenerate := NumberAtLength - NumberOfSampledIndividuals]
		# Get the indices of the rows of the Catch table for which we should generate individuals:
	    atGenerateIndividuals <- which(
	    	data$Catch$NumberAtLength > data$Catch$NumberOfSampledIndividuals & 
	    	!is.na(data$Catch$LengthCode)
	    )
	    
	    # Generate the individuals:
	    generatedBiology <- lapply(
	    	atGenerateIndividuals, 
	    	specialMerge, 
	    	Catch = data$Catch, 
	    	byVars = byVars
	    )
	    # Combine results and merge add to the Biology table:
	    generatedBiology <- rbindlist(generatedBiology, use.names = TRUE)
	    data$Biology <- rbind(
	    	data$Biology, 
	    	generatedBiology, # Place the generated individuals last.
	    	fill = TRUE
	    )
	    
	    
	    # Merge in also the StationName from Haul to Biology, as it is not a key in the ICESBiotic:
	    data$Biology <- mergeByIntersect(data$Biology, unique(data$Catch[, c(byVars, "StationName"), with = FALSE]))
	    
	    # Order by the keys:
	    data.table::setorderv(data$Biology, byVars)
	    
	    
	    # Sanity check (old Biology row number must be the same with the merged product, _if there is no WeightMeasurement == FALSE_)
	    if(nrowC != nrow(data$Biology[WeightMeasurement == TRUE,])) {
	    	stop("Error in merging.")
	    }
	    
	    ### # Keep only the original columns and the "StationName", which is not a key in the ICESBiotic:
	    ### columnsToKeep <- c(
	    ### 	names(data$Biology), 
	    ### 	"StationName"
	    ### )
	    ### data$Biology <- data$Biology[, .SD, .SDcols = columnsToKeep]

	    # Fix Individual ID for those coming from Catch:
	    FishIDBy <- head(sapply(tableKey, "[[", 1), -1)
	    
	    # Does not work, as FishID is integer
	    #data$Biology[is.na(FishID), FishID := paste0("FromCatch_", seq_len(.N)), by = FishIDBy]
	    
	    data$Biology[, maxFishID := max(0, FishID, na.rm = TRUE), by = FishIDBy]
	    data$Biology[is.na(FishID), FishID := seq_len(.N) + maxFishID, by = FishIDBy]
	    data$Biology[, maxFishID := NULL]
	    
	    # Fix the SampleNumber
		#colAgg <- setdiff(colnames(data$Catch), c("NumberAtLength", "WeightAtLength", "LengthCode", "LengthClass", "LengthType"))
		data$Catch[, SubsampledNumber:=ifelse(!is.na(NumberAtLength), sum(NumberAtLength), SubsampledNumber), by=byVars]
		# Purge duplicate samples
		#data$Catch <- data$Catch[!duplicated(data$Catch[, ..byVars])]
		data$Catch <- unique(data$Catch, by = byVars)
	  } 
    else {
	    warning("StoX: Invalid data input format ", datatype, ". Only NMD Biotic ver 1.4 / ver 3 and ices Biotic formats that are supported for now.")
	    return(NULL)
    }
    
    # 2. Making keys
    for(curr in names(data)) {
        tmpKeys <- c()
        for(key in tableKey) {
            if(all(key[[1]] %in% names(data[[curr]]))) {
                data[[curr]][, key[[2]] := do.call(paste, c(.SD, sep="/")), .SDcols = key[[1]]]
                tmpKeys <- c(tmpKeys,  key[[2]])
            }
        }
        setindexv(data[[curr]], tmpKeys)
    }
    
    # Special warning if there are duplicate station for NMDBiotic:
    CruiseStationKeys <- c("cruise", "station")
    if("fishstation" %in% names(data) && any(duplicated(data$fishstation, by = CruiseStationKeys))) {
    	numStations <- NROW(unique(data$fishstation, by = CruiseStationKeys))
    	
    	dup <- duplicated(data$fishstation, by = CruiseStationKeys) | duplicated(data$fishstation, by = CruiseStationKeys, fromLast = TRUE)
    	stationsWithDuplicatedSerialnumber <- unique(subset(data$fishstation, dup, select = CruiseStationKeys))
    	stationsWithDuplicatedSerialnumber <- sort(stationsWithDuplicatedSerialnumber[, do.call(paste, c(.SD, list(sep = "/")))])
    	
    	numDup <- length(stationsWithDuplicatedSerialnumber)
    	
    	warning("StoX: There are more than one 'serialnumber' (HaulKey in StoxBioticData) for ", numDup, " out of ", numStations," 'station'(StationKey in StoxBioticData) in the NMDBiotic data. In DefineBioticAssignment() it is currently only possible to asssing all hauls of a station in the map (manual assignment). If certain Hauls should be exclcuded, use FilterStoxBiotic(). Duplicated serialnumber for the following cruise/station (of the fishstation table of the BioticData):", printErrorIDs(stationsWithDuplicatedSerialnumber))
    }
    
    # 3. One to one mapping and keys
    firstPhaseTables <- list()
    
    # Add the tables which should simply be renamed, and not split into several tables:
    for(map in tableMap) {
        firstPhaseTables[[map[[2]]]] <- data[[map[[1]]]]
    }
    
    # 4. "COMPLEX" mapping
    complexMap <- getComplexMap(
    	datatype = datatype, 
    	stoxBioticObject = stoxBioticObject, 
    	#AddToLowestTable = AddToLowestTable
    	SplitTableAllocation = SplitTableAllocation
    )
    
    # Get the different origin tables, and loop through these:
    levels <- unlist(unique(complexMap[, "level"]))
    for(origin in levels) {
    	# Get the possible destination tables, and loop through these:
    	destinations <- unlist(unique(complexMap[level == origin, "target"]))
    	for (dest in destinations) {
    		# List the columns to copy to the destination table:
            colList <- unlist(complexMap[target==dest & level==origin, "variable"])
            # Allow missing columns, specifically agedeterminationid, as this was used for merging (and is thus missing in the data):
            colList <- intersect(colList, names(data[[origin]]))
            
            if(!is.null(firstPhaseTables[[dest]])) stop("Error")
            # Copy the columns:
            firstPhaseTables[[dest]] <- data[[origin]][, ..colList]
            # Uniquify, as splitting a table may produce dupicates, e.g. for multiple samples of the same species:
            firstPhaseTables[[dest]] <- unique(firstPhaseTables[[dest]])
        }
    }
    
    ## Set keys for complex mapping
    for (dest in unlist(unique(complexMap[, "target"]))) {
        tmpKeys <- c()
        for(key in tableKey) {
            if(any(key[[2]] %in% names(firstPhaseTables[[dest]]))) {
                tmpKeys <- c(tmpKeys,  key[[2]])
            }
        }
        setindexv(firstPhaseTables[[dest]], tmpKeys)
    }
    
    
    return(firstPhaseTables)
    
}

getComplexMap <- function(
	datatype, 
	stoxBioticObject, 
	#AddToLowestTable = FALSE
	SplitTableAllocation = c("Default", "Lowest", "Highest")
) {
	#if(AddToLowestTable) {
	#	stoxBioticObject$complexMaps_lowestTable[[datatype]]
	#}
	#else {
	#	stoxBioticObject$complexMaps[[datatype]]
	#}
	
	# Read the appropriate mapping data:
	SplitTableAllocation <- match_arg_informative(SplitTableAllocation)
	if(SplitTableAllocation == "Default") {
		stoxBioticObject$complexMaps[[datatype]]
	}
	else if(SplitTableAllocation == "Lowest") {
		stoxBioticObject$complexMaps_lowestTable[[datatype]]
	}
	else if(SplitTableAllocation == "Highest") {
		stoxBioticObject$complexMaps_highestTable[[datatype]]
	}
	
}



# Function for merging the appropriate Catch row with the corresponding Biology record
# Also check for missing biology
# This function generates individuals from the "NumberAtLength", "WeightAtLength", "LengthCode", "LengthClass", "LengthType":
specialMerge <- function(catchRowIndex, Catch, byVars) {
	
	# For convenience extract the current row and the LengthCode:
	thisCatch <- Catch[catchRowIndex, ]
	
	# Create NumberOfIndividualsToGenerate identical individuals:
	averageWeigth <- rep(
		thisCatch$WeightAtLength / thisCatch$NumberAtLength, 
		thisCatch$NumberOfIndividualsToGenerate
	)
	
	generatedBiologyOneCatch <- data.table::data.table(
		thisCatch[, byVars, with = FALSE], 
		FishID = NA, 
		LengthCode = thisCatch$LengthCode, 
		LengthClass = thisCatch$LengthClass, 
		WeightMeasurement = FALSE, 
		IndividualWeight = averageWeigth, 
		WeightUnit = thisCatch$WeightUnit,  
		
		LengthCode.Biology = thisCatch$LengthCode, 
		LengthClass.Biology = thisCatch$LengthClass, 
		WeightUnit.Biology = thisCatch$WeightUnit
		
		
		
	)
	
	return(generatedBiologyOneCatch)
}

# Function to get the StoxBiotic on one file:
StoxBiotic_firstPhase <- function(
	BioticData, 
	#AddToLowestTable = FALSE
	SplitTableAllocation = c("Default", "Lowest", "Highest")
) {
    # Get data type: 
	datatype <- unlist(BioticData[["metadata"]][1, "useXsd"])
	
	if(!exists("stoxBioticObject")) {
        data(stoxBioticObject, package="RstoxData", envir = environment())
    }
    
    # Do first phase
	first <- firstPhase(
		BioticData, 
		datatype, 
		stoxBioticObject, 
		#AddToLowestTable = AddToLowestTable
		SplitTableAllocation = SplitTableAllocation
	)
    
	# Add the metadata:
    first$metadata <- BioticData$metadata
    
    return(first)
}

# Function to convert from the general sampling hierarchy to the StoxBiotic format for each file:
#' @importFrom data.table indices
secondPhase <- function(data, datatype, stoxBioticObject) {
	
	# Getting conversion function for datatype
	convertLenRes <- stoxBioticObject$convertLenRes[[datatype]]
    convertLen <- stoxBioticObject$convertLen[[datatype]]
    convertWt <- stoxBioticObject$convertWt[[datatype]]
    getCatchFractionWeight <- stoxBioticObject$getCatchFractionWeight[[datatype]]
    getSampleWeight <- stoxBioticObject$getSampleWeight[[datatype]]
    getIndividualRoundWeight <- stoxBioticObject$getIndividualRoundWeight[[datatype]]
    getIndividualTotalLength <- stoxBioticObject$getIndividualTotalLength[[datatype]]
    getBottomDepth <- stoxBioticObject$getBottomDepth[[datatype]]
    getDateTime <- stoxBioticObject$getDateTime[[datatype]]
    
    # Try to stop data.table warnings (https://github.com/Rdatatable/data.table/issues/2988)
    .. <- function (x, env = parent.frame()) {
        stopifnot(inherits(x, "character"))
        stopifnot(length(x) == 1)
        get(x, envir = parent.env(env))
    }
    
	columns <- c("variable", "level", datatype, "class")
    if(!datatype %in% names(stoxBioticObject$convertTable)) {
    	stop("The input format ", datatype, " is not yet supprted in RstoxData (")
    }
    convertTable <- stoxBioticObject$convertTable[, ..columns]
    
    # Data placeholder
    secondPhaseTables <- list()
    
    # Borrow variables for use in the conversion defined by convertTable:
    borrowVariables <- stoxBioticObject$borrowVariables[[datatype]]
    if(length(borrowVariables)) {
    	for(ind in seq_along(borrowVariables)) {
    		data[[borrowVariables[[ind]]$target]] <- mergeByStoxKeys(
    			x = data[[borrowVariables[[ind]]$target]], 
    			y = data[[borrowVariables[[ind]]$source]], 
    			StoxDataType = "StoxBiotic", 
    			toMergeFromY = borrowVariables[[ind]]$variable, 
    			all.x = TRUE, 
    			sort = FALSE
    		)
    	}
    }
    
    # Apply the conversions, such as pasting keys, converting to POSIX, etc.:
    for (i in unique(convertTable[, level])) {
    	
    	if(NROW(data[[i]])) {
    		# Process conversion table
    		for(j in 1:nrow(convertTable[level==i,])) {
    			k <- convertTable[level==i,][j,]
    			data[[i]][, (unlist(k[,"variable"])) := eval(parse(text=k[, get(..("datatype"))]))]
    		}
    		# Get key for transfer
    		sourceColumns <- unlist(indices(data[[i]], vectors = TRUE))
    		sourceColumns <- c(sourceColumns, unlist(convertTable[level==i, "variable"]))
    		secondPhaseTables[[i]] <- data[[i]][, ..sourceColumns]
    	}
        else {
        	secondPhaseTables[[i]] <- createEmptyDataTable(convertTable[level == i,]$variable, convertTable[level == i,]$class)
        }
    }
    
    # Remove duplicated rows from SpeciesCategory
    # This has been moved to **** and made general for all tables. The rule is to remove dows of duplicated keys.
    #secondPhaseTables[["SpeciesCategory"]] <- unique(secondPhaseTables[["SpeciesCategory"]])
    # secondPhaseTables <- lapply(secondPhaseTables, unique)
    
    return(secondPhaseTables)
}

# Function to get the StoxBiotic on one file:
StoxBiotic_secondPhase <- function(BioticData) {
    # Get data type: 
    datatype <- unlist(BioticData[["metadata"]][1, "useXsd"])
    
    if(!exists("stoxBioticObject"))
        data(stoxBioticObject, package="RstoxData", envir = environment())
    
    # Do second phase	
    second <- secondPhase(BioticData, datatype, stoxBioticObject)
    
    return(second)
}


#' Merge StoxBioticData
#'
#' @param StoxBioticData A list of StoX biotic data (StoX data type \code{\link{StoxBioticData}}).
#' @param TargetTable The name of the table up until which to merge (the default "Individual" implies merging all tables)
#'
#' @return An object of StoX data type \code{\link{MergeStoxBioticData}}.
#'
#' @importFrom data.table setattr
#' @export
#' 
MergeStoxBiotic <- function(
	StoxBioticData, 
	TargetTable = "Individual"
) {
	
	# Get the tables to merge:
	StoxBioticDataTableNames <- names(StoxBioticData)
	if(! TargetTable %in% StoxBioticDataTableNames) {
		stop("TargetTable must be one of ", paste(StoxBioticDataTableNames, collapse = ", "))
	}
	tableNames <- StoxBioticDataTableNames[seq_len(which(StoxBioticDataTableNames == TargetTable))]
	
	# Get the variable names of the different tables, to add as attributes to the merged data:
	stoxDataVariableNames <- lapply(StoxBioticData, names)
	stoxDataVariableNames <- stoxDataVariableNames[tableNames]
	
	
	# Merge:
    MergeStoxBioticData <- mergeDataTables(StoxBioticData, tableNames = tableNames, output.only.last = TRUE, all = TRUE)
    
    # Move all keys to the start of the table:
    data.table::setcolorder(
    	MergeStoxBioticData, 
    	intersect(
    		names(MergeStoxBioticData), 
    		getRstoxDataDefinitions("StoxBioticKeys")
    	)
    )
    
    # Add the variable names as attributes:
    setattr(
    	MergeStoxBioticData, 
    	"stoxDataVariableNames",
    	stoxDataVariableNames
    )
    
    return(MergeStoxBioticData)
}


#' Add variables to StoxBioticData from BioticData
#'
#' @inheritParams ModelData
#' @param VariableNames A character vector with names of the variables to add from the \code{BioticData}.
#' @param SplitTableAllocation A string indicating how to split tables of the BioticData into tables of the StoxBiotic format. See Details.
#' 
#' @details The default \code{SplitTableAllocation} allocates variables to the StoxBiotic tables according to the mapping defined in \cr\cr
#' \code{RstoxData::stoxBioticObject$complexMaps}. \cr\cr
#' When \code{SplitTableAllocation} is "Lowest" or "Highest" variables are allocated to the lowest or highest table, respectively, as defined in \cr\cr
#' \code{RstoxData::stoxBioticObject$complexMaps_lowestTable} \cr\cr
#' and \cr\cr
#' \code{RstoxData::stoxBioticObject$complexMaps_highestTable}, \cr\cr
#' respectively. In NMDBiotic the tables that are split are fishstation -> Station/Haul (where the first table is the highest and the second is the lowest in the \code{\link[=generalSamplingHierarhcy]{General sampling hierarchy}}) and catchsample -> SpeciesCategory/Sample. In ICESBiotic the tables that are split are Haul -> Station/Haul and Catch -> SpeciesCategory/Sample for NMDBiotic.
#'
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
#'
#' @export
#' 
AddToStoxBiotic <- function(
	StoxBioticData, 
	BioticData, 
	VariableNames = character(), 
	#AddToLowestTable = FALSE, 
	SplitTableAllocation = c("Default", "Lowest", "Highest")
) {
	StoxBioticData <- AddToStoxData(
		StoxData = StoxBioticData, 
		RawData = BioticData, 
		VariableNames = VariableNames, 
		#AddToLowestTable = AddToLowestTable, 
		SplitTableAllocation = SplitTableAllocation, 
		NumberOfCores = 1L, 
		StoxDataFormat = "Biotic"
	)
	
	# Remove rows of duplicated keys:
	StoxBioticData <- removeRowsOfDuplicatedKeys(
		StoxData = StoxBioticData, 
		stoxDataFormat = "Biotic"
	)
	
	return(StoxBioticData)
}



# This can be used later if TowDuration is needed:
# TowDuration,Haul,"as.numeric(difftime(as.POSIXct(paste0(stationstopdate, stationstoptime), format='%Y-%m-%dT%H:%M:%OSZ', tz='GMT'), as.POSIXct(paste0(stationstartdate, stationstarttime), format='%Y-%m-%dZ%H:%M:%OSZ', tz='GMT'), units = 'mins'))","as.numeric(difftime(as.POSIXct(paste0(stationstopdate, stationstoptime), format='%Y-%m-%dZ%H:%M:%OSZ', tz='GMT'), as.POSIXct(paste0(stationstartdate, stationstarttime), format='%Y-%m-%dZ%H:%M:%OSZ', tz='GMT'), units = 'mins'))","as.numeric(difftime(as.POSIXct(paste(stopdate.fishstation, stoptime), format='%d/%m/%Y %H:%M:%S', tz='GMT'), as.POSIXct(paste(startdate.fishstation, starttime), format='%d/%m/%Y %H:%M:%S', tz='GMT'), units = 'mins'))","as.numeric(difftime(as.POSIXct(paste(stopdate.fishstation, stoptime), format='%d/%m/%Y %H:%M:%S', tz='GMT'), as.POSIXct(paste(startdate.fishstation, starttime), format='%d/%m/%Y %H:%M:%S', tz='GMT'), units = 'mins'))","Duration",
# EffectiveTowDuration,Haul,TowDuration,TowDuration,TowDuration,TowDuration,TowDuration,



#' Divide EffectiveTowDistance by fishingdepthcount for NMDBiotic data
#' 
#' This function is specific for use with \code{\link{StoxBioticData}} generated from files of the format NMDBiotic3.0 or higher, where the variable fishingdepthcount can be used to indicate that the trawl was hauled at multiple depths. Use this function with causion, as it assumes that the target species is ONLY located in one of the depths at which the trawl was hauled, and that the trawl was hauled for equal distance at each fishing depth.
#'
#' @inheritParams ModelData
#'
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
#'
#' @export
#' 
CompensateEffectiveTowDistanceForFishingDepthCount <- function(StoxBioticData) {
	# This function requires fishingdepthcount to be present (added using AddToStoxBiotic()):
	if(!"fishingdepthcount" %in% names(StoxBioticData$Haul)) {
		stop("fishingdepthcount must be present in StoxBioticData in order to compensate for multiple fishing depths. Add this using AddToStoxBiotic(). This is only present in NMDBiotic v3 and later.")
	}
	
	# Make a copy and change EffectiveTowDistance:
	StoxBioticDataCopy <- data.table::copy(StoxBioticData)
	StoxBioticDataCopy$Haul[!is.na(fishingdepthcount), EffectiveTowDistance := EffectiveTowDistance / fishingdepthcount]
	
	return(StoxBioticDataCopy)
}


