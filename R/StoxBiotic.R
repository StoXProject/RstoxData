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
    setRstoxPrecisionLevel(StoxBioticData)
    
    return(StoxBioticData)
}

# Function to convert each element (representing input files) a BioticData object to the general sampling hierarchy:
BioticData2GeneralSamplingHierarchy <- function(
	BioticData, 
	NumberOfCores = 1L
) {
    # Run the first phase possibly on several cores:
	lapplyOnCores(
		BioticData, 
		FUN = StoxBiotic_firstPhase, 
		NumberOfCores = NumberOfCores
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
firstPhase <- function(data, datatype, stoxBioticObject) {
    
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
    		tablesToTranslate <- setdiff(names(data), "vocabulary")
    		tablesToTranslate <- tablesToTranslate[sapply(data[tablesToTranslate], nrow) > 0]
    		
    		vocabulary <- findVariablesMathcinigVocabulary(
    			vocabulary = data$vocabulary, 
    			data = data[tablesToTranslate]
    		)
    		# Uniqueify since some columns (keys) are present in several tables:
    		vocabulary <- unique(vocabulary)
    		
    		data[tablesToTranslate] <- translateVariables(
    			data = data[tablesToTranslate], 
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

	    # Function for merging the appropriate Catch row with the corresponding Biology record
	    # Also check for missing biology
	    # This function generates individuals from the "NumberAtLength", "WeightAtLength", "LengthCode", "LengthClass", "LengthType":
	    specialMerge <- function(x) {
	    	xLenC <- unlist(x[1, "LengthCode"])

			# Two scenarios, catch samples are by length or not
			if(!is.na(xLenC)) {
				byVarX <- c(byVars, "LengthCode", "LengthClass")
				byVarY <- c(byVars, "LengthCode.Biology", "LengthClass.Biology")
			} else {
				byVarX <- byVars
				byVarY <- byVars
			}
	    	
	    	
			xtmp <- merge(x, data$Biology, by.x = byVarX, by.y = byVarY)
			xtmp[, WeightMeasurement:= TRUE]

			if(!is.na(xLenC)) {
				xtmp[, `:=`(LengthCode.Biology=LengthCode, LengthClass.Biology=LengthClass)]

				# Check NumberAtLength == Number of individual
				countNL <- unlist(x[1, "NumberAtLength"])
				lenDiff <- countNL - nrow(xtmp)

				if(!is.na(lenDiff) && lenDiff > 0) {
					# Calculate weight
					avWt <- unlist(x[1, "WeightAtLength"])/countNL
					WtUnit <- unlist(x[1, "WeightUnit"])
					# Generate a sample individual
					xadd <- merge(x, data$Biology[0, ], by.x = byVarX, by.y = byVarY, all.x = TRUE)
					xadd[, `:=`(LengthCode.Biology=LengthCode, LengthClass.Biology=LengthClass)]
					xadd[, `:=`(WeightMeasurement = FALSE, IndividualWeight = avWt, WeightUnit.Biology = WtUnit)]
					# Duplicate as required
					if( lenDiff > 1 ){
						xaddList <- rep(list(xadd), lenDiff)
						xadd <- rbindlist(xaddList)
					}
					# Combine
					xtmp <- rbind(xtmp, xadd)
				}
			}

			return(xtmp)
		}

	    # Get row count
	    nrowC <- nrow(data$Biology)
	    
	    # Loop merge
	    tmpBiology <- list()
	    for(y in seq_len(nrow(data$Catch))) {
			tmpBiology[[y]] <- specialMerge(data$Catch[y,])
	    }
	    
	    # Combine results
	    data$Biology <- rbindlist(tmpBiology, use.names=TRUE)
	    
	    # Sanity check (old Biology row number must be the same with the merged product, _if there is no WeightMeasurement == FALSE_)
	    if(nrowC != nrow(data$Biology[WeightMeasurement == TRUE,])) {
	    	stop("Error in merging.")
	    }
	    
	    # Keep only the original columns and the "StationName", which is not a key in the ICESBiotic:
	    columnsToKeep <- c(
	    	names(data$Biology), 
	    	"StationName"
	    )
	    data$Biology <- data$Biology[, .SD, .SDcols = columnsToKeep]

	    # Fix Individual ID for those coming from Catch:
	    FishIDBy <- head(sapply(tableKey, "[[", 1), -1)
	    
	    # Does not work, as FishID is integer
	    #data$Biology[is.na(FishID), FishID := paste0("FromCatch_", seq_len(.N)), by = FishIDBy]
	    
	    data$Biology[, maxFishID := max(0, FishID, na.rm = TRUE), by = FishIDBy]
	    data$Biology[is.na(FishID), FishID := seq_len(.N) + maxFishID, by = FishIDBy]
	    data$Biology[, maxFishID := NULL]
	    
	    

		# Fix the SampleCount
		colAgg <- setdiff(colnames(data$Catch), c("NumberAtLength", "WeightAtLength", "LengthCode", "LengthClass", "LengthType"))
		data$Catch[, SubsampledNumber:=ifelse(!is.na(NumberAtLength), sum(NumberAtLength), SubsampledNumber), by=colAgg]

		# Purge duplicate samples
		data$Catch <- data$Catch[!duplicated(data$Catch[, ..colAgg])]
	  } 
    else {
	    warning("Invalid data input format ", datatype, ". Only NMD Biotic ver 1.4 / ver 3 and ices Biotic formats that are supported for now.")
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
    
    # 3. One to one mapping and keys
    firstPhaseTables <- list()
    
    # Add the tables which should simply be renamed, and not split into several tables:
    for(map in tableMap) {
        firstPhaseTables[[map[[2]]]] <- data[[map[[1]]]]
    }
    
    # 4. "COMPLEX" mapping
    complexMap <- stoxBioticObject$complexMaps[[datatype]]
    # Get the different origin tables, and loop through these:
    levels <- unlist(unique(complexMap[, "level"]))
    for(origin in levels) {
    	# Get the possible destination tables, and loop through these:
    	destinations <- unlist(unique(complexMap[level == origin, "target"]))
    	for (dest in destinations) {
    		# List the columns to copy to the destination table:
            colList <- unlist(complexMap[target==dest & level==origin, "variable"])
            if(!is.null(firstPhaseTables[[dest]])) stop("Error")
            # Copy the columns:
            firstPhaseTables[[dest]] <- data[[origin]][, ..colList]
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




# Function to get the StoxBiotic on one file:
StoxBiotic_firstPhase <- function(BioticData) {
    # Get data type: 
	datatype <- unlist(BioticData[["metadata"]][1, "useXsd"])
	
	if(!exists("stoxBioticObject")) {
        data(stoxBioticObject, package="RstoxData", envir = environment())
    }
    
    # Do first phase
    first <- firstPhase(BioticData, datatype, stoxBioticObject)
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
    
    # Try to stop data.table warnings (https://github.com/Rdatatable/data.table/issues/2988)
    .. <- function (x, env = parent.frame()) {
        stopifnot(inherits(x, "character"))
        stopifnot(length(x) == 1)
        get(x, envir = parent.env(env))
    }
    
    columns <- c("variable", "level", datatype)
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
    			all.x = TRUE
    		)
    	}
    }
    
    # Apply the conversions, such as pasting keys, converting to POSIX, etc.:
    for (i in unique(convertTable[, level])) {
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
#'
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
#'
#' @export
#' 
AddToStoxBiotic <- function(
	StoxBioticData, 
	BioticData, 
	VariableNames = character()
) {
	AddToStoxData(
		StoxData = StoxBioticData, 
		RawData = BioticData, 
		VariableNames = VariableNames, 
		NumberOfCores = 1L, 
		StoxDataFormat = "Biotic"
	)
}




checkDataSource <- function(BioticData) {
	# Function to match the metadata against data source strings:
	matchSource <- function(x, BioticData) {
		matched <- startsWith(sapply(lapply(BioticData, "[[", "metadata"), "[[", "useXsd"), x)
		output <- rep(NA, length(matched))
		output[matched] <- x
		return(output)
	}
	
	# Detect the data source:
	possibleDataSources <- c("nmd", "ices")
	detectedDataSources <- sapply(possibleDataSources, matchSource, BioticData = BioticData, simplify = FALSE)
	numberOfFormats <- sum(sapply(detectedDataSources, function(x) any(!is.na(x))))
	#detectedDataSources <- apply(detectedDataSources, 1, min, na.rm = TRUE)
	# Accept only BioticData from a single source:
	if(numberOfFormats > 1) {
		stop("The function AddToStoxBiotic can only be applied to BioticData where all files read are of the same data source (NMD or ICES)")
	}
	
	return(detectedDataSources)
}



# This can be used later if TowDuration is needed:
# TowDuration,Haul,"as.numeric(difftime(as.POSIXct(paste0(stationstopdate, stationstoptime), format='%Y-%m-%dZ%H:%M:%OSZ', tz='GMT'), as.POSIXct(paste0(stationstartdate, stationstarttime), format='%Y-%m-%dZ%H:%M:%OSZ', tz='GMT'), units = 'mins'))","as.numeric(difftime(as.POSIXct(paste0(stationstopdate, stationstoptime), format='%Y-%m-%dZ%H:%M:%OSZ', tz='GMT'), as.POSIXct(paste0(stationstartdate, stationstarttime), format='%Y-%m-%dZ%H:%M:%OSZ', tz='GMT'), units = 'mins'))","as.numeric(difftime(as.POSIXct(paste(stopdate.fishstation, stoptime), format='%d/%m/%Y %H:%M:%S', tz='GMT'), as.POSIXct(paste(startdate.fishstation, starttime), format='%d/%m/%Y %H:%M:%S', tz='GMT'), units = 'mins'))","as.numeric(difftime(as.POSIXct(paste(stopdate.fishstation, stoptime), format='%d/%m/%Y %H:%M:%S', tz='GMT'), as.POSIXct(paste(startdate.fishstation, starttime), format='%d/%m/%Y %H:%M:%S', tz='GMT'), units = 'mins'))","Duration",
# EffectiveTowDuration,Haul,TowDuration,TowDuration,TowDuration,TowDuration,TowDuration,

