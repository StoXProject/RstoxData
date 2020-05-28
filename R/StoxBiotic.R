#' Convert BioticData to StoxBioticData
#'
#' @param BioticData A list of biotic data (StoX data type \code{\link{BioticData}}), one element for each input biotic file.
#' @param Cores Overrides multi-core auto detection. Default to NULL.
#'
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
#'
#' @export
#' 
StoxBiotic <- function(BioticData, Cores = integer()) {
    
    # Convert from BioticData to the general sampling hierarchy:
    GeneralSamplingHierarchy <- BioticData2GeneralSamplingHierarchy(BioticData, Cores = Cores)
    
    # Extract the StoxBiotic data and rbind across files:
    StoxBioticData <- GeneralSamplingHierarchy2StoxBiotic(GeneralSamplingHierarchy, Cores = Cores)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(StoxBioticData)
    
    return(StoxBioticData)
}

# Function to convert each element (representing input files) a BioticData object to the general sampling hierarchy:
BioticData2GeneralSamplingHierarchy <- function(BioticData, Cores = integer()) {
    
    # Process Biotic data in parallel
    if(length(Cores) == 0) {
        Cores <- getCores()
    }
    if(Cores == 1) {
        GeneralSamplingHierarchy <- lapply(BioticData, StoxBiotic_firstPhase)
    }
    else {
    	# Do not use more cores than the number of files:
    	Cores <- min(length(BioticData), Cores)
    	
        if(get_os() == "win") {
            cl <- parallel::makeCluster(Cores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
            GeneralSamplingHierarchy <- parallel::parLapply(cl, BioticData, StoxBiotic_firstPhase)
            parallel::stopCluster(cl)
        } 
        else {
            GeneralSamplingHierarchy <- parallel::mclapply(BioticData, StoxBiotic_firstPhase, mc.cores = Cores)
        }
    }
    
    GeneralSamplingHierarchy
}

# Function to convert rbind :
GeneralSamplingHierarchy2StoxBiotic <- function(GeneralSamplingHierarchy, Cores = integer()) {
    
    # Process Biotic data in parallel
    if(length(Cores) == 0) {
        Cores <- getCores()
    }
    if(Cores == 1) {
        StoxBioticData <- lapply(GeneralSamplingHierarchy, StoxBiotic_secondPhase)
    }
    else {
    	# Do not use more cores than the number of files:
    	Cores <- min(length(GeneralSamplingHierarchy), Cores)
    	
    	if(get_os() == "win") {
        	cl <- parallel::makeCluster(Cores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
            StoxBioticData <- parallel::parLapply(cl, GeneralSamplingHierarchy, StoxBiotic_secondPhase)
            parallel::stopCluster(cl)
        } 
        else {
            StoxBioticData <- parallel::mclapply(GeneralSamplingHierarchy, StoxBiotic_secondPhase, mc.cores = Cores)
        }
    }
    
    tableNames <- names(StoxBioticData[[1]])
    StoxBioticData <- lapply(
        tableNames, 
        function(name) data.table::rbindlist(lapply(StoxBioticData, "[[", name))
    )
    names(StoxBioticData) <- tableNames
    
    StoxBioticData
}


# Function to convert the data read from one type of biotic data into the general sampling hierarchy for biotic data defined by StoX:
firstPhase <- function(data, datatype, stoxBioticObject) {
    
    # Getting data for the datatype
    tableKey <- stoxBioticObject$tableKeyList[[datatype]]
    tableMap <- stoxBioticObject$tableMapList[[datatype]]
    indageHeaders <- stoxBioticObject$indageHeadersList[[datatype]]
    
    # 1. Merge: a) Cruise number with everything b) age reading and individual (specific to data source type)
    
    if( datatype == "nmdbioticv3") {
        
        ## If preferredagereading in indivdiual is NA, use 1 as the preferred reading
        data$individual[,preferredagereading:= ifelse(is.na(preferredagereading), 1, preferredagereading)]
        
        ## Merge individual and age
        data$individual <- merge(data$individual, data$agedetermination, by.x=c(indageHeaders, "preferredagereading"), by.y=c(indageHeaders, "agedeterminationid"), all.x=TRUE)
        
        ## Cascading merge tables
        toMerge <- c("mission", "fishstation", "catchsample", "individual")
        data <- mergeDataTables(data, toMerge)
    } else if(datatype == "nmdbioticv1.4") {

	    ## Merge individual and age
	    indageHeaders <- intersect(names(data$agedetermination), names(data$individual))
	    data$individual[,preferredagereading:= 1]
	    data$individual <- merge(data$individual, data$agedetermination, by.x=c(indageHeaders, "preferredagereading"), by.y=c(indageHeaders, "no"), all.x=TRUE)

	    ## Cascading merge tables
	    toMerge <- c("mission", "fishstation", "catchsample", "individual")

	    # Use custom suffixes as it contains duplicate header name between tables
	    data <- mergeDataTables(data, toMerge)
	  } else if(datatype == "icesBiotic") {

	    # Merge Cruise and Survey name
	    data[["Cruise"]][, Survey:=tail(data[["Survey"]],1)]

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

		# Fix Individual ID
		data$Biology[, FishID:=seq_len(.N)]

		# Fix the SampleCount
		colAgg <- setdiff(colnames(data$Catch), c("NumberAtLength", "WeightAtLength", "LengthCode", "LengthClass", "LengthType"))
		data$Catch[, SubsampledNumber:=ifelse(!is.na(NumberAtLength), sum(NumberAtLength), SubsampledNumber), by=colAgg]

		# Purge duplicate samples
		data$Catch <- data$Catch[!duplicated(data$Catch[, ..colAgg])]

	    # Sanity check (old Biology row number must be the same with the merged product, _if there is no WeightMeasurement == FALSE_)
	    if(nrowC != nrow(data$Biology[WeightMeasurement == TRUE,])) {
			stop("Error in merging.")
	    }
	  } else {
	    print("Error: Invalid data input format. Only NMD Biotic ver 1.4 / ver 3 and ices Biotic formats that are supported for now.")
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
    
    for(map in tableMap) {
        firstPhaseTables[[map[[2]]]] <- data[[map[[1]]]]
    }
    
    
    # 4. "COMPLEX" mapping
    complexMap <- stoxBioticObject$complexMaps[[datatype]]
    for(origin in unlist(unique(complexMap[, "level"]))) {
        for (dest in unlist(unique(complexMap[level == origin, "target"]))) {
            colList <- unlist(complexMap[target==dest & level==origin, "variable"])
            if(!is.null(firstPhaseTables[[dest]])) stop("Error")
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
    	stop("The input format ", datatype, " is not yet supprted in RstoxData")
    }
    convertTable <- stoxBioticObject$convertTable[, ..columns]
    
    # Data placeholder
    secondPhaseTables <- list()
    
    for (i in unique(convertTable[, level])) {
        # Process conversion table
        for(j in 1:nrow(convertTable[level==i,])) {
            k <- convertTable[level==i,][j,]
            data[[i]][, (unlist(k[,"variable"])):=eval(parse(text=k[, get(..("datatype"))]))]
        }
        # Get key for transfer
        sourceColumns <- unlist(indices(data[[i]], vectors = TRUE))
        sourceColumns <- c(sourceColumns, unlist(convertTable[level==i, "variable"]))
        secondPhaseTables[[i]] <- data[[i]][, ..sourceColumns]
    }
    
    # Remove duplicated rows from SpeciesCategory
    secondPhaseTables[["SpeciesCategory"]] <- unique(secondPhaseTables[["SpeciesCategory"]])
    
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
#'
#' @return An object of StoX data type \code{\link{MergedStoxBioticData}}.
#'
#' @export
#' 
MergeStoxBiotic <- function(StoxBioticData, TargetTable = "Individual") {
	# Get the tables to merge:
	StoxBioticDataTableNames <- names(StoxBioticData)
	if(! TargetTable %in% StoxBioticDataTableNames) {
		stop("TargetTable must be one of ", paste(StoxBioticDataTableNames, collapse = ", "))
	}
	tableNames <- StoxBioticDataTableNames[seq_len(which(StoxBioticDataTableNames == TargetTable))]
	# Merge:
    mergeDataTables(StoxBioticData, tableNames = tableNames, output.only.last = TRUE, all = TRUE)
}


#' Merge StoxBioticData
#'
#' @param StoxBioticData A list of StoX biotic data (StoX data type \code{\link{StoxBioticData}}).
#'
#' @return An object of StoX data type \code{\link{MergedStoxBioticData}}.
#'
#' @export
#' 
AddStoxBioticVariables <- function(StoxBioticData, BioticData, TableName, VariableName, Cores = integer()) {
	
	# Check the the BioticData are all from the same source (ICES/NMD):
	checkDataSource(BioticData)
	
	# Convert from BioticData to the general sampling hierarchy:
	GeneralSamplingHierarchy <- BioticData2GeneralSamplingHierarchy(BioticData, Cores = Cores)
	
	# Get the variables:
	if(length(TableName) != length(VariableName)) {
		stop("TableName and VariableName must have the same length")
	}
	
	# Split the TableName and VariableName into lists by the TableName:
	VariableName <- split(VariableName, TableName)
	TableName <- split(TableName, TableName)
	
	# Get the variables:
	extractVariablesFromTable <- function(tab, var, data) {
		if(length(tableName)) {
			data[[tab]]
		}
		
	}
	
	
	
	
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
	detectedDataSources <- sapply(possibleDataSources, matchSource, BioticData = BioticData)
	detectedDataSources <- apply(detectedDataSources, 1, min, na.rm = TRUE)
	# Accept only BioticData from a single source:
	if(any(detectedDataSources[1] != detectedDataSources)) {
		stop("The function AddStoxBioticVariables can only be applied to BioticData where all files read are of the same data source (NMD or ICES)")
	}
	
	return(detectedDataSources)
}





