#' Convert BioticData to StoxBioticData
#'
#' @param BioticData A list of biotic data (StoX data type \code{\link{BioticData}}), one element for each input biotic file.
#'
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
#'
#' @import data.table
#' @importFrom parallel makeCluster parLapply stopCluster mclapply
#' @export
#' 
StoxBiotic <- function(BioticData) {

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

	    # Set counter
	    bioCtr <- 1

	    # Function for merging the appropriate Catch row with the corresponding Biology record
	    specialMerge <- function(x) {
	    increment <- NA

			# Two scenarios, catch samples are by length or not
			if(!is.na(unlist(x[1, "LengthCode"]))) {
				increment <- unlist(x[1, "NumberAtLength"])
			} else {
				increment <- unlist(x[1, "SubsampledNumber"])
			}

			if(is.na(increment) || increment == 0)
				return(NULL)

			bioRange <- c(bioCtr:(bioCtr + increment - 1))
			bioCtr <<- bioCtr + increment
			return(merge(x, data$Biology[bioRange, ], by = byVars))
		}

	    # Get row count
	    nrowC <- nrow(data$Biology)

	    # Loop merge
	    tmpBiology <- list()
	    for(y in seq_len(nrow(data$Catch))) {
			tmpBiology[[y]] <- specialMerge(data$Catch[y,])
	    }

	    # Combine results
	    data$Biology <- rbindlist(tmpBiology)

	    # Sanity check (old Biology row number must be the same with the merged product)
	    if(nrowC != nrow(data$Biology)) {
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

	# 2nd phase 
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
	StoxBioticOne <- function(BioticData) {
		# Get data type: 
		datatype <- unlist(BioticData[["metadata"]][1, "useXsd"])
		
		if(!exists("stoxBioticObject"))
			data(stoxBioticObject, package="RstoxData", envir = environment())
		
		# Do first phase
		first <- firstPhase(BioticData, datatype, stoxBioticObject)
		# Do second phase	
		second <- secondPhase(first, datatype, stoxBioticObject)
		
		return(second)
	}

	# Process Biotic data in parallel
	cores <- getCores()
	if(get_os() == "win") {
		cl <- makeCluster(cores)
		StoxBioticData <- parLapply(cl, BioticData, StoxBioticOne)
		stopCluster(cl)
	} else {
		StoxBioticData <- mclapply(BioticData, StoxBioticOne, mc.cores = cores)
	}

	tableNames <- names(StoxBioticData[[1]])
	StoxBioticData <- lapply(
		tableNames, 
		function(name) data.table::rbindlist(lapply(StoxBioticData, "[[", name))
	)
	names(StoxBioticData) <- tableNames
	
	StoxBioticData
}



#' Merge StoxBioticData
#'
#' @param StoxBioticData A list of StoX biotic data (StoX data type \code{\link{StoxBioticData}}).
#'
#' @return An object of StoX data type \code{\link{MergedStoxBioticData}}.
#'
#' @export
#' 
MergeStoxBiotic <- function(StoxBioticData) {
    mergeDataTables(StoxBioticData, tableNames = NULL, output.only.last = TRUE)
}








