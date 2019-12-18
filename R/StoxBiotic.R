#' Convert BioticData to StoxBioticData
#'
#' @param BioticData A list of biotic data (StoX data type \code{\link{BioticData}}), one element for each input biotic file.
#'
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
#'
#' @import data.table
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

	    ## Convert the agedeterminationid to integer
	    data$agedetermination[,agedeterminationid:= as.integer(agedeterminationid)]

	    ## Merge individual and age
	    data$individual <- merge(data$individual, data$agedetermination, by.x=c(indageHeaders, "preferredagereading"), by.y=c(indageHeaders, "agedeterminationid"), all.x=TRUE)
	    
	    ## Cascading merge tables
	    toMerge <- c("mission", "fishstation", "catchsample", "individual")
	    data <- mergeDataTables(data, toMerge)
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
	
	StoxBioticData <- lapply(BioticData, StoxBioticOne)

	tableNames <- names(StoxBioticData[[1]])
	StoxBioticData <- lapply(
		tableNames, 
		function(name) data.table::rbindlist(lapply(StoxBioticData, "[[", name))
	)
	names(StoxBioticData) <- tableNames
	
	StoxBioticData
}






