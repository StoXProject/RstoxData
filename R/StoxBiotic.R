StoxBiotic <- function(data) {

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
	    sequence <- c("mission", "fishstation", "catchsample", "individual")
	    for(ii in 2:length(sequence)) {
		curr <- sequence[ii]
		prev <- sequence[(ii-1)]
		vars <- names(data[[curr]])[names(data[[curr]]) %in% names(data[[prev]])]
		data[[curr]] <- merge(data[[prev]], data[[curr]], by=vars)
	    }
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

	  columns <- c("variable", "level", datatype)
	  convertTable <- stoxBioticObject$convertTable[, ..columns]

	  # Data placeholder
	  secondPhaseTables <- list()

	  for (i in unique(convertTable[, level])) {
		  # Process conversion table
		  for(j in 1:nrow(convertTable[level==i,])) {
		    k <- convertTable[level==i,][j,]
		    data[[i]][, (unlist(k[,"variable"])):=eval(parse(text=k[, ..datatype]))]
		  }
		  # Get key for transfer
		  sourceColumns <- unlist(indices(data[[i]], vectors = TRUE))
		  sourceColumns <- c(sourceColumns, unlist(convertTable[level==i, "variable"]))
		  secondPhaseTables[[i]] <- data[[i]][, ..sourceColumns]
	  }
	  
	  return(secondPhaseTables)

	}

	# Get data type	
	datatype <- data[["metadata"]][["useXsd"]]

	if(!exists("stoxBioticObject"))
		data(stoxBioticObject)

	# Do first phase
	first <- firstPhase(data, datatype, stoxBioticObject)
	# Do second phase	
	second <- secondPhase(first, datatype, stoxBioticObject)

	return (second)
}

