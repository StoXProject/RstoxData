#' Propagate filter downwards
#' @param retained data retained by filter
#' @param deleted data explicitly deleted from 'table'
#' @param table name of table to propagate deletions to
#' @param treeStruct the hierarchical structure of the data
#' @noRd
propogateFilterDown <- function(retained, deleted, table, treeStruct){
  
  if (is.null(treeStruct[[table]])){
    return(retained)
  }
  
  for (child in treeStruct[[table]]){
    keys <- names(deleted)
    deleteKeys <- unique(deleted[,.SD, .SDcol=keys])
    retainKeys <- data.table::fsetdiff(retained[[child]][,.SD, .SDcol=keys], deleteKeys)
    retained[[child]] <- retained[[child]][retainKeys, on=keys]
    retained <- propogateFilterDown(retained, deleted, child, treeStruct)
  }
  return(retained)
  
}

#' Propagate filter upwards
#' Remove any ancestor that have all data removed from all decendents by application of the filter
#' @param retained data retained by filter
#' @param table name of table to propagate deletions from
#' @param treeStruct the hierarchical structure of the data
#' @param propDown whether to propagate changes down after deletions
#' @noRd
propogateFilterUp <- function(retained, table, treeStruct, propDown){
  
  parent <- NULL
  for (p in names(treeStruct)){
    if (table %in% treeStruct[[p]]){
      parent <- p
    }
  }
  
  #terminate at root node
  if (is.null(parent)){
    return(retained)
  }
  
  #skip empty levels
  if (nrow(retained[[parent]]) == 0){
    return(propogateFilterUp(retained, parent, treeStruct, propDown))
  }
  
  keys <- names(retained[[parent]])[names(retained[[parent]]) %in% names(retained[[table]])]
  
  retainKeys <- unique(retained[[table]][,.SD, .SDcol=keys])
  retainKeys <- data.table::fintersect(retainKeys, unique(retained[[parent]][, .SD, .SDcol=keys]))
  deleted <- data.table::fsetdiff(retained[[parent]][, .SD, .SDcol=keys], retainKeys)
  retained[[parent]] <- retained[[parent]][retainKeys, on=keys]
  
  retained <- propogateFilterUp(retained, parent, treeStruct, propDown)
  
  if (propDown){
    
    retained <- propogateFilterDown(retained, deleted, parent, treeStruct)
  }
  
  return(retained)
}

#' Extract keys from Stox convention
#' by comparing names between parent and decendant
#' Return empty set of keys if no descendant has data
#' @noRd
getKeysDown <- function(data, parent, treeStruct){

  #leaf node has empty set of keys
  if (is.null(treeStruct[[parent]])){
    return(c())
  }
  
  #return once a set of keys are found
  for (child in treeStruct[[parent]]){
    if (data.table::is.data.table(data[[child]])){
      return(names(data[[child]])[names(data[[child]]) %in% names(data[[parent]])])
    }
    
    #only return if some decentent have a non-empty set of keys
    decKeys <- getKeysDown(data, child, treeStruct)
    if (length(decKeys)>0){
      return(decKeys)
    }
  }
  
  #return empty set of keys if keys are not otherwise found
  return(c())
}

#' Extract keys from Stox convention
#' by comparing names between child and parent
#' halt with error if no parent can be found
#' @noRd
getKeysUp <- function(data, child, treeStruct){
  for (parent in names(treeStruct)){
    if (child %in% treeStruct[[parent]]){
      return(names(data[[child]]) %in% names(data[[parent]]))
    }
  }
  warning(paste("The table ", child, " has no parent table. FilterUpwards has no effect."))
  return(NULL)
}


#' Filter a set of tables with hierarchical relations
#' 
#' @description
#'  Applies filters to hierarchical data formats
#'
#' @details
#'  Applies filters to a set of tables that are hierarchically related, and provides options for
#'  propagating filters up or down in the data hierarchy.
#'  
#'  Filters are specified as strings encoding logical expressions in standard R syntax, each associated to one of
#'  the tables in the input. 
#'  
#'  After a filter is applied to a table, data that corresponds to the removed in other tables will be removed
#'  as specified by the options:
#'  
#'  * propagateDownwards removes all data lower in the hierarchy that was linked to data explicitly removed by filter.
#'  * propagateUpwards removes all data higher in the hierarchy that is not linked with anything at the levels where explicitly filtered data has been removed.
#'  
#'  The argument treeStruct specify the hierarcical relations between the tables in 'inputTables' ```treeStruct[[A]] == B``` implies that B is a child of A.
#'  ```treeStruct[[A]] == NULL``` (which is equivalent to A not defined in treeStruct) implies that A is a leaf node.
#'  treeStruct is formatted as in the RstoxData format descriptions \code{\link[RstoxData]{xsdObjects}}.
#'  
#'  Column names are assumed to be unique across all levels of the data hierarchy, except that each table is expected to
#'  repeated the key-columns of all its ancestors.
#' 
#' @param inputTables A list of \code{\link[data.table]{data.table}}s with the data to be filtered
#' @param filterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#' @param treeStruct A list specifying the hierarchical relations in the data. Maps tables in 'inputTables' to their direct children. See details.
#' @param propagateDownwards Whether the filter action will propagate in the downwards direction. Default to TRUE. See details.
#' @param propagateUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE. See details.
#' 
#' @return An object of filtered data in the same format as the input data.
#'
#' @examples 
#'  filenames <- system.file("testresources",
#'        "biotic3.1_w_ageandprey.xml", package="RstoxData")
#'  inputData <- RstoxData:::ReadBiotic(filenames)
#'  
#'  #filters are nested list matching the structure of inputData
#'  filterExpression <- list()
#'  filterExpression$agedetermination <- c(
#'      'age < 10'
#'  )
#'  
#'  filteredData <- RstoxData::filterTables(inputData$biotic3.1_w_ageandprey.xml, 
#'              filterExpression, 
#'              treeStruct=RstoxData::xsdObjects$nmdbioticv3.1.xsd$treeStruct)
#'  nrow(filteredData$fishstation)
#'  filteredPropUp <- RstoxData::filterTables(inputData$biotic3.1_w_ageandprey.xml, 
#'              filterExpression, 
#'              propagateUpwards = TRUE, 
#'              treeStruct=RstoxData::xsdObjects$nmdbioticv3.1.xsd$treeStruct)
#'  nrow(filteredPropUp$fishstation)
#'
#' @importFrom utils head
#' @importFrom data.table fsetdiff fintersect is.data.table %like% %flike% %ilike% %inrange% %chin% %between%
#' @export
#' @md
filterTables <- function(inputTables, filterExpression, treeStruct, propagateDownwards = TRUE, propagateUpwards = FALSE){
  
  if (!is.list(inputTables)){
    stop("Argument 'inputTables' must be a list of data.tables.")
  }
  if (!all(sapply(inputTables, data.table::is.data.table))){
    stop("Argument 'inputTables' must be a list of data.tables.")
  }
  
  if (!length(treeStruct) & length(inputTables)>1){
    stop("Argument 'treeStruct' must be provided when 'inputTables' contains more than one table.")
  }
  if (!any(names(inputTables) %in% names(treeStruct)) & length(inputTables)>1){
    stop("Argument 'treeStruct' does not specify a hierarchy for 'inputTables'.")
  }
  
	
  # Special operators defined for filter operations. These are also supported in RstoxFramework:
  `%notin%` <- Negate(`%in%`)
  `%notequal%` <- function(x, table) is.na(x) | x %notin% table
  
  
  processFilter <- function(filters) {
    # Do not accept system calls in filters:
    sanitizeExpression(filters)
    
    # Assume each individual filters relation are the AND (&) operator 
    parsedFilterTxt <- paste(filters, collapse=" & ")
    parsedFilter <- parse(text = parsedFilterTxt)
    return(parsedFilter)
  }
  
  applyFilter <- function(tableName, filter, data) {
    ret <- data
    
    filts <- filter[[tableName]]
    
    if(!nrow( data[[tableName]])) {
      warning("StoX: Empty table ", tableName)
      return(ret)
    }
    
    # Run the filter:
    test <- try(ret[[tableName]] <- data[[tableName]][eval(filts),], silent = TRUE)
    if(class(test)[1] == "try-error") {
      warning("StoX: Apply filter error:\n", test[1])
    } else {
      # 3. propagate up
      if(propagateUpwards) {
        
        key <- getKeysUp(data, tableName, treeStruct)
        # if key has zero length no descendants have data, so we do not propagate
        if (length(key)!=0){
          ret <- propogateFilterUp(ret, tableName, treeStruct, propagateDownwards)
        }
      }
      
      # 4. propagate down
      if(propagateDownwards) {
        
        key <- getKeysDown(data, tableName, treeStruct)
        # if key has zero length no descendants have data, so we do not propagate
        if (length(key)!=0){
          deleted <- data.table::fsetdiff(data[[tableName]][,.SD,.SDcol=key], ret[[tableName]][,.SD,.SDcol=key])
          ret <- propogateFilterDown(ret, deleted, tableName, treeStruct)
        }
        
      }
      
    }
    
    return(ret)
  }
  
  processedFilters <- lapply(filterExpression, processFilter)
  names(processedFilters) <- names(filterExpression)
  
  for (tableName in names(processedFilters)){
    if (!(tableName) %in% names(inputTables)){
      warning(paste("StoX: Filter specified for tables", tableName, "which are not found in data. This filter is not applied."))
    }
    else{
      inputTables <- applyFilter(tableName, processedFilters, inputTables)    
    }
  }

  return(inputTables)  
}

#' Run filter on any StoX related data source
#'
#' @name filterData
#'
#' @description
#'  Applies filters to hierarchical data formats.
#'
#' @details
#'  This is a convenient wrapper around \code{\link[RstoxData]{filterTables}}, which determines hierarchy based on available metadata.
#'  For more explicit control of the hierarchical relations used, consider \code{\link[RstoxData]{filterTables}}.
#'  
#'  Applies filters to a set of table that are hierarchically related, and provides options for
#'  propagating filters up or down in the data hierarchy. Accepts either a list of data.tables or a
#'  list of such lists.
#'  
#'  Filters are specified as strings encoding logical expressions in standard R syntax, each associated to one of
#'  the tables in the input. 
#'  
#'  After a filter is applied to a table, data that corresponds to the removed in other tables will be removed
#'  as specified by the options propagateDownwards and propagateUpwards. These are defined in \code{\link[RstoxData]{filterTables}}:
#'  
#'  The argument 'useXsd' specifies if the hierarchical relation between tables in 'inputData', should be inferred
#'  from \code{\link[RstoxData]{xsdObjects}} via the conventional use of a metadata table on the 'inputData'
#'  
#'  If useXsd is FALSE, each table in the hierarchy is assumed to be strictly lower in the hierarchy than any table preceding it,
#'  and any table called 'metadata' is ignored.
#'  
#'  Column names are assumed to be unique across all levels of the data hierarchy, except that each table is expected to
#'  repeated the key-columns of all its ancestors.
#'
#' @param inputData An input data. Can be a list of biotic data (StoX data type \code{\link{BioticData}}), list of acoustic data, StoxBiotic data, or StoxAcoustic data.
#' @param filterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#' @param propagateDownwards Whether the filter action will propagate in the downwards direction. Default to TRUE.
#' @param propagateUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE.
#' @param useXsd logical determining if treeStruct should be read from xsd provided in metadata table. See details.
#' 
#' @return An object of filtered data in the same format as the input data.
#'
#' @examples 
#'  filenames <- system.file("testresources",
#'        "biotic3.1_w_ageandprey.xml", package="RstoxData")
#'  inputData <- RstoxData:::ReadBiotic(filenames)
#'  
#'  #filters are nested list matching the structure of inputData
#'  filterExpression <- list()
#'  filterExpression$`biotic3.1_w_ageandprey.xml`$agedetermination <- c(
#'      'age < 10'
#'  )
#'  
#'  filteredData <- RstoxData::filterData(inputData, 
#'              filterExpression, 
#'              useXsd=TRUE)
#'  nrow(filteredData$biotic3.1_w_ageandprey.xml$fishstation)
#'  filteredPropUp <- RstoxData::filterData(inputData, 
#'              filterExpression, 
#'              propagateUpwards = TRUE, 
#'              useXsd=TRUE)
#'  nrow(filteredPropUp$biotic3.1_w_ageandprey.xml$fishstation)
#'
#' @importFrom utils head
#' @importFrom data.table fsetdiff fintersect is.data.table %like% %flike% %ilike% %inrange% %chin% %between%
#' @export
#' @md
filterData <- function(inputData, filterExpression, propagateDownwards = TRUE, propagateUpwards = FALSE, useXsd = FALSE) {

  getTreestruct <- function(tables){
    #extract treeStruct for xsd
    if (!useXsd){
      levels <- setdiff(names(tables), "metadata")
      treeStruct <- as.list(tail(levels,-1))
      names(treeStruct) <- head(levels,-1)
      
      if ("metadata" %in% names(tables)){
        warning("member 'metadata' of argument 'inputData' is ignored when argument 'useXsd' is FALSE.")
      }
    }
    else if (!("metadata" %in% names(tables))){
      stop("'useXsd' is TRUE, but no table named 'metadata' is found.")
    }
    else{
      xsdname  <- paste(tables$metadata$useXsd, "xsd", sep=".")
      if (!(xsdname %in% names(RstoxData::xsdObjects))){
        stop(paste("No xsd object found for", xsdname))
      }
      treeStruct <- RstoxData::xsdObjects[[xsdname]]$treeStruct
    }
    return(treeStruct)
  }
  
  applyFilter <- function(table, filter, warn=T){
    treeStruct <- getTreestruct(table)
    rowsPre <- lapply(table, nrow)
    table <- filterTables(table, filter, propagateDownwards = propagateDownwards, propagateUpwards = propagateUpwards, treeStruct=treeStruct)
    rowsPost <- lapply(table, nrow)
    zeroed <- intersect(names(rowsPre[rowsPre>0]), names(rowsPost[rowsPost==0]))
    
    if (length(zeroed)>0 & warn){
      warning("StoX: Filter returned empty tables \"", paste(zeroed, collapse=", "), "\"")
    }
    return(table)
  }
  
	applyFilterWrap <- function(fileName, filters, data) {
		
	  if (!(fileName %in% names(data))){
	    warning(paste("StoX: Filter specified for file not found in data: ", fileName, ".", sep=""))
	    return(list())
	  }
	 # Do per file filtering
	 tables <- data[[fileName]]
	 filter <- filters[[fileName]]
	    
	 rowsPre <- lapply(tables, nrow)
	 tables <- applyFilter(tables, filter, warn=F)
	 rowsPost <- lapply(tables, nrow)
	    
	 zeroed <- intersect(names(rowsPre[rowsPre>0]), names(rowsPost[rowsPost==0]))
	    
	 if (length(zeroed)>0){
	      warning("StoX: Filter on data from ", fileName, " returned empty tables \"", paste(zeroed, collapse=", "), "\"")
	 }  
	 return(tables)
	}

	level <- 0
	
	# 1. Check Validity/Level of data
	if(!length(filterExpression)) {
		return(inputData)
	} else if(!is.list(filterExpression)) {
		warning("StoX: Invalid filter parameter (must be a list)!")
		return(NULL)
	} else if(!is.list(inputData) || !length(inputData)) {
		warning("StoX: Invalid or empty input data!")
		return(NULL)
	} else if(is.data.table(inputData[[1]])) {
		level <- 1
		if(!is.character(filterExpression[[1]])) {
			warning("StoX: Data/Filter level mismatch!")
			return(NULL)
		}
	} else if(is.data.table(inputData[[1]][[1]])) {
		level <- 2
		if(!is.character(filterExpression[[1]][[1]])) {
			warning("StoX: Data/Filter level mismatch!")
			return(NULL)
		}
	} else {
		warning("StoX: Something wrong with the input!")
		return(NULL)
	}

	# 3. Apply filters
	if(level == 1) {
	  merged <- applyFilter(inputData, filterExpression)
	} else {
	  ret <- lapply(names(filterExpression), applyFilterWrap, filterExpression, inputData)
		names(ret) <- names(filterExpression)
		merged <- replace(inputData, intersect(names(ret), names(inputData)), ret[intersect(names(ret), names(inputData))])
	}

	return(merged)
}




expandFilterExpressionList <- function(FilterExpressionList, sep = "/") {
	
	# Error if not a list:
    if(!is.list(FilterExpressionList)) {
        #stop("FilterExpressionList must be a list")
        return(FilterExpressionList)
    }
	else if(length(FilterExpressionList) == 0) {
        return(FilterExpressionList)
	}
    # If the input list of expressions has 2 levels, return immediately:
    if(is.list(FilterExpressionList[[1]])) {
        return(FilterExpressionList)
    }
	
    # Get the file names and the table names:
    splited <- strsplit(names(FilterExpressionList), split = sep)
    fileNames <- sapply(splited, function(x) x[seq_len(length(x) - 1)])
    tableNames <- sapply(splited, utils::tail, 1)
    tableNames <- split(tableNames, fileNames)
    
    # Split the FilterExpression by the fileNames:
    FilterExpressionList <- split(FilterExpressionList, fileNames)
    
    names(FilterExpressionList) <- unique(fileNames)
    
    # Change the names of the individual tables:
    for(ind in seq_along(FilterExpressionList)) {
        names(FilterExpressionList[[ind]]) <- tableNames[[ind]]
    }
    
    return(FilterExpressionList)
}


#' Filter (raw) Biotic data
#' 
#' Filters \code{\link{BioticData}}.
#'
#' @param BioticData  Input \code{\link{BioticData}} data.
#' @param FilterExpression Filter expression given as a list of strings. The name of the list and structures should be identical to the names of the input data list. To extract or exclude missing values (NAs) use the operator \code{\%in\%} or the special operator \code{\%notin\%}, which is defined in RstoxData.
#' @param FilterUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE. Use this option with caution, particularly for swept-area survey estimates, where setting \code{FilterUpwards} to TRUE could affect the estimated mean density.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @export
#' 
FilterBiotic <- function(BioticData, FilterExpression = list(), FilterUpwards = FALSE) {
    # For filtering directly on the input data, we need to split the list filter expression to one level for the file and one for the table:
    FilterExpression <- expandFilterExpressionList(FilterExpression)
    
    filterData(
        BioticData, 
        filterExpression = FilterExpression, 
        propagateDownwards = TRUE, 
        propagateUpwards = FilterUpwards,
        useXsd = TRUE
    )
}

#' Filter (raw) Acoustic data
#'
#' Filters \code{\link{AcousticData}}.
#' 
#' @param AcousticData  Input \code{\link{AcousticData}} data.
#' @param FilterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#' @param FilterUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @export
#' 
FilterAcoustic <- function(AcousticData, FilterExpression = list(), FilterUpwards = FALSE) {
    # For filtering directly on the input data, we need to split the list filter expression to one level for the file and one for the table:
    FilterExpression <- expandFilterExpressionList(FilterExpression)
    
    filterData(
        AcousticData, 
        filterExpression = FilterExpression, 
        propagateDownwards = TRUE, 
        propagateUpwards = FilterUpwards,
        useXsd = T
    )
}


#' Filter LandingData
#'
#' Filters \code{\link{LandingData}}.
#' 
#' @param LandingData  Input \code{\link{LandingData}} data.
#' @param FilterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#' @param FilterUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @export
#' 
FilterLanding <- function(LandingData, FilterExpression = list(), FilterUpwards = FALSE) {
	filterData(
		LandingData, 
		filterExpression = FilterExpression, 
		propagateDownwards = TRUE, 
		propagateUpwards = FilterUpwards,
		useXsd = TRUE
	)
}

#' Filter StoxBiotic data
#'
#' Filters \code{\link{StoxBioticData}}.
#' 
#' @param StoxBioticData  Input \code{\link{StoxBioticData}} data.
#' @param FilterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#' @param FilterUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @export
#' 
FilterStoxBiotic <- function(StoxBioticData, FilterExpression = list(), FilterUpwards = FALSE) {
	
	FilterStoxBiotic_FilterExpressionWarning(FilterExpression)
	
	filterData(
        StoxBioticData, 
        filterExpression = FilterExpression, 
        propagateDownwards = TRUE, 
        propagateUpwards = FilterUpwards
    )
}

FilterStoxBiotic_FilterExpressionWarning <- function(FilterExpression) {
	FilterStoxBioticWarningMessges <- getRstoxDataDefinitions("FilterStoxBioticWarningMessges")
	msg <- unlist(
		mapply(
			getFilterStoxBiotic_FilterExpressionWarningMessage, 
			keyWord = names(FilterStoxBioticWarningMessges), 
			message = unlist(FilterStoxBioticWarningMessges), 
			MoreArgs = list(FilterExpression = FilterExpression), 
			SIMPLIFY = FALSE
		)
	)
	if(length(msg))	{
		lapply(msg, warning)
	}
}

getFilterStoxBiotic_FilterExpressionWarningMessage <- function(FilterExpression, keyWord, message) {
	if(length(FilterExpression) && any(mapply(grepl, keyWord, FilterExpression))) {
		return(message)
	}
	else {
		return(NULL)
	}	
}

#' Filter StoxAcoustic data
#'
#' Filters \code{\link{StoxAcousticData}}.
#' 
#' @param StoxAcousticData  Input \code{\link{StoxAcousticData}} data.
#' @param FilterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#' @param FilterUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @export
#' 
FilterStoxAcoustic <- function(StoxAcousticData, FilterExpression = list(), FilterUpwards = FALSE) {
    filterData(
        StoxAcousticData, 
        filterExpression = FilterExpression, 
        propagateDownwards = TRUE, 
        propagateUpwards = FilterUpwards
    )
}

#' Filter StoxLanding data
#'
#' Filters \code{\link{StoxLandingData}}.
#' 
#' @param StoxLandingData  Input \code{\link{StoxLandingData}} data.
#' @param FilterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @export
#' 
FilterStoxLanding <- function(StoxLandingData, FilterExpression = list()) {
  
  StoxLandingData <- filterData(
    StoxLandingData, 
    filterExpression = FilterExpression, 
    propagateDownwards = TRUE, 
    propagateUpwards = FALSE
  )
  
  return(StoxLandingData)
}


#' Filter ICESBiotic data
#'
#' Filters \code{\link{ICESBioticData}}.
#' 
#' @param ICESBioticData  Input \code{\link{ICESBioticData}} data.
#' @param FilterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#' @param FilterUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @export
#' 
FilterICESBiotic <- function(ICESBioticData, FilterExpression = list(), FilterUpwards = FALSE) {
	# For filtering directly on the input data, we need to split the list filter expression to one level for the file and one for the table:
	#FilterExpression <- expandFilterExpressionList(FilterExpression)
	
	filterData(
		ICESBioticData, 
		filterExpression = FilterExpression, 
		propagateDownwards = TRUE, 
		propagateUpwards = FilterUpwards
	)
}

#' Filter ICESAcoustic data
#'
#' Filters \code{\link{ICESAcousticData}}.
#' 
#' @param ICESAcousticData  Input \code{\link{ICESAcousticData}} data.
#' @param FilterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#' @param FilterUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @export
#' 
FilterICESAcoustic <- function(ICESAcousticData, FilterExpression = list(), FilterUpwards = FALSE) {
	# For filtering directly on the input data, we need to split the list filter expression to one level for the file and one for the table:
	#FilterExpression <- expandFilterExpressionList(FilterExpression)
	
	filterData(
		ICESAcousticData, 
		filterExpression = FilterExpression, 
		propagateDownwards = TRUE, 
		propagateUpwards = FilterUpwards
	)
}

#' Filter ICESDatras data
#'
#' Filters \code{\link{ICESDatrasData}}.
#' 
#' @param ICESDatrasData  Input \code{\link{ICESDatrasData}} data.
#' @param FilterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @export
#' 
FilterICESDatras <- function(ICESDatrasData, FilterExpression = list()) {
	# For filtering directly on the input data, we need to split the list filter expression to one level for the file and one for the table:
	#FilterExpression <- expandFilterExpressionList(FilterExpression)
	
	ICESDatrasData <- filterData(
		ICESDatrasData, 
		filterExpression = FilterExpression, 
		propagateDownwards = TRUE, 
		propagateUpwards = FALSE
	)
	
	return(ICESDatrasData)
}

