#' Run filter on any StoX related data source
#'
#' @param inputData An input data. Can be a list of biotic data (StoX data type \code{\link{BioticData}}), list of acoustic data, StoxBiotic data, or StoxAcoustic data.
#' @param filterExpression Filter expression in list of strings. The name of the list and structures should be identical to the names of the input data list.
#' @param propagateDownwards Whether the filter action will propagate in the downwards direction. Default to TRUE.
#' @param propagateUpwards Whether the filter action will propagate in the upwards direction. Default to FALSE.
#'
#' @return An object of filtered data in the same format as the input data.
#'
#' @import data.table
#' @export
#' 
filterData <- function(inputData, filterExpression, propagateDownwards = TRUE, propagateUpwards = FALSE) {
	
	`%notin%` <- Negate(`%in%`)

	processFilter <- function(filters) {
		# Assume each individual filters relation are the AND (&) operator 
		parsedFilterTxt <- paste(filters, collapse=" & ")
		parsedFilter <- parse(text = parsedFilterTxt)
		return(parsedFilter)
	}

	processFilterWrap <- function(x) {
		out <- lapply(x, processFilter)
		names(out) <- names(x)
		return(out)
	}

	applyFilter <- function(tableName, x, y, propDown, propUp) {
		ret <- list()

		filts <- x[[tableName]]

		if(!nrow( y[[tableName]])) {
			warning(paste("Empty table", tableName))
			return(ret)
		}

		test <- try(ret[[tableName]] <- y[[tableName]][eval(filts),], silent = TRUE)
		if(class(test)[1] == "try-error") {
			warning(paste0("Apply filter error:\n", test[1]))
		} else {
			# 3. propagate down
			if(propDown) {
				start <- which(names(y) == tableName)
				range <- c(start:length(names(y)))
				# Don't propage if it's the only table
				if(length(range) > 1) {
					for(parent in head(range, -1)) {
						# Find similar columns (assume those are keys)
						key <- intersect(names(y[[parent + 1]]), names(y[[parent]]))
						if(length(key) > 0) {
							# Find the not deleted keys after filtering
							deleted <- fsetdiff(y[[parent]][, ..key], ret[[names(y)[parent]]][, ..key])
							# Propagate to child (using Map)
							ret[[names(y)[parent+1]]] <- y[[parent+1]][do.call(pmin, Map(`%in%`, y[[parent+1]][, names(deleted), with=FALSE], deleted)) != 1L]
						}
					}
				}
			}
		}
		return(ret)
	}

	applyFilterWrap <- function(fileName, x, y, propDown, propUp) {
		# Do per file filtering
		merged <- y[[fileName]]
		for (tName in intersect(names(merged), names(x[[fileName]]))) {
			out <- applyFilter(tName, x[[fileName]], merged, propDown, propUp)
			merged <- replace(merged, intersect(names(out), names(merged)), out[intersect(names(out), names(merged))])
		}

		return(merged)
	}

	level <- 0
	# 1. Check Validity/Level of data
	if(!is.list(inputData) || !is.list(filterExpression) || !length(inputData) || !length(filterExpression)) {
		warning("Data / Filter parameters is empty!")
		return(NULL)
	} else if(is.data.table(inputData[[1]])) {
		level <- 1
		if(!is.character(filterExpression[[1]])) {
			warning("Data/Filter level mismatch!")
			return(NULL)
		}
	} else if(is.data.table(inputData[[1]][[1]])) {
		level <- 2
		if(!is.character(filterExpression[[1]][[1]])) {
			warning("Data/Filter level mismatch!")
			return(NULL)
		}
	} else {
		warning("Something wrong with the input!")
		return(NULL)
	}

	# 2. Check and parse filters
	if(level == 1) {
		pFilters <- lapply(filterExpression, processFilter)
	} else {
		pFilters <- lapply(filterExpression, processFilterWrap)
	}
	names(pFilters) <- names(filterExpression)
	
	# 3. Apply filters
	if(level == 1) {
		merged <- inputData
		for (tName in intersect(names(merged), names(pFilters))) {
			out <- applyFilter(tName, pFilters, merged, propagateDownwards, propagateUpwards)
			merged <- replace(merged, intersect(names(out), names(merged)), out[intersect(names(out), names(merged))])
		}
	} else {
		ret <- lapply(names(pFilters), applyFilterWrap, pFilters, inputData, propagateDownwards, propagateUpwards)
		names(ret) <- names(pFilters)
		merged <- replace(inputData, intersect(names(ret), names(inputData)), ret[intersect(names(ret), names(inputData))])
	}

	# 4. (TODO) Propagate up

	return(merged)
}
