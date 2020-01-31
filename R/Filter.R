#' Convert BioticData to StoxBioticData
#'
#' @param BioticData A list of biotic data (StoX data type \code{\link{BioticData}}), one element for each input biotic file.
#'
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
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
		for (tName in names(x[[fileName]])) {
			out <- applyFilter(tName, x[[fileName]], y[[fileName]], propDown, propUp)
			merged <- replace(y[[fileName]], intersect(names(out), names(y[[fileName]])), out[intersect(names(out), names(y[[fileName]]))])
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
		for (tName in names(pFilters)) {
			out <- applyFilter(tName, pFilters, inputData, propagateDownwards, propagateUpwards)
			merged <- replace(inputData, intersect(names(out), names(inputData)), out[intersect(names(out), names(inputData))])
		}
	} else {
		ret <- lapply(names(pFilters), applyFilterWrap, pFilters, inputData, propagateDownwards, propagateUpwards)
		names(ret) <- names(pFilters)
		merged <- replace(inputData, intersect(names(ret), names(inputData)), ret[intersect(names(ret), names(inputData))])
	}

	# 4. (TODO) Propagate up

	return(merged)
}
