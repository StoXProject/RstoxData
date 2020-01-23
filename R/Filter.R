#' Convert BioticData to StoxBioticData
#'
#' @param BioticData A list of biotic data (StoX data type \code{\link{BioticData}}), one element for each input biotic file.
#'
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
#'
#' @import data.table
#' @export
#' 
filterData <- function(inputData, filterExpression, propagateUpwards = "") {
	
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

	applyFilter <- function(tableName, x, y) {
		filts <- x[[tableName]]
		table <- y[[tableName]]
		if(!nrow(table)) {
			warning(paste("Empty table", tableName))
			return(table)
		}

		test <- try(table <- table[eval(filts),], silent = TRUE)
		if(class(test)[1] == "try-error") {
			warning(paste0("Apply filter error:\n", test[1]))
		}
		return(table)
	}

	applyFilterWrap <- function(fileName, x, y) {
		out <- lapply(names(x[[fileName]]), applyFilter, x[[fileName]], y[[fileName]])
		names(out) <- names(x[[fileName]])

		merged <- replace(y[[fileName]], intersect(names(out), names(y[[fileName]])), out[intersect(names(out), names(y[[fileName]]))])

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
		ret <- lapply(names(pFilters), applyFilter, pFilters, inputData)
	} else {
		ret <- lapply(names(pFilters), applyFilterWrap, pFilters, inputData)
	}
	names(ret) <- names(pFilters)

	merged <- replace(inputData, intersect(names(ret), names(inputData)), ret[intersect(names(ret), names(inputData))])

	# 3. (TODO) Propagate down

	# 4. (TODO) Propagate up

	return(merged)
}
