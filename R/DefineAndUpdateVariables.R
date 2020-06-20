##################################################
#' Biotic variable conversion
#' 
#' This function reads a file holding a table for converting values of one or more variables to new values in \code{\link{BioticData}}. The actual conversion is done by \code{\link{ConvertBioticVariables}}.
#' 
#' @param processData The current data produced by a previous instance of the function.
#' @param FileName A file from which to read the \code{VariableConversionTable}.
#' @param UseProcessData Logical: If TRUE use the existing function output in the process. 
#' 
#' @return
#' A \code{\link{BioticVariableConversion}} object.
#' 
#' @export
#' 
DefineBioticVariableConversion <- function(processData, FileName, UseProcessData = FALSE) {
	# Get the conversion table:
	readVariableConversionTable(
		processData = processData, 
		FileName = FileName, 
		UseProcessData = UseProcessData
	)
}

##################################################
#' Convert Biotic variables
#' 
#' This function converts specific vaiables of specific tables of BioticData to used defined new values.
#' 
#' @inheritParams FilterBiotic
#' @param ConversionMethod  Character: A string naming the method to use, one of "Table", for providing the old and new values of the variables in the table \code{VariableConversionTable}; and "PreDefined" for providing the table using the argument \code{BioticVariableConversion}.
#' @param VariableConversionTable A table of the columns \code{VariableName}, \code{Value} and \code{NewValue}, specifying the conversion from Value to NewValue for any number of variables of any number of tables of \code{\link{BioticData}}.
#' @param BioticVariableConversion The \code{\link{BioticVariableConversion}} process data.
#' 
#' @return
#' A \code{\link{BioticData}} object.
#' 
#' @export
#' 
ConvertBioticVariables <- function(BioticData, ConversionMethod = c("Table", "PreDefined"), VariableConversionTable = data.table::data.table(), BioticVariableConversion) {
	
	# This function is not applicable to NMDBiotic1.4:
	if(!checkUniqueFormat(BioticData)) {
		stop("ConvertBioticVariables() cannot be used on data of format ",  paste(getRstoxDataDefinitions("nonUniqueFormats"), collapse = ", "))
	}
	
	# Get the conversion table, either directly from the input or from the output from a previous process:
	VariableConversionTable <- getVariableConversionTable(
		ConversionMethod = ConversionMethod, 
		VariableConversionTable = VariableConversionTable, 
		PreDefinedVariableConversionTable = BioticVariableConversion
	)
	
	# Apply the conversion:
	convertVariables(data = BioticData, VariableConversionTable = VariableConversionTable)
}


##################################################
#' StoxBiotic variable conversion
#' 
#' This function reads a file holding a table for converting values of one or more variables to new values in \code{\link{StoxBioticData}}. The actual conversion is done by \code{\link{ConvertStoxBioticVariables}}.
#' 
#' @inheritParams DefineBioticVariableConversion
#' 
#' @return
#' A \code{\link{StoxBioticVariableConversion}} object.
#' 
#' @export
#' 
DefineStoxBioticVariableConversion <- function(processData, FileName, UseProcessData = FALSE) {
	# Get the conversion table:
	readVariableConversionTable(
		processData = processData, 
		FileName = FileName, 
		UseProcessData = UseProcessData
	)
}

##################################################
#' Convert StoxBiotic variables
#' 
#' This function converts specific vaiables of specific tables of StoxBioticData to used defined new values.
#' 
#' @inheritParams FilterStoxBiotic
#' @inheritParams ConvertBioticVariables
#' @param StoxBioticVariableConversion The (optional) \code{\link{StoxBioticVariableConversion}} process data.
#' 
#' @return
#' A \code{\link{StoxBioticData}} object.
#' 
#' @export
#' 
ConvertStoxBioticVariables <- function(StoxBioticData, BioticData, ConversionType = c("Mapping", "ReplaceFromBioticData"), ConversionMethod = c("Table", "PreDefined"), VariableConversionTable = data.table::data.table(), StoxBioticVariableConversion, VariableReplacementTable = data.table::data.table()) {
	
	ConversionType <- match.arg(ConversionType)
	
	if(ConversionType == "Mapping") {
		# Get the conversion table, either directly from the input or from the output from a previous process:
		VariableConversionTable <- getVariableConversionTable(
			ConversionMethod = ConversionMethod, 
			VariableConversionTable = VariableConversionTable, 
			PreDefinedVariableConversionTable = StoxBioticVariableConversion
		)
		
		# Apply the conversion:
		convertVariables(data = StoxBioticData, VariableConversionTable = VariableConversionTable)
	}
	else if(ConversionType == "ReplaceFromBioticData") {
		replaceVariables(StoxBioticData = StoxBioticData, BioticData = BioticData, VariableReplacementTable = VariableReplacementTable)
	}
	else {
		stop("ConversionType must be one of \"Mapping\" and \"ReplaceFromBioticData\"")
	}
}


##################################################
#' Acoustic variable conversion
#' 
#' This function reads a file holding a table for converting values of one or more variables to new values in \code{\link{AcousticData}}. The actual conversion is done by \code{\link{ConvertAcousticVariables}}.
#' 
#' @param processData The current data produced by a previous instance of the function.
#' @param FileName A file from which to read the \code{VariableConversionTable}.
#' @param UseProcessData Logical: If TRUE use the existing function output in the process. 
#' 
#' @return
#' A \code{\link{AcousticVariableConversion}} object.
#' 
#' @export
#' 
DefineAcousticVariableConversion <- function(processData, FileName, UseProcessData = FALSE) {
	# Get the conversion table:
	readVariableConversionTable(
		processData = processData, 
		FileName = FileName, 
		UseProcessData = UseProcessData
	)
}

##################################################
#' Convert Acoustic variables
#' 
#' This function converts specific vaiables of specific tables of AcousticData to used defined new values.
#' 
#' @inheritParams FilterAcoustic
#' @param ConversionMethod  Character: A string naming the method to use, one of "Table", for providing the old and new values of the variables in the table \code{VariableConversionTable}; and "PreDefined" for providing the table using the argument \code{AcousticVariableConversion}.
#' @param VariableConversionTable A table of the columns \code{VariableName}, \code{Value} and \code{NewValue}, specifying the conversion from Value to NewValue for any number of variables of any number of tables of \code{\link{AcousticData}}.
#' @param AcousticVariableConversion The \code{\link{AcousticVariableConversion}} process data.
#' 
#' @return
#' A \code{\link{AcousticData}} object.
#' 
#' @export
#' 
ConvertAcousticVariables <- function(AcousticData, ConversionMethod = c("Table", "PreDefined"), VariableConversionTable = data.table::data.table(), AcousticVariableConversion) {
	
	# Get the conversion table, either directly from the input or from the output from a previous process:
	VariableConversionTable <- getVariableConversionTable(
		ConversionMethod = ConversionMethod, 
		VariableConversionTable = VariableConversionTable, 
		PreDefinedVariableConversionTable = AcousticVariableConversion
	)
	
	# Apply the conversion:
	convertVariables(data = AcousticData, VariableConversionTable = VariableConversionTable)
}


##################################################
#' StoxAcoustic variable conversion
#' 
#' This function reads a file holding a table for converting values of one or more variables to new values in \code{\link{StoxAcousticData}}. The actual conversion is done by \code{\link{ConvertStoxAcousticVariables}}.
#' 
#' @inheritParams DefineAcousticVariableConversion
#' 
#' @return
#' A \code{\link{StoxAcousticVariableConversion}} object.
#' 
#' @export
#' 
DefineStoxAcousticVariableConversion <- function(processData, FileName, UseProcessData = FALSE) {
	# Get the conversion table:
	readVariableConversionTable(
		processData = processData, 
		FileName = FileName, 
		UseProcessData = UseProcessData
	)
}

##################################################
#' Convert StoxAcoustic variables
#' 
#' This function converts specific vaiables of specific tables of StoxAcousticData to used defined new values.
#' 
#' @inheritParams FilterStoxAcoustic
#' @inheritParams ConvertAcousticVariables
#' @param StoxAcousticVariableConversion The (optional) \code{\link{StoxAcousticVariableConversion}} process data.
#' 
#' @return
#' A \code{\link{StoxAcousticData}} object.
#' 
#' @export
#' 
ConvertStoxAcousticVariables <- function(StoxAcousticData, ConversionMethod = c("Table", "PreDefined"), VariableConversionTable = data.table::data.table(), StoxAcousticVariableConversion) {
	
	# Get the conversion table, either directly from the input or from the output from a previous process:
	VariableConversionTable <- getVariableConversionTable(
		ConversionMethod = ConversionMethod, 
		VariableConversionTable = VariableConversionTable, 
		PreDefinedVariableConversionTable = StoxAcousticVariableConversion
	)
	
	# Apply the conversion:
	convertVariables(data = StoxAcousticData, VariableConversionTable = VariableConversionTable)
}




# General function for creating a variable conversion table:
readVariableConversionTable <- function(processData, FileName, UseProcessData = FALSE) {
	# Return immediately if UseProcessData = TRUE:
	if(UseProcessData) {
		return(processData)
	}
	
	VariableConversionTable <- data.table::fread(FileName)
	
	# Check columns and store only valid columns:
	requiredColumns <- getRstoxDataDefinitions("variableConversionTableRequiredColumns")
	if(! all(requiredColumns %in% names(VariableConversionTable))) {
		stop("The VariableConversionTable must contain the columns ", paste(requiredColumns, collapse = ", "))
	}
	
	# Keep only required columns:
	VariableConversionTable <- VariableConversionTable[, ..requiredColumns]
	
	## Error if any of the cells are missing:
	#rowsWithMissingValues <- rowSums(VariableConversionTable[, lapply(.SD, function(x) nchar(x) == 0 | is.na(x))])
	#if(any(rowsWithMissingValues)) {
	#	stop("The columns ", paste(requiredColumns, collapse = ", "), " of the file FileName contain missing cells (empty string or NA). All #cells must be given to map from old to new values. (rows ", paste(which(as.numeric(rowsWithMissingValues)), collapse = ", "))
	#}
	
	return(VariableConversionTable)
}

# Function to treat the ConversionMethod:
getVariableConversionTable <- function(ConversionMethod = c("Table", "PreDefined"), VariableConversionTable, PreDefinedVariableConversionTable) {
	
	ConversionMethod <- match.arg(ConversionMethod)
	
	if(ConversionMethod == "Table") {
		if(length(VariableConversionTable) == 0) {
			stop("VariableConversionTable must be given if ConversionMethod = \"Table\"")
		}
	}
	else if(ConversionMethod == "PreDefined") {
		VariableConversionTable <- PreDefinedVariableConversionTable
	}
	else {
		stop("Wrong ConversionMethod")
	}
	
	return(VariableConversionTable)
}

# Function to convert variables given a conversion table:
convertVariables <- function(data, VariableConversionTable) {
	
	dataCopy <- data.table::copy(data)
	
	requiredColumns <- getRstoxDataDefinitions("variableConversionTableRequiredColumns")
	if(! all(requiredColumns %in% names(VariableConversionTable))) {
		stop("The VariableConversionTable must contain the columns ", paste(requiredColumns, collapse = ", "))
	}
	
	# Split into a list, thus treating only one row at the time. This is probably sloppy coding:
	conversionList <- split(VariableConversionTable, seq_len(nrow(VariableConversionTable)))
	# Run the conversion for each row of the VariableConversionTable:
	lapply(conversionList, convertVariable, data = dataCopy)
	
	return(dataCopy[])
}

# Function to convert variables given one row of a conversion table:
convertVariable <- function(conversionList, data) {
	lapplyToStoxData(data, convertOneTable, conversionList = conversionList)
}


# Function to apply to all tables of the input data, converting the variables:
convertOneTable <- function(x, conversionList) {
	# Check that the table contains the variable to convert:
	if(conversionList$VariableName %in% names(x)) {
		# Do nothing if the variable is a key:
		isKeys <- endsWith(conversionList$VariableName, "Key")
		if(isKeys) {
			warning("StoX: The variable", conversionList$VariableName, " is a key and cannot be modified ")
		}
		else {
			# Convert the class to the class of the existing value in the table:
			conversionList <- convertClassToExisting(conversionList, x)
			# Replace by the new value:
			x[, c(conversionList$VariableName) := replace(
				x = get(conversionList$VariableName), 
				list = get(conversionList$VariableName) %in% conversionList$Value, 
				values = conversionList$NewValue)]
		}
	}
}


# Function to apply a function to StoX data, which may be a list of tables or a list of lists of tables:
lapplyToStoxData <- function(x, fun, ...) {
	# Check the depth of the list, either with tables at the top level og with lists of tables:
	if(is.list(x[[1]]) && !data.table::is.data.table(x[[1]])) {
		lapply(x, function(y) lapply(y, fun, ...))
	}
	else {
		lapply(x, fun, ...)
	}
}

# Function to convert the class of the Value and NewValue of a conversionList to the class of the existing value:
convertClassToExisting <- function(conversionList, x) {
	# Convert the NewValue to the class of the existing value:
	existingClass <- class(x[[conversionList$VariableName]])[1]
	newClass <- class(conversionList$Value)[1]
	if(!identical(existingClass, newClass)) {
		class(conversionList$Value) <- existingClass
		class(conversionList$NewValue) <- existingClass
	}
	return(conversionList)
}



replaceVariables <- function(StoxBioticData, BioticData, VariableReplacementTable) {
	# Add the requested variable:
	StoxBioticData <- AddStoxBioticVariables(StoxBioticData = StoxBioticData, BioticData = BioticData, VariableName = VariableReplacementTable$Replacement)
	# Remove the old:
	lapply(StoxBioticData, replaceAndDelete, VariableReplacementTable = VariableReplacementTable)
	
	return(StoxBioticData)
}

# Function to replace the existing column by the new, as stored in the VariableReplacementTable:
replaceAndDelete <- function(table, VariableReplacementTable) {
	present <- which(VariableReplacementTable$VariableName %in% names(table))
	if(any(present)) {
		# Delete the present column:
		table[, (VariableReplacementTable[present, VariableName]) := NULL]
		# ... and then rename the new to the old name:
		setnames(table, VariableReplacementTable[present, Replacement], VariableReplacementTable[present, VariableName])
	}
}
