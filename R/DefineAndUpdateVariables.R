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
#' @param VariableConversionTable A table of the columns \code{TableName}, \code{VariableName}, \code{Value} and \code{NewValue}, specifying the conversion from Value to NewValue for any number of variables of any number of tables of \code{\link{BioticData}}.
#' @param BioticVariableConversion The \code{\link{BioticVariableConversion}} process data.
#' 
#' @return
#' A \code{\link{BioticData}} object.
#' 
#' @export
#' 
ConvertBioticVariables <- function(BioticData, ConversionMethod = c("Table", "PreDefined"), VariableConversionTable = data.table::data.table(), BioticVariableConversion) {
	
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
ConvertStoxBioticVariables <- function(StoxBioticData, ConversionMethod = c("Table", "PreDefined"), VariableConversionTable = data.table::data.table(), StoxBioticVariableConversion) {
	
	# Get the conversion table, either directly from the input or from the output from a previous process:
	VariableConversionTable <- getVariableConversionTable(
		ConversionMethod = ConversionMethod, 
		VariableConversionTable = VariableConversionTable, 
		PreDefinedVariableConversionTable = StoxBioticVariableConversion
	)
	
	# Apply the conversion:
	convertVariables(data = StoxBioticData, VariableConversionTable = VariableConversionTable)
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
#' @param VariableConversionTable A table of the columns \code{TableName}, \code{VariableName}, \code{Value} and \code{NewValue}, specifying the conversion from Value to NewValue for any number of variables of any number of tables of \code{\link{AcousticData}}.
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
	
	VariableConversionTable
}

# Function to convert variables given a conversion table:
convertVariables <- function(data, VariableConversionTable) {
	
	dataCopy <- data.table::copy(data)
	
	requiredColumns <- c("TableName", "VariableName", "Value", "NewValue")
	#requiredColumns2 <- c("FileName", requiredColumns)
	
	if(is.list(dataCopy[[1]]) && !data.table::is.data.table(dataCopy[[1]])) {
		#if(! all(requiredColumns2 %in% names(VariableConversionTable))) {
		if(! all(requiredColumns %in% names(VariableConversionTable))) {
			stop("The VariableConversionTable must contain the columns ", paste(requiredColumns, collapse = ", "))
		}
		else if(all(grepl("/", VariableConversionTable$TableName, fixed = TRUE))){
			# Extract the FileNames and TableNames:
			s <- strsplit(VariableConversionTable$TableName, "/")
			s <- lapply(s, function(x) data.table::data.table(FileName = x[1], TableName = x[2]))
			s <- data.table::rbindlist(s)
			VariableConversionTable <- data.table::data.table(
				s, 
				VariableConversionTable[, TableName := NULL]
			)
		}
		else {
			# Repeat the file names of the input data:
			VariableConversionTable <- cbind(
				data.table::data.table(
					FileName = rep(names(data), nrow(VariableConversionTable))
				), 
				VariableConversionTable
			)
		}
	}
	else {
		if(! all(requiredColumns %in% names(VariableConversionTable))) {
			stop("The VariableConversionTable must contain the columns ", paste(requiredColumns, collapse = ", "))
		}
	}
	
	# Split into a list, thus treatin only one row at the time. This is probably sloppy coding:
	conversionList <- split(VariableConversionTable, seq_len(nrow(VariableConversionTable)))
	# Run the conversion for each row of the VariableConversionTable:
	lapply(conversionList, convertVariable, data = dataCopy)
	
	return(dataCopy[])
}

# Function to convert variables given one row of a conversion table:
convertVariable <- function(conversionList, data) {
	
	# Convert the NewValue to the class of the existing value:
	if(length(conversionList$FileName)) {
		existingClass <- class(data[[conversionList$FileName]][[conversionList$TableName]][[conversionList$VariableName]])[1]
	}
	else {
		existingClass <- class(data[[conversionList$TableName]][[conversionList$VariableName]])[1]
	}
	newClass <- class(conversionList$Value)[1]
	if(!identical(existingClass, newClass)) {
		class(conversionList$Value) <- existingClass
	}
	
	# If FileName is given, step into the files:
	if(length(conversionList$FileName)) {
		# Do nothing if the variable is a key:
		isKeys <- conversionList$VariableName %in% getKeys(data[[conversionList$FileName]])
		if(isKeys) {
			warning("The variable", conversionList$VariableName, " is a key and cannot be modified ")
		}
		else {
			# Replace by the new value:
			data[[conversionList$FileName]][[conversionList$TableName]][, c(conversionList$VariableName) := replace(
				x = get(conversionList$VariableName), 
				list = get(conversionList$VariableName) %in% conversionList$Value, 
				values = conversionList$NewValue)]
		}
	}
	# Otherwise, only treat tables:
	else {
		# Do nothing if the variable is a key:
		isKeys <- endsWith(conversionList$VariableName, "Key")
		if(isKeys) {
			warning("The variable", conversionList$VariableName, " is a key and cannot be modified ")
		}
		else {
			# Replace by the new value:
			data[[conversionList$TableName]][, c(conversionList$VariableName) := replace(
				x = get(conversionList$VariableName), 
				list = get(conversionList$VariableName) %in% conversionList$Value, 
				values = conversionList$NewValue)]
		}
	}
}
