
# The general function for redefining StoxData:
RedefineData <- function(
	StoxData, RawData, 
	Redefinition = data.table::data.table(), 
	StoxDataFormat = c("Biotic", "Acoustic"), 
	NumberOfCores = 1L
) {
	
	StoxDataFormat <- match.arg(StoxDataFormat)
	
	# Add the requested variable:
	StoxData <- AddToStoxData(
		StoxData = StoxData, 
		RawData = RawData, 
		VariableNames = Redefinition$ReplaceBy, 
		NumberOfCores = NumberOfCores, 
		StoxDataFormat = StoxDataFormat
	)
	
	# Remove the old:
	lapply(StoxData, replaceAndDelete, VariableReplacement = Redefinition)
	
	return(StoxData)
}

##################################################
#' Redefine StoxBioticData variables by data from BioticData
#' 
#' This function redefines one or more columns of \code{\link{StoxBioticData}} by columns of \code{\link{BioticData}}.
#' 
#' @param StoxBioticData An input of \link{ModelData} object
#' @param BioticData An input of \link{ModelData} object
#' @param Redefinition A table of the columns "VariableName", representing the variable to redefine; and "ReplaceBy", representing the variable from BioticData to replace by. 
#' 
#' @return
#' A \code{\link{StoxBioticData}} object.
#' 
#' @export
#' 
RedefineStoxBiotic <- function(
	StoxBioticData, BioticData, 
	Redefinition = data.table::data.table()
) {
	# Redefine StoxBioticData:
	RedefineData(
		StoxData = StoxBioticData, RawData = BioticData, 
		Redefinition = Redefinition, 
		StoxDataFormat = "Biotic"
	)
}


##################################################
#' Define translation
#' 
#' This function defines the translation table used as input to \code{\link{TranslateStoxBiotic}} and similar functions to translate values of one or more columns to new values given by a table or read from a CSV file.
#' 
#' @inheritParams general_arguments
#' @param DefinitionMethod  Character: A string naming the method to use, one of "TranslationTable" for defining the \code{TranslationTable}, and "ResourceFile" for reading the table from the file given by \code{FileName}.
#' @param TranslationTable A table of the columns \code{VariableName}, representing the variable to translate; \code{Value}, giving the values to translate; and \code{NewValue}, giving the values to translate to. Use NA in the Value column to translate missing values (shown as "-" in View output in the StoX GUI, and usually as empty cell in excel). In the current version NAs cannot be mixed with non-NAs in the Value column. Please use a separate DefineTranslation & Translate procecss to translate NAs.
#' @param ValueColumn,NewValueColumn The name of the columns of \code{TranslationTable} representing the current values and the values to translate to, respectively.
#' @param Conditional Logical: If TRUE the columns \code{ConditionalVariableName} and \code{ConditionalValue} are expected in the \code{TranslationTable}. These define a variable interacting with the \code{VariableName} and \code{Value}, so that \code{VariableName} is changed from \code{Value} to \code{NewValue} only when \code{ConditionalVariableName} has the value given by \code{ConditionalValue}. Note that \code{ConditionalVariableName} must exist in the same table as \code{VariableName}. 
#' @param FileName The csv file holding a table with the three variables listed for \code{TranslationTable}.
#' 
#' @return
#' A \code{\link{Translation}} object.
#' 
#' @seealso \code{\link{TranslateStoxBiotic}}, \code{\link{TranslateStoxAcoustic}}, \code{\link{TranslateBiotic}}, \code{\link{TranslateAcoustic}}, \code{\link{TranslateStoxLanding}}, \code{\link{TranslateLanding}}, \code{\link{TranslateICESBiotic}} and \code{\link{TranslateICESAcoustic} for applying the translation.
#'
#' @export
#' 
DefineTranslation <- function(
	processData, UseProcessData = FALSE, 
	DefinitionMethod = c("ResourceFile", "TranslationTable"), 
	TranslationTable = data.table::data.table(), 
	ValueColumn = character(), 
	NewValueColumn = character(), 
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	FileName = character()
) {
	
	# Return immediately if UseProcessData = TRUE:
	if(UseProcessData) {
		return(processData)
	}
	
	DefinitionMethod = match.arg(DefinitionMethod)
	
	if(DefinitionMethod == "ResourceFile") {
		# Get the conversion table:
		Translation <- readVariableTranslation(
			processData = processData, 
			FileName = FileName, 
			ValueColumn = ValueColumn, 
			NewValueColumn = NewValueColumn, 
			UseProcessData = UseProcessData
		)
	}
	else if(DefinitionMethod == "TranslationTable"){
		Translation <- TranslationTable
	}
	else {
		stop("Invalid DefinitionMethod")
	}
	
	return(Translation)
}


# Function for reading a conversion table:
readVariableTranslation <- function(processData, FileName, ValueColumn, NewValueColumn, UseProcessData = FALSE) {
	
	# Return immediately if UseProcessData = TRUE:
	if(UseProcessData) {
		return(processData)
	}
	
	conversion <- data.table::fread(FileName, encoding = "UTF-8")
	
	# Get the Value and NewValue:
	conversion[, Value := get(ValueColumn)]
	conversion[, NewValue := get(NewValueColumn)]
	
	return(conversion)
}

# Function to convert variables given a conversion table:
translateVariables <- function(data, Translation, translate.keys = FALSE, warnMissingTranslation = FALSE) {
	
	# Check whether the reuired columns are present in the translation table:
	requiredColumns <- getRstoxDataDefinitions("TranslationRequiredColumns")
	if(! all(requiredColumns %in% names(Translation))) {
		stop("The Translation must contain the columns ", paste(requiredColumns, collapse = ", "))
	}
	
	# Run the conversion for each table of the data:
	lapplyToStoxData(
		data, 
		translateOneTable, 
		Translation = Translation, 
		translate.keys = translate.keys, 
		warnMissingTranslation = warnMissingTranslation
	)
	
	a = 1 #????????
}

# Function to translate one table:
translateOneTable <- function(table, Translation, translate.keys = FALSE, warnMissingTranslation = FALSE) {
	
	# Check that the table contains the variable to translate:
	if(any(Translation$VariableName %in% names(table))) {
		
		## Check for values of the data that are not covered by the translation:
		#notPresentInTranslation <- sort(
		#	setdiff(
		#		table[[Translation$VariableName[1]]], 
		#		Translation$Value
		#	)
		#)
		
		if(warnMissingTranslation) {
			# Get the combinations of ConditionalVariableName and VariableName missing in three Translation:
			variableKeys <- intersect(c("ConditionalVariableName", "VariableName"), names(Translation))
			TranslationSplit <- split(Translation, by = variableKeys)
			missingCombinations <- lapply(TranslationSplit, getMissingCombinations, table = table, variableKeys = variableKeys)
			
			
			if(any(unlist(lapply(missingCombinations, nrow)))) {
				warnMessage <- c(
					"StoX: The following values are present in the data but not in the translation, and were not translated:", 
					unlist(lapply(missingCombinations, getMissingCombinationsWarning))
				)
				warnMessage <- paste(warnMessage, collapse = "\n")
				
				warning(warnMessage)
			}
		}
		
		# Split the translation table into a list, thus treating only one row at the time. This is probably sloppy coding, but it works:
		translationList <- split(Translation, seq_len(nrow(Translation)))
		
		# Apply the translation, one line at the time:
		lapply(
			translationList, 
			translateOneTranslationOneTable, 
			table = table, 
			translate.keys = translate.keys
		)
	}
	
	a = 1 #????????
}

getMissingCombinations <- function(translation, table, variableKeys) {
	# Get the names of those columns in the data:
	transNames <- translation[, ..variableKeys]
	variablesInTable <- unname(unlist(transNames[1,]))
	if(!all(variablesInTable %in% names(table))) {
		return(NULL)
	}
	
	tab <- table[, ..variablesInTable]
	# And the names of columns holding the values of those variables in the translation:
	valueVariablesInTranslation <- intersect(c("ConditionalValue", "Value"), names(translation))
	trans <- translation[, ..valueVariablesInTranslation]
	names(trans) <- variablesInTable
	# Match the type of the table:
	matchClass(trans, tab)
	
	# Then simply set diff those values from those in the data, and unniquify:
	missingCombinations <- tab[!trans, on = variablesInTable]
	missingCombinations <- data.table::setorderv(unique(missingCombinations), variablesInTable)
	return(missingCombinations)
}

getMissingCombinationsWarning <- function(x) {
	c(
		paste(names(x), collapse = ", "), 
		x[, do.call(paste, c(.SD, list(sep = ", ")))], 
		""
	)
}


matchClass <- function(A, B) {
	for (x in colnames(A)) {
		A[, eval(x) := eval( call( paste0("as.", class(B[[x]])), get(x) ))]
	}
}




# Function to apply to all tables of the input data, converting the variables:
translateOneTranslationOneTable <- function(translationListOne, table, translate.keys = FALSE) {
	
	# Do nothing if the variable is a key:
	isKeys <- endsWith(translationListOne$VariableName, "Key")
	if(!translate.keys && isKeys) {
		warning("StoX: The variable ", translationListOne$VariableName, " is a key and cannot be modified ")
	}
	else {
		# Convert the class to the class of the existing value in the table:
		translationListOne <- convertClassToExistingOne(translationListOne, table)
		
		# Replace by the new value:
		if(translationListOne$VariableName %in% names(table)) {
			if("ConditionalVariableName" %in% names(translationListOne)) {
				table[, c(translationListOne$VariableName) := replace(
					x = get(translationListOne$VariableName), 
					list = get(translationListOne$ConditionalVariableName) %in% translationListOne$ConditionalValue  &  
						get(translationListOne$VariableName) %in% translationListOne$Value, 
					values = translationListOne$NewValue)]
			}
			else {
				table[, c(translationListOne$VariableName) := replace(
					x = get(translationListOne$VariableName), 
					list = get(translationListOne$VariableName) %in% translationListOne$Value, 
					values = translationListOne$NewValue)]
			}
		}
	}
}

# Function to convert the class of the Value and NewValue of a translationList to the class of the existing value:
convertClassToExistingOne <- function(translationList, x, elements = c("Value", "NewValue")) {
	for(element in elements) {
		translationList <- convertClassToExistingOneElement(
			translationList = translationList, 
			x = x, 
			element = element
		)
	}
	return(translationList)
}
convertClassToExistingOneElement <- function(translationList, x, element) {
	# Convert the NewValue to the class of the existing value:
	existingClass <- class(x[[translationList$VariableName]])[1]
	newClass <- class(translationList[[element]])[1]
	if(!identical(existingClass, newClass) && !is.na(translationList[[element]])) {
		class(translationList[[element]]) <- existingClass
		#class(translationList$NewValue) <- existingClass
	}
	return(translationList)
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

# Function to replace the existing column by the new, as stored in the VariableReplacement:
replaceAndDelete <- function(table, VariableReplacement) {
	
	presentVariableName <- which(VariableReplacement$VariableName  %in%  names(table))
	hasReplaceBy <- VariableReplacement$ReplaceBy  %in%  names(table)
	if(length(presentVariableName) && hasReplaceBy) {
		# Delete the present column:
		table[, (VariableReplacement[presentVariableName, VariableName]) := NULL]
		# ... and then rename the new to the old name:
		setnames(table, VariableReplacement[presentVariableName, ReplaceBy], VariableReplacement[presentVariableName, VariableName])
	}
}







##################################################
#' Translate StoxBioticData
#' 
#' This function translates one or more columns of \code{\link{StoxBioticData}} to new values given by the input \code{Translation}.
#' 
#' @param StoxBioticData An input of \link{ModelData} object
#' @param Translation The process from which to get the \code{\link{Translation}} definition.
#' 
#' @return
#' A \code{\link{StoxBioticData}} object.
#' 
#' @export
#' 
TranslateStoxBiotic <- function(
	StoxBioticData, 
	Translation
) {
	# Translate StoxBioticData:
	translateVariables(
		data = StoxBioticData, 
		Translation = Translation
	)
	
	return(StoxBioticData)
}


##################################################
#' Convert StoxBioticData
#' 
#' This function converts one or more columns of \code{\link{StoxBioticData}} by the function given by \code{ConversionFunction}.
#' 
#' @param StoxBioticData An input of \link{ModelData} object
#' @param TargetVariable The variable to modify.
#' @param ConversionFunction  Character: The function to convert by, one of "Constant", for replacing the specified columns by a constant value; "Addition", for adding to the columns; "Scaling", for multiplying by a factor; and "AdditionAndScaling", for both adding and multiplying.
#' @param GruopingVariables A vector of variables to specify in the \code{Conversion}. The parameters specified in the table are valid for the combination of the \code{GruopingVariables} in the data.
#' @param Conversion A table of the \code{GruopingVariables} and the columns "TargetVariable", "SourceVariable" and the parameters of the \code{ConversionFunction} (see details).
#' 
#' The parameters of the \code{ConversionFunction} are "Constant" for ConversionFunction "Constant", "Addition" for ConversionFunction"Addition", "Scaling" for ConversionFunction "Scaling", and "Addition" and "Scaling" for ConversionFunction "AdditionAndScaling".
#' 
#' @return
#' A \code{\link{StoxBioticData}} object.
#' 
#' @export
#' 
ConvertStoxBiotic <- function(
	StoxBioticData, 
	TargetVariable = character(), 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(),  
	Conversion = data.table::data.table()
) {
	
	# Convert StoxBioticData:
	ConvertData(
		StoxData = StoxBioticData, 
		TargetVariable = TargetVariable, 
		ConversionFunction = ConversionFunction,
		GruopingVariables = GruopingVariables,
		Conversion = Conversion
	)
}




ConvertStoxBioticOld <- function(
	StoxBioticData, 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(),  
	Conversion = data.table::data.table()
) {
	# Convert StoxBioticData:
	ConvertData(
		StoxData = StoxBioticData, 
		ConversionFunction = ConversionFunction,
		GruopingVariables = GruopingVariables,
		Conversion = Conversion
	)
}



ConvertStoxBioticFree <- function(
	StoxBioticData, 
	TargetVariable = character(),  
	Conversion = character()
) {
	# Convert StoxBioticData:
	ConvertData(
		StoxData = StoxBioticData, 
		TargetVariable = TargetVariable,
		Conversion = Conversion
	)
}





##################################################
#' Translate StoxAcousticData
#' 
#' This function translates one or more columns of \code{\link{StoxAcousticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams TranslateStoxBiotic
#' @param StoxAcousticData An input of \link{ModelData} object
#' 
#' @return
#' A \code{\link{StoxAcousticData}} object.
#' 
#' @export
#' 
TranslateStoxAcoustic <- function(
	StoxAcousticData, 
	Translation
) {
	translateVariables(
		data = StoxAcousticData, 
		Translation = Translation
	)
	
	return(StoxAcousticData)
}


##################################################
#' Convert StoxAcousticData
#' 
#' This function converts one or more columns of \code{\link{StoxAcousticData}} by the function given by \code{ConversionFunction}.
#' 
#' @inheritParams ConvertStoxBiotic
#' @param StoxAcousticData An input of \link{ModelData} object
#' 
#' The parameters of the \code{ConversionFunction} are "Constant" for ConversionFunction "Constant", "Addition" for ConversionFunction"Addition", "Scaling" for ConversionFunction "Scaling", and "Addition" and "Scaling" for ConversionFunction "AdditionAndScaling".
#' 
#' @return
#' A \code{\link{StoxAcousticData}} object.
#' 
#' @export
#' 
ConvertStoxAcoustic <- function(
	StoxAcousticData, 
	TargetVariable = character(), 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(),  
	Conversion = data.table::data.table()
) {
	# Convert StoxAcousticData:
	ConvertData(
		StoxData = StoxAcousticData, 
		TargetVariable = TargetVariable, 
		ConversionFunction = ConversionFunction,
		GruopingVariables = GruopingVariables,
		Conversion = Conversion
	)
}



ConvertStoxAcousticOld <- function(
	StoxAcousticData, 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(),  
	Conversion = data.table::data.table()
) {
	# Convert StoxAcousticData:
	ConvertData(
		StoxData = StoxAcousticData, 
		ConversionFunction = ConversionFunction,
		GruopingVariables = GruopingVariables,
		Conversion = Conversion
	)
}





ConvertStoxAcousticFree <- function(
	StoxAcousticData, 
	TargetVariable = character(),  
	Conversion = character()
) {
	# Convert StoxAcousticData:
	ConvertData(
		StoxData = StoxAcousticData, 
		TargetVariable = TargetVariable,
		Conversion = Conversion
	)
}




##################################################
#' Translate BioticData
#' 
#' This function translates one or more columns of \code{\link{BioticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams TranslateStoxBiotic
#' @param BioticData An input of \link{ModelData} object
#' 
#' @return
#' A \code{\link{BioticData}} object.
#' 
#' @export
#' 
TranslateBiotic <- function(
	BioticData, 
	Translation
) {
	translateVariables(
		data = BioticData, 
		Translation = Translation
	)
	
	return(BioticData)
}


##################################################
#' Convert BioticData
#' 
#' This function converts one or more columns of \code{\link{BioticData}} by the function given by \code{ConversionFunction}.
#' 
#' @inheritParams ConvertStoxBiotic
#' @param BioticData An input of \link{ModelData} object
#' 
#' The parameters of the \code{ConversionFunction} are "Constant" for ConversionFunction "Constant", "Addition" for ConversionFunction"Addition", "Scaling" for ConversionFunction "Scaling", and "Addition" and "Scaling" for ConversionFunction "AdditionAndScaling".
#' 
#' @return
#' A \code{\link{BioticData}} object.
#' 
#' @export
#' 
ConvertBiotic <- function(
	BioticData, 
	TargetVariable = character(), 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(),  
	Conversion = data.table::data.table()
) {
	# Convert BioticData:
	ConvertData(
		StoxData = BioticData, 
		TargetVariable = TargetVariable, 
		ConversionFunction = ConversionFunction,
		GruopingVariables = GruopingVariables,
		Conversion = Conversion
	)
}



ConvertBioticOld <- function(
	BioticData, 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(),  
	Conversion = data.table::data.table()
) {
	# Convert BioticData:
	ConvertData(
		StoxData = BioticData, 
		ConversionFunction = ConversionFunction,
		GruopingVariables = GruopingVariables,
		Conversion = Conversion
	)
}



ConvertBioticFree <- function(
	BioticData, 
	TargetVariable = character(),  
	Conversion = character()
) {
	# Convert BioticData:
	ConvertData(
		StoxData = BioticData, 
		TargetVariable = TargetVariable,
		Conversion = Conversion
	)
}




##################################################
#' Translate AcousticData
#' 
#' This function translates one or more columns of \code{\link{AcousticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams TranslateStoxBiotic
#' @param AcousticData An input of \link{ModelData} object
#' 
#' @return
#' A \code{\link{AcousticData}} object.
#' 
#' @export
#' 
TranslateAcoustic <- function(
	AcousticData, 
	Translation
) {
	translateVariables(
		data = AcousticData, 
		Translation = Translation
	)
	
	return(AcousticData)
}


##################################################
#' Convert AcousticData
#' 
#' This function converts one or more columns of \code{\link{AcousticData}} by the function given by \code{ConversionFunction}.
#' 
#' @inheritParams ConvertStoxBiotic
#' @param AcousticData An input of \link{ModelData} object
#' 
#' The parameters of the \code{ConversionFunction} are "Constant" for ConversionFunction "Constant", "Addition" for ConversionFunction"Addition", "Scaling" for ConversionFunction "Scaling", and "Addition" and "Scaling" for ConversionFunction "AdditionAndScaling".
#' 
#' @return
#' A \code{\link{AcousticData}} object.
#' 
#' @export
#' 
ConvertAcoustic <- function(
	AcousticData, 
	TargetVariable = character(), 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(), 
	Conversion = data.table::data.table()
) {
	# Convert AcousticData:
	ConvertData(
		StoxData = AcousticData, 
		TargetVariable = TargetVariable, 
		ConversionFunction = ConversionFunction,
		GruopingVariables = GruopingVariables,
		Conversion = Conversion
	)
}


ConvertAcousticOld <- function(
	AcousticData, 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(),  
	Conversion = data.table::data.table()
) {
	# Convert AcousticData:
	ConvertData(
		StoxData = AcousticData, 
		ConversionFunction = ConversionFunction,
		GruopingVariables = GruopingVariables,
		Conversion = Conversion
	)
}


ConvertAcousticFree <- function(
	AcousticData, 
	TargetVariable = character(),  
	Conversion = character()
) {
	# Convert AcousticData:
	ConvertData(
		StoxData = AcousticData, 
		TargetVariable = TargetVariable,
		Conversion = Conversion
	)
}





##################################################
#' Translate StoxLandingData
#' 
#' This function translates one or more columns of \code{\link{StoxLandingData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams TranslateStoxBiotic
#' @param StoxLandingData An input of \link{ModelData} object
#' 
#' @return
#' A \code{\link{StoxLandingData}} object.
#' 
#' @export
#' 
TranslateStoxLanding <- function(
	StoxLandingData, 
	Translation
) {
	translateVariables(
		data = StoxLandingData, 
		Translation = Translation
	)
	
	return(StoxLandingData)
}


##################################################
#' Translate LandingData
#' 
#' This function translates one or more columns of \code{\link{LandingData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams TranslateStoxBiotic
#' @param LandingData An input of \link{ModelData} object
#' 
#' @return
#' A \code{\link{LandingData}} object.
#' 
#' @export
#' 
TranslateLanding <- function(
	LandingData, 
	Translation
) {
	translateVariables(
		data = LandingData, 
		Translation = Translation
	)
	
	return(LandingData)
}






##################################################
#' Translate ICESBioticData
#' 
#' This function translates one or more columns of \code{\link{ICESBioticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams TranslateStoxBiotic
#' @param ICESBioticData An input of \link{ModelData} object
#' 
#' @return
#' A \code{\link{ICESBioticData}} object.
#' 
#' @export
#' 
TranslateICESBiotic <- function(
	ICESBioticData, 
	Translation
) {
	translateVariables(
		data = ICESBioticData, 
		Translation = Translation
	)
	
	return(ICESBioticData)
}


##################################################
#' Translate ICESAcousticData
#' 
#' This function translates one or more columns of \code{\link{ICESAcousticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams TranslateStoxBiotic
#' @param ICESAcousticData An input of \link{ModelData} object
#' 
#' @return
#' A \code{\link{ICESAcousticData}} object.
#' 
#' @export
#' 
TranslateICESAcoustic <- function(
	ICESAcousticData, 
	Translation
) {
	translateVariables(
		data = ICESAcousticData, 
		Translation = Translation
	)
	
	return(ICESAcousticData)
}





# The general function for converting StoxData:
ConvertDataOld <- function(
	StoxData, 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(), 
	Conversion = data.table::data.table()
) {
	
	# Get the ConversionFunction input:
	ConversionFunction <- match.arg(ConversionFunction)
	
	# Check the Conversion for unique grouping variables:
	if(!all(GruopingVariables %in% names(Conversion))) {
		stop("All grouping variables must be present in the Conversion")
	}
	
	# Make a copy to allow for applying functions by reference:
	StoxDataCopy <- data.table::copy(StoxData)
	
	# Merge the data to allow for use of variables from different tables (e.g., length and lengthmeasurement for NMDBiotic 3.0)
	StoxDataCopyMerged <- mergeDataTables(StoxDataCopy, output.only.last = TRUE)
	
	# Merge in the Conversion:
	StoxDataCopyMerged <- mergeByIntersect(StoxDataCopyMerged, Conversion, all.x = TRUE)
	
	# Apply the conversion function for each row of the Conversion:
	ConversionList <- split(Conversion, seq_len(nrow(Conversion)))
	for(Conversion in ConversionList) {
		applyConversionFunction(
			data = StoxDataCopyMerged, 
			ConversionFunction = ConversionFunction, 
			TargetVariable = Conversion$TargetVariable, 
			SourceVariable = Conversion$SourceVariable, 
			RoundOffTo = Conversion$RoundOffTo
		)
	}
	
	# Extract the StoxData from the merged:
	StoxDataCopy <- getStoxDataFromMerged(
		StoxDataMerged = StoxDataCopyMerged, 
		StoxData = StoxDataCopy
	)
	
	
	
	
	return(StoxDataCopy)
}



# The general function for converting StoxData:
ConvertData <- function(
	StoxData, 
	TargetVariable, 
	GruopingVariables = character(), 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	Conversion = data.table::data.table()
) {
	
	# Get the ConversionFunction input:
	ConversionFunction <- match.arg(ConversionFunction)
	
	# Check the Conversion for unique grouping variables:
	if(!all(GruopingVariables %in% names(Conversion))) {
		stop("All grouping variables must be present in the Conversion")
	}
	
	# Make a copy to allow for applying functions by reference:
	StoxDataCopy <- data.table::copy(StoxData)
	# Merge the data to allow for use of variables from different tables (e.g., length and lengthmeasurement for NMDBiotic 3.0)
	StoxDataCopyMerged <- mergeDataTables(StoxDataCopy, output.only.last = TRUE)
	
	# Apply the conversion function for each row of the Conversion:
	ConversionList <- split(Conversion, seq_len(nrow(Conversion)))
	for(Conversion in ConversionList) {
		parameterNames <- setdiff(names(Conversion), c("SourceVariable", "RoundOffTo"))
		applyConversionFunction(
			data = StoxDataCopyMerged, 
			ConversionFunction = ConversionFunction, 
			#TargetVariable = Conversion$TargetVariable, 
			TargetVariable = TargetVariable, 
			SourceVariable = Conversion$SourceVariable, 
			RoundOffTo = Conversion$RoundOffTo, 
			parameters = lapply(split(Conversion[, ..parameterNames], seq_len(nrow(Conversion))), as.list)
			#parameters = as.list(Conversion[, ..parameterNames])
		)
	}
	
	# Extract the StoxData from the merged:
	StoxDataCopy <- getStoxDataFromMerged(
		StoxDataMerged = StoxDataCopyMerged, 
		StoxData = StoxDataCopy
	)
	
	return(StoxDataCopy)
}



# The general function for converting StoxData:
ConvertDataFree <- function(
	StoxData, 
	TargetVariable = character(), 
	Conversion = character()
) {
	
	if(!nchar(TargetVariable) || !nchar(Conversion)) {
		warning("Unspecified TargetVariable or empty conversion expression. Data returned unchanged.")
		return(StoxData)
	}
	
	# Make a copy to allow for applying functions by reference:
	StoxDataCopy <- data.table::copy(StoxData)
	
	# Merge the data to allow for use of variables from different tables (e.g., length and lengthmeasurement for NMDBiotic 3.0)
	StoxDataCopyMerged <- mergeDataTables(StoxDataCopy, output.only.last = TRUE)
	
	# Apply the conversion function for each row of the Conversion:
	StoxDataCopyMerged[, c(TargetVariable) := eval(parse(text  = Conversion))]
	
	
	
	# Extract the StoxData from the merged:
	StoxDataCopy <- getStoxDataFromMerged(
		StoxDataMerged = StoxDataCopyMerged, 
		StoxData = StoxDataCopy
	)
	
	return(StoxDataCopy)
}


getStoxDataFromMerged <- function(StoxDataMerged, StoxData) {
	toExtract <- lapply(StoxData, names)
	lapply(toExtract, function(x) unique(StoxDataMerged[, ..x]))
}

# Function to convert one or more variables of StoxData:
applyConversionFunctionTemp <- function(data, ConversionFunction, TargetVariable, SourceVariable, RoundOffTo) {
	
	# Get the conversion function:
	ConversionFunctionName <- paste("ConversionFunction", ConversionFunction, sep = "_")
	do.call(ConversionFunctionName, list(
		data, 
		TargetVariable = TargetVariable, 
		SourceVariable = SourceVariable, 
		RoundOffTo = RoundOffTo
	)
	)
	
	return(data)
}

applyConversionFunction <- function(data, ConversionFunction, TargetVariable, SourceVariable, RoundOffTo, parameters) {
	
	# Get the conversion function:
	ConversionFunctionName <- paste("ConversionFunction", ConversionFunction, sep = "_")
	
	# Add the current parameters:
	for(parameter in parameters) {
		#data[, eval(parameterName) := parameters[[parameterName]]]
		data[, eval(names(parameter)) := parameter]
		
		do.call(ConversionFunctionName, 
				list(
					data, 
					TargetVariable = TargetVariable, 
					SourceVariable = SourceVariable, 
					RoundOffTo = RoundOffTo
				)
		)
		
		# Remove the parameters again:
		data[, eval(names(parameter)) := NULL]
	}
	
	return(data)
}


# Function to convert one or more variables of StoxData:
applyConversionFunctionFree <- function(data, ConversionFunction, TargetVariable, SourceVariable, RoundOffTo, parameters) {
	
	# Get the conversion function:
	ConversionFunctionName <- paste("ConversionFunction", ConversionFunction, sep = "_")
	
	
	# Add the current parameters:
	#for(parameterName in names(parameters)) {
	#data[, eval(parameterName) := parameters[[parameterName]]]
	data[, eval(names(parameters)) := parameters]
	#}
	
	
	do.call(ConversionFunctionName, list(
		data, 
		TargetVariable = TargetVariable, 
		SourceVariable = SourceVariable, 
		RoundOffTo = RoundOffTo
	)
	)
	
	# Remove the parameters again:
	data[, eval(names(parameters)) := NULL]
	
	return(data)
}


###  # Function to convert one or more variables of StoxData:
###  applyConversionFunction <- function(data, ConversionFunction) {
###  	
###  	# Get the conversion function:
###  	ConversionFunctionName <- paste("ConversionFunction", ConversionFunction, sep = "_")
###  	
###  	# Get the unique target	and source variable (since the Conversion has been merged with the data):
###  	uniqueTargetAndSource <- getUniqueTargetAndSource(data)
###  	
###  	# Loop through the rows of uniqueTargetAndSource:
###  	rowIndicesOfTargetAndSource <- seq_len(nrow(uniqueTargetAndSource))
###  	for(row in rowIndicesOfTargetAndSource) {
###  		
###  		# Get the current TargetVariable:
###  		TargetVariable <- uniqueTargetAndSource$target[row]
###  		SourceVariable <- uniqueTargetAndSource$source[row]
###  		
###  		# Get the row indices of the data:
###  		#rowIndicesOfData <- 
###  		#	data[[TargetVariable]] == data[[TargetVariable]][row] & 
###  		#	data[[SourceVariable]] == data[[SourceVariable]][row]
###  		#
###  		
###  		# Apply the conversion function:
###  		#data[rowIndicesOfData, eval(TargetVariable) := do.call(ConversionFunctionName, list(.SD, SourceVariable = SourceVariable))]
###  		##data[, eval(TargetVariable) := do.call(ConversionFunctionName, list(.SD, SourceVariable = SourceVariable))]
###  		
###  		do.call(ConversionFunctionName, list(data, TargetVariable = TargetVariable, SourceVariable = SourceVariable))
###  	}
###  	
###  	return(data)
###  }

# The different available conversion functions, reflecting the methods listed as default in ConvertData (parameter ConversionFunction):
ConversionFunction_Constant <- function(data, TargetVariable, SourceVariable, RoundOffTo) {
	# Get the valid rows (those for which the parameters are defined):
	valid <- !is.na(data$Constant)
	# Apply the function:
	data[valid, eval(TargetVariable) := Constant]
	# Round off:
	roundOffValid(data = data, valid = valid, TargetVariable = TargetVariable, RoundOffTo = RoundOffTo)
	#data[valid, eval(TargetVariable) := RoundOff(get(TargetVariable), get(RoundOffTo))]
}

ConversionFunction_Addition <- function(data, TargetVariable, SourceVariable, RoundOffTo) {
	# Get the valid rows (those for which the parameters are defined):
	valid <- !is.na(data$Addition)
	# Apply the function:
	data[valid, eval(TargetVariable) := Addition + get(SourceVariable)]
	# Round off:
	roundOffValid(data = data, valid = valid, TargetVariable = TargetVariable, RoundOffTo = RoundOffTo)
}

ConversionFunction_Scaling <- function(data, TargetVariable, SourceVariable, RoundOffTo) {
	# Get the valid rows (those for which the parameters are defined):
	valid <- !is.na(data$Scaling)
	# Apply the function:
	data[valid, eval(TargetVariable) := Scaling * get(SourceVariable)]
	# Round off:
	roundOffValid(data = data, valid = valid, TargetVariable = TargetVariable, RoundOffTo = RoundOffTo)
}

ConversionFunction_AdditionAndScaling <- function(data, TargetVariable, SourceVariable, RoundOffTo) {
	# Get the valid rows (those for which the parameters are defined):
	valid <- !is.na(data$Addition) & !is.na(data$Scaling)
	# Apply the function:
	data[valid, eval(TargetVariable) := Addition + Scaling * get(SourceVariable)]
	# Round off:
	roundOffValid(data = data, valid = valid, TargetVariable = TargetVariable, RoundOffTo = RoundOffTo)
}


roundOffValid <- function(data, valid, TargetVariable, RoundOffTo) {
	# Round off either to the values of a column or to oa numeric:
	if(!RoundOffTo %in% names(data)) {
		if(length(RoundOffTo) && nchar(RoundOffTo)) {
			RoundOffToNumeric <- as.numeric(RoundOffTo)
			if(!is.na(RoundOffToNumeric)) {
				# Round off to the RoundOffToNumeric by reference:
				#RoundOffTo <- RoundOffToNumeric
				data[valid, eval(TargetVariable) := roundOff(get(TargetVariable), eval(RoundOffToNumeric))]
			}
			else {
				stop("RoundOffTo must be a character string with either the name of column or a single numeric (coercable to numeric)")
			}
		}
	}
	else {
		# Round off by reference:
		data[valid, eval(TargetVariable) := roundOff(get(TargetVariable), get(RoundOffTo))]	
	}
}

roundOff <- function(x, RoundOffTo) {
	if(length(RoundOffTo)) {
		round(x / RoundOffTo) * RoundOffTo
	}
	else {
		x
	}
}


# Helper function to get unique conversions:
getUniqueTargetAndSource <- function(data) {
	# Get the defined names of the target and source columns:
	targetAndSourceVariables <- unlist(getRstoxDataDefinitions("targetAndSourceVariables"))
	#targetAndSourceVariablesPresent <- intersect(names(data), targetAndSourceVariables)
	# Uniquify and rename:
	output <- unique(data[, ..targetAndSourceVariables])
	setnames(output, c("target", "source"))
	# Remoev rows with all NAs:
	valid <- rowSums(is.na(output)) < ncol(output)
	output <- output[valid, ]
	
	return(output)
}