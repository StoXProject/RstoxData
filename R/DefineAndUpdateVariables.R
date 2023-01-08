
# The general function for redefining StoxData:
RedefineData <- function(
	StoxData, RawData, 
	Redefinition = data.table::data.table(), 
	StoxDataFormat = c("Biotic", "Acoustic"), 
	NumberOfCores = 1L
) {
	
	StoxDataFormat <- match_arg_informative(StoxDataFormat)
	
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
#' @param VariableName The name of the variable to translate.
#' @param Conditional Logical: If TRUE condition the translation on values of other variables.
#' @param ConditionalVariableNames The names of the variables to condition the translation on. Must be given if \code{Conditional == TRUE}.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "ResourceFile", for reading the table from the file given by \code{FileName}; and "Table", for defining the \code{TranslationTable} directly as an input.
#' @param FileName The csv file holding a table with the \code{TranslationTable}. Required columns are given by \code{ValueColumn} and \code{NewValueColumn}, and, in the case that Conditional == TRUE, \code{ConditionalValueColumns}.
#' @param ValueColumn,NewValueColumn The name of the columns of \code{FileName} representing the current values and the values to translate to, respectively.
#' @param ConditionalValueColumns The names of the columns of \code{FileName} representing the conditional values.
#' @param TranslationTable A table holding the following columns: The first column holds the values to translate FROM, the second column holds the values to translate TO, and the remaining zero or more columns holds the values for the conditional variables specified in \code{ConditionalVariableNames}. I.e., if \code{VariableName} = "IndividualAge" and \code{ConditionalVariableNames} = "IndividualSex", and the \code{TranslationTable} has values 3, 4 and "F" in the first row, female fish at age 3 are translated to age 4. Use NA to translate missing values (shown as "-" in Preview in the StoX GUI, and usually as empty cell in excel). Values in the \code{TranslationTable} can be given either as single values or as expressions of functions of the variable specified by the column name. See details of \code{\link{DefineTranslation}}. 
#' 
#' @details The columns of the \code{TranslationTable} (except the NewValue column) can be given in one of two ways: (1) A single value or a string to be evaluated and matched using the "\%in\%" operator, such as "HER" or "c(\"HER\", \"CLU\")"; or (2) a string expressing a function of the variable given by the column name, such as "function(IndividualTotalLength) IndividualTotalLength > 10". When the \code{TranslationnTable} is given in the StoX GUI the strings need not be escaped ((1) HER or c("HER", "CLU"); or (2) function(IndividualTotalLength) IndividualTotalLength > 10). 
#' 
#' E.g., to set all individuals with missing IndividualMaturity as "Adult" if longer than 10 cm, use \code{function(IndividualMaturity) is.na(IndividualMaturity)} in the first column named "IndividualMaturity", \code{Adult} in the "NewValue" column, and \code{function(IndividualTotalLength) IndividualTotalLength > 10} in the third (conditional) column named "IndividualTotalLength". To translate all IndividualMaturity to a e.g. NA, use \code{function(IndividualMaturity) TRUE} in the "IndividualMaturity" column and \code{NA} in the "NewValue" column.
#' 
#' @return
#' A \code{\link{Translation}} object.
#' 
#' @seealso \code{\link{TranslateStoxBiotic}}, \code{\link{TranslateStoxAcoustic}}, \code{\link{TranslateBiotic}}, \code{\link{TranslateAcoustic}}, \code{\link{TranslateStoxLanding}}, \code{\link{TranslateLanding}}, \code{\link{TranslateICESBiotic}} and \code{\link{TranslateICESAcoustic}} for applying the translation.
#'
#' @export
#' 
DefineTranslation <- function(
	processData, UseProcessData = FALSE, 
	DefinitionMethod = c("ResourceFile", "Table"), 
	FileName = character(), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	ValueColumn = character(), 
	NewValueColumn = character(), 
	ConditionalValueColumns = character()
) {
	
	# Return immediately if UseProcessData = TRUE:
	if(UseProcessData) {
		return(processData)
	}
	
	DefinitionMethod <- match_arg_informative(DefinitionMethod)
	
	if(DefinitionMethod == "ResourceFile") {
		# Get the conversion table:
		Translation <- readVariableTranslation(
			FileName = FileName, 
			VariableName = VariableName, 
			ValueColumn = ValueColumn, 
			NewValueColumn = NewValueColumn, 
			Conditional = Conditional, 
			ConditionalVariableNames = ConditionalVariableNames, 
			ConditionalValueColumns = ConditionalValueColumns, 
			UseProcessData = UseProcessData
		)
	}
	else if(DefinitionMethod == "Table"){
		Translation <- TranslationTable
	}
	else {
		stop("Invalid DefinitionMethod")
	}
	
	# We need VariableName:
	if(!length(VariableName)) {
		warning("VariableName must be given.")
	}
	relevantColumns <- c(
		VariableName, 
		"NewValue"
	)
	# Append the conditional variables:
	if(Conditional) {
		if(length(ConditionalVariableNames)) {
			relevantColumns <- append(relevantColumns, ConditionalVariableNames)
		}
		else {
			warning("ConditionalVariableNames should be given if Conditional == TRUE.")
		}
	}
	
	Translation <- Translation[, ..relevantColumns]
	
	# Sanitize the table:
	sanitizeExpression(Translation)
	
	return(Translation)
}


# Function for reading a conversion table:
readVariableTranslation <- function(FileName, VariableName, ValueColumn, NewValueColumn, Conditional, ConditionalVariableNames, ConditionalValueColumns, UseProcessData = FALSE) {
	
	# Read the file:
	tanslation <- data.table::fread(FileName, encoding = "UTF-8", colClasses = "character")
	
	if(!length(ValueColumn) || !nchar(ValueColumn)) {
		stop("ValueColumn must be given as the name of the column giving the values to be translated.")
	}
	else {
		if(! ValueColumn %in% names(tanslation)) {
			stop("The ValueColumn (", ValueColumn, ") must be in the file ", FileName, ". Available column names are ", paste(names(tanslation), collapse = ", "), " .")
		}
	}
	if(!length(NewValueColumn) || !nchar(NewValueColumn)) {
		stop("NewValueColumn must be given as the name of the column giving the values to be translated to.")
	}
	else {
		if(! NewValueColumn %in% names(tanslation)) {
			stop("The NewValueColumn (", NewValueColumn, ") must be in the file ", FileName, ". Available column names are ", paste(names(tanslation), collapse = ", "), " .")
		}
	}
	
	# Get the Value and NewValue:
	tanslation[, eval(VariableName) := get(ValueColumn)]
	tanslation[, NewValue := get(NewValueColumn)]
	
	if(Conditional) {
		if(!length(ConditionalValueColumns) || !nchar(ConditionalValueColumns)) {
			stop("ConditionalValueColumns must be given as the name of the column giving the values to be translated to.")
		}
		else {
			if(! all(ConditionalValueColumns %in% names(tanslation))) {
				stop("All of the ConditionalValueColumns (", paste(NewValueColumn, collapse = ", "), ") must be in the file ", FileName, ".")
			}
		}
		
		for(ConditionalValueColumn in ConditionalValueColumns) {
			tanslation[, eval(ConditionalValueColumn) := get(ConditionalValueColumn)]
		}
	}
	
	return(tanslation)
}



oldToNewTranslationOne <- function(x) {
	# Detect whether the translation is conditional:
	conditional <- "ConditionalVariableName" %in% names(x)
	if(conditional) {
		# We only treat one row at the time, so we do not need to extract the first element of x$VariableName:
		out <- data.table::data.table(
			# Use the Value as the first column and ConditionalValue as the last, and add the VariableName and ConditionalVariableName afterwards: 
			x$Value, 
			NewValue = x$NewValue, 
			x$ConditionalValue
		)
		names(out) <- c(x$VariableName, "NewValue", x$ConditionalVariableName)
	}
	else {
		# We only treat one row at the time, so we do not need to extract the first element of x$VariableName:
		out <- data.table::data.table(
			# Use the Value as the first column, and add the VariableName afterwards: 
			x$Value, 
			NewValue = x$NewValue
		)
		names(out) <- c(x$VariableName, "NewValue")
	}
	
	return(out)
}



# Function to convert variables given a conversion table:
translateVariables <- function(
	data, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	Translation, 
	TranslationTable = data.table::data.table(), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	translate.keys = FALSE, 
	PreserveClass = TRUE, 
	warnMissingTranslation = FALSE
) {
	
	# Get the Translation:
	TranslationDefinition <- match_arg_informative(TranslationDefinition)
	if(TranslationDefinition == "FunctionParameter") {
		Translation <- TranslationTable
		# Sanitize the table:
		sanitizeExpression(Translation)
	}
	
	if(!nrow(Translation)) {
		return(NULL)
	}
	# Split the translation table into a list, thus treating only one row at the time. This is probably sloppy coding, but it works:
	translationList <- split(Translation, seq_len(nrow(Translation)))
	
	# Check whether the old required columns are present in the translation table, and transform to the new form:
	oldRequiredColumns <- getRstoxDataDefinitions("TranslationOldRequiredColumns")
	if(all(oldRequiredColumns %in% names(translationList[[1]]))) {
		translationList <- lapply(translationList, oldToNewTranslationOne)
	}
	
	# Warn if there are columns in the translation that are not present in the data. Check only the first translation, as all translations have the same columns in the new form, and in the old form (that is used by StoxBiotic()) this check is not as important:
	someButNotAllPresentOneTable <- function(table, translationList) {
		translationNames <- setdiff(names(translationList[[1]]), "NewValue")
		any(translationNames %in% names(table)) && !all(translationNames %in% names(table))
	}
	someButNotAllPresent <- unlist(lapply(
		data, 
		someButNotAllPresentOneTable, 
		translationList =  translationList
	))
	
	#if(any(someButNotAllPresent)) {
	#	warning("StoX: The following tables contain some but not all of the variables of the Translation: ", paste0(names(data)[someButNotAllPresent], collapse = ", "))
	#}
		
	# Run the conversion for each table of the data:
	lapplyToStoxData(
		data, 
		translateOneTable, 
		translationList = translationList, 
		translate.keys = translate.keys, 
		PreserveClass = PreserveClass, 
		warnMissingTranslation = warnMissingTranslation
	)
}

# Function to translate one table:
translateOneTable <- function(table, translationList, translate.keys = FALSE, PreserveClass = TRUE, warnMissingTranslation = FALSE) {
	
	# Apply the translation, one line at the time:
	lapply(
		translationList, 
		translateOneTranslationOneTable, 
		table = table, 
		translate.keys = translate.keys, 
		PreserveClass = PreserveClass, 
		warnMissingTranslation = warnMissingTranslation
	)
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
translateOneTranslationOneTable <- function(translationListOne, table, translate.keys = FALSE, PreserveClass = TRUE, warnMissingTranslation = FALSE) {
	
	# Check that the table contains the variable to translate:
	if(names(translationListOne)[1] %in% names(table)) {
		
		## Check for values of the data that are not covered by the translation:
		#notPresentInTranslation <- sort(
		#	setdiff(
		#		table[[Translation$VariableName[1]]], 
		#		Translation$Value
		#	)
		#)
		
		if(warnMissingTranslation) {
			# Get the combinations of ConditionalVariableNames and VariableName missing in the Translation:
			variableKeys <- setdiff(names(translationListOne), "NewValue")
			missingCombinations <- getMissingCombinations(translationListOne, table = table, variableKeys = variableKeys)
			
			
			if(nrow(missingCombinations)) {
				warnMessage <- c(
					"StoX: The following values are present in the data but not in the translation, and were not translated: ", 
					getMissingCombinationsWarning(missingCombinations)
				)
				warnMessage <- paste(warnMessage, collapse = "\n")
				warning(warnMessage)
			}
		}
	}
	variableToTranslate <- names(translationListOne)[1]
	
	
	# Do nothing if the variable is a key:
	isKeys <- endsWith(variableToTranslate, "Key")
	if(!translate.keys && isKeys) {
		warning("StoX: The variable ", variableToTranslate, " is a key and cannot be modified ")
	}
	else {
		# Convert the class to the class of the existing value in the table:
		#browser()
		
		
		if(PreserveClass) {
			
			# Convert the class of the translationList to that of the variable to transate:
			translationListOneConverted <- convertTranslationListClass(
				translationList = translationListOne, 
				table = table
			)
			
			### translationListOneConverted <- convertClassToExistingOneElement(
			### 	list = translationListOne, 
			### 	x = table, 
			### 	element = "NewValue"
			### )
			
			# Check whether the NewValue has changed, and if it has changegd to NA, report a warning:
			if(!identical(translationListOneConverted, translationListOne)) {
				
				atNAAndDiffering <- is.na(translationListOne) & !is.na(translationListOneConverted)
				
				if(any(atNAAndDiffering)) {
					warning("StoX: NAs introduced when converting class of the columns ", paste(names(translationListOne)[atNAAndDiffering], collapse = ", "), ", to the class of the corresponding column of the data. To prevent converting class of the translation to the class of the data, use PreserveClass = FALSE.")
				}
				translationListOne <- translationListOneConverted
			}
		}
		
		# Otherwise change the class of the existing column. This is new in RstoxData 1.8.0-9003, as it was discovered that changing the class of the existing column did not work:
		else {
			setColumnClasses(table, structure(list(class(translationListOne$NewValue)), names = variableToTranslate))
		}
		
		# Replace by the new value:
		if(variableToTranslate %in% names(table)) {
			varsToMatch <- setdiff(names(translationListOne), "NewValue")
			mathces <- lapply(varsToMatch, mathcVariable, list = translationListOne, table = table)
			mathces <- apply(do.call(cbind, mathces), 1, all)
			
			# Replace:
			replacement <- translationListOne$NewValue
			table[mathces, eval(variableToTranslate) := ..replacement]
		}
	}
}

# Function to match a condition given in the element 'variableName' of 'list' to the corresponding element in 'table', either as an evaluable function expression string, or an evaluable string:
mathcVariable <- function(variableName, list, table) {
	# Special case for NA:
	if(is.na(list[[variableName]]) || identical(list[[variableName]], "NA")) {
		is.na(table[[variableName]])
	}
	# Check whether the translation key is a function string to be evaluated:
	else if(isFunctionString(list[[variableName]], variableName)) {
		# Evaluate the function:
		eval(parse(text = list[[variableName]]))(table[[variableName]])
	}
	# Otherwise check whether the table column is in the evaluated string:
	else {
		vector <- eval(parse(text = deparse(list[[variableName]])))
		if(!is.list(vector)  &&  is.vector(vector)  &&  length(dim(vector)) < 2) {
			table[[variableName]] %in% vector
		}
		else {
			stop("Expressions given in the TranslationTable can either be functions given as strings, og expressions which when evaluated result in a one dimensional non-list vectors.")
		}
	}
}

# Test if a value is a function string:
isFunctionString <- function(x, variableName) {
	if(is.character(x)) {
		functionPrefix <- paste0("function(", variableName, ")")
		xWithoutWhitespace <- gsub("[[:space:]]", "", x)
		startsWith(xWithoutWhitespace, functionPrefix)
	}
	else {
		FALSE
	}
}

convertTranslationListClass <- function(translationList, table) {
	variableToTanslate <- names(translationList)[1]
	if(NROW(table) && variableToTanslate %in% names(table)) {
		# Convert class of NewValue to that of the corresponding column in the data:
		existingClass <- firstClass(table[[variableToTanslate]])
		newClass <- firstClass(translationList$NewValue)
		
		if(!identical(existingClass, newClass)) {
			if(all(is.na(translationList$NewValue))) {
				translationList$NewValue <- getNAByType(existingClass)
			}
			else {
				class(translationList$NewValue) <- existingClass
			}
		}
	}
	
	return(translationList)
}

### # Function to convert the class of list[[element]] to the class of :
### convertClassToExistingOneElement <- function(list, x, element) {
### 	
### 	# Convert the NewValue to the class of the existing value:
### 	if(length(x[[names(list)[1]]])) {
### 		existingClass <- firstClass(x[[names(list)[1]]])
### 		newClass <- class(list[[element]])[1]
### 		
### 		if(!identical(existingClass, newClass)) {
### 			if(all(is.na(list[[element]]))) {
### 				list[[element]] <- getNAByType(existingClass)
### 			}
### 			else {
### 				class(list[[element]]) <- existingClass
### 			}
### 			#class(list[[element]]) <- existingClass
### 		}
### 	}
### 	
### 	return(list)
### }

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









##################################################
#' Translate StoxBioticData
#' 
#' This function translates one or more columns of \code{\link{StoxBioticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param TranslationDefinition The method to use for defining the Translation, one of \code{FunctionParameter} to define the Translation on the fly in this function using the \code{TranslationTable}, or \code{FunctionInput} to import \code{\link{Translation}} process data from a previously run process using the function \code{\link{DefineTranslation}}. When reading the translation from a file \code{FunctionInput} must be used. 
#' @param PreserveClass Logical: If TRUE (the default) do not convert the class of the data by the class of the translation table. E.g., with the default (\code{PreserveClass} = TRUE), if the variable to translate is integer (1, 2, etc) and the NewValue is character ("First", "Second", etc), the NewValue will be converted to integer before translation, resulting in NA if the character strings are not convertible to integer. In this example it could be the user's intention to convert the class of the variable to translate instead, which is possible using \code{PreserveClass} = FALSE.
#' 
#' @return
#' A \code{\link{StoxBioticData}} object.
#' 
#' @export
#' 
TranslateStoxBiotic <- function(
	StoxBioticData, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	Translation,  
	PreserveClass = TRUE
) {
	
	# Make a copy, as we are translating by reference:
	StoxBioticDataCopy <- data.table::copy(StoxBioticData)
	
	# Translate StoxBioticData:
	translateVariables(
		data = StoxBioticDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		PreserveClass = PreserveClass
	)
	
	return(StoxBioticDataCopy)
}


##################################################
#' Translate StoxAcousticData
#' 
#' This function translates one or more columns of \code{\link{StoxAcousticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams TranslateStoxBiotic
#' 
#' @return
#' A \code{\link{StoxAcousticData}} object.
#' 
#' @export
#' 
TranslateStoxAcoustic <- function(
	StoxAcousticData, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	Translation,  
	PreserveClass = TRUE
) {
	# Make a copy, as we are translating by reference:
	StoxAcousticDataCopy <- data.table::copy(StoxAcousticData)
	
	translateVariables(
		data = StoxAcousticDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		PreserveClass = PreserveClass
	)
	
	return(StoxAcousticDataCopy)
}


##################################################
#' Translate BioticData
#' 
#' This function translates one or more columns of \code{\link{BioticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams TranslateStoxBiotic
#' 
#' @return
#' A \code{\link{BioticData}} object.
#' 
#' @export
#' 
TranslateBiotic <- function(
	BioticData, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	Translation,  
	PreserveClass = TRUE
) {
	# Make a copy, as we are translating by reference:
	BioticDataCopy <- data.table::copy(BioticData)
	
	translateVariables(
		data = BioticDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		PreserveClass = PreserveClass
	)
	
	return(BioticDataCopy)
}


##################################################
#' Translate AcousticData
#' 
#' This function translates one or more columns of \code{\link{AcousticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams TranslateStoxBiotic
#' 
#' @return
#' A \code{\link{AcousticData}} object.
#' 
#' @export
#' 
TranslateAcoustic <- function(
	AcousticData, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	Translation,  
	PreserveClass = TRUE
) {
	# Make a copy, as we are translating by reference:
	AcousticDataCopy <- data.table::copy(AcousticData)
	
	translateVariables(
		data = AcousticDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		PreserveClass = PreserveClass
	)
	
	return(AcousticDataCopy)
}


##################################################
#' Translate StoxLandingData
#' 
#' This function translates one or more columns of \code{\link{StoxLandingData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams TranslateStoxBiotic
#' 
#' @return
#' A \code{\link{StoxLandingData}} object.
#' 
#' @export
#' 
TranslateStoxLanding <- function(
	StoxLandingData, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	Translation,  
	PreserveClass = TRUE
) {
	# Make a copy, as we are translating by reference:
	StoxLandingDataCopy <- data.table::copy(StoxLandingData)
	
	translateVariables(
		data = StoxLandingDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		PreserveClass = PreserveClass
	)
	
	return(StoxLandingDataCopy)
}


##################################################
#' Translate LandingData
#' 
#' This function translates one or more columns of \code{\link{LandingData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams TranslateStoxBiotic
#' 
#' @return
#' A \code{\link{LandingData}} object.
#' 
#' @export
#' 
TranslateLanding <- function(
	LandingData, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	Translation,  
	PreserveClass = TRUE
) {
	# Make a copy, as we are translating by reference:
	LandingDataCopy <- data.table::copy(LandingData)
	
	translateVariables(
		data = LandingDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		PreserveClass = PreserveClass
	)
	
	return(LandingDataCopy)
}






##################################################
#' Translate ICESBioticData
#' 
#' This function translates one or more columns of \code{\link{ICESBioticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams TranslateStoxBiotic
#' 
#' @return
#' A \code{\link{ICESBioticData}} object.
#' 
#' @export
#' 
TranslateICESBiotic <- function(
	ICESBioticData, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	Translation,  
	PreserveClass = TRUE
) {
	# Make a copy, as we are translating by reference:
	ICESBioticDataCopy <- data.table::copy(ICESBioticData)
	
	translateVariables(
		data = ICESBioticDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		PreserveClass = PreserveClass
	)
	
	return(ICESBioticDataCopy)
}


##################################################
#' Translate ICESAcousticData
#' 
#' This function translates one or more columns of \code{\link{ICESAcousticData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams TranslateStoxBiotic
#' 
#' @return
#' A \code{\link{ICESAcousticData}} object.
#' 
#' @export
#' 
TranslateICESAcoustic <- function(
	ICESAcousticData, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	Translation,  
	PreserveClass = TRUE
) {
	# Make a copy, as we are translating by reference:
	ICESAcousticDataCopy <- data.table::copy(ICESAcousticData)
	
	translateVariables(
		data = ICESAcousticDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		PreserveClass = PreserveClass
	)
	
	return(ICESAcousticDataCopy)
}


##################################################
#' Translate ICESDatrasData
#' 
#' This function translates one or more columns of \code{\link{ICESDatrasData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams TranslateStoxBiotic
#' 
#' @return
#' A \code{\link{ICESDatrasData}} object.
#' 
#' @export
#' 
TranslateICESDatras <- function(
	ICESDatrasData, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	ConditionalVariableNames = character(),
	TranslationTable = data.table::data.table(), 
	Translation,  
	PreserveClass = TRUE
) {
	# Make a copy, as we are translating by reference:
	ICESDatrasDataCopy <- data.table::copy(ICESDatrasData)
	
	translateVariables(
		data = ICESDatrasDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		PreserveClass = PreserveClass
	)
	
	return(ICESDatrasDataCopy)
}





# The general function for converting StoxData:
ConvertDataOld <- function(
	StoxData, 
	ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
	GruopingVariables = character(), 
	Conversion = data.table::data.table()
) {
	
	# Get the ConversionFunction input:
	ConversionFunction <- match_arg_informative(ConversionFunction)
	
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
	ConversionFunction <- match_arg_informative(ConversionFunction)
	
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
		warning("StoX: Unspecified TargetVariable or empty conversion expression. Data returned unchanged.")
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





