
# The general function for redefining StoxData:
RedefineData <- function(
	StoxData, RawData, 
	Redefinition = data.table::data.table(), 
	StoxDataFormat = c("Biotic", "Acoustic"), 
	NumberOfCores = 1L, 
	SplitTableAllocation = c("Default", "Lowest", "Highest")
) {
	
	StoxDataFormat <- match_arg_informative(StoxDataFormat)
	
	# Add the requested variable:
	StoxData <- AddToStoxData(
		StoxData = StoxData, 
		RawData = RawData, 
		VariableNames = Redefinition$ReplaceBy, 
		NumberOfCores = NumberOfCores, 
		StoxDataFormat = StoxDataFormat, 
		SplitTableAllocation = SplitTableAllocation
	)
	
	# Remove the old:
	lapply(
		names(StoxData), 
		replaceAndDelete, 
		StoxData = StoxData, 
		VariableReplacement = Redefinition
	)
	
	# Remove rows of duplicated keys:
	StoxData <- removeRowsOfDuplicatedKeys(
		StoxData = StoxData, 
		stoxDataFormat = StoxDataFormat
	)
	
	return(StoxData)
}

# Function to replace the existing column by the new, as stored in the VariableReplacement:
replaceAndDelete <- function(tableName, StoxData, VariableReplacement) {
	for(ind in seq_len(nrow(VariableReplacement))) {
		replaceAndDeleteOne(tableName, StoxData, VariableReplacement[ind, ])
	}
}


replaceAndDeleteOne <- function(tableName, StoxData, VariableReplacementOne) {
	hasVariableToReplace <- VariableReplacementOne$VariableName  %in%  names(StoxData[[tableName]])
	hasReplaceBy <- VariableReplacementOne$ReplaceBy  %in%  names(StoxData[[tableName]])
	# If the variable to replace is in the table, and the variable to replace by is also in the table, delete the old variable and rename the new to the name of the variable to replace:
	if(hasVariableToReplace && hasReplaceBy) {
		# Delete the present column:
		StoxData[[tableName]][, (VariableReplacementOne$VariableName) := NULL]
		# ... and then rename the new to the old name:
		setnames(StoxData[[tableName]], VariableReplacementOne$ReplaceBy, VariableReplacementOne$VariableName)
	}
	# If the variable to replace is not in the table, but the variable to replace by is, this implies that the replacement is from a different table. In this case we delete the replacement with a warning:
	else if(hasReplaceBy) {
		warning("StoX: The variable ", VariableReplacementOne$ReplaceBy, " cannot replace the variable ", VariableReplacementOne$VariableName, " as it does not map to the table ", tableName, ".")
		StoxData[[tableName]][, (VariableReplacementOne$ReplaceBy) := NULL]
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
#' @inheritParams AddToStoxBiotic
#' 
#' @return
#' A \code{\link{StoxBioticData}} object.
#' 
#' @export
#' 
RedefineStoxBiotic <- function(
	StoxBioticData, BioticData, 
	Redefinition = data.table::data.table(), 
	SplitTableAllocation = c("Default", "Lowest", "Highest")
) {
	# Redefine StoxBioticData:
	RedefineData(
		StoxData = StoxBioticData, RawData = BioticData, 
		Redefinition = Redefinition, 
		StoxDataFormat = "Biotic", 
		SplitTableAllocation = SplitTableAllocation
	)
}


##################################################
#' Define translation
#' 
#' This function defines the translation table used as input to \code{\link{TranslateStoxBiotic}} and similar functions to translate values of one or more columns to new values given by a table or read from a CSV file.
#' 
#' @inheritParams general_arguments
#' @param VariableName The name of the variable to translate. This will be the name of the first column of the TranslationTable when generated from the StoX GUI.
#' @param Conditional Logical: If TRUE condition the translation on values of other variables.
#' @param ConditionalVariableNames The names of the variables to condition the translation on. Must be given if \code{Conditional == TRUE}. This will be the name(s) of the third column and onwards of the TranslationTable when generated from the StoX GUI.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "ResourceFile", for reading the table from the file given by \code{FileName}; and "Table", for defining the \code{TranslationTable} directly as an input.
#' @param FileName The csv file holding a table with the \code{TranslationTable}. Required columns are given by \code{ValueColumn} and \code{NewValueColumn}, and, in the case that Conditional == TRUE, \code{ConditionalValueColumns}.
#' @param ValueColumn,NewValueColumn The name of the columns of \code{FileName} representing the current values and the values to translate to, respectively.
#' @param ConditionalValueColumns The names of the columns of \code{FileName} representing the conditional values.
#' @param TranslationTable A table holding the values to translate FROM in the first column, the values to translate TO (column NewValue), and zero or more columns named by the conditional variables specified in \code{ConditionalVariableNames} giving the values of these variables at which to perform the translation. Use NA to translate missing values (shown as "-" in Preview in the StoX GUI, and usually as empty cell in excel). Values in the \code{TranslationTable} can be given either as single values or as expressions of functions of the variable specified by the column name. See details of \code{\link{DefineTranslation}}. 
#' 
#' @details The columns of the \code{TranslationTable} can be given in one of two ways: (1) A single value or a string to be evaluated and matched using the "\%in\%" operator, such as "HER" or "c(\"HER\", \"CLU\")"; or (2) a string expressing a function of the variable given by the column name, such as "function(IndividualTotalLength) IndividualTotalLength > 10". 
#'
#' Specifying NewValue as a function can be used to transform numeric values, e.g. "function(IndividualTotalLength) IndividualTotalLength * 1.1" to compensate for different length measurement. This can be useful for length that are not total length, in which case TranslateBiotic can be used to translate the lengthmeasurement (NMDBiotic) or LengthType (ICESBiotic) to the accepted "E" or "1", respectively. For BioticData read from ICESBiotic XML files the the LengthType specifies the type of length measurement. These values are not translated using the vocabulary from the XML file, so that total length is represented as "AC_LengthMeasurementType_1" instead of the code "1". For these data the translation can be either to "AC_LengthMeasurementType_1" or to "1". 
#'
#' Similar to transforming IndividualTotalLength, IndividualRoundWeight can also be transformed if the individualproducttype is not 1 for NMDBiotic XML files.
#'
#' When the \code{TranslationnTable} is given in the StoX GUI the strings need not be escaped ((1) HER or c("HER", "CLU"); or (2) function(IndividualTotalLength) IndividualTotalLength > 10).
#' 
#' As an example, if there are three columns in the \code{TranslationTable} named "IndividualAge", "NewValue" and "IndividualSex" (as specified by ConditionalVariableNames), with values 3, 4 and "F" in the first row, female fish at age 3 are translated to age 4.
#'
#' As another example, to set all individuals with missing IndividualMaturity as "Adult" if longer than 10 cm, use \code{function(IndividualMaturity) is.na(IndividualMaturity)} in the first column named "IndividualMaturity", \code{Adult} in the "NewValue" column, and \code{function(IndividualTotalLength) IndividualTotalLength > 10} in the third (conditional) column named "IndividualTotalLength". To translate all IndividualMaturity to a e.g. NA, use \code{function(IndividualMaturity) TRUE} in the "IndividualMaturity" column and \code{NA} in the "NewValue" column.
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
		if(!length(ConditionalValueColumns) || any(nchar(ConditionalValueColumns) == 0)) {
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



# Function to convert variables given a conversion table (by reference):
translateVariables <- function(
	data, 
	TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
	Translation, 
	TranslationTable = data.table::data.table(), 
	# This is only used by the GUI. This function assumes that the name of the first column of the TranslationTable is the name of the variable to translate:
	VariableName = character(),
	Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
	# This is only used by the GUI. This function assumes that the name of the columns past the "NewValue" in the TranslationTable are the names of the conditional variables:
	ConditionalVariableNames = character(),
	translate.keys = FALSE, 
	preserveClass = TRUE, 
	warnMissingTranslation = FALSE, 
	
	keys = NULL
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
	
	# Check whether the old required columns are present in the translation table, and transform to the new form. This can only be done after transforming to the translationList, as each row in the Translation can have different VariableName in the old definition (which is still used by StoxAcoustic()):
	oldRequiredColumns <- getRstoxDataDefinitions("TranslationOldRequiredColumns")
	if(all(oldRequiredColumns %in% names(translationList[[1]]))) {
		translationList <- lapply(translationList, oldToNewTranslationOne)
	}
	
	
	
	
	# Do the translations:
	if(!data.table::is.data.table(data[[1]])) {
		for(ind in seq_along(data)) {
			
			# Get the keys of the tables in the data:
			if(!length(keys)) {
				keys <- getKeys(data[[ind]])
			}
			# Do the translation:
			lapply(
				translationList, 
				translateOne, 
				data = data[[ind]], 
				keys = keys, 
				translate.keys = translate.keys, 
				preserveClass = preserveClass, 
				warnMissingTranslation = warnMissingTranslation
			)
			
		}
	}
	else {
		# Get the keys of the tables in the data:
		if(!length(keys)) {
			keys <- getKeys(data)
		}
		
		lapply(
			translationList, 
			translateOne, 
			data = data, 
			keys = keys, 
			translate.keys = translate.keys, 
			preserveClass = preserveClass, 
			warnMissingTranslation = warnMissingTranslation
		)
		
	}
	
	
	
	
}


# Function to translate one table:
translateOne <- function(
		translationListOne, 
		data, 
		keys, 
		translate.keys = FALSE, 
		preserveClass = TRUE, 
		warnMissingTranslation = FALSE
) {
	
	# Get the variable and the conditional variables:
	variableToTranslate <- names(translationListOne)[1]
	conditionalVariables <- names(translationListOne)[-seq_len(2)]
	tablesHoldingTheVariableToTranslate <- names(data)[sapply(data, function(table) variableToTranslate %in% names(table))]
	
	
	conditional <- length(conditionalVariables) > 0
	
	# If we are conditioning on variable possibly in parent tables match both the conditions and the variableToTranslate in those parent tables:
	if(conditional) {
		
		#browser()
		# Find the keys of rows in the table and parent tables where the conditional variables match:
		tableNames <- includeParents(tablesHoldingTheVariableToTranslate, data)
		
		# Match the conditions and the variable in the different tables:
		matches <- mapply(
			getMatchesOneTranslationOneTable, 
			data[tableNames], 
			keys = keys[tableNames], 
			MoreArgs = list(
				variableToTranslate = variableToTranslate, 
				conditionalVariables = conditionalVariables, 
				translationListOne = translationListOne
			), 
			SIMPLIFY = FALSE
		)
		
		# Detect tables where there are variables to be matched, but no matches were found (in which case getMatchesOneTranslationOneTable() returns NA):
		nomatches <- sapply(matches, function(x) length(x) == 1 && is.na(x))
		
		# Translate only if there were ane matches:
		if(!any(nomatches)) {
			# Remove empty matches:
			matches <- matches[sapply(matches, NROW) > 0]
			
			# Loop through the tables containing the variableToTranslate and perform the translation:
			tablesContainingVariableToTranslate <- sapply(names(matches), function(x) variableToTranslate %in% names(data[[x]]))
			
			# If there are any matches:
			if(length(matches)) {
				for(tableName in names(matches)[tablesContainingVariableToTranslate]) {
					
					# Get translation matches:
					thisMatches <- Reduce(function(x, y) mergeByIntersect(x, y, all = FALSE), matches[seq_len(which(tableName == names(matches)))])
					
					if(length(thisMatches)) {
						
						# Kepp pnly the columns present in the table to translate, so that we can use thisMatches to dientify the rows in which to translate:
						thisMatches <- subset(thisMatches, select = intersect(names(thisMatches), names(data[[tableName]])))
						
						# Translate
						if(isFunctionString(translationListOne$NewValue, variableToTranslate)) {
							#data[[tableName]] [thisMatches, eval(variableToTranslate) := eval(parse(text = translationListOne$NewValue)) (get(variableToTranslate)), on = names(thisMatches)]
							
							# Get the replacement by applying the function defined by translationListOne$NewValue:
							replacement <- data[[tableName]] [thisMatches, eval(parse(text = translationListOne$NewValue)) (get(variableToTranslate)), on = names(thisMatches)]
						}
						else {
							#data[[tableName]] [thisMatches, eval(variableToTranslate) := translationListOne$NewValue, on = names(thisMatches)]
							
							# Get the replacement by applying the function defined by translationListOne$NewValue:
							replacement <- data[[tableName]] [thisMatches, translationListOne$NewValue, on = names(thisMatches)]
						}
						
						# Do the replacement, preserving class if requested:
						if(!preserveClass) {
							setColumnClasses(data[[tableName]], structure(list(class(replacement)), names = variableToTranslate))
						}
						
						data[[tableName]] [thisMatches, eval(variableToTranslate) := replacement, on = names(thisMatches)]
						
					}
				}
			}
		}
	}
	# If not conditional, simply translate in each of the tables holding the variableToTranslate, with no use of keys:
	else {
		
		for(tableName in tablesHoldingTheVariableToTranslate) {
			
			thisMatches <- matchVariable(variableToTranslate, translationListOne, data[[tableName]])
			
			if(any(thisMatches)) {
				# Translate
				if(isFunctionString(translationListOne$NewValue, variableToTranslate)) {
					#data[[tableName]] [thisMatches, eval(variableToTranslate) := eval(parse(text = translationListOne$NewValue)) (get(variableToTranslate)), on = names(thisMatches)]
					
					# Get the replacement by applying the function defined by translationListOne$NewValue:
					replacement <- data[[tableName]] [thisMatches, eval(parse(text = translationListOne$NewValue)) (get(variableToTranslate)), on = names(thisMatches)]
				}
				else {
					#data[[tableName]] [thisMatches, eval(variableToTranslate) := translationListOne$NewValue, on = names(thisMatches)]
					
					# Get the replacement by applying the function defined by translationListOne$NewValue:
					replacement <- data[[tableName]] [thisMatches, translationListOne$NewValue, on = names(thisMatches)]
				}
				
				# Do the replacement, preserving class if requested:
				if(!preserveClass) {
					setColumnClasses(data[[tableName]], structure(list(class(replacement)), names = variableToTranslate))
				}
				
				data[[tableName]] [thisMatches, eval(variableToTranslate) := replacement, on = names(thisMatches)]
				
			}
		}
	}

}





# Function to apply to all tables of the input data, rows matching the criteria of the target variable and the conditional variables:
getMatchesOneTranslationOneTable <- function(table, variableToTranslate, conditionalVariables, translationListOne, keys) {
	
	varsToMatch <- c(variableToTranslate, conditionalVariables)
	
	varsToMatch <- intersect(varsToMatch, names(table))
	
	# Find the rows matching the criteria of the target variable and the conditional variables:
	if( length(varsToMatch) ) {
		
		# Get the logical vector of matches for each variable to match (the variable to translate and the conditional variables):
		matches <- lapply(varsToMatch, matchVariable, list = translationListOne, table = table)
		
		# Any empty matches are returned as FALSE to indicate no match:
		emptyMatches <- lengths(matches) == 0
		if(any(emptyMatches)) {
			matches[emptyMatches] <- rep(list(FALSE), sum(emptyMatches))
		}
		
		# Require that all variables match to each row:
		matches <- apply(do.call(cbind, matches), 1, all)
		
		# If no matches, return NA:
		if(!any(matches)) {
			return(NA)
		}
		
		# Get the key values of the matches:
		keyTable <- subset(table, matches, select = keys)
		
		
		return(keyTable)
	}
	else {
		return(NULL)
	}
	
}
























includeParents <- function(tableName, data) {
	unique(unlist(lapply(tableName, includeParentsOneTableName, data)))
}

includeParentsOneTableName <- function(tableName, data) {
	
	# Get the tree structure (list of children):
	treeStruct <- getTreestruct(data)
	tableNames <- names(treeStruct)
	
	# Get parents:
	children  <- unlist(treeStruct, use.names = FALSE)
	parents <- rep(names(treeStruct), lengths(treeStruct))
	childrenParentTable <- data.table::data.table(child = children, parent = parents)
	parentList <- split(childrenParentTable$parent, childrenParentTable$child)
	
	# Set the first child:
	parentsRecursively <- tableName
	child <- parentsRecursively
	
	
	while(TRUE) {
		
		# Get the parent:
		parent <- parentList[[child]]
		
		# Check if the parent exists
		if(length(parent) && parent %in% tableNames) {
			parentsRecursively <- append(parentsRecursively, parent)
			child <- parent
		}
		else {
			break
		}
		
	}
	
	# Reverse the order, since we want the grandparents first for consistency with the function mergeDataTables which assumes ordered tables from the highest to the lowest:
	parentsRecursively <- rev(parentsRecursively)
	
	return(parentsRecursively)
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








# Function to match a condition given in the element 'variableName' of 'list' to the corresponding element in 'table', either as a function expression string that can be evaluated, or a replacement string:
matchVariable <- function(variableName, list, table) {
	# Special case for NA:
	if(is.na(list[[variableName]]) || identical(list[[variableName]], "NA")) {
		is.na(table[[variableName]])
	}
	# Check whether the translation key is a function string to be evaluated:
	else if(isFunctionString(list[[variableName]], variableName)) {
		# If the variable is present in the table, apply the function:
		if(variableName %in% names(table)) {
			eval(parse(text = list[[variableName]]))(table[[variableName]])
		}
		# If not present return FALSE to indicate no matches:
		else {
			stop("The variable given by 'variableName' is not present in the table.")
		}
		
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
	if(!is.na(x) && is.character(x)) {
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
		preserveClass = PreserveClass
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
		preserveClass = PreserveClass
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
		preserveClass = PreserveClass
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
		preserveClass = PreserveClass
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
		preserveClass = PreserveClass
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
		preserveClass = PreserveClass
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
	
	# Get keys:
	keys <- expandICESKeysWithPrefix(xsdObjects$icesBiotic.xsd$keys)
	
	translateVariables(
		data = ICESBioticDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		preserveClass = PreserveClass, 
		keys = keys
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
	
	# Get keys:
	keys <- expandICESKeysWithPrefix(xsdObjects$icesAcoustic.xsd$keys)
	
	translateVariables(
		data = ICESAcousticDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		preserveClass = PreserveClass, 
		keys = keys
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
	
	# Get keys:
	keys <- getRstoxDataDefinitions("keys")$ICESDatras
	
	translateVariables(
		data = ICESDatrasDataCopy, 
		TranslationDefinition = TranslationDefinition, 
		TranslationTable = TranslationTable, 
		VariableName = VariableName,
		Conditional = Conditional,
		ConditionalVariableNames = ConditionalVariableNames,
		Translation = Translation,  
		preserveClass = PreserveClass, 
		keys = keys
	)
	
	return(ICESDatrasDataCopy)
}

##################################################
#' Translate ICESDatsuscData
#' 
#' This function translates one or more columns of \code{\link{ICESDatsuscData}} to new values given by the input \code{Translation}.
#' 
#' @inheritParams DefineTranslation
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams TranslateStoxBiotic
#' 
#' @return
#' A \code{\link{ICESDatsuscData}} object.
#' 
#' @export
#' 
TranslateICESDatsusc <- function(
    ICESDatsuscData, 
    TranslationDefinition = c("FunctionParameter", "FunctionInput"), 
    VariableName = character(),
    Conditional = FALSE, # If TRUE, adds a column to the parameter format translationTable.
    ConditionalVariableNames = character(),
    TranslationTable = data.table::data.table(), 
    Translation,  
    PreserveClass = TRUE
) {
  # Make a copy, as we are translating by reference:
  ICESDatsuscDataCopy <- data.table::copy(ICESDatsuscData)
  
  # Get keys:
  keys <- getRstoxDataDefinitions("keys")$ICESDatsusc
  
  translateVariables(
    data = ICESDatsuscDataCopy, 
    TranslationDefinition = TranslationDefinition, 
    TranslationTable = TranslationTable, 
    VariableName = VariableName,
    Conditional = Conditional,
    ConditionalVariableNames = ConditionalVariableNames,
    Translation = Translation,  
    preserveClass = PreserveClass, 
    keys = keys
  )
  
  return(ICESDatsuscDataCopy)
}
















# Will these be used?
getStoxDataFromMerged <- function(StoxDataMerged, StoxData) {
	toExtract <- lapply(StoxData, names)
	lapply(toExtract, function(x) unique(StoxDataMerged[, ..x]))
}
roundOff <- function(x, RoundOffTo) {
	if(length(RoundOffTo)) {
		round(x / RoundOffTo) * RoundOffTo
	}
	else {
		x
	}
}
