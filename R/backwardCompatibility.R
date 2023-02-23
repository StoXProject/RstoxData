#' Function to convert from the VarialbeName-Value-NewValue translation form to the foor where variable names are column names.
#' 
#' @param translationList A list of lists of each row of the Translation table, as read in using RstoxFramework:::readProjectDescription(). 
#' 
#' @export
oldToNewTranslationList <- function(translationList) {
	
	# Reshape only old Translation:
	if(length(translationList) && all(c("VariableName", "Value", "NewValue") %in% names(translationList[[1]]))) {
		
		values <- unique(unlist(lapply(translationList, "[[", "VariableName")))
		
		if(length(values) > 1 ) {
			warning("StoX: The existing Translation process data containis multiple unique values in the Value column. This cannot be converted to the new form of the Translation process data, where only one variable can be defined for translation (but multiple conditional variables), and where the variable names in the data are used as column names in the Translation process data. StoX should still be able to apply the translation, but making changes may break the process or loose the translation infomation.")
		}
		else {
			conditional <- "ConditionalVariableName" %in% names(translationList[[1]])
			if(conditional) {
				translationList <- lapply(
					translationList, 
					function(x) {
						# We only treat one row at the time, so we do not need to extract the first element of x$VariableName:
						out <- list(
							# Use the Value as the first column and ConditionalValue as the last, and add the VariableName and ConditionalVariableName afterwards: 
							x$Value, 
							NewValue = x$NewValue, 
							x$ConditionalValue
						)
						names(out) <- c(x$VariableName, "NewValue", x$ConditionalVariableName)
						
						return(out)
					}
				)
			}
			else {
				translationList <- lapply(
					translationList, 
					function(x) {
						# We only treat one row at the time, so we do not need to extract the first element of x$VariableName:
						out <- list(
							# Use the Value as the first column, and add the VariableName afterwards: 
							x$Value, 
							NewValue = x$NewValue
						)
						names(out) <- c(x$VariableName, "NewValue")
						
						return(out)
					}
				)
			}
		}
	}
	
	return(translationList)	
}

#' Backward compabitibility actions:
#' @export
backwardCompatibility <- list(
	
	addParameter = list(
		list(
			changeVersion = "1.1.5", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "Conditional", 
			parameterValue = FALSE
		), 
		list(
			changeVersion = "1.2.11", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ValueColumn", 
			parameterValue = "Value"
		), 
		list(
			changeVersion = "1.2.11", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "NewValueColumn", 
			parameterValue = "NewValue"
		), 
		list(
			changeVersion = "1.2.17", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "VariableName", 
			parameterValue = "VariableName"
		), 
		list(
			changeVersion = "1.2.17", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalValueColumn", 
			parameterValue = "ConditionalValue"
		), 
		list(
			changeVersion = "1.2.17", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalVariableName", 
			parameterValue = "ConditionalVariableName"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateBiotic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateAcoustic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateLanding", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateStoxBiotic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateStoxAcoustic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateStoxLanding", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateICESAcoustic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateICESBiotic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateICESDatras", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.17", 
			functionName = "AddToStoxBiotic", 
			modelName = "baseline", 
			parameterName = "AddToLowestTable", 
			parameterValue = FALSE
		), 
		list(
			changeVersion = "1.6.1", 
			functionName = "AddToStoxBiotic", 
			modelName = "baseline", 
			parameterName = "SplitTableAllocation", 
			parameterValue = "Default"
		), 
		list(
			changeVersion = "1.9.0-9001", 
			functionName = "RegroupLengthICESDatras", 
			modelName = "baseline", 
			parameterName = "RegroupMethod", 
			parameterValue = "HighestResolution"
		)
	), 
	
	renameFunction = list(
		list(
			changeVersion = "1.0.23", 
			functionName = "ICESAcousticCSV", 
			modelName = "baseline", 
			newFunctionName = "RstoxData::ICESAcoustic"
		), 
		list(
			changeVersion = "1.0.23", 
			functionName = "ICESBioticCSV", 
			modelName = "baseline", 
			newFunctionName = "RstoxData::ICESBiotic"
		), 
		list(
			changeVersion = "1.0.24", 
			functionName = "DefineStoxBioticTranslation", 
			modelName = "baseline", 
			newFunctionName = "RstoxData::DefineTranslation"
		), 
		# Renamed from Report to Write for the ICES export functions:
		list(
			changeVersion = "1.1.6", 
			functionName = "ReportICESAcoustic", 
			modelName = "report", 
			newFunctionName = "RstoxData::WriteICESAcoustic"
		), 
		list(
			changeVersion = "1.1.6", 
			functionName = "ReportICESBiotic", 
			modelName = "report", 
			newFunctionName = "RstoxData::WriteICESBiotic"
		), 
		list(
			changeVersion = "1.1.6", 
			functionName = "ReportICESDatras", 
			modelName = "report", 
			newFunctionName = "RstoxData::WriteICESDatras"
		)
	), 
	
	removeParameter = list(
		list(
			changeVersion = "1.0.18", 
			functionName = "ReadBiotic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.18", 
			functionName = "ReadAcoustic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.18", 
			functionName = "StoxBiotic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.18", 
			functionName = "StoxAcoustic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.18", 
			functionName = "AddToStoxBiotic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.20", 
			functionName = "ICESDatras", 
			modelName = "baseline", 
			parameterName = "SurveyName"
		), 
		list(
			changeVersion = "1.0.23", 
			functionName = "ICESDatras", 
			modelName = "baseline", 
			parameterName = "AddStationType"
		), 
		list(
			changeVersion = "1.0.24", 
			functionName = "TranslateStoxBiotic", 
			modelName = "baseline", 
			parameterName = "Translation"
		), 
		list(
			changeVersion = "1.0.24", 
			functionName = "TranslateStoxBiotic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition"
		), 
		list(
			changeVersion = "1.6.1", 
			functionName = "AddToStoxBiotic", 
			modelName = "baseline", 
			parameterName = "AddToLowestTable"
		)
	),  
	
	renameParameter = list(
		list(
			changeVersion = "1.0.24", 
			functionName = "TranslateStoxBiotic", 
			modelName = "baseline", 
			parameterName = "StoxBioticTranslation",
			newParameterName = "Translation"
		), 
		list(
			changeVersion = "1.0.24", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "Translation",
			newParameterName = "TranslationTable"
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalVariableName",
			newParameterName = "ConditionalVariableNames"
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalValueColumn",
			newParameterName = "ConditionalValueColumns"
		), 
		list(
			changeVersion = "1.9.0-9001", 
			functionName = "RegroupLengthICESDatras", 
			modelName = "baseline", 
			parameterName = "AggregationVariables",
			newParameterName = "AggregationVariablesHL"
		)
	),  
	
	translateParameter = list(
		list(
			changeVersion = "1.0.24", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "DefinitionMethod", 
			value = "Table", 
			newValue = "TranslationTable"
		), 
		list(
			changeVersion = "1.3.1", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "DefinitionMethod", 
			value = "TranslationTable", 
			newValue = "Table"
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "VariableName",
			value = list(
				list(), 
				"VariableName"
			), # Empty value was allowed in 3.3.0, implying that VariableName was given in the file to read, but will no longer be allowed.
			newValue = function(projectDescriptionOne) {
				projectDescriptionOne$processData$Translation[[1]]$VariableName
			}
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalVariableNames",
			# Multiple values must be given in a list!!! Also if only :
			value = list(
				list(), 
				"ConditionalVariableName"
			), # Empty value was allowed in 3.3.0, implying that ConditionalVariableNames was given in the file to read, but will no longer be allowed.
			newValue = function(projectDescriptionOne) {
				if(isTRUE(projectDescriptionOne$functionParameters$Conditional)) {
					# Has not yet been renamed from ConditionalVariableName to ConditionalVariableNames as per the order defiend in RstoxFramework::getRstoxFrameworkDefinitions("backwardCompatibilityActionNames"):
					projectDescriptionOne$processData$Translation[[1]]$ConditionalVariableName
				}
				else {
					list()
				}
			}
		)
	), 
	
	reshapeParameter = list(
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "TranslationTable",
			newValue = function(projectDescriptionOne) {
				# Return the reshaped parameter:
				projectDescriptionOne$functionParameters$TranslationTable <- RstoxData::oldToNewTranslationList(projectDescriptionOne$functionParameters$TranslationTable)
				# Return the projectDescriptionOne:
				return(projectDescriptionOne)
			}
		)
	), 
	
	renameProcessData = list(
		list(
			changeVersion = "1.0.24", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			processDataName = "StoxBioticTranslation",
			newProcessDataName = "Translation"
		)
	), 
	
	reshapeProcessData = list(
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			processDataName = "Translation",
			newProcessData = function(projectDescriptionOne) {
				projectDescriptionOne$processData$Translation <- RstoxData::oldToNewTranslationList(projectDescriptionOne$processData$Translation)
				# Return the processData:
				return(projectDescriptionOne)
			}
		)
	)
	
)
