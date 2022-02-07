#' Backward compabitibility actions:
#' @export
backwardCompatibility <- list(
	
	addParameter = list(
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
			value = list(), # Empty value was allowed in 3.3.0, implying that VariableName was given in the file to read, but will no longer be allowed.
			newValue = function(projectDescriptionOne) {
				projectDescriptionOne$processData$Translation[[1]]$VariableName
			}
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalVariableNames",
			value = list(), # Empty value was allowed in 3.3.0, implying that VariableName was given in the file to read, but will no longer be allowed.
			newValue = function(projectDescriptionOne) {
				# Has been renamed from ConditionalVariableName to ConditionalVariableNames first:
				projectDescriptionOne$processData$Translation[[1]]$ConditionalVariableNames
			}
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "VariableName",
			value = "VariableName", # Add the value if not given or if given as per erroneous BWC in 1.2.17.
			newValue = function(projectDescriptionOne) {
				projectDescriptionOne$processData$Translation[[1]]$VariableName
			}
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalVariableNames",
			value = "ConditionalVariableName", # Add the value if not given or if given as per erroneous BWC in 1.2.17.
			newValue = function(projectDescriptionOne) {
				# Has been renamed from ConditionalVariableName to ConditionalVariableNames first:
				projectDescriptionOne$processData$Translation[[1]]$ConditionalVariableNames
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
			newProcessData = function(processData) {
				
				values <- unique(unlist(lapply(processData$Translation, "[[", "Value")))
				
				if(length(values) > 1 ) {
					warning("StoX: The existing Translation process data containis multiple unique values in the Value column. This cannot be converted to the new form of the Translation process data, where only one variable can be defined for translation (but multiple conditional variables), and where the variable names in the data are used as column names in the Translation process data. StoX should still be able to apply the translation, but making changes may break the process or loose the translation infomation.")
				}
				else {
					conditional <- "ConditionalVariableName" %in% names(processData$Translation[[1]])
					if(conditional) {
						processData$Translation <- lapply(
							processData$Translation, 
							function(x) {
								names(x)[names(x) == "Value"] <- x$VariableName
								x$VariableName <- NULL
								names(x)[names(x) == "ConditionalValue"] <- x$ConditionalVariableName
								x$ConditionalVariableName <- NULL
								return(x)
							}
						)
					}
					else {
						processData$Translation <- lapply(
							processData$Translation, 
							function(x) {
								names(x)[names(x) == "Value"] <- x$VariableName
								x$VariableName <- NULL
								return(x)
							}
						)
					}
				}	
			
			return(processData)	
			}
		)
	)
	
)
