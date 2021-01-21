#' Backward compabitibility actions:
#' @export
backwardCompatibility <- list(
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
	)
	
)
