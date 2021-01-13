#' Backward compabitibility actions:
#' @export
backwardCompatibility <- list(
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
		)
	)
)
