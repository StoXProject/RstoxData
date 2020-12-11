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
		)
	)
)