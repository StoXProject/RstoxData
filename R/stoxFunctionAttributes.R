# Function to get variable names, given a different variable name in the input VariableName. This funciton is used e.g. to list possible ConditionalVariableNames:
getVariableNamesStoxData <- function(BioticData, StoxBioticData, ICESBioticData, ICESDatrasData, ICESDatsuscData, AcousticData, StoxAcousticData, ICESAcousticData, LandingData, StoxLandingData, VariableName) {
	
	# Function to get the names of a table if that table contains certain variables:
	getNamesIfVariableIsPresent <- function(table, requiredVariableNames) {
		output <- names(table)
		if(length(requiredVariableNames) && !requiredVariableNames %in% output) {
			output <- NULL
		}
		return(output)
	}
	
	# Get the variables:
	if(!missing(BioticData)) {
		output <- lapply(BioticData, function(x) lapply(x, getNamesIfVariableIsPresent, requiredVariableNames = VariableName))
	}
	else if(!missing(StoxBioticData)) {
		output <- lapply(StoxBioticData, getNamesIfVariableIsPresent, requiredVariableNames = VariableName)
	}
	else if(!missing(ICESBioticData)) {
		output <- lapply(ICESBioticData, getNamesIfVariableIsPresent, requiredVariableNames = VariableName)
	}
	else if(!missing(ICESDatrasData)) {
		output <- lapply(ICESDatrasData, getNamesIfVariableIsPresent, requiredVariableNames = VariableName)
	}
	else if(!missing(ICESDatsuscData)) {
		output <- lapply(ICESDatsuscData, getNamesIfVariableIsPresent, requiredVariableNames = VariableName)
	}
	else if(!missing(AcousticData)) {
		output <- lapply(AcousticData, function(x) lapply(x, getNamesIfVariableIsPresent, requiredVariableNames = VariableName))
	}
	else if(!missing(StoxAcousticData)) {
		output <- lapply(StoxAcousticData, getNamesIfVariableIsPresent, requiredVariableNames = VariableName)
	}
	else if(!missing(ICESAcousticData)) {
		output <- lapply(ICESAcousticData, getNamesIfVariableIsPresent, requiredVariableNames = VariableName)
	}
	else if(!missing(LandingData)) {
		output <- lapply(LandingData, function(x) lapply(x, getNamesIfVariableIsPresent, requiredVariableNames = VariableName))
	}
	else if(!missing(StoxLandingData)) {
		output <- lapply(StoxLandingData, getNamesIfVariableIsPresent, requiredVariableNames = VariableName)
	}
	else {
		return(NULL)
	}
	
	output <- sort(unique(unlist(output)))
	output <- setdiff(output, VariableName)
	
	return(output)
	
	#else {
	#	stop("Any of BioticData, StoxBioticData, ICESBioticData, ICESDatrasData, AcousticData, StoxAcousticData, ICESAcousticData, Land#ingData and StoxLandingData must be given.")
	#}
}


# Function to get variable names from all tables:
getVariableNameStoxData <- function(BioticData, StoxBioticData, ICESBioticData, ICESDatrasData, ICESDatsuscData, AcousticData, StoxAcousticData, ICESAcousticData, LandingData, StoxLandingData) {
	
	output <- getVariableNamesStoxData(BioticData, StoxBioticData, ICESBioticData, ICESDatrasData, ICESDatsuscData, AcousticData, StoxAcousticData, ICESAcousticData, LandingData, StoxLandingData, VariableName = NULL) 
	
	return(output)
}




getValueColumns <- function(FileName, ValueColumn = NULL, NewValueColumn = NULL, ConditionalValueColumns = NULL) {
	# Read the table from file:
	tanslation <- data.table::fread(FileName, encoding = "UTF-8", colClasses = "character")
	# Discard already given columns:
	setdiff(names(tanslation), c(ValueColumn, NewValueColumn, ConditionalValueColumns))
}

#' Function specification for inclusion in StoX projects
#' @export
stoxFunctionAttributes <- list(
	# Read input biotic data:
	ReadBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "BioticData", 
		#functionParameterType = list(FileNames = "character"), 
		functionParameterFormat = list(FileNames = "filePaths"), 
		functionArgumentHierarchy = list()
	), 
	
	# Read input biotic data:
	ReadAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "AcousticData", 
		#functionParameterType = list(FileNames = "character"), 
		functionParameterFormat = list(FileNames = "filePaths"), 
		functionArgumentHierarchy = list()
	), 
	
	# Read input biotic data:
	ReadLanding = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "LandingData", 
	  functionParameterFormat = list(FileNames = "filePaths"), 
	  functionArgumentHierarchy = list()
	), 
	
	# Convert AcousticData to StoxAcousticData:
	StoxAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxAcousticData", 
		functionArgumentHierarchy = list()
	),
	
	# Convert BioticData to StoxBioticData:
	StoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionArgumentHierarchy = list()
	),
	
	# Convert LandingData to StoxLandingData:
	StoxLanding = list(
	 functionType = "modelData",
	 functionCategory = "baseline",
	 functionOutputDataType = "StoxLandingData",
	 functionArgumentHierarchy = list()
	), 
	
	# Convert BioticData to StoxBioticData:
	FilterBiotic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "BioticData", 
	    functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Convert BioticData to StoxBioticData:
	FilterAcoustic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "AcousticData", 
	    functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Convert BioticData to StoxBioticData:
	FilterStoxBiotic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "StoxBioticData", 
	    functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Convert BioticData to StoxBioticData:
	FilterStoxAcoustic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "StoxAcousticData", 
	    functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Filter StoxLandingData:
	FilterStoxLanding = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "StoxLandingData", 
	  functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Filter LandingData:
	FilterLanding = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "LandingData", 
	  functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	
	
	FilterICESBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESBioticData", 
		functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	FilterICESAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESAcousticData", 
		functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	FilterICESDatras  = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESDatrasData", 
		functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	FilterICESDatsusc  = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "ICESDatsuscData", 
	  functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	
	# Copy:
	CopyBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "BioticData", 
		functionParameterFormat = list(
			FromVariable = "fromVariable_StoxData"#, 
			#ConditionalVariableNames = "conditionalVariableNames_Copy", 
			#ConditionalTable = "conditionalTable_Copy"
		), 
		functionArgumentHierarchy = list(
			PreserveClass = list(
				overwrite = TRUE
			)#, 
			#ConditionalVariableNames = list(
			#	Conditional = TRUE
			#), 
			#ConditionalTable = list(
			#	Conditional = TRUE
			#)
		)
	),
	CopyStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterFormat = list(
			FromVariable = "fromVariable_StoxData"
		), 
		functionArgumentHierarchy = list(
			PreserveClass = list(
				overwrite = TRUE
			)
		)
	),
	CopyICESBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESBioticData", 
		functionParameterFormat = list(
			FromVariable = "fromVariable_StoxData"
		), 
		functionArgumentHierarchy = list(
			PreserveClass = list(
				overwrite = TRUE
			)
		)
	),
	CopyICESDatras = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESDatrasData", 
		functionParameterFormat = list(
			FromVariable = "fromVariable_StoxData"
		), 
		functionArgumentHierarchy = list(
			PreserveClass = list(
				overwrite = TRUE
			)
		)
	),
	CopyICESDatsusc = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "ICESDatsuscData", 
	  functionParameterFormat = list(
	    FromVariable = "fromVariable_StoxData"
	  ), 
	  functionArgumentHierarchy = list(
	    PreserveClass = list(
	      overwrite = TRUE
	    )
	  )
	),
	  
	CopyAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "AcousticData", 
		functionParameterFormat = list(
			FromVariable = "fromVariable_StoxData"
		), 
		functionArgumentHierarchy = list(
			PreserveClass = list(
				overwrite = TRUE
			)
		)
	),
	CopyStoxAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxAcousticData", 
		functionParameterFormat = list(
			FromVariable = "fromVariable_StoxData"
		), 
		functionArgumentHierarchy = list(
			PreserveClass = list(
				overwrite = TRUE
			)
		)
	),
	CopyICESAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESAcousticData", 
		functionParameterFormat = list(
			FromVariable = "fromVariable_StoxData"
		), 
		functionArgumentHierarchy = list(
			PreserveClass = list(
				overwrite = TRUE
			)
		)
	),
	CopyLanding = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "LandingData", 
		functionParameterFormat = list(
			FromVariable = "fromVariable_StoxData"
		), 
		functionArgumentHierarchy = list(
			PreserveClass = list(
				overwrite = TRUE
			)
		)
	),
	CopyStoxLanding = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxLandingData", 
		functionParameterFormat = list(
			FromVariable = "fromVariable_StoxData"
		), 
		functionArgumentHierarchy = list(
			PreserveClass = list(
				overwrite = TRUE
			)
		)
	),
	
	
	
	# Convert AcousticData to StoxAcousticData:
	MergeStoxAcoustic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "MergeStoxAcousticData", 
	    functionArgumentHierarchy = list()
	),
	
	# Convert BioticData to StoxBioticData:
	MergeStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "MergeStoxBioticData", 
		functionArgumentHierarchy = list()
	),
	
	
	##### Define and Convert variables: #####
	# StoxBiotic:
	RedefineStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterFormat = list(
			Redefinition = "redefinitionTable"
		)
	),
	
	
	AddToStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterFormat = list(
			VariableNames = "variableNames_AddToStoxBiotic"
		)
	), 
	
	CompensateEffectiveTowDistanceForFishingDepthCount = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData"
	),
	
	ICESAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESAcousticData"
	), 
	ICESBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESBioticData"
	), 
	ICESDatras = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESDatrasData"
	),
	ICESDatsusc = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "ICESDatsuscData"
	),
	
	WriteICESAcoustic = list(
		functionType = "modelData", 
		functionCategory = "report", 
		functionOutputDataType = "WriteICESAcousticData"
	), 
	WriteICESBiotic = list(
		functionType = "modelData", 
		functionCategory = "report", 
		functionOutputDataType = "WriteICESBioticData"
	), 
	WriteICESDatras = list(
		functionType = "modelData", 
		functionCategory = "report", 
		functionOutputDataType = "WriteICESDatrasData"
	),
	WriteICESDatsusc = list(
	  functionType = "modelData", 
	  functionCategory = "report", 
	  functionOutputDataType = "WriteICESDatsuscData"
	),
	
	RegroupLengthICESDatras = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESDatrasData", 
		functionParameterFormat = list(
			GroupingVariables = "groupingVariables_RegroupLengthICESDatras", 
			AggregationVariablesHL = "groupingVariables_RegroupLengthICESDatras", 
			AggregationVariablesCA = "groupingVariables_RegroupLengthICESDatras", 
			ResolutionTableVariables = "groupingVariables_RegroupLengthICESDatras", 
			ResolutionTable = "resolutionTable_RegroupLengthICESDatras"
		),
		functionArgumentHierarchy = list(
			AggregationVariablesHL = list(
				AggregateHLNoAtLngt = TRUE
			), 
			AggregationVariablesCA = list(
				AggregateCANoAtLngt = TRUE
			),
			ResolutionTableVariables = list(
				RegroupMethod = "ResolutionTable"
			),
			ResolutionTable = list(
				RegroupMethod = "ResolutionTable"
			),
			GroupingVariables = list(
				RegroupMethod = "HighestResolution"
			)
		), 
		functionParameterDefaults = list(
			GroupingVariables = c("HaulNumber", "SpeciesCode"), 
			# This need verification
			AggregationVariablesHL = c("HaulNumber", "SpeciesCode", "SpeciesCategory", "SpeciesSex", "LengthClass"), 
			# From https://datras.ices.dk/Data_products/ReportingFormat.aspx, clicking on CANoAtLngt:
			# "Amount of fish at the given category (per haul, species, length class, sex, maturity, age)."
			# Here a haul is represented not by the HaulNumber, which is a sequential numbering of the hauls
			AggregationVariablesCA = c("HaulNumber", "SpeciesCode", "LengthClass", "SpeciesSex", "IndividualMaturity", "IndividualAge")
		)
	),
	
	# Translation:
	DefineTranslation = list(
		functionType = "processData", 
		functionCategory = "baseline", 
		functionOutputDataType = "Translation", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			FileName = "filePath", 
			ConditionalVariableNames = "conditionalVariableNames_translate", 
			ValueColumn = "valueColumn", 
			NewValueColumn = "newValueColumn", 
			ConditionalValueColumns = "conditionalValueColumns"
		), 
		functionArgumentHierarchy = list(
			DefinitionMethod = list(
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				DefinitionMethod = "Table", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			FileName = list(
				DefinitionMethod = "ResourceFile", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				#DefinitionMethod = "TranslationTable", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			ValueColumn = list(
				DefinitionMethod = "ResourceFile", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			NewValueColumn = list(
				DefinitionMethod = "ResourceFile", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				Conditional = TRUE, 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalValueColumns = list(
				DefinitionMethod = "ResourceFile", 
				Conditional = TRUE, 
				UseProcessData = FALSE
			)
		)
	),
	
	# Translate raw data:
	TranslateBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "BioticData", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			VariableName = "variableName_translate", 
			ConditionalVariableNames = "conditionalVariableNames_translate"
		), 
		functionArgumentHierarchy = list(
			Translation = list(
				TranslationDefinition = "FunctionInput"
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				TranslationDefinition = "FunctionParameter",
				Conditional = TRUE
			)
		)
	),
	
	TranslateAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "AcousticData", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			VariableName = "variableName_translate", 
			ConditionalVariableNames = "conditionalVariableNames_translate"
		), 
		functionArgumentHierarchy = list(
			Translation = list(
				TranslationDefinition = "FunctionInput"
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				TranslationDefinition = "FunctionParameter" 
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				TranslationDefinition = "FunctionParameter",
				Conditional = TRUE
			)
		)
	),
	
	TranslateLanding = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "LandingData", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			VariableName = "variableName_translate", 
			ConditionalVariableNames = "conditionalVariableNames_translate"
		), 
		functionArgumentHierarchy = list(
			Translation = list(
				TranslationDefinition = "FunctionInput"
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				TranslationDefinition = "FunctionParameter" 
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				TranslationDefinition = "FunctionParameter",
				Conditional = TRUE
			)
		)
	),
	
	# Translate StoX data:
	TranslateStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			VariableName = "variableName_translate", 
			ConditionalVariableNames = "conditionalVariableNames_translate"
		), 
		functionArgumentHierarchy = list(
			Translation = list(
				TranslationDefinition = "FunctionInput"
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				TranslationDefinition = "FunctionParameter",
				Conditional = TRUE
			)
		)
	),
	
	TranslateStoxAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxAcousticData", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			VariableName = "variableName_translate", 
			ConditionalVariableNames = "conditionalVariableNames_translate"
		), 
		functionArgumentHierarchy = list(
			Translation = list(
				TranslationDefinition = "FunctionInput"
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				TranslationDefinition = "FunctionParameter",
				Conditional = TRUE
			)
		)
	),
	
	TranslateStoxLanding = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxLandingData", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			VariableName = "variableName_translate", 
			ConditionalVariableNames = "conditionalVariableNames_translate"
		), 
		functionArgumentHierarchy = list(
			Translation = list(
				TranslationDefinition = "FunctionInput"
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				TranslationDefinition = "FunctionParameter",
				Conditional = TRUE
			)
		)
	),
	
	# Translate ICES data:
	TranslateICESAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESAcousticData", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			VariableName = "variableName_translate", 
			ConditionalVariableNames = "conditionalVariableNames_translate"
		), 
		functionArgumentHierarchy = list(
			Translation = list(
				TranslationDefinition = "FunctionInput"
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				TranslationDefinition = "FunctionParameter",
				Conditional = TRUE
			)
		)
	),
	
	TranslateICESBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESBioticData", 
		functionParameterFormat = list(
			TranslationTable = "translationTable",
			VariableName = "variableName_translate", 
			ConditionalVariableNames = "conditionalVariableNames_translate"
		), 
		functionArgumentHierarchy = list(
			Translation = list(
				TranslationDefinition = "FunctionInput"
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				TranslationDefinition = "FunctionParameter",
				Conditional = TRUE
			)
		)
	),
	
	TranslateICESDatras = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESDatrasData", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			VariableName = "variableName_translate", 
			ConditionalVariableNames = "conditionalVariableNames_translate"
		), 
		functionArgumentHierarchy = list(
			Translation = list(
				TranslationDefinition = "FunctionInput"
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				TranslationDefinition = "FunctionParameter"
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableNames = list(
				TranslationDefinition = "FunctionParameter",
				Conditional = TRUE
			)
		)
	),
	
	TranslateICESDatsusc = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "ICESDatsuscData", 
	  functionParameterFormat = list(
	    TranslationTable = "translationTable", 
	    VariableName = "variableName_translate", 
	    ConditionalVariableNames = "conditionalVariableNames_translate"
	  ), 
	  functionArgumentHierarchy = list(
	    Translation = list(
	      TranslationDefinition = "FunctionInput"
	    ), 
	    # These two are joined with AND, and must both be fulfilled:
	    TranslationTable = list(
	      TranslationDefinition = "FunctionParameter"
	    ), 
	    # These two are joined with AND, and must both be fulfilled:
	    Conditional = list(
	      TranslationDefinition = "FunctionParameter"
	    ), 
	    # These two are joined with AND, and must both be fulfilled:
	    VariableName = list(
	      TranslationDefinition = "FunctionParameter"
	    ), 
	    # These two are joined with AND, and must both be fulfilled:
	    ConditionalVariableNames = list(
	      TranslationDefinition = "FunctionParameter",
	      Conditional = TRUE
	    )
	  )
	)
)


#getConversionTableFormat <- function(type = c("StoxBiotic", "StoxAcoustic", "Biotic", "Acoustic")) {
#	
#	type <- match.arg(type)
#	
#	conversionTable = list(
#		class = "table", 
#		title = function(ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling")) {
#			ConversionFunction <- match.arg(ConversionFunction)
#			
#			if(identical(ConversionFunction, "Constant")) {
#				title <- "Replace variables of StoX data by a constant \"Constant\""
#			}
#			else if(identical(ConversionFunction, "Addition")) {
#				title <- "Add the value \"Addition\" to variables of StoX data"
#			}
#			else if(identical(ConversionFunction, "Scaling")) {
#				title <- "Multiply variables of StoX data by the value \"Scaling\""
#			}
#			else if(identical(ConversionFunction, "AdditionAndScaling")) {
#				title <- "Multiply variables of StoX data by the value \"Scaling\" and add the value \"Addition\""
#			}
#			else {
#				stop("Wrong ConversionFunction.")
#			}
#			
#			return(title)
#		}, 
#		columnNames = function(ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), GruopingVariables = NULL) {
#			ConversionFunction <- match.arg(ConversionFunction)
#			
#			if(identical(ConversionFunction, "Constant")) {
#				parameters <- "Constant"
#			}
#			else if(identical(ConversionFunction, "Addition")) {
#				parameters <- "Addition"
#			}
#			else if(identical(ConversionFunction, "Scaling")) {
#				parameters <- "Scaling"
#			}
#			else if(identical(ConversionFunction, "AdditionAndScaling")) {
#				parameters <- c("Addition", "Scaling")
#			}
#			else {
#				stop("Wrong ConversionFunction.")
#			}
#			
#			columnNames <- c(
#				GruopingVariables, 
#				#c("TargetVariable", "SourceVariable"), 
#				"SourceVariable", 
#				parameters, 
#				"RoundOffTo"
#			)
#			
#			return(columnNames)
#		}, 
#		variableTypes = function(ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), GruopingVariables = NULL) #{
#			ConversionFunction <- match.arg(ConversionFunction)
#			
#			if(identical(ConversionFunction, "Constant")) {
#				types <- "double"
#			}
#			else if(identical(ConversionFunction, "Addition")) {
#				types <- "double"
#			}
#			else if(identical(ConversionFunction, "Scaling")) {
#				types <- "double"
#			}
#			else if(identical(ConversionFunction, "AdditionAndScaling")) {
#				types <- c("double", "double")
#			}
#			else {
#				stop("Wrong ConversionFunction.")
#			}
#			
#			variableTypes <- c(
#				rep("character", length(GruopingVariables)), 
#				#c("character", "character"), 
#				"character", 
#				types, 
#				"double"
#			)
#			
#			return(variableTypes)
#		}, 
#		possibleValues = switch(
#			type, 
#			StoxBiotic = function(
#				StoxBioticData, 
#				ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
#				GruopingVariables = NULL
#			) {
#				conversionTableFormatPossibleValuesFunction(
#					data = StoxBioticData, 
#					ConversionFunction = ConversionFunction, 
#					GruopingVariables = GruopingVariables
#				)
#			}, 
#			StoxAcoustic = function(
#				StoxAcousticData, 
#				ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
#				GruopingVariables = NULL
#			) {
#				conversionTableFormatPossibleValuesFunction(
#					data = StoxAcousticData, 
#					ConversionFunction = ConversionFunction, 
#					GruopingVariables = GruopingVariables
#				)
#			}, 
#			Biotic = function(
#				BioticData, 
#				ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
#				GruopingVariables = NULL
#			) {
#				conversionTableFormatPossibleValuesFunction(
#					data = BioticData, 
#					ConversionFunction = ConversionFunction, 
#					GruopingVariables = GruopingVariables
#				)
#			}, 
#			Acoustic = function(
#				AcousticData, 
#				ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
#				GruopingVariables = NULL
#			) {
#				conversionTableFormatPossibleValuesFunction(
#					data = AcousticData, 
#					ConversionFunction = ConversionFunction, 
#					GruopingVariables = GruopingVariables
#				)
#			}
#		)
#	)
#	
#	return(conversionTable)
#}



conversionTableFormatPossibleValuesFunction = function(data, ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), GruopingVariables = NULL) {
	ConversionFunction <- match_arg_informative(ConversionFunction)
	
	if(identical(ConversionFunction, "Constant")) {
		numTypes <- 1
	}
	else if(identical(ConversionFunction, "Addition")) {
		numTypes <- 1
	}
	else if(identical(ConversionFunction, "Scaling")) {
		numTypes <- 1
	}
	else if(identical(ConversionFunction, "AdditionAndScaling")) {
		numTypes <- 2
	}
	else {
		stop("Wrong ConversionFunction.")
	}
	
	dataFlatList <- unlist(lapply(unname(data), as.list), recursive = FALSE)
	variableNames <- sort(names(dataFlatList))
	possibleValues <- c(
		lapply(GruopingVariables, function(variableMame) sort(unique(dataFlatList[[variableMame]]))), 
		#rep(list(variableNames), 2), 
		list(variableNames), 
		rep(list(list()), numTypes + 1)
	)
	
	return(possibleValues)
}


#' Define the process property formats:
#' 
#' @export
#' 
processPropertyFormats <- list(
	filePath = list(
		class = "single", 
		title = "The path to a single file"
	), 
	# The StoX GUI assumes an additional directoryPath, which us used by RstoxFDA
	filePaths = list(
		class = "vector", 
		title = "The path to one or more files", 
		variableTypes = "character"
	), 
	filterExpressionList = list(
		class = "list", 
		title = "A list of filter expressions, one for each table to filter on"
	), 
	
	redefinitionTable = list(
		class = "table", 
		title = "Define columns of StoX data by columns of the raw data", 
		columnNames = c(
			"VariableName", 
			"ReplaceBy"
		), 
		variableTypes = c(
			"character",
			"character"
		), 
		possibleValues = function(StoxBioticData, BioticData) {
			list(
				sort(unique(unlist(lapply(StoxBioticData, names)))), 
				sort(unique(unlist(lapply(BioticData, function(x) lapply(x, names)))))
			)
		}
	), 
	translationTable = list(
		class = "table", 
		title = "Translate columns of StoX data", 
		columnNames = function(VariableName, Conditional, ConditionalVariableNames = NULL) {
			if(!length(VariableName)) {
				warning("StoX: VariableName must be set to apply the TranslationTable in the StoX GUI.")
				return(NULL)
			}
			columnNames <- c(
				VariableName, 
				"NewValue"
			)
			# Add a conditional variable:
			if(Conditional) {
				if(!length(ConditionalVariableNames)) {
					warning("StoX: ConditionalVariableNames must be set when Conditional = TRUE to apply the TranslationTable in the StoX GUI.")
					return(NULL)
				}
				
				columnNames <- c(
					columnNames, 
					ConditionalVariableNames
				)
			}
			
			return(columnNames)
		}, 
		variableTypes = function(VariableName, Conditional, ConditionalVariableNames = NULL) {
			rep("character", 2 + as.numeric(Conditional) * length(ConditionalVariableNames))
		}
		#columnNames = c(
		#	"VariableName", 
		#	"Value", 
		#	"NewValue"
		#), 
		#variableTypes = c(
		#	"character",
		#	"character",
		#	"character"
		#)
	), 
	
	#conditionalTable_Copy = list(
	#	class = "table", 
	#	title = "Select the values which the copy is restricted to.", 
	#	columnNames = function(ConditionalVariableNames) {
	#		if(!length(ConditionalVariableNames)) {
	#			stop("StoX: ConditionalVariableNames must be set when Conditional = TRUE.")
	#			#return(NULL)
	#		}
	#		return(ConditionalVariableNames)
	#	}, 
	#	variableTypes = function(ConditionalVariableNames) {
	#		if(!length(ConditionalVariableNames)) {
	#			stop("StoX: ConditionalVariableNames must be set when Conditional = TRUE.")
	#			#return(NULL)
	#		}
	#		return(rep("character", length(ConditionalVariableNames)))
	#	}
	#), 
	
	
	conditionalVariableNames_translate = list(
		class = "vector", 
		title = "One or more conditional variables to translate by.", 
		possibleValues = getVariableNamesStoxData, 
		variableTypes = "character"
	), 
	variableName_translate = list(
		class = "single", 
		title = "Select one variable to translate.", 
		possibleValues = getVariableNameStoxData, 
		variableTypes = "character"
	), 
	valueColumn = list(
		class = "single", 
		title = "The name of the column in the input file giving the values to tranlate.", 
		variableTypes = "character", 
		possibleValues = getValueColumns
	), 
	newValueColumn = list(
		class = "single", 
		title = "The name of the column in the input file giving the (new) values to tranlate to.", 
		variableTypes = "character", 
		possibleValues = getValueColumns
	), 
	conditionalValueColumns = list(
		class = "vector", 
		title = "The column name in the input file of one or more conditional variables to translate by.", 
		variableTypes = "character", 
		possibleValues = getValueColumns
	), 
	
	
	#conversionTable_StoxBioticData = getConversionTableFormat("StoxBiotic"),
	#
	#conversionTable_StoxAcousticData = getConversionTableFormat("StoxAcoustic"),
	#
	#conversionTable_BioticData = getConversionTableFormat("Biotic"),
	#
	#conversionTable_AcousticData = getConversionTableFormat("Acoustic"), 
	
	variableNames_AddToStoxBiotic = list(
		class = "vector", 
		title = "One or more variables to add to the StoxBioticData from BioticData", 
		possibleValues = function(BioticData, tablesToExclude = c("prey", "preylengthfrequencytable", "copepodedevstagefrequencytable")) {
			sort(unique(unlist(lapply(BioticData[setdiff(names(BioticData), tablesToExclude)], function(x) lapply(x, names)))))
		}, 
		variableTypes = "character"
	), 
	fromVariable_StoxData = list(
		class = "single", 
		title = "Select one variable to copy", 
		possibleValues = getVariableNameStoxData, 
		variableTypes = "character"
	), 

	
	roundingTable = list(
		class = "table", 
		title = "Table linking LengthCode and the length resolution", 
		columnNames = c(
			"LengthCode", 
			"LengthResolution"
		), 
		variableTypes = c(
			"character", 
			"double"
		)
	), 
	
	groupingVariables_RegroupLengthICESDatras = list(
		class = "vector", 
		title = "One or more variables to group by when regrouping lengths in Datras", 
		possibleValues = function(ICESDatrasData) {
			sort(names(ICESDatrasData$HL))
		}, 
		variableTypes = "character"
	), 
	
	
	resolutionTable_RegroupLengthICESDatras = list(
		class = "table", 
		title = "Table of requested length resolution (column ResolutionCode) for combinations of the ResolutionTableVariables", 
		columnNames = function(ResolutionTableVariables) {
			c(ResolutionTableVariables, "ResolutionCode")
		}, 
		variableTypes = function(ResolutionTableVariables) {
			rep("character", 1 + length(ResolutionTableVariables))
		}, 
		possibleValues = function(ICESDatrasData, ResolutionTableVariables) {
			c(
				lapply(ResolutionTableVariables, function(x) sort(unique(ICESDatrasData[[x]]))), 
				list(getRstoxDataDefinitions("lengthCode_unit_table")$shortnameNMDBiotic)
			)
		}
	)
)





