# Read a biotic file and create a StoxBiotic object:
exampleFile <- system.file("testresources","biotic_2020821.zip", package="RstoxData")
suppressWarnings(exampleData <- RstoxData::StoxBiotic(RstoxData::ReadBiotic(exampleFile)))

# Translate SpeciesCategory from "sild'G05/161722.G05/126417/NA" to "HER":
TranslationTable_SpeciesCategory = data.table::data.table(
	SpeciesCategory = "sild'G05/161722.G05/126417/NA",
	NewValue = "HER"
)
Translation_SpeciesCategory <- RstoxData::DefineTranslation(
	DefinitionMethod = "Table", 
	TranslationTable = TranslationTable_SpeciesCategory, 
	VariableName = "SpeciesCategory"
)
exampleDataTranslated_SpeciesCategory <- RstoxData::TranslateStoxBiotic(StoxBioticData = exampleData, TranslationDefinition = "FunctionInput", Translation = Translation_SpeciesCategory)

expect_equal(which(exampleDataTranslated_SpeciesCategory$SpeciesCategory$SpeciesCategory == "HER"), c(1, 11))

# Translate IndividualSex to Male if IndividualTotalLength > 20. All IndividualSex are NA, so we need to use a function both for IndividualSex and for IndividualTotalLength:
TranslationTable_IndividualSex = data.table::data.table(
	IndividualSex = "function(IndividualSex) TRUE",
	NewValue = "M", 
	IndividualTotalLength = "function(IndividualTotalLength) IndividualTotalLength > 20"
)
Translation_IndividualSex <- RstoxData::DefineTranslation(
	DefinitionMethod = "Table", 
	TranslationTable = TranslationTable_IndividualSex, 
	VariableName = "IndividualSex", 
	Conditional = TRUE, 
	ConditionalVariableNames = "IndividualTotalLength"
)
exampleDataTranslated_IndividualSex <- RstoxData::TranslateStoxBiotic(StoxBioticData = exampleData, TranslationDefinition = "FunctionInput",  Translation = Translation_IndividualSex)

expect_equal(sum(exampleDataTranslated_IndividualSex$Individual$IndividualSex == "M", na.rm = TRUE), 135)
