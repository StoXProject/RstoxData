# Properly unload gadget shared lib
.onUnload <- function (libpath) {
  library.dynam.unload("RstoxData", libpath)
}

# Global variables
utils::globalVariables(c("RstoxDataEnv", "xsdObjects", ":=", "..allDuplicated", "..colAgg",
    ".", "..colList", "..columns", "..digits", "..keep", "..key",
    "..signifDigits", "..sourceColumns", "..targetAndSourceVariables",
    "..varToExtract", "..x", "AcousticCategory", "Addition", "age", "agingstructure", "ap",
    "aphia", "BeamKey", "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount",
    "catchpartnumber", "catchproducttype", "CatchSpeciesCategoryNumber",
    "CatchSpeciesCategoryWeight", "CatchSpeciesCode", "CatchSubsampledNumber",
    "CatchSubsampleWeight", "catchweight", "cc", "Constant", "Country", "cruise", "Cruise",
    "CruiseKey", "cw", "DateTime", "direction", "DoorType", "EchoType", "EDSU", "FishID",
    "fishingdepthmax", "fishingdepthmin", "freq", "g", "gear", "Gear", "gearcondition",
    "GearExp", "gearflow", "HaulNo", "HaulNumber", "HaulVal", "HaulValidity", "hv",
    "inapplicableFormats", "individualweight", "isCrustacean", "isHerringOrSprat",
    "isHerringOrSpratOrMackerel", "latitudeend", "latitudestart", "LengthClass",
    "LengthCode", "lengthmeasurement", "lengthsamplecount", "lengthsampleweight",
    "lenInterval", "level", "lngtClass", "lngtCode", "LocalID", "LogDuration", "LogKey",
    "LogOrigin", "longitudeend", "longitudestart", "lsc", "lsCountTot", "maturationstage",
    "maturity", "meanW", "MiddleDateTime", "missionstartdate", "missionstopdate", "ms", "N",
    "nation", "nInd", "noMeas", "NumberAtLength", "nWithWeight", "parasite", "platformname",
    "preferredagereading", "Quarter", "readability", "ReplaceBy", "reportInMM", "res",
    "rowIndex", "s", "SaCategory", "sampleFac", "samplequality", "sampletype", "Scaling",
    "serialnumber", "sex", "Ship", "sp", "specialstage", "specimenid", "SpecVal", "start_time",
    "StartDateTime", "startyear", "station", "stationstartdate", "stationstarttime",
    "stationstopdate", "stationstoptime", "stationtype", "StatRec", "stomach",
    "StopDateTime", "stoxBioticObject", "subFactor", "SubsampledNumber", "subWeight",
    "suffixes", "Survey", "SweepLngt", "target", "Time", "tissuesample", "totalNo", "totWeight",
    "transceiver", "trawldoorarea", "trawldoorspread", "trawldoortype",
    "trawldoorweight", "VariableName", "verticaltrawlopening", "WeightMeasurement",
    "winddirection", "windspeed", "wingspread", "wiredensity", "wirediameter",
    "wirelength"))
