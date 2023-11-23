#' Checks that input conforms to NMDbioticdata v3.x as read by ReadBiotic.
#' Any addition of levels, removal of fields or changes to the key structure should prompt a major revision of biotic,
#' so checking against biotic 3, biotix 3.1 and so on shold pass.
#' @noRd
is.NMDBiotic <- function(BioticData){
  #check that data conforms to expectation
  message <- "'BioticData' should be a list of lists of data.tables that corresponds to levels in NMDbiotic 3.x."
  # is a list
  if (!is.list(BioticData)){
    stop(message)
  }
  #contains lists
  for (f in names(BioticData)){
    if (!is.list(BioticData[[f]])){
      stop(message)
    }
    #contains the right tables
    missingTables <- names(RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders)[!(names(RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders) %in% names(BioticData[[f]]))]
    if (length(missingTables)>0){
      stop(paste(message, paste(f, "is missing tables:", paste(missingTables, collapse=", "))))
    }
    
    #contains the right keys
    for (t in names(BioticData[[f]])){
      if (t != "missions" && (t %in% names(RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders))){
        keys <- RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders[[t]][1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[[t]])]
        missingKeys <- keys[!(keys %in% names(BioticData[[f]][[t]]))]
        if (length(missingKeys)>0){
          stop(paste(message, paste("Table", t, "from", f, "is missing keys:", paste(keys, collapse=", "))))
        }
      }
    }
  }
}

#' @noRd
check_reserved_names <- function(BioticData, reserved_names){
  #check that reserved names has not be added already
    usednames <- c()
    for (f in names(BioticData)){
      for (t in names(BioticData[[f]])){
        usednames <- c(usednames, reserved_names[reserved_names %in% names(BioticData[[f]][[t]])])
      }
    }
    if (length(usednames) > 0){
      stop(paste("Cannot add identifiers since the data already contains columns with names reserved for identifiers:", paste(usednames, collapse=", ")))
    }
}

#' @noRd
addIds <- function(mergedTable){
  if (all(c("cruise", "missiontype", "startyear", "platform", "missionnumber") %in% names(mergedTable))){
    mergedTable$CruiseKey <- apply(mergedTable[,.SD,.SDcol=c("cruise", "missiontype", "startyear", "platform", "missionnumber")],1,paste,collapse="/")
  }
  if (all(c("CruiseKey", "station") %in% names(mergedTable))){
    mergedTable$Station <- apply(mergedTable[,.SD,.SDcol=c("CruiseKey","station")],1,paste,collapse="-")
    mergedTable$Station[is.na(mergedTable$station)] <- as.character(NA)
  }
  if (all(c("Station", "serialnumber") %in% names(mergedTable))){
    mergedTable$FishStation <- apply(mergedTable[,.SD,.SDcol=c("Station","serialnumber")],1,paste,collapse="-")
    mergedTable$FishStation[is.na(mergedTable$serialnumber)] <- as.character(NA)
  }
  if (all(c("FishStation", "catchsampleid") %in% names(mergedTable))){
    mergedTable$CatchSample <- apply(mergedTable[,.SD,.SDcol=c("FishStation","catchsampleid")],1,paste,collapse="-")
    mergedTable$CatchSample[is.na(mergedTable$catchsampleid)] <- as.character(NA)
  }
  if (all(c("CatchSample", "specimenid") %in% names(mergedTable))){
    mergedTable$Individual <- apply(mergedTable[,.SD,.SDcol=c("CatchSample","specimenid")],1,paste,collapse="-")
    mergedTable$Individual[is.na(mergedTable$specimenid)] <- as.character(NA)
  }
  if (all(c("Individual", "agedeterminationid") %in% names(mergedTable))){
    mergedTable$AgeReading <- apply(mergedTable[,.SD,.SDcol=c("Individual","agedeterminationid")],1,paste,collapse="-")
    mergedTable$AgeReading[is.na(mergedTable$agedeterminationid)] <- as.character(NA)
  }
  if (all(c("Individual", "preysampleid") %in% names(mergedTable))){
    mergedTable$Prey <- apply(mergedTable[,.SD,.SDcol=c("Individual","preysampleid")],1,paste,collapse="-")
    mergedTable$Prey[is.na(mergedTable$preysampleid)] <- as.character(NA)
  }
  if (all(c("Individual", "tagid") %in% names(mergedTable))){
    mergedTable$Tag <- apply(mergedTable[,.SD,.SDcol=c("Individual","tagid")],1,paste,collapse="-")
    mergedTable$Tag[is.na(mergedTable$tagid)] <- as.character(NA)
  }
  if (all(c("Individual", "preylengthid") %in% names(mergedTable))){
    mergedTable$PreyLengthBin <- apply(mergedTable[,.SD,.SDcol=c("Individual","preylengthid")],1,paste,collapse="-")
    mergedTable$PreyLengthBin[is.na(mergedTable$preylengthid)] <- as.character(NA)
  }
  if (all(c("Individual", "copepodedevstage") %in% names(mergedTable))){
    mergedTable$PreyDevstageBin <- apply(mergedTable[,.SD,.SDcol=c("Individual","copepodedevstage")],1,paste,collapse="-")
    mergedTable$PreyDevstageBin[is.na(mergedTable$copepodedevstage)] <- as.character(NA)
  }
  
  return(mergedTable)
}

#' @noRd
prepNmdBioticMission <- function(BioticData){
  Mission <- BioticData$mission
  Mission <-data.table::setkeyv(Mission, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$mission[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["mission"]])])
  return(Mission)
}

#' @noRd
prepNmdBioticStation <- function(BioticData){
  Mission <- prepNmdBioticMission(BioticData)
  Station <- Mission[BioticData$fishstation]
  data.table::setkeyv(Station, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$fishstation[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["fishstation"]])])
  return(Station)
}

#' @noRd
prepNmdBioticSample <- function(BioticData){
  Station <- prepNmdBioticStation(BioticData)
  Sample <- Station[BioticData$catchsample]
  data.table::setkeyv(Sample, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$catchsample[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["catchsample"]])])
  return(Sample)
}

#' @noRd
prepNmdBioticIndividual <- function(BioticData){
  Sample <- prepNmdBioticSample(BioticData)
  Individual <- Sample[BioticData$individual]
  data.table::setkeyv(Individual, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])])
  
  ages <- merge(BioticData$individual, BioticData$agedetermination, by=RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])])
  ages <- ages[is.na(ages$preferredagereading) | (!is.na(ages$preferredagereading) & ages$preferredagereading==ages$agedeterminationid),]
  
  indids <- apply(ages[,.SD,.SDcol=RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])]], 1, paste, collapse="-")
  duplicates <- indids[duplicated(indids)]
  if (length(duplicates)>0){
    stop(paste("Data contains individuals with several age readings, but without indication of preferredage. E.g. age: ", duplicates[1], "..."))
  }
  ages <- ages[,.SD,.SDcol=names(BioticData$agedetermination)]
  data.table::setkeyv(ages, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])])
  
  tags <- merge(BioticData$individual, BioticData$tag, by=RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])])
  indids <- apply(tags[,.SD,.SDcol=RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])]], 1, paste, collapse="-")
  duplicates <- indids[duplicated(indids)]
  if (length(duplicates)>0){
    warning(paste("Data contains individuals with several tags. Tags have not been added for these individuals."))
  }
  tags <- tags[!(indids %in% duplicates),]
  tags <- tags[,.SD,.SDcol=names(BioticData$tag)]
  data.table::setkeyv(tags, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])])
  
  
  Individual <- ages[Individual]
  Individual <- tags[Individual]
  
  return(Individual)
}

#' @noRd
prepNmdBioticPrey <- function(BioticData){
  Sample <- prepNmdBioticSample(BioticData)
  Individual <- Sample[BioticData$individual]
  data.table::setkeyv(Individual, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])])
  Prey <- Individual[BioticData$prey]
  data.table::setkeyv(Prey, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$prey[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["prey"]])])
  return(Prey)
}

#' @noRd
prepNmdBioticTag <- function(BioticData){
  Sample <- prepNmdBioticSample(BioticData)
  Individual <- Sample[BioticData$individual]
  data.table::setkeyv(Individual, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])])
  Tag <- Individual[BioticData$tag]
  data.table::setkeyv(Tag, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$tag[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["tag"]])])
  return(Tag)
}

#' @noRd
prepNmdBioticMultipleReadings <- function(BioticData){
  Sample <- prepNmdBioticSample(BioticData)
  Individual <- Sample[BioticData$individual]
  data.table::setkeyv(Individual, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["individual"]])])
  AgeReadings <- Individual[BioticData$agedetermination]
  data.table::setkeyv(AgeReadings, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$agedetermination[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["agedetermination"]])])
  
  return(AgeReadings)
}

#' @noRd
prepNmdBioticPreyLengthFrequencies <- function(BioticData){
  Prey <- prepNmdBioticPrey(BioticData)
  LF <- Prey[BioticData$preylengthfrequencytable]
  data.table::setkeyv(LF, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$preylengthfrequencytable[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["preylengthfrequencytable"]])])
  return(LF)
}

#' @noRd
prepNmdBioticPreyDevstageFrequencies <- function(BioticData){
  Prey <- prepNmdBioticPrey(BioticData)
  CF <- Prey[BioticData$copepodedevstagefrequencytable]
  data.table::setkeyv(CF, RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$copepodedevstagefrequencytable[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["copepodedevstagefrequencytable"]])])
  return(CF)
}

#' @noRd
check_mission_duplicates <- function(BioticData){
  missionIds <- c()
  for (f in names(BioticData)){
    m <- apply(BioticData[[f]]$mission[,.SD,.SDcol=RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$mission[1:(RstoxData::xsdObjects$nmdbioticv3.xsd$prefixLens[["mission"]])]], 1, paste, collapse="-")
    missionIds <- c(missionIds, m)
  }
  duplicates <- missionIds[duplicated(missionIds)]
  if (length(duplicates)>0){
    stop(paste("'BioticData' contains duplicate missions:", paste(missionIds, collapse=", ")))
  }
}

#' PrepareNmdBioticTable
#' 
#' @description 
#'  Provides some standard flat tables for analysing NMDbiotic v 3.x data.
#'  
#' @details 
#'  NMDbiotic is a hierarchical data format with extendible reference lists, which is represented as a set of linked tables when red with \code{\link[RstoxData]{ReadBiotic}}.
#'  This function provides some standard merges and concatenations of these tables in order to provide simpler views of the data that fits standard tools for analysis and plotting in R.
#'  A detailed description of the format can be found at https://www.imr.no/formats/nmdbiotic/v3/. Note that this is not the same as the StoxBiotic-format.
#'  
#'  Different tables are prepared as specified by the argument 'TargetTable':
#'  \describe{
#'    \item{Mission}{The table 'mission'}
#'    \item{FishStation}{The table 'fishstaion', merged with 'mission'}
#'    \item{CatchSample}{The table 'catcsample', merged with 'fishstaton' and 'mission'}
#'    \item{Individual}{The table 'individual', merged with 'catchsample', 'fishstaton', 'mission', 'agedetermination' and 'tag'. Only the preferred age reading for each individual is kept from the table 'agedetermination' (see the field 'preferredagereading'). If an individual has multiple tags, a warning issued and no tags are added to the table for that individual.}
#'    \item{Prey}{The table 'prey', merged with individual', 'catchsample', 'fishstaton', 'mission' and 'agedetermination'. Only the preferred age reading for each individual is kept from the table 'agedetermination' (see the field 'preferredagereading')}
#'    \item{MultipleTags}{The table 'tag', merged with individual', 'catchsample', 'fishstaton', and 'mission.'}
#'    \item{MultipleReadings}{The table 'agedetermination', merged with 'individual', 'catchsample', 'fishstaton' and 'mission'}
#'    \item{PreyLengthFrequencies}{The table 'preylengthfrequencytable', merged with 'prey', 'individual', 'catchsample', 'fishstaton', 'mission' and 'agedetermination'. Only the preferred age reading for each individual is kept from the table 'agedetermination' (see the field 'preferredagereading')}
#'    \item{PreyDevstageFrequencies}{The table 'copepodedevstagefrequencytable', merged with 'prey', 'individual', 'catchsample', 'fishstaton', 'mission' and 'agedetermination'. Only the preferred age reading for each individual is kept from the table 'agedetermination' (see the field 'preferredagereading')}
#'  }
#'  Table names above refer to fields in the detailed description of the nmdbiotic format (https://www.imr.no/formats/nmdbiotic/v3/).
#'  
#'  Note that not all tables have corresponding registrations in lower tables. For instance a fishstation does not have to have any records on the individual-table.
#'  Preparing e.g. the table 'Individual' will discard any stations or catches that does not contain records for in the individual-table.
#'  
#'  If several age reading records are present for an individual, and the preferred age reading is not indicated in the data, the function will halt with an error.
#'  
#'  The different levels in NMDbiotic are addressable by a combination of fields. The id for an individual for instance, is only guaranteed to be unique within the sample (catchsample) it belongs to.
#'  Often it is convenient to construct identifiers that are unqiue across the entire data-set. This is provided by the argument 'addIds'. If 'addIds' is TRUE
#'  the columns CruiseKey (identifying mission), Station (identifying station) and Fishstation (identifying fishstation or haul) are added in the same
#'  manner that these fields are constructed when biotic data is converted to StoxBiotic (\code{\link[RstoxData]{StoxBiotic}}). 
#'  'FishStation' identifies station via serialnumber, which strictly speaking identifies a fishing operation, 
#'  but which is conventionally used to identify a station, since most stations have only one fishing operation.
#'  'Station' is a similar unique identifier that identifies actual stations in data sets where the field 'station' is used to distinguish station from fishing operation.
#'  In addition 'addIds' may add the columns 'CatchSample', 'Individual', 'Prey', 'Tag', 'AgeReading', 'PreyLengthBin', or 'PreyDevstageBin', which does not have a correspondance in StoxBiotic.
#'
#' @param BioticData A list of list of data.tables corresponding to the different levels of NMD-biotic as read for instance by \code{\link[RstoxData]{ReadBiotic}}.
#' @param TargetTable character with the name of the lowest table in the hierarchy to include.
#' @param addIds logical indicating whether to add convenient ids for the levels in NMDbiotic.
#'
#' @return a \code{\link[data.table]{data.table}} with columns as specified in the details section.
#'
#' @md
#' @export
#' 
PrepareNmdBioticTable <- function(
    BioticData, 
    TargetTable = c("Mission", "FishStation", "CatchSample", "Individual", "Prey", "MultipleTags", "MultipleReadings", "PreyLengthFrequencies", "PreyDevstageFrequencies"),
    addIds = FALSE
){
  reserved_names <- c("CruiseKey", "Station", "FishStation", "CatchSample", "Individual", "Prey", "Tag", "AgeReading", "PreyLengthBin", "PreyDevstageBin")
  
  is.NMDBiotic(BioticData)
  
  check_mission_duplicates(BioticData)
  
  if (addIds){
    check_reserved_names(BioticData, reserved_names)    
  }

  TargetTable <- match.arg(TargetTable, TargetTable)
  if (TargetTable == "Mission"){
    prepFunction <- prepNmdBioticMission
  }
  else if (TargetTable == "FishStation"){
    prepFunction <- prepNmdBioticStation
  }
  else if (TargetTable == "CatchSample"){
    prepFunction <- prepNmdBioticSample
  }
  else if (TargetTable == "Individual"){
    prepFunction <- prepNmdBioticIndividual
  }
  else if (TargetTable == "Prey"){
    prepFunction <- prepNmdBioticPrey
  }
  else if (TargetTable == "MultipleTags"){
    prepFunction <- prepNmdBioticTag
  }
  else if (TargetTable == "MultipleReadings"){
    prepFunction <- prepNmdBioticMultipleReadings
  }
  else if (TargetTable == "PreyLengthFrequencies"){
    prepFunction <- prepNmdBioticPreyLengthFrequencies
  }
  else if (TargetTable == "PreyDevstageFrequencies"){
    prepFunction <- prepNmdBioticPreyDevstageFrequencies
  }
  else{
    stop(paste0("Option '", TargetTable, "' is not supported for argument 'TargetTable'."))
  }
  
  output <- NULL
  for (f in names(BioticData)){
    if (is.null(output)){
      output <- prepFunction(BioticData[[f]])
    }
    else{
      keys <- data.table::key(output)
      output <- rbind(output, prepFunction(BioticData[[f]]))      
      data.table::setkeyv(output, keys)
    }
  }
  
  if (addIds){
    output <- addIds(output)
  }
  
  return(output)
} 
