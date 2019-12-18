#' Check if argument is LandingData
#' @description 
#'  Checks if argument conforms to specification for \code{\link[RstoxData]{LandingData}}
#' @param LandingData argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxData]{LandingData}}
#' @name is.LandingData
#' @export
is.LandingData <- function(LandingData){

  if (!is.list(LandingData)){
    return(FALSE)
  }
  if (!("Seddellinje" %in% names(LandingData))){
    return(FALSE)
  }
  if (!data.table::is.data.table(LandingData$Seddellinje)){
    return(FALSE)
  }
  if (!all(c("Dokumentnummer", 
             "Linjenummer", 
             "Art_kode", 
             "Registreringsmerke_seddel", 
             "SisteFangstdato", 
             "Redskap_kode")
              %in% names(LandingData$Seddellinje))){
    return(FALSE)
  }
  
  return(TRUE)
}


loadResource <- function(name){
  
  if (name == "gear"){
    filename = "gear.csv"
    col_types = "ccc"
  }
  else if (name == "coastal"){
    filename = "coastal.csv"
    col_types = "cc"
  }
  else if (name == "n62"){
    filename = "n62.csv"
    col_types = "cc"
  }
  else if (name == "usage"){
    filename = "usage.csv"
    col_types = "ccc"
  }
  else{
    stop(paste("Resource", name, "not recognized"))
  }

  loc <- readr::locale()
  loc$encoding <- "UTF-8"
  return(readr::read_delim(system.file("extdata","codeDescriptions", filename, package="RstoxData"), delim = "\t", locale = loc, col_types = col_types))
  
}

#' Convert landing data
#' @description
#'  StoX function
#'  Convert landing data to the aggregated format \code{\link[RstoxData]{StoxLandingData}}
#' @param LandingData Sales-notes data. See \code{\link[RstoxData]{LandingData}}
#' @return \code{\link[RstoxData]{StoxLandingData}}, aggregated sales-notes data.
#' @name StoxLanding
#' @export
StoxLanding <- function(LandingData){
  
  flatLandings <- merge(LandingData$Seddellinje, LandingData$Fangstdata)
  flatLandings <- merge(flatLandings, LandingData$Art)
  flatLandings <- merge(flatLandings, LandingData$Produkt)
  flatLandings <- merge(flatLandings, LandingData$Mottaker)

  #
  # Note: if non-character columns are added to aggColumns. Handle accoridngly in NA-aggregation below
  #
  aggColumns <- c("ArtFAO_kode", 
                  "Art_kode", 
                  "Art_bokm\u00E5l", 
                  "Fangst\u00E5r", 
                  "SisteFangstdato", 
                  "Redskap_kode", 
                  "Hovedomr\u00E5de_kode", 
                  "Lokasjon_kode",
                  "Omr\u00E5degruppering_bokm\u00E5l", 
                  "KystHav_kode", 
                  "NordS\u00F8rFor62GraderNord", 
                  "St\u00F8rsteLengde", 
                  "Fart\u00F8ynasjonalitet_kode",
                  "Mottaksstasjon",
                  "Mottakernasjonalitet_kode",
                  "HovedgruppeAnvendelse_kode")
  
  flatLandings <- flatLandings[,c(aggColumns, "Rundvekt"), with=F]

  aggList <- list()
  for (aggC in aggColumns){
    flatLandings[[aggC]][is.na(flatLandings[[aggC]])] <- "<NA>" #set NAs to text-string for aggregation
    aggList[[aggC]] <- flatLandings[[aggC]]
  }
  names(aggList) <- aggColumns
  
  aggLandings <- stats::aggregate(list(Rundvekt=flatLandings$Rundvekt), by=aggList, FUN=function(x){sum(x, na.rm=T)})
  aggLandings <- aggLandings[,c(aggColumns, "Rundvekt")]
  
  
  #reset NAs
  for (aggC in aggColumns){
    aggLandings[[aggC]][aggLandings[[aggC]] == "<NA>"] <- NA
  }
  
  # rename headers
  names(aggLandings) <- c("speciesFAOCommercial",
                           "speciesCategoryCommercial",
                           "commonNameCommercial",
                           "year",
                           "catchDate",
                           "gear",
                           "area",
                           "location",
                           "icesAreaGroup",
                           "coastal",
                           "n62Code",
                           "vesselLength",
                           "countryVessel",
                           "landingSite",
                           "countryLanding",
                           "usage",
                           "weight"
                           )
  
  
  gear <- loadResource("gear")[,c("gear", "gearDescription")]
  aggLandings <- merge(aggLandings, gear, all.x=T, by="gear")
  usage <- loadResource("usage")[,c("usage", "usageDescription")]
  aggLandings <- merge(aggLandings, usage, all.x=T, by="usage")
  coastal <- loadResource("coastal")[,c("coastal", "coastalDescription")]
  aggLandings <- merge(aggLandings, coastal, all.x=T, by="coastal")
  n62 <- loadResource("n62")[,c("n62Code", "n62Description")]
  aggLandings <- merge(aggLandings, n62, all.x=T, by="n62Code")
  
  # format conversions
  cd <- as.POSIXct(aggLandings$catchDate, format="%d.%m.%Y")
  attributes(cd)$tzone <- "UTC"
  aggLandings$catchDate <- as.POSIXct(substr(as.character(cd),1,10), format="%Y-%m-%d", tzone="UTC")
  
  aggLandings$vesselLength[aggLandings$vesselLength == "<NA>"] <- NA
  aggLandings$vesselLength <- as.numeric(aggLandings$vesselLength)
  
  aggLandings$year[aggLandings$year == "<NA>"] <- NA
  aggLandings$year <- as.integer(aggLandings$year)
  
  returnOrder <- c("speciesFAOCommercial",
                   "speciesCategoryCommercial",
                   "commonNameCommercial",
                   "year",
                   "catchDate",
                   "gear",
                   "gearDescription",
                   "area",
                   "location",
                   "icesAreaGroup",
                   "coastal",
                   "coastalDescription",
                   "n62Code",
                   "n62Description",
                   "vesselLength",
                   "countryVessel",
                   "landingSite",
                   "countryLanding",
                   "usage",
                   "usageDescription",
                   "weight")
  
  return(data.table::as.data.table(aggLandings[,returnOrder]))
}

#' Check if argument is StoxLandingData
#' @description 
#'  Checks if argument conforms to specification for \code{\link[RstoxData]{StoxLandingData}}
#' @param StoxLandingData argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxData]{StoxLandingData}}
#' @name is.StoxLandingData
#' @export
is.StoxLandingData <- function(StoxLandingData){
  
  expected_colums <- c("speciesFAOCommercial",
                       "speciesCategoryCommercial",
                       "commonNameCommercial",
                       "year",
                       "catchDate",
                       "gear",
                       "gearDescription",
                       "area",
                       "location",
                       "icesAreaGroup",
                       "coastal",
                       "coastalDescription",
                       "n62Code",
                       "n62Description",
                       "vesselLength",
                       "countryVessel",
                       "landingSite",
                       "countryLanding",
                       "usage",
                       "usageDescription",
                       "weight"
  )
  
  if (!data.table::is.data.table(StoxLandingData)){
    return(FALSE)
  }
  
  if (!all(expected_colums %in% names(StoxLandingData))){
    return(FALSE)
  }
  
  return(TRUE)
}
