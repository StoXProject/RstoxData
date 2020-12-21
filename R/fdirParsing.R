#' Parses landings (sales notes)
#' @description
#'  Parses sales notes data from the Norwegian Directorate of Fisheries (FDIR) on the LSS format
#' @details
#'  The LSS format is a pipe-separated format encoding landings (sales-notes).
#'  It is provided to IMR on a regular basis from FDIR.
#'  Column headers are in Norwegian.
#'
#'  Historically, columns in the landings provided from FDIR has been adapted for each data delivery
#'  Lately data deliveries has become standardized, but in order to support variants
#'  adherence to the standardization is not enforced by this function, unless option 'strict' is selected.
#'  If column names does not match specification, but data is otherwise parse-able, a warning will be issued.
#'  
#'  If 'strict' is not selection, data types may be inferred from data.
#'  The parameter 'guessMax' limits how many lines are inspected for data type inference (passed to \code{\link[readr]{read_delim}})
#' @param file path to file with LSS landings
#' @param encoding encoding for 'file'
#' @param guessMax passed to \code{\link[readr]{read_delim}}, unless 'strict' is true
#' @param strict enforce strict adherence to data format.
#' @return data.table with LSS landings
#' @importFrom data.table as.data.table
#' @export
readLssFile <- function(file, encoding="latin1", guessMax = 100000, strict=T){
  
  spec_land <- readr::cols(
    Dokumentnummer = readr::col_character(),
    `Dokumenttype (kode)` = readr::col_character(),
    Dokumenttype = readr::col_character(),
    `Dokument versjonsnummer` = readr::col_character(),
    `Dokument salgsdato` = readr::col_character(),
    `Dokument versjonstidspunkt` = readr::col_character(),
    `Salgslag ID` = readr::col_character(),
    `Salgslag (kode)` = readr::col_character(),
    Salgslag = readr::col_character(),
    `Mottakernasjonalitet (kode)` = readr::col_character(),
    Mottakernasjonalitet = readr::col_character(),
    Mottaksstasjon = readr::col_character(),
    `Landingskommune (kode)` = readr::col_character(),
    Landingskommune = readr::col_character(),
    `Landingsfylke (kode)` = readr::col_character(),
    Landingsfylke = readr::col_character(),
    `Landingsnasjon (kode)` = readr::col_character(),
    Landingsnasjon = readr::col_character(),
    Produksjonsanlegg = readr::col_character(),
    `Produksjonskommune (kode)` = readr::col_character(),
    Produksjonskommune = readr::col_character(),
    `Fiskerkommune (kode)` = readr::col_character(),
    Fiskerkommune = readr::col_character(),
    `Fiskernasjonalitet (kode)` = readr::col_character(),
    Fiskernasjonalitet = readr::col_character(),
    Fartoynavn = readr::col_character(),
    `Fartoy ID` = readr::col_character(),
    `Registreringsmerke (seddel)` = readr::col_character(),
    `Radiokallesignal (seddel)` = readr::col_character(),
    `Storste lengde` = readr::col_double(),
    `Lengdegruppe (kode)` = readr::col_character(),
    Lengdegruppe = readr::col_character(),
    `Bruttotonnasje 1969` = readr::col_double(),
    `Bruttotonnasje annen` = readr::col_double(),
    Byggear = readr::col_integer(),
    Ombyggingsar = readr::col_integer(),
    Motorkraft = readr::col_double(),
    Motorbyggear = readr::col_integer(),
    `Fartoy gjelder fra dato` = readr::col_character(),
    `Fartoy gjelder til dato` = readr::col_character(),
    `Fartoytype (kode)` = readr::col_character(),
    Fartoytype = readr::col_character(),
    `Kvotefartoy reg.merke` = readr::col_character(),
    `Fartoykommune (kode)` = readr::col_character(),
    Fartoykommune = readr::col_character(),
    `Fartoyfylke (kode)` = readr::col_character(),
    Fartoyfylke = readr::col_character(),
    `Fartoynasjonalitet (kode)` = readr::col_character(),
    Fartoynasjonalitet = readr::col_character(),
    `Mottakende fartoy reg.merke` = readr::col_character(),
    `Mottakende fartoy rkal` = readr::col_character(),
    `Mottakende fartoytype (kode)` = readr::col_character(),
    `Mottakende fart.type` = readr::col_character(),
    `Mottakende fartoynasj. (kode)` = readr::col_character(),
    `Mottakende fart.nasj` = readr::col_character(),
    Fangstar = readr::col_integer(),
    `Siste fangstdato` = readr::col_date(format="%d.%m.%Y"),
    `Kvotetype (kode)` = readr::col_character(),
    Kvotetype = readr::col_character(),
    `Redskap (kode)` = readr::col_character(),
    Redskap = readr::col_character(),
    `Redskap - hovedgruppe (kode)` = readr::col_character(),
    `Redskap - hovedgruppe` = readr::col_character(),
    `Fangstfelt (kode)` = readr::col_character(),
    `Kyst/hav (kode)` = readr::col_character(),
    `Hovedomrade (kode)` = readr::col_character(),
    Hovedomrade = readr::col_character(),
    `Lokasjon (kode)` = readr::col_character(),
    `Sone (kode)` = readr::col_character(),
    Sone = readr::col_character(),
    Omradegruppering = readr::col_character(),
    `Hovedomrade FAO (kode)` = readr::col_character(),
    `Hovedomrade FAO` = readr::col_character(),
    `Nord/sor for 62 grader nord` = readr::col_character(),
    `Fangstdagbok (nummer)` = readr::col_character(),
    `Fangstdagbok (turnummer)` = readr::col_character(),
    Landingsdato = readr::col_character(),
    Landingsklokkeslett = readr::col_character(),
    `Dellanding (signal)` = readr::col_character(),
    `Neste mottaksstasjon` = readr::col_character(),
    `Forrige mottakstasjon` = readr::col_character(),
    Linjenummer = readr::col_integer(),
    `Art - FDIR (kode)` = readr::col_character(),
    `Art - FDIR` = readr::col_character(),
    `Art - gruppe (kode)` = readr::col_character(),
    `Art - gruppe` = readr::col_character(),
    `Art - hovedgruppe (kode)` = readr::col_character(),
    `Art - hovedgruppe` = readr::col_character(),
    `Art FAO (kode)` = readr::col_character(),
    `Art FAO` = readr::col_character(),
    `Produkttilstand (kode)` = readr::col_character(),
    Produkttilstand = readr::col_character(),
    `Konserveringsmate (kode)` = readr::col_character(),
    Konserveringsmate = readr::col_character(),
    `Landingsmate (kode)` = readr::col_character(),
    Landingsmate = readr::col_character(),
    `Kvalitet (kode)` = readr::col_character(),
    Kvalitet = readr::col_character(),
    `Storrelsesgruppering (kode)` = readr::col_character(),
    `Anvendelse (kode)` = readr::col_character(),
    Anvendelse = readr::col_character(),
    `Anvendelse hovedgruppe (kode)` = readr::col_character(),
    `Anvendelse hovedgruppe` = readr::col_character(),
    `Antall stykk` = readr::col_integer(),
    Bruttovekt = readr::col_double(),
    Produktvekt = readr::col_double(),
    Rundvekt = readr::col_double()
  )
  names(spec_land$cols)[26] <- "Fart\u00F8ynavn"
  names(spec_land$cols)[27] <- "Fart\u00F8y ID"
  names(spec_land$cols)[30] <- "St\u00F8rste lengde"
  names(spec_land$cols)[35] <- "Bygge\u00E5r"
  names(spec_land$cols)[36] <- "Ombyggings\u00E5r"
  names(spec_land$cols)[38] <- "Motorbygge\u00E5r"
  names(spec_land$cols)[39] <- "Fart\u00F8y gjelder fra dato"
  names(spec_land$cols)[40] <- "Fart\u00F8y gjelder til dato"
  names(spec_land$cols)[41] <- "Fart\u00F8ytype (kode)"
  names(spec_land$cols)[42] <- "Fart\u00F8ytype"
  names(spec_land$cols)[43] <- "Kvotefart\u00F8y reg.merke"
  names(spec_land$cols)[44] <- "Fart\u00F8ykommune (kode)"
  names(spec_land$cols)[45] <- "Fart\u00F8ykommune"
  names(spec_land$cols)[46] <- "Fart\u00F8yfylke (kode)"
  names(spec_land$cols)[47] <- "Fart\u00F8yfylke"
  names(spec_land$cols)[48] <- "Fart\u00F8ynasjonalitet (kode)"
  names(spec_land$cols)[49] <- "Fart\u00F8ynasjonalitet"
  names(spec_land$cols)[50] <- "Mottakende fart\u00F8y reg.merke"
  names(spec_land$cols)[51] <- "Mottakende fart\u00F8y rkal"
  names(spec_land$cols)[52] <- "Mottakende fart\u00F8ytype (kode)"
  names(spec_land$cols)[54] <- "Mottakende fart\u00F8ynasj. (kode)"
  names(spec_land$cols)[56] <- "Fangst\u00E5r"
  names(spec_land$cols)[66] <- "Hovedomr\u00E5de (kode)"
  names(spec_land$cols)[67] <- "Hovedomr\u00E5de"
  names(spec_land$cols)[71] <- "Omr\u00E5degruppering"
  names(spec_land$cols)[72] <- "Hovedomr\u00E5de FAO (kode)"
  names(spec_land$cols)[73] <- "Hovedomr\u00E5de FAO"
  names(spec_land$cols)[74] <- "Nord/s\u00F8r for 62 grader nord"
  names(spec_land$cols)[93] <- "Konserveringsm\u00E5te (kode)"
  names(spec_land$cols)[94] <- "Konserveringsm\u00E5te"
  names(spec_land$cols)[95] <- "Landingsm\u00E5te (kode)"
  names(spec_land$cols)[96] <- "Landingsm\u00E5te"
  names(spec_land$cols)[99] <- "St\u00F8rrelsesgruppering (kode)"
  
  loc <- readr::default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- encoding
  if (strict){
    headers <- names(readr::read_delim(file, delim="|", col_names=T, col_types=paste(rep("c",107), collapse=""), trim_ws=TRUE, na=c("", "na", "NA"), locale=loc, n_max = 1))
    
    if (length(headers) != length(spec_land$cols)){
      stop("Number of columns in file does not match specification.")
    }
    if (!all(headers == names(spec_land$cols))){
      differences <- sum(headers != names(spec_land$cols))
      warning(paste("StoX: Header names does not match specification,", differences, "column names differ."))
    }
      
    db <- readr::read_delim(file, delim="|", col_names=names(spec_land$cols), trim_ws=TRUE, na=c("", "na", "NA"), locale=loc, col_types = spec_land, skip = 1) 
    db <- data.table::as.data.table(db) 
    db$`Siste fangstdato` <- as.POSIXct(db$`Siste fangstdato`)
  }
  else{
    db <- readr::read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"), locale=loc, guess_max = guessMax)    
    db <- data.table::as.data.table(db) 
  }
  return(db)
}


#' Read pipe separated file with specified columns
#' @noRd
read_psv <- function(file, encoding, col_types){
  loc <- readr::default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- encoding
  db <- readr::read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"),locale=loc, col_types = col_types)
  return(db)
}

#' Parses logbooks (ERS) 
#' @description 
#'  Parses electronic logbooks (ERS) from tabular format delivered by Directorate of Fisheries (FDIR)
#' @details 
#'  The format is a pipe-separated format encoding aggregated ERS records (logbooks).
#'  It is provided to IMR on a regular basis from FDIR.
#'  Column headers are in Norwegian.
#' @param file path to file
#' @param encoding encoding for 'file'
#' @return data.table() with logbooks
#' @export
readErsFile <- function(file, encoding="latin1"){
  
  spec_log <- readr::cols(
    RC = readr::col_character(),
    REGM = readr::col_character(),
    STORSTE_LENGDE = readr::col_double(),
    BRUTTOTONNASJE = readr::col_integer(),
    MOTORKRAFT = readr::col_integer(),
    TM1 = readr::col_character(),
    AKTIVITET_KODE = readr::col_character(),
    AKTIVITET = readr::col_character(),
    PUMPET_FRA = readr::col_character(),
    FANGSTAR = readr::col_integer(),
    STARTTIDSPUNKT = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    START_LT = readr::col_double(),
    START_LG = readr::col_double(),
    SONE = readr::col_character(),
    KVOTETYPE_KODE = readr::col_character(),
    KVOTETYPE = readr::col_character(),
    REDSKAP_FAO = readr::col_character(),
    REDSKAP_NS = readr::col_character(),
    REDSKAP = readr::col_character(),
    REDSKAPSSPESIFIKASJON_KODE = readr::col_character(),
    REDSKAPSSPESIFIKASJON = readr::col_character(),
    MASKEVIDDE = readr::col_integer(),
    REDSKAP_PROBLEMER_KODE = readr::col_character(),
    REDSKAP_PROBLEMER = readr::col_character(),
    STOPPTIDSPUNKT = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    STOPP_LT = readr::col_double(),
    STOPP_LG = readr::col_double(),
    VARIGHET = readr::col_integer(),
    INNSATS = readr::col_number(),
    SILD_BESTAND_KODE = readr::col_character(),
    SILD_BESTAND_NS = readr::col_character(),
    SILD_BESTAND = readr::col_character(),
    HOVEDART_FAO = readr::col_character(),
    HOVEDART_NS = readr::col_character(),
    HOVEDART = readr::col_character(),
    INT_OMR_GML_START = readr::col_character(),
    INT_OMR_NY_START = readr::col_character(),
    INT_OMR_GML_STOPP = readr::col_character(),
    INT_OMR_NY_STOPP = readr::col_character(),
    HAV_DYBDE_START = readr::col_number(),
    HAV_DYBDE_STOPP = readr::col_number(),
    LOKASJON_START = readr::col_character(),
    LOKASJON_STOPP = readr::col_character(),
    TREKK_AVSTAND_METER = readr::col_integer(),
    FANGSTART_FAO = readr::col_character(),
    FANGSTART_NS = readr::col_character(),
    FANGSTART = readr::col_character(),
    RUNDVEKT = readr::col_double()
  )
  names(spec_log$cols) <- c(names(spec_log$cols)[1:2], "ST\u00D8RSTE_LENGDE", names(spec_log$cols)[4:9], "FANGST\u00C5R", names(spec_log$cols)[11:length(spec_log$cols)])
  
  logb <- read_psv(file, encoding, col_types=spec_log)
  
  return(data.table::as.data.table(logb))
}
