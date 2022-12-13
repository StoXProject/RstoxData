


#' @noRd
writeXmlDeclaration <- function(stream, version, encoding, standalone){
  
  if (standalone){
    standalone = "yes"
  }
  else{
    standalone = "no"
  }
  
  cat(paste0("<?xml version=\"", version, "\" encoding=\"", encoding, "\" standalone=\"", standalone, "\"?>\n"), file=stream)
}

#' @noRd
openTag <- function(stream, tagname, attributes=NULL, indent=""){
  
  tagstring <- paste0(indent, "<",tagname)
  if (length(attributes)){
    #stopifnot(nrow(attributes)==1)
    
    for (n in names(attributes)){
      if (!is.na(attributes[[n]][[1]])){
        tagstring <- paste(tagstring, paste0(n,"=\"",attributes[[n]][[1]],"\""))
      }
    }
  }
  tagstring <- paste0(tagstring, ">")
  writeLines(tagstring, con=stream, useBytes = T)
}

#' @noRd
closeTag <- function(stream, tagname, indent=""){
  tagstring <- paste0(indent, "</",tagname, ">")
  writeLines(tagstring, con=stream, useBytes = T)
}

#' @noRd
writeSimpleTags <- function(stream, tags, indent=""){
  string <- ""
  for (n in names(tags)){
    if (!is.na(tags[[n]][[1]])){
      string <- paste0(string, "<",n,">",tags[[n]][[1]],"</",n,">")
    }
  }
  writeLines(paste0(indent, string), con=stream, useBytes = T)
}


writeLevel <- function(stream, data, level, parentKeys, xsdObject, indent = "", namespace = "", keepEmptyLevels = FALSE){
  
  # Get the data to write:
  leveldata <- data[[level]]
  if (NROW(leveldata) && length(parentKeys)){
    leveldata <- leveldata[as.list(parentKeys), on = names(parentKeys), nomatch = NULL]
  }
  
  # Identify children:
  children <- xsdObject$treeStruct[[level]]
  
  # get keys
  keys <- names(leveldata)[seq_len(xsdObject$prefixLens[[level]])]
  attribsNames <- keys
  if (!is.null(parentKeys)){
    attribsNames <- attribsNames[!(attribsNames %in% names(parentKeys))]
  }
  
  if(level == xsdObject$root) {
    rootAttribs <- c(xmlns = namespace)
  }
  else {
    rootAttribs <- NULL
  }
  # write opening tag and attributes
  if(NROW(leveldata)) {
    for (i in seq_len(nrow(leveldata))){
      openTag(stream, level, c(leveldata[i,.SD, .SDcols = attribsNames], rootAttribs), indent)
      
      # write simple element tags
      simpletags <- xsdObject$tableHeaders[[level]][!(xsdObject$tableHeaders[[level]] %in% keys)]
      writeSimpleTags(stream, leveldata[i, .SD, .SDcols = simpletags], paste0(indent, "\t"))
      
      # write complex element tags
      for (ch in children){
        writeLevel(stream, data, ch, leveldata[i, .SD, .SDcols = keys], xsdObject, paste0(indent, "\t"), keepEmptyLevels = keepEmptyLevels)
      }
      
      # write closing tag
      closeTag(stream, level, indent)
    }
  }
  else if(keepEmptyLevels || length(rootAttribs)){
    openTag(stream, level, rootAttribs, indent)
    for (sub in xsdObject$treeStruct[[level]]){
      writeLevel(stream, data, sub, NULL, xsdObject, paste0(indent, "\t"), keepEmptyLevels = keepEmptyLevels)
    }
    closeTag(stream, level, indent)
    #return()
  }
  
}


#' converts everything to UTF-8 character before XML writing
#' @noRd
typeConvert <- function(dataTables, xsdObject){
  
  conv <- function(x){
    return(enc2utf8(x))
  }
  
  for (n in names(xsdObject$tableTypes)){
    if(n %in% names(dataTables)){
      if (nrow(dataTables[[n]])>0){
        for (i in 1:length(xsdObject$tableHeaders[[n]])){
          name <- xsdObject$tableHeaders[[n]][[i]]
          xsdType <- xsdObject$tableTypes[[n]][[i]]
          
          if(!(name %in% names(dataTables[[n]]))){
            stop(paste("Column", name, "not found in data tables. Possible mismatch with xsdObject."))
          }
          
          if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:string"){
            
          }
          else if (is.integer(dataTables[[n]][[name]]) & xsdType == "xs:long"){
            stopifnot(all(is.na(dataTables[[n]][[name]]) | dataTables[[n]][[name]] >= -9223372036854775808))
            stopifnot(all(is.na(dataTables[[n]][[name]]) | dataTables[[n]][[name]] <= 9223372036854775807))
            dataTables[[n]][[name]] <- conv(as.character(dataTables[[n]][[name]]))
          }
          else if (is.numeric(dataTables[[n]][[name]]) & xsdType == "xs:string"){
            dataTables[[n]][[name]] <- conv(as.character(dataTables[[n]][[name]]))
          }
          else if (is.logical(dataTables[[n]][[name]]) & xsdType == "xs:string"){
            dataTables[[n]][[name]] <- conv(as.character(dataTables[[n]][[name]]))
          }
          else if (is.logical(dataTables[[n]][[name]]) & xsdType == "xs:integer"){
            dataTables[[n]][[name]] <- as.character(as.integer(dataTables[[n]][[name]]))
          }
          else if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:integer"){
            dataTables[[n]][[name]] <- as.character(as.integer(dataTables[[n]][[name]]))
          }
          else if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:long"){
            dataTables[[n]][[name]] <- as.character(as.integer(dataTables[[n]][[name]]))
          }
          else if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:integer"){
            dataTables[[n]][[name]] <- as.character(as.integer(dataTables[[n]][[name]]))
          }
          else if (is.integer(dataTables[[n]][[name]]) & xsdType == "xs:integer"){
            dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
          }
          else if (is.numeric(dataTables[[n]][[name]]) & xsdType == "xs:decimal"){
            dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
          }
          else if (is.numeric(dataTables[[n]][[name]]) & xsdType == "xs:double"){
            dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
          }
          else if (is.numeric(dataTables[[n]][[name]]) & xsdType == "xs:integer"){
            dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
          }
          else if (is.numeric(dataTables[[n]][[name]]) & xsdType == "xs:long"){
            dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
          }
          
          else if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:date"){
            ok <- is.na(dataTables[[n]][[name]])
            ok <- ok | !is.na(as.POSIXct(dataTables[[n]][[name]], format="%Y-%m-%d"))
            if (!all(ok)){
              warning("Data type conversion from character to xs:date is not configured for some date formats in data.")
            }
          }
          else if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:time"){
            ok <- is.na(dataTables[[n]][[name]])
            ok <- ok | !is.na(as.POSIXct(dataTables[[n]][[name]], format="%H:%M:%S"))
            if (!all(ok)){
            	warning("Data type conversion from character to xs:time is not configured for some time formats in data.")
            }
          }
          else{
          	warning("Data type conversion from ", class(dataTables[[n]][[name]]), " to ", xsdType, " for variable ", name, " of table ", n, " is not configured")
          }
        }
      }
      else{
        for (coln in names(dataTables[[n]])){
          dataTables[[n]][[coln]] <- conv(as.character(dataTables[[n]][[coln]]))
        }
      } 
    }
  }
  
  return(dataTables)
}

#' @noRd
setKeysDataTables <- function(dataTables, xsdObject){
  for (dt in names(dataTables)){
    if (length(xsdObject$tableHeaders[[dt]])>0){
      data.table::setkeyv(dataTables[[dt]], xsdObject$tableHeaders[[dt]][1:xsdObject$prefixLens[[dt]]])
    }
  }
  return(dataTables)
}

#' Insert C_DATA in chr columns that will be mapped to text nodes of elements,
#' if they need it (contain reserved characters, or other characters that are useful to escape)
#' @noRd
insertcdata <- function(dataTables, xsdObject){
  for (dt in names(dataTables)){
    if (ncol(dataTables[[dt]]) > 0 && dt != "metadata"){
      ns <- names(dataTables[[dt]])
      ns <- ns[xsdObject$prefixLens[[dt]]:length(ns)]
      
      for (coln in ns){
        if ("character" %in% class(dataTables[[dt]][[coln]])){
          #reserved characters
          indLt <- grepl("<", dataTables[[dt]][[coln]], fixed=T)
          indGt <- grepl(">", dataTables[[dt]][[coln]], fixed=T)
          indAmp <- grepl("&", dataTables[[dt]][[coln]], fixed=T)
          
          #others that are useful to escape
          indQm <- grepl("?", dataTables[[dt]][[coln]], fixed=T)
          indPc <- grepl("%", dataTables[[dt]][[coln]], fixed=T)
          
          mask <- indLt | indGt | indAmp | indQm | indPc
          if (any(mask)){
            dataTables[[dt]][[coln]][mask] <- paste("<![CDATA[", dataTables[[dt]][[coln]][mask], "]]>", sep="")
          }
        } 
      }
    }
  }
  return(dataTables)
}

#' Generic xml writer
#' @description
#'  Support generic writing of xml formats parsed by RstoxData.
#'  Does not preserve ordering of elements, but order by keys.
#' @details
#'  The file is written without namespace prefixes, and requires all names to specified by the same namespace.
#'  This function is applicable when the relational input and the xml format adheres to certain conditions,
#'  specified below. These conditions are met by biotic and landing, but not by e.g. ICES acoustic.
#'
#'  Conditions for relational input 'dataTables'
#'  \itemize{
#'   \item{xsdobject specifies the keys for each table by the 'n' leftmost columns, where 'n' is given in 'prefixLens'}
#'   \item{NAs does only occur for optional elements}
#'  }
#'
#'  Conditions for hierarchical XML format:
#'  \itemize{
#'   \item{The root node has no attributes}
#'   \item{All node names are unique across all levels}
#'   \item{All keys are attributes}
#'   \item{Only keys are attributes}
#'   \item{The xsdobject specifies names of attributes and elements, and any constraint on their order in the xml format in 'tableHeaders'}
#'   \item{The xsdobject specifies names of complex type elements, and any constraints on their order in 'tableOrder'}
#'   \item{The xml-format either does not constrain the order of elements or simple types always preceed complex types.}
#'  }
#'
#' @param fileName filename to write xml to
#' @param dataTable relational structure to write as XML, as parsed by readXmlFile
#' @param xsdObject specification for xml format, e.g xsdObjects$nmdbioticv3.1.xsd
#' @param namespace namespace for the xml format
#' @param encoding specifices the encoding (charset)
#' @param xmlStandard specifies the xml version used
#' @noRd
writeXmlFile <- function(fileName, dataTables, xsdObject, namespace, encoding="UTF-8", xmlStandard="1.0", keepEmptyLevels = FALSE){
  
  # Notes for development:
  # consider adding XML version to xsdObjects
  # consider adding information about which columns are attributes / elements in xsdObjects
  # consider adding ordering information about all elements in xsdObjects
  # (including the relative ordering of complex and simple elements)
  # consider adding information about key structure in xsdObjects
  
  if (encoding != "UTF-8"){
    stop(paste("Encoding", encoding, "is not supported."))
  }
	
	
  dataTables <- insertcdata(dataTables, xsdObject)
  dataTables <- typeConvert(dataTables, xsdObject)
  # Disabled this on 2022-09-01, as it reorders so that what is written and read back in differs from the original:
  #dataTables <- setKeysDataTables(dataTables, xsdObject)
  
  stream = file(fileName, open="w", encoding="native.enc")
  writeXmlDeclaration(stream, version=xmlStandard, encoding=encoding, standalone=T)
  writeLevel(stream, dataTables, xsdObject$root, NULL, xsdObject, "", namespace, keepEmptyLevels = keepEmptyLevels)    
    close(stream)
  
}

#' write landings xml with data.table-routines for writing tabular data
#' @noRd
fWriteLandings <- function(fileName, dataTables, namespace="http://www.imr.no/formats/landinger/v2", encoding="UTF-8", xmlStandard="1.0"){
  
  #
  # temporary encoding restriction
  #
  # Better encoding support fordata.table::fwrite is expected in future releases of data.table
  # see https://github.com/Rdatatable/data.table/pull/4785
  #
  # Set dependency to appropriate data.table version and remove the following checks
  #
  
  if (encoding != "UTF-8"){
    stop(paste("Encoding", encoding, "is not supported."))
  }
  if (!l10n_info()[["UTF-8"]]){
    stop("R must be set to a UTF-8 locale to write UTF-8 encoded files with 'fWriteLandings'")
  }
  
  #
  # /temporary encoding restriction
  #
  
  if (namespace=="http://www.imr.no/formats/landinger/v2"){
    xsdObject <- xsdObjects$landingerv2.xsd
  }
  else{
    stop(paste("Namespace", namespace, "not supported."))
  }
  
  dataTables <- insertcdata(dataTables, xsdObject)
  dataTables <- typeConvert(dataTables, xsdObject)
  
  # write prequel and opening tag
  stream = file(fileName, open="w", encoding=encoding)
  writeXmlDeclaration(stream, version=xmlStandard, encoding=encoding, standalone=T)
  writeLines(paste("<Landingsdata xmlns=\"", namespace, "\">", sep=""), stream)
  close(stream)
  
  #
  # manipulate data frame to include xml markup for all non-root levels
  # for landings they are all the same number of rows
  #
  
  keys <- xsdObject$tableHeaders$Seddellinje[1:xsdObject$prefixLens[["Seddellinje"]]]
  output <- dataTables$Seddellinje[, names(dataTables$Seddellinje)[names(dataTables$Seddellinje) %in% keys], with=F]
  
  # open seddellinje
  namesSL <- names(output)
  output$SLopen <- "<Seddellinje"
  output <- output[,c("SLopen", namesSL), with=F]
  
  for (k in keys){
    output[[k]][is.na(output[[k]])] <- ""
    output[[k]] <- paste(k, "=\"", output[[k]], "\"", sep="")
  }
  output[[k]] <- paste(output[[k]], ">", sep="")
  
  nonkeys <- xsdObject$tableHeaders$Seddellinje[(xsdObject$prefixLens[["Seddellinje"]]+1):length(xsdObject$tableHeaders$Seddellinje)]
  for (k in nonkeys){
    output[[k]][!is.na(dataTables$Seddellinje[[k]])] <- paste("<",k,">",dataTables$Seddellinje[[k]][!is.na(dataTables$Seddellinje[[k]])],paste("</",k,">",sep=""),sep="")
  }
  
  #
  # do all other levels
  #
  
  otherLevels <- names(xsdObject$treeStruct)[!(names(xsdObject$treeStruct) %in% c("Landingsdata", "Seddellinje"))]
  for (l in otherLevels){
    output[[paste("open", l)]] <- paste("<",l,">", sep="")
    nonkeys <- xsdObject$tableHeaders[[l]][!(xsdObject$tableHeaders[[l]] %in% keys)]
    for (k in nonkeys){
      output[[paste("open",k)]][!is.na(dataTables[[l]][[k]])] <- paste("<",k,">",dataTables[[l]][!is.na(dataTables[[l]][[k]])][[k]],"</",k,">",sep="")
    }
    output[[paste("close", l)]] <- paste("</",l,">", sep="")
  }
  
  output$SLclose <- "</Seddellinje>"
  
  data.table::fwrite(output, fileName, append = T, sep=" ", quote = F)
  
  #close opening tag
  stream = file(fileName, open="a", encoding=encoding)
  writeLines("</Landingsdata>", stream)
  close(stream)
  
}

#' Write landing
#' @description 
#'  Write landing data as XML file(s).
#' @details
#'  \code{\link[RstoxData]{LandingData}} may contain several data sets.
#'  In that case the parameters 'FileNames' and 'namespaces' must be provided for each
#'  in the order they appear in 'LandingData'.
#' @param LandingData \code{\link[RstoxData]{LandingData}} data to write.
#' @param FileNames paths to files that should be written to
#' @param namespaces XML namespaces to use for formatting.
#' @param encoding encoding to use for writing files
#' @param overwrite whether to overwrite any existing file(s)
#' @noRd
WriteLanding <- function(LandingData, FileNames, namespaces=NULL, encoding="UTF-8", overwrite=F){
  
  if (!is.LandingData(LandingData)){
    stop("'LandingData' is not a 'LandingData object' see '?RstoxData::LandingData.")
  }
  
  #
  # set default namespace if not specified
  #
  if (!length(namespaces)){
    namespaces <- c()
    for (l in LandingData){
      if (l$metadata$useXsd == "landingerv2"){
        namespaces <- c(namespaces, "http://www.imr.no/formats/landinger/v2")
      }
      else{
        stop(paste("Does not recognize namespace for", l$metadata$useXsd, "Provide explicit namespace."))
      }
    }
  }
  
  
  if (!length(LandingData) == length(FileNames)){
    stop("Provide exactly one file name and one namespace for each data set in 'LandingData'")
  }
  if (!length(LandingData) == length(namespaces)){
    stop("Provide exactly one file name and one namespace for each data set in 'LandingData'")
  }
  
  #
  # write files
  #
  for (i in 1:length(LandingData)){
    
    FileName <- FileNames[[i]]
    data <- LandingData[[i]] 
    namespace <- namespaces[[i]]
    
    if (file.exists(FileName) & !overwrite){
      stop(paste("File", FileName, "already exists."))
    }
    
    if (l10n_info()[["UTF-8"]] && encoding == "UTF-8"){
      fWriteLandings(FileName, data, namespace, encoding) 
    }
    else{
      warning("Fast file writing cannot be enabled. Falling back to slow mode.")
      writeXmlFile(FileName, data, xsdObjects$landingerv2.xsd, namespace, encoding)  
    }
  }
  
}

#' Write Biotic
#' @description 
#'  Write biotic data as XML file(s).
#' @details
#'  \code{\link[RstoxData]{BioticData}} may contain several data sets.
#'  In that case the parameters 'FileNames' and 'namespaces' must be provided for each
#'  in the order they appear in 'BioticData'.
#' @details 
#'  Supports writing to namespaces:
#'   http://www.imr.no/formats/nmdbiotic/v1.1
#'   http://www.imr.no/formats/nmdbiotic/v1.2
#'   http://www.imr.no/formats/nmdbiotic/v1.3
#'   http://www.imr.no/formats/nmdbiotic/v3
#'   http://www.imr.no/formats/nmdbiotic/v3.1
#' @param BioticData \code{\link[RstoxData]{BioticData}} data to write.
#' @param FileNames paths to files that should be written to
#' @param namespaces XML namespaces to use for formatting.
#' @param encoding encoding to use for writing files
#' @param overwrite whether to overwrite any existing file(s)
#' 
#' @examples 
#' exampleFile <- system.file("testresources","biotic3.1_example.xml", package="RstoxData")
#' bioticData <- ReadBiotic(exampleFile)
#' newFile <- tempfile(fileext = ".xml")
#' WriteBiotic(bioticData, newFile, namespaces = "http://www.imr.no/formats/nmdbiotic/v3.1")
#' bioticDataReadBackIn <- ReadBiotic(newFile)
#' all.equal(
#'   bioticData[[1]][names(bioticData[[1]]) != "metadata"], 
#'   bioticDataReadBackIn[[1]][names(bioticDataReadBackIn[[1]]) != "metadata"]
#' )
#' 
#' @export
#' 
WriteBiotic <- function(BioticData, FileNames = character(), namespaces = character(), encoding = "UTF-8", overwrite = FALSE){
  
	WriteBioticOrAcoustic(Data = BioticData, DataType = "BioticData", FileNames = FileNames, namespaces = namespaces, encoding = encoding, overwrite = overwrite)
}



#' Write Acoustic
#' @description 
#'  Write acoustic data as XML file(s).
#' @details
#'  \code{\link[RstoxData]{AcousticData}} may contain several data sets.
#'  In that case the parameters 'FileNames' and 'namespaces' must be provided for each
#'  in the order they appear in 'AcousticData'.
#' @details 
#'  Supports writing to namespaces:
#'   http://www.imr.no/formats/nmdechosounder/v1
#' @param AcousticData \code{\link[RstoxData]{AcousticData}} data to write.
#' @inheritParams WriteBiotic
#' 
#' @examples 
#' exampleFile <- system.file(
#'     "testresources","libas_ListUserFile20__L40.0-2259.9_small.xml", package="RstoxData")
#' acousticData <- ReadAcoustic(exampleFile)
#' newFile <- tempfile(fileext = ".xml")
#' WriteAcoustic(acousticData, newFile, namespaces = "http://www.imr.no/formats/nmdechosounder/v1")
#' acousticDataReadBackIn <- ReadAcoustic(newFile)
#' all.equal(
#'   acousticData[[1]][names(acousticData[[1]]) != "metadata"], 
#'   acousticDataReadBackIn[[1]][names(acousticDataReadBackIn[[1]]) != "metadata"]
#' )
#' 
#' @export
#' 
WriteAcoustic <- function(AcousticData, FileNames = character(), namespaces = character(), encoding = "UTF-8", overwrite = FALSE){
  
	WriteBioticOrAcoustic(Data = AcousticData, DataType = "AcousticData", FileNames = FileNames, namespaces = namespaces, encoding = encoding, overwrite = overwrite)
}
  


# Common funtion for biotic and acoustic data:
WriteBioticOrAcoustic <- function(Data, DataType, FileNames = character(), namespaces = character(), encoding = "UTF-8", overwrite = FALSE){
  
  # set default namespace if not specified
  if (!length(namespaces)){
    namespaces <- c()
    for (l in Data){
      
      if (length(l$metadata$useXsd)){
        xsdObj <- RstoxData::xsdObjects[[paste(l$metadata$useXsd, "xsd", sep=".")]]
        ns <- xsdObj$targetNamespace
        if (is.na(ns)){
          ns <- ""
        }
        namespaces <- c(namespaces, ns)
      }
      else{
        stop(paste("Does not recognize namespace for", l$metadata$useXsd, "Provide explicit namespace."))
      }
    }
  }
  
  if (!length(Data) == length(FileNames)){
    stop("Provide exactly one file name for each data set in the ", DataType)
  }
  if (!length(Data) == length(namespaces)){
    stop("Provide exactly one namespace for each data set in the ", DataType)
  }
  
  
  Data <- createBioticOrAcousticData(data = Data, namespace = namespaces) 
  
  #
  # write files
  #
  for (i in 1:length(Data)){
    
    FileName <- FileNames[[i]]
    thisData <- Data[[i]] 
    namespace <- namespaces[[i]]
    
    xsdObject <- NULL
    for (x in xsdObjects){
      if (!is.na(x$targetNamespace) & x$targetNamespace == namespace){
        xsdObject <- x
      }
    }
    
    if (!length(xsdObject)){
      stop(paste("Namespace", namespace, "not supported."))
    }
    
    if (file.exists(FileName) & !overwrite){
      stop(paste("File", FileName, "already exists."))
    }
    
    writeXmlFile(FileName, thisData, xsdObject, namespace, encoding, keepEmptyLevels = namespace == "http://www.imr.no/formats/nmdechosounder/v1")    
  }
  
}



#' Converts biotic
#' @description 
#'  Converts between compatible nmdbiotic versions
#'  
#' @details
#'  Different versions of the nmdbiotic format are compatible and can be converted with this function
#'  if they have the same major release number. So that v3.1 is compatible with v3.0, but not with version 1.4
#'  
#'  Compatibility is not enforced by this function, and conversion will be attempted without checking the version of 'source'
#' @param sourceFile path to file that should be converted
#' @param targetFile path that the converted files should be written to
#' @param targetFormat name of xsdObject specifying the format of the target file. One of names(RstoxData::xsdObjects).
#' @param overwrite if TRUE any existing file in 'targetFile' will be overwritten.
#' @export
convertBioticFile <- function(sourceFile, targetFile, targetFormat = "nmdbioticv3.xsd", overwrite = FALSE){
  
  if (!is.character(targetFormat) | !(targetFormat %in% names(RstoxData::xsdObjects))){
    stop("'targetFormat must be one of those in names(RstoxData::xsdObjects).")
  }
  
  targetFormat <- RstoxData::xsdObjects[[targetFormat]]
  namespace=targetFormat$targetNamespace
  
  if (!file.exists(sourceFile)){
    stop(paste("File", sourceFile, "does not exsist."))
  }
  
  if (file.exists(targetFile) & !overwrite){
    stop(paste("File", targetFile, "already exsists."))
  }
  
  data <- readXmlFile(sourceFile)
  writeXmlFile(targetFile, data, targetFormat, namespace = namespace)
  
}











# Functions to create BioticData:
createBioticOrAcousticData <- function(data = list(), namespace = character()) {
  data <- mapply(
    createBioticOrAcousticDataOne, 
    dataOne = data, 
    namespace = namespace, 
    SIMPLIFY = FALSE
  )
  
  return(data)
}
createBioticOrAcousticDataOne <- function(dataOne = list(), namespace = character()) {
  namespace <- getNamespace(namespace)
  
  tableNames <- names(RstoxData::xsdObjects[[namespace$xsd]]$tableHeaders)
  
  dataOne <- lapply(
    tableNames, 
    createOneTable, 
    data = dataOne, 
    namespace = namespace
  )
  names(dataOne) <- tableNames
  
  return(dataOne)
}
createOneTable <- function(tableName, data, namespace) {
  
  data <- data[[tableName]]
  
  if(NROW(data) && ! data.table::is.data.table(data)) {
  	warning("The table ", tableName, " converted from data.frame to data.table.")
  	data <- data.table::as.data.table(data)
  }
  
  tableHeadersOne <- RstoxData::xsdObjects[[namespace$xsd]]$tableHeaders[[tableName]]
  prefixLensOne <- RstoxData::xsdObjects[[namespace$xsd]]$prefixLens[[tableName]]
  
  # Get class:
  tableTypesOne <- RstoxData::xsdObjects[[namespace$xsd]]$tableTypes[[tableName]]
  type_class_table <- data.table::data.table(
    type = c("xs:date", "xs:decimal", "xs:double", "xs:integer", "xs:string", "xs:time"), 
    class = c("character", "numeric", "numeric", "integer", "character", "character")
  )
  tableClassesOne <- match(
    tableTypesOne, 
    type_class_table$type, 
  )
  tableClassesOne <- type_class_table[tableClassesOne, class]
  
  # Create NAs:
  emptyListOfTableHeaders <- as.list(rep(NA, length(tableHeadersOne)))
  names(emptyListOfTableHeaders) <- tableHeadersOne
  
  # Set class:
  emptyListOfTableHeaders <- mapply(
    function(x, newClass) do.call(paste0("as.", newClass), list(x)),  
    x = emptyListOfTableHeaders, 
    newClass = tableClassesOne, 
    SIMPLIFY = FALSE
  )
  
  if(!NROW(data)) {
  	data <- data.table::as.data.table(emptyListOfTableHeaders)[0, ]
  	return(data)
  }
  if(NROW(data) && !all(tableHeadersOne[seq_len(prefixLensOne)] %in% names(data))) {
  	warning("The following required fields are missing in the table '", tableName, "': ", paste(setdiff(tableHeadersOne[seq_len(prefixLensOne)], names(data)), collapse = ", "), ".")
  	
    data <- data.table::as.data.table(emptyListOfTableHeaders)[0, ]
    return(data)
  }
  
  # Replace with the data:
  validNames <- intersect(names(data), tableHeadersOne)
  #data <- data[validNames]
  data <- data[, validNames, with = FALSE]
  
  emptyListOfTableHeaders[names(data)] <- data
  
  data <- data.table::as.data.table(
    emptyListOfTableHeaders
  )
  
  data <- unique(data)
  
  return(data)
}

# Functions to treat xml namespaces:
getNamespace <- function(format, element = character()) {
  
  # Get a table of all possiblew namespaces:
  namespaceTable <- namespaceTable()
  
  # If starting with "http" assume that 'format' given as an explicit namespace:
  if(startsWith(tolower(format), "http")) {
    atNamespace <- which(namespaceTable$namespace == format)
  }
  # If ending with "xsd" assume that 'format' is the name in the xsdObjects:
  else if(endsWith(tolower(format), "xsd")) {
    atNamespace <- which(namespaceTable$xsd == format)
  }
  else {
    atNamespace <- which(namespaceTable$format == format)
  }
  
  if(length(atNamespace)) {
    output <- namespaceTable[atNamespace, ]
  }
  else {
    warning("The format/xsd/namespace not recognized. Implemented namespaces are ", paste(namespaceTable()$namespace, collapse = ", "), ".")
    output <- data.table::data.table(
      format = NA, 
      xsd = NA, 
      namespace = NA
    )
  }
  
  if(length(element) && element %in% names(output)) {
    output <- output[[element]]
  }
  
  return(output)
}
namespaceTable <- function() {
  data.table::data.table(
    format = sub(".xsd", "", names(RstoxData::xsdObjects), fixed = TRUE), 
    xsd = names(RstoxData::xsdObjects), 
    namespace = unlist(lapply(RstoxData::xsdObjects, "[[", "targetNamespace"))
  )
}






#' Merge AcousticData
#' 
#' Function to merge the tables of \code{\link{AcousticData}}.
#'  
#' @inheritParams ModelData
#' 
#' @export
#' 
MergeAcoustic <- function(AcousticData) {
	lapply(AcousticData, MergeAcousticOne)
}

MergeAcousticOne <- function(AcousticDataOne) {
	if(grepl("nmdechosounderv1", AcousticDataOne$metadata$useXsd)) {
		MergeNMDEchosounderOne(AcousticDataOne)
	}
	else if(grepl("icesAcoustic", AcousticDataOne$metadata$useXsd)) {
		MergeICESAcousticOne(AcousticDataOne)
	}
	else {
		stop("Unsupported format ", AcousticDataOne$metadata$useXsd, " in MergeAcoustic().")
	}
}


MergeNMDEchosounderOne <- function(NMDEchosounderOne) {
	tablesToMerge <- c("distance", "frequency", "ch_type", "sa_by_acocat", "sa")
	
	merged <- RstoxData::mergeDataTables(NMDEchosounderOne[tablesToMerge], output.only.last = TRUE, all = TRUE)
	
	# Add the echosounder_dataset:
	merged <- cbind(NMDEchosounderOne$echosounder_dataset, merged)
	
	return(merged)
}

MergeICESAcousticOne <- function(ICESAcousticOne) {
	stop("Not yet implemented.")
}


# We need also MergeBiotic()






#' Un-merge MergeBioticData
#' 
#' Function to split MergeBioticData into \code{\link{BioticData}}
#'  
#' @param MergeBioticData The data to be split into \code{\link{BioticData}}.
#' @param useXsd The xsd to use when splitting the flat tables into the different levels.
#' @param ... Additionial values to inclcude in the output \code{\link{BioticData}}. Applies to all tables in MergeBioticData. The length of each of the elements given must match the number of rows of the table to add the values to.
#' 
#' @export
#' 
unMergeBiotic <- function(
		MergeBioticData,  
		useXsd, 
		...
) {
	
	useXsd <- match_arg_informative(useXsd, getRstoxDataDefinitions("getImplementedXsd")("Biotic"))
	lapply(MergeBioticData, unMergeBioticOrAcousticOne, useXsd = useXsd, ...)
}



#' Un-merge MergeAcousticData
#' 
#' Function to split MergeAcousticData into \code{\link{AcousticData}}
#'  
#' @param MergeAcousticData The data to be split into \code{\link{AcousticData}}.
#' @param useXsd The xsd to use when splitting the flat tables into the different levels.
#' @param ... Additionial values to inclcude in the output \code{\link{AcousticData}}. Applies to all tables in MergeAcousticData. The length of each of the elements given must match the number of rows of the table to add the values to.
#' 
#' @export
#' 
unMergeAcoustic <- function(
		MergeAcousticData,  
		useXsd, 
		...
) {
	useXsd <- match_arg_informative(useXsd, getRstoxDataDefinitions("getImplementedXsd")("Acoustic"))
	lapply(MergeAcousticData, unMergeBioticOrAcousticOne, useXsd = useXsd, ...)
}


#' Un-merge MergeAcousticData
#' 
#' Function to convert the sonarData to a table with all channels as columns, and write to NMDEchosounder xml file
#'  
#' @param MergeDataOne The \code{\link{AcousticData}} merged to one table for each file.
#' @param useXsd The xsd to add as metadata.
#' @param ... Additionial values to inclcude in the \code{\link{AcousticData}} The length of each of the elements given must match the number of rows of the table to add the values to.
#' @noRd
#' 
unMergeBioticOrAcousticOne <- function(
		MergeDataOne,  
		useXsd, 
		...
) {
	
	#  Extract the data at each level, adding any additional data from ...:
	lll <- list(...)
	
	# Use the XML schema of the NMDEchosounder:
	if(!grepl(".xsd", tolower(useXsd))) {
		useXsd <- paste0(useXsd, ".xsd")
	}
	xsd <- RstoxData::xsdObjects[[useXsd]]
	AcousticData <- lapply(xsd$tableOrder, getUniqueVariablesOfLevel, data = MergeDataOne, xsd = xsd, lll = lll)
	names(AcousticData) <- xsd$tableOrder
	
	# Add the useXsd as metadata: 
	AcousticData$metadata <- data.table::data.table(
		useXsd = useXsd
	)
	
	return(AcousticData)
}


# Run through the levels and extract the columns that should be written as each level, adding the optional inputs in given ...:
getUniqueVariablesOfLevel <- function(level, data, xsd, lll) {
	# Identify the variables to extract at this level:
	varsToExtract <- xsd$tableHeaders[[level]]
	keys <- varsToExtract[seq_len(xsd$prefixLens[[level]])]
	keys <- intersect(names(data), keys)
	
	# Extract these variables present in the data: and in lll
	varsToExtractFromData <- intersect(names(data), varsToExtract)
	thisData <- data[, varsToExtractFromData, with = FALSE]
	# Uniqueify:
	
	if(length(keys)) {
		thisData <- unique(thisData, by = keys)
	}
	else {
		thisData <- unique(thisData)
	}
	
	
	thisAdditionalData <- lll[intersect(names(lll), varsToExtract)]
	
	# Try to cbind the data with the lll:
	if(all(lengths(thisAdditionalData) == nrow(thisData) | lengths(thisAdditionalData) == 1)) {
		for(var in names(thisAdditionalData)) {
			thisData[, eval(var) := thisAdditionalData[[var]]]
		}
		#levelData <- data.table(
		#	thisData, 
		#	thisAdditionalData
		#)
	}
	else {
		stop("Variables given in ... must match the lengths of the data at the appropriate level.")
	}
	
	if(length(keys)) {
		data.table::setorderv(thisData, keys)
	}
	
	return(thisData)
}




