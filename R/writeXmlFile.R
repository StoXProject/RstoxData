

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
  if (!is.null(attributes)){
    #stopifnot(nrow(attributes)==1)
    
    for (n in names(attributes)){
      if (!is.na(attributes[[n]][[1]])){
        tagstring <- paste(tagstring, paste0(n,"=\"",attributes[[n]][[1]],"\""))
      }
    }
  }
  tagstring <- paste0(tagstring, ">")
  writeLines(tagstring, con=stream)
}

#' @noRd
closeTag <- function(stream, tagname, indent=""){
  tagstring <- paste0(indent, "</",tagname, ">")
  writeLines(tagstring, con=stream)
}

#' @noRd
writeSimpleTags <- function(stream, tags, indent=""){
  string <- ""
  for (n in names(tags)){
    if (!is.na(tags[[n]][[1]])){
      string <- paste0(string, "<",n,">",tags[[n]][[1]],"</",n,">")
    }
  }
  writeLines(paste0(indent, string), con=stream)
}


#' @noRd
writeLevel <- function(stream, data, level, parentKeys, xsdObject, indent="", namespace=""){
  
  # handle root
  if (level == xsdObject$root){
    #stopifnot(is.null(parentKeys))
    openTag(stream, level, data.table::data.table(xmlns=namespace), indent)
    for (sub in xsdObject$treeStruct[[level]]){
      writeLevel(stream, data, sub, NULL, xsdObject, paste0(indent, "\t"))
    }
    closeTag(stream, level, indent)
    return()
  }
  
  # handle non root
  leveldata <- data[[level]]
  if (!is.null(parentKeys)){
    #stopifnot(all(names(parentKeys) %in% names(leveldata)))
    #filter <- apply(leveldata[names(leveldata) %in% names(parentKeys)], 1, function(x){paste(x, collapse=" ")}) == apply(parentKeys, 1, function(x){paste(x, collapse=" ")})
    #leveldata <- leveldata[filter, ]
    leveldata <- leveldata[as.list(parentKeys), nomatch = NULL]
    
  }
  if (nrow(leveldata) == 0){
    return()
  }
  
  children <- xsdObject$treeStruct[[level]]
  
  # get keys
  keys <- names(leveldata)[1:xsdObject$prefixLens[[level]]]
  
  #stopifnot(!any(duplicated(Reduce(paste, data[[level]][,keys, with=F]))))
  #stopifnot(all(names(parentKeys) %in% keys))
  
  # write opening tag and attributes
  for (i in 1:nrow(leveldata)){
    openTag(stream, level, leveldata[i,keys,with=F], indent)
    
    # write simple element tags
    simpletags <- xsdObject$tableHeaders[[level]][!(xsdObject$tableHeaders[[level]] %in% keys)]
    writeSimpleTags(stream, leveldata[i,simpletags,with=F], paste0(indent, "\t"))
    
    # write complex element tags
    for (ch in children){
      writeLevel(stream, data, ch, leveldata[i,keys,with=F], xsdObject, paste0(indent, "\t"))
    }
    
    # write closing tag
    closeTag(stream, level, indent)
  }
  
}

#' converts evrything to character before XML writing
#' @noRd
typeConvert <- function(dataTables, xsdObject){
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
            dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
          }
          else if (is.numeric(dataTables[[n]][[name]]) & xsdType == "xs:string"){
            dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
          }
          else if (is.logical(dataTables[[n]][[name]]) & xsdType == "xs:string"){
            dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
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
          else if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:date"){
            ok <- is.na(dataTables[[n]][[name]])
            ok <- ok | !is.na(as.POSIXct(dataTables[[n]][[name]], format="%Y-%m-%d"))
            if (!all(ok)){
              stop(paste("Data type conversion from character to xs:date is not configured for some date formats in data."))
            }
          }
          else if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:time"){
            ok <- is.na(dataTables[[n]][[name]])
            ok <- ok | !is.na(as.POSIXct(dataTables[[n]][[name]], format="%H:%M:%S"))
            if (!all(ok)){
              stop(paste("Data type conversion from character to xs:time is not configured for some time formats in data."))
            }
          }
          else{
            stop(paste("Data type conversion from", class(dataTables[[n]][[name]]), "to", xsdType, "is not configured"))
          }
        }
      }
      else{
        for (coln in names(dataTables[[n]])){
          dataTables[[n]][[coln]] <- as.character(dataTables[[n]][[coln]])
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
writeXmlFile <- function(fileName, dataTables, xsdObject, namespace, encoding="UTF-8", xmlStandard="1.0"){
  
  # Notes for development:
  # consider adding namespace name to xsdObjects
  # consider adding XML version to xsdObjects
  # consider adding information about which columns are attributes / elements in xsdObjects
  # consider adding ordering information about all elements in xsdObjects
  # (including the relative ordering of complex and simple elements)
  # consider adding information about key structure in xsdObjects
  
  dataTables <- typeConvert(dataTables, xsdObject)
  dataTables <- setKeysDataTables(dataTables, xsdObject)
  
  stream = file(fileName, open="w", encoding=encoding)
  writeXmlDeclaration(stream, version=xmlStandard, encoding=encoding, standalone=T)
  writeLevel(stream, dataTables, xsdObject$root, NULL, xsdObject, "", namespace)    
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
    xsdObject <- RstoxData::xsdObjects$landingerv2.xsd
  }
  else{
    stop(paste("Namespace", namespace, "not supported."))
  }
  
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
#'  In that case the parameters 'FileNames' and 'namespaces' must be procided for each
#'  in the order they appear in 'LandingData'.
#' @param LandingData \code{\link[RstoxData]{LandingData}} data to write.
#' @param FileNames paths to files that should be written to
#' @param namespaces XML namespaces to use for formatting.
#' @param encoding encoding to use for writing files
#' @param overwrite whether to overwrite any existing file(s)
#' @noRd
WriteLanding <- function(LandingData, FileNames, namespaces=NULL, encoding="UTF-8", overwrite=F){
  
  #
  # set default namespace if not specified
  #
  if (is.null(namespaces)){
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
    
    if (!l10n_info()[["UTF-8"]] && encoding == "UTF-8"){
      fWriteLandings(FileName, data, namespace, encoding) 
    }
    else{
      writeXmlFile(FileName, data, RstoxData::xsdObjects$landingerv2.xsd, namespace, encoding)  
    }
  }
  
}

#' Write Biotic
#' @description 
#'  Write biotic data as XML file(s).
#' @details
#'  \code{\link[RstoxData]{BioticData}} may contain several data sets.
#'  In that case the parameters 'FileNames' and 'namespaces' must be procided for each
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
#' @noRd
WriteBiotic <- function(BioticData, FileNames, namespaces=NULL, encoding="UTF-8", overwrite=F){
  
  # set default namespace if not specified
  if (is.null(namespaces)){
    namespaces <- c()
    for (l in BioticData){
      if (l$metadata$useXsd == "nmdbioticv1.1"){
        namespaces <- c(namespaces, "http://www.imr.no/formats/nmdbiotic/v1.1")
      }
      if (l$metadata$useXsd == "nmdbioticv1.2"){
        namespaces <- c(namespaces, "http://www.imr.no/formats/nmdbiotic/v1.2")
      }
      if (l$metadata$useXsd == "nmdbioticv1.3"){
        namespaces <- c(namespaces, "http://www.imr.no/formats/nmdbiotic/v1.3")
      }
      if (l$metadata$useXsd == "nmdbioticv1.4"){
        namespaces <- c(namespaces, "http://www.imr.no/formats/nmdbiotic/v1.4")
      }
      if (l$metadata$useXsd == "nmdbioticv3"){
        namespaces <- c(namespaces, "http://www.imr.no/formats/nmdbiotic/v3")
      }
      if (l$metadata$useXsd == "nmdbioticv3.1"){
        namespaces <- c(namespaces, "http://www.imr.no/formats/nmdbiotic/v3.1")
      }
      else{
        stop(paste("Does not recognize namespace for", l$metadata$useXsd, "Provide explicit namespace."))
      }
    }
  }
  
  if (!length(BioticData) == length(FileNames)){
    stop("Provide exactly one file name and one namespace for each data set in 'LandingData'")
  }
  if (!length(BioticData) == length(namespaces)){
    stop("Provide exactly one file name and one namespace for each data set in 'LandingData'")
  }
  
  
  #
  # write files
  #
  for (i in 1:length(BioticData)){
    
    FileName <- FileNames[[i]]
    data <- BioticData[[i]] 
    namespace <- namespaces[[i]]
    
    if (namespace == "http://www.imr.no/formats/nmdbiotic/v3.1"){
      xsdObject = RstoxData::xsdObjects$nmdbioticv3.1.xsd
    }
    else if (namespace == "http://www.imr.no/formats/nmdbiotic/v3"){
      xsdObject = RstoxData::xsdObjects$nmdbioticv3.xsd
    }
    else if (namespace == "http://www.imr.no/formats/nmdbiotic/v1.1"){
      xsdObject = RstoxData::xsdObjects$nmdbioticv1.1.xsd
    }
    else if (namespace == "http://www.imr.no/formats/nmdbiotic/v1.2"){
      xsdObject = RstoxData::xsdObjects$nmdbioticv1.2.xsd
    }
    else if (namespace == "http://www.imr.no/formats/nmdbiotic/v1.3"){
      xsdObject = RstoxData::xsdObjects$nmdbioticv1.3.xsd
    }
    else{
      stop(paste("Namespace", namespace, "not supported."))
    }
    
    if (file.exists(FileName) & !overwrite){
      stop(paste("File", FileName, "already exists."))
    }
    
    writeXmlFile(FileName, data, xsdObject, namespace, encoding)    
  }

}