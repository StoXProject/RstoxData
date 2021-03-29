

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

