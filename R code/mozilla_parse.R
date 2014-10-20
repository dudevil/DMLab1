require('XML')
require('data.table')

parse_reports <- function(filename='raw data/reports.xml'){
  reportsdf <- xmlToDataFrame(xmlParse(filename), stringsAsFactors = F)
  ids <- xpathSApply(reportsxml, "//reports/report[@id]", xmlAttrs)
  reportsdf$id <- as.numeric(ids)
  reportsdf$reporter <- as.numeric(reportsdf$reporter)
}

parse_other <-function(filename='raw data/resolution.xml'){
  xml <- xmlParse(filename)
  parse_report <- function(xml){
    xdf <- xmlToDataFrame(xml, stringsAsFactors = F)
    xdf$id <- xmlAttrs(xml)
    return(xdf)
  }
  rbindlist(xpathApply(xml, '//*/report',parse_report), use.names=T)
}

parse_class <- function(filename='raw data/resolution.xml'){
  resolution_xml <- xmlParse(filename)
  idNodes <- getNodeSet(resolution_xml, "//report[@id]")
  ids <- lapply(idNodes, function(x) xmlAttrs(x)['id'])
  val_when <- lapply(idNodes, xpathApply, path = './update/when', xmlValue)
  val_what <- lapply(idNodes, xpathApply, path = './update/what', xmlValue)
  resolutions <- data.frame(do.call(rbind.data.frame, mapply(cbind, ids, val_when, val_what)), stringsAsFactors = F)
  names(resolutions) <- c("report id", "update_when", "update_what")
  return(resolutions)
}