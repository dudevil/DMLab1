require('XML')
require('data.table')

parse_reports <- function(filename='raw data/reports.xml'){
  # Parses the reports.xml file 
  #
  # Args:
  #   filename: path to the xml file
  #
  # Returns:
  #   A dataframe containing report id, reporter, and opening time
  
  reportsxml <- xmlParse(filename)
  reportsdf <- xmlToDataFrame(reportsxml, stringsAsFactors = F)
  # get report id's from xml attributes
  ids <- xpathSApply(reportsxml, "//reports/report[@id]", xmlAttrs)
  # cast data to integers
  reportsdf$id <- as.integer(ids)
  reportsdf$reporter <- as.integer(reportsdf$reporter)
  reportsdf$opening_time <- as.integer(reportsdf$opening_time)
  return(reportsdf)
}

parse_other <-function(filename='raw data/resolution.xml', data_names=c()){
  # Parses the xml file into the data.table
  #
  # Args:
  #   filename: path to the xml file
  #   data_names: names for the collumns in the resulting data.table
  #
  # Returns:
  #   A data.table
  xml <- xmlParse(filename)
  # parse the 'report' xml element, return a dataframe 
  parse_report <- function(xml){
    xdf <- xmlToDataFrame(xml, stringsAsFactors = F)
    xdf$id <- xmlAttrs(xml)
    return(xdf)
  }
  # merge the dataframes from all 'report' elemets into a data.table
  res = rbindlist(xpathApply(xml, '//*/report',parse_report), use.names=T)
  # if data_names was given rename collumns
  if(length(data_names)){
      names(res)[names(res)!='id'] <- data_names
  }
  return(res)
}

merge_files <- function(files, data_names){
  # A wrapper around parse_reports and parse_other for multiple files
  #
  # Args:
  #   files: a list of vector of filenames to parse
  #   data_names: a list of names for corresponding files
  #
  # Returns:
  #   A merged data.table for all files
  reports <- parse_reports()
  # parse files
  frames <- mapply(parse_other, files, data_names, SIMPLIFY = F)
  # merge results
  return(Reduce(
    function(x, y) merge(x,y,by='id', allow.cartesian=TRUE), 
    append(list(reports), frames)))
}

build_data <-function(){
  # An example, parses and merges a number of files into one data.table
  # and tidies the resulting data
  df <- merge_files(
    c('raw data/resolution.xml', 'raw data/op_sys.xml'), 
    list(c('resolved_when', 'resolved_status'), 
         c('os_when', 'os')))
  df$id <- as.integer(df$id)
  df$os <- as.factor(df$os)
  df$resolved_status <- as.factor(df$resolved_status)
  df$os_when <- as.integer(df$os_when)
  df$resolved_when <- as.integer(df$resolved_when)
  return(df)
}

###what was done in class
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