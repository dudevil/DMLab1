reportsxml <- xmlParse("mozilla/Firefox/reports.xml")
reportsdf <- xmlToDataFrame(reportsxml, stringsAsFactors = F)
ids <- xpathSApply(reportsxml, "//reports/report[@id]", xmlAttrs)
reportsdf$id <- ids

resolution_xml <- xmlParse(resolutionURL)
idNodes <- getNodeSet(resolution_xml, "//report[@id]")
ids <- lapply(idNodes, function(x) xmlAttrs(x)['id'])

val_when <- lapply(idNodes, xpathApply, path = './update/when', xmlValue)
val_what <- lapply(idNodes, xpathApply, path = './update/what', xmlValue)

resolutions <- data.frame(do.call(rbind.data.frame, mapply(cbind, ids, val_when, val_what)), stringsAsFactors = F)
names(resolutions) <- c("report id", "update_when", "update_what")