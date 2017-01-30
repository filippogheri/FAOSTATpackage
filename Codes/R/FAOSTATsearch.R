##' A function to find domain, element, item, and area code for a
##' specific FAOSTAT query.
##'
##' @export

FAOSTATsearch <- function() {
  with(FAOmetaTable, {
    ##############
    ## GROUP CODE
    ##############
    gc <- NA
    ## while loop iterates until a valid value is supplied
    while(length(gc)==0 || is.na(gc)){
      cat(paste(paste("(", 1:length(groupTable$groupName), ") ",
                      groupTable$groupName, sep = ""), collapse = "\n"))
      gcn <- readline("Which Group are you looking for: ")
      gc <- groupTable[as.numeric(gcn), "groupCode"]
    }
    
    ###############
    ## DOMAIN CODE
    ###############
    subdomainTable <- subset(domainTable, groupCode == gc)
    dc <- NA
    ## while loop iterates until a valid value is supplied
    while(length(dc)==0 || is.na(dc)){
      cat(paste(paste("(", 1:length(subdomainTable$domainName), ") ",
                      subdomainTable$domainName, sep = ""),
                collapse = "\n"))
      dcn <- readline("Which Domain are you looking for: ")
      dc <- subdomainTable[as.numeric(dcn), "domainCode"]
    }
    
    ########
    ## ITEM
    ########
    useAgg <- NA
    
    if (dc %in% c("QD", "QP", "TM", "CP", "PD", "PE", "RF", "RV", "RP",
                  "RT", "RL", "OA", "RM", "IC", "FDI", "CISP", "CS",
                  "MK", "EC", "EE", "EF", "EL", "EK", "EP", "ES", "EW",
                  "GR", "GY", "GC", "GG", "AF", "AE", "FS", "FA")) {
      useAgg <- "0" ## Just individual items
    } else if (dc %in% c("QI", "TI")) {
      ## Production indices, Trade indices
      useAgg <- "1" ## Just aggregated items
    } else if (dc %in% c("CC", "CL", "FT", "RA", "RY")) {
      while(is.na(useAgg) || !useAgg %in% c("0", "1")){
        cat("(0) Individual item (e.g. Apples, Wheat)\n")
        cat("(1) Aggregated item (e.g. Total cereals, Total meat)\n")
        useAgg <- readline(paste("Are you looking for individual item or",
                                 "aggregated item:"))
      }
    } else if (dc %in% c("PP", "PM", "PA", "IG")) {
      while(is.na(useAgg) || !useAgg %in% c("0", "2")){
        cat("(0) Individual item (e.g. Apples, Wheat)\n")
        cat("(2) Groups of items (e.g. Cereals, Vegetables)\n")
        useAgg <- readline(paste("Are you looking for individual item or",
                                 "aggregated item:"))
      }
    } else {
      ## Other domains
      while(is.na(useAgg) || !useAgg %in% c("0", "1", "2")){
        cat("(0) Individual item (e.g. Apples, Wheat)\n")
        cat("(1) Aggregated item (e.g. Total cereals, Total meat)\n")
        cat("(2) Groups of items (e.g. Cereals, Vegetables)\n")
        useAgg <- readline(paste("Are you looking for individual item or",
                                 "aggregated item:"))
      }
    }
    
    if(useAgg == "0"){
      ## Find the item code
      subitemTable <- subset(itemTable, domainCode == dc)
    } else if (useAgg == "1") {
      ## Find the aggregated item code
      subitemTable <- subset(itemAggTable, domainCode == dc)
    } else if (useAgg == "2") {
      ## Find the item group code
      subitemTable <- subset(itemGroupTable, domainCode == dc)
    }
    
    ic <- NA
    ## while loop iterates until a valid value is supplied
    while(length(ic)==0 || is.na(ic)){
      cat(paste(paste("(", 1:length(subitemTable$itemName), ") ",
                      subitemTable$itemName, sep = ""),
                collapse = "\n"))
      icn <- readline(paste("Which Item are you looking for?",
                            "('All' for everything):"))
      if(icn == "All")
        icn <- 1:length(subitemTable$itemName)
      ic <- subitemTable[as.numeric(icn), "itemCode"]
    }
    
    ###########
    ## ELEMENT
    ###########
    subelementTable <- subset(elementTable, domainCode == dc)
    ec <- NA
    ## while loop iterates until a valid value is supplied
    while(length(ec)==0 || is.na(ec)){
      cat(paste(paste("(", 1:length(subelementTable$elementName), ") ",
                      subelementTable$elementName, sep = ""),
                collapse = "\n"))
      ecn <- readline(paste("Which Element are you looking for?",
                            "('All' for everything):"))
      if(ecn == "All")
        ecn <- 1:length(subelementTable$elementName)
      ec <- subelementTable[as.numeric(ecn), "elementCode"]
    }
    
    ########
    ## AREA
    ########
    area <- NA
    if (dc %in% c("PP", "PM", "PI", "PA", "CP", "PD", "PE", "IG", "IC",
                  "CISP", "CS", "EP", "EW", "AF", "AE", "TM", "FT")) {
      ## Price domain
      while(is.na(area) || !area %in% c("0", "2")){
        cat("(0) Countries (e.g. Afghanistan, Albania, etc.)\n")
        cat("(2) Group of countries (e.g. Countries in Africa, countries in European Union etc.)\n")
        area <- readline(paste("Area you are looking for:"))
      }
    } else {
      while(is.na(area) || !area %in% c("0", "1", "2")){
        cat("(0) Countries (e.g. Afghanistan, Albania, etc.)\n")
        cat("(1) Aggregates (e.g. World, European Union, etc.)\n")
        cat("(2) Group of countries (e.g. Countries in Africa, countries in European Union etc.)\n")
        area <- readline(paste("Area you are looking for:"))
      }
    }
    
    if (area == "0"){
      ## Find the area code
      subareaTable <- subset(areaTable, domainCode == dc)
    } else if (area == "1") {
      ## Find the aggregated area code
      subareaTable <- subset(areaAggTable, domainCode == dc)
    } else if (area == "2") {
      ## Find the area group code
      subareaTable <- subset(areaGroupTable, domainCode == dc)
    }
    
    ac <- NA
    ## while loop iterates until a valid value is supplied
    while(length(ac)==0 || is.na(ac)){
      cat(paste(paste("(", 1:length(subareaTable$areaName), ") ",
                      subareaTable$areaName, sep = ""),
                collapse = "\n"))
      acn <- readline(paste("Which Area are you looking for?",
                            "('All' for everything):"))
      if(acn == "All")
        acn <- 1:length(subareaTable$areaName)
      ac <- subareaTable[as.numeric(acn), "areaCode"]
    }
    
    ############
    ## OUTPUT
    ############
    tmp <- expand.grid(dc, ic, ec, ac, stringsAsFactors = FALSE)
    colnames(tmp) <- c("domainCode", "itemCode", "elementCode", "areaCode")
    tmp <- merge(tmp, domainTable[, c("domainCode", "domainName")],
                 all.x = TRUE)
    tmp <- merge(tmp, subitemTable[, c("itemCode", "itemName")],
                 all.x = TRUE)
    tmp <- merge(tmp, subelementTable[, c("elementCode", "elementName")],
                 all.x = TRUE)
    final.df <- merge(tmp, subareaTable[, c("areaCode", "areaName")],
                      all.x = TRUE)
    final.df$name <-
      with(final.df, paste(domainName, itemName, elementName, areaName, sep = "_"))
    final.df$domainName <- NULL
    final.df$itemName <- NULL
    final.df$elementName <- NULL
    final.df$areaName <- NULL
    .LastSearch <<- final.df
    cat("\n** Search result saved as .LastSearch**\n")
    
  }
  )
}

utils::globalVariables(names = c("FAOmetaTable"))