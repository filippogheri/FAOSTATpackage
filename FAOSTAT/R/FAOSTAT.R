##' Access to FAOSTAT data
##'
##' A wrapper function to obtain multiple data sets under the FAOSTAT 
##' domains: "Production"; "Trade" (excluding "Detailed trade matrix");
##' "Food Balance"; the subdomain "Suite of Food Security Indicators"
##' under the domain "Food Security"; "Prices"; "Inputs" (excluding 
##' "Employment Indicators"); "Population"; "Investment" (excluding "Development 
##' Flows to Agriculture"); "Macro-Statistics"; "Agri-Environmental Indicators";
##' "Emissions - Agriculture"; "Emissions - Land Use"; the subdomain 
##' "Forestry Production and Trade" under the domain "Forestry"; "ASTI R&D Indicators".
##'
##' @param name The name to be given to the variable.
##' @param domainCode The domain code of the variable, see details.
##' @param elementCode The element code of the variable, see details.
##' @param itemCode The item code of the variable, see details.
##' @param yearRange A numeric vector containing the years to be downloaded.
##' @param countrySet A numeric vector of FAOSTAT codes of those countries to be downloaded.
##' @param query The object created if using the FAOsearch function.
##' @param toDataFrame Logical, whether to return the results in data frames instead of data tables.
##' 
##' @return A list containing the following elements
##' \describe{
##'     \item{entity}{The entity level data}
##'     \item{aggregates}{The aggregates provided by the FAO}
##'     \item{results}{The status of the download, whether success/failed}
##' }
##' @export
##'
##' @examples
##' 
##' ## production.dt <- 
##' ## data.table(varName = c("Grapes_AreaHarv", 
##' ##                       "Wine_ProdQuantity", 
##' ##                       "CattleBuffaloes_Stocks", 
##' ##                       "MilkProducts_Yield",
##' ##                       "Yoghurt_ProdQuantity",
##' ##                       "Food _GPIN",
##' ##                       "Agriculture_NetProdValue"),
##' ##           domainCode = c("QC", "QD", "QA", "QL", "QP", "QI", "QV"),
##' ##           itemCode = c(560, 564, 1746, "1780>", 891, 2054, 2051),
##' ##           elementCode = c(2312, 2510, 2111, 2413, 2510, 432, 154))
##' ## production.lst <- with(production.dt,
##' ##                       FAOSTAT(name = varName, domainCode = domainCode,
##' ##                               itemCode = itemCode, elementCode = elementCode,
##' ##                               yearRange = c(2005:2010), countrySet = c(106, 5400)))
##' 
FAOSTAT <- 
  function(name = NULL, domainCode = "QC", elementCode = 2510, 
           itemCode = NULL, yearRange = NULL, countrySet = NULL, 
           query, toDataFrame = FALSE){
    
    ## Year range
    if (!is.null(yearRange)) {
      if (!is.numeric(yearRange)) {
        stop("Please, provide a numeric vector for the year range.")
      } else {
        yearRange <- paste(yearRange, collapse = ",")
      }
    }
    
    ## Country set
    if (!is.null(countrySet)) {
      countrySet <- paste(countrySet, collapse = ",")
    }
    
    ## Query
    if(!missing(query)){
      domainCode = query$domainCode
      itemCode = query$itemCode
      elementCode = query$elementCode
      countrySet = query$areaCode
      if(is.null(query$name)){
        name <- with(query, paste(domainCode, itemCode, elementCode, countrySet, sep = "_"))
      } else {
        name <- query$name
      }
    }
    
    ## Name
    if(is.null(name))
      name <- paste(domainCode, itemCode, elementCode, sep = "_")
    n <- length(name)
    
    ## Check length
    if(any(length(domainCode) != n, length(elementCode) != n))
      stop("domainCode and elementCode should have the same length")
    
    ## Initializing ...
    faoData <- data.table()
    results.dt <- data.frame(Name = name, Success = logical(length(name)),
                             Reason = character(length(name)),
                             Time = as.POSIXct(rep(NA, length(name))),
                             stringsAsFactors = FALSE)
    printLab(paste("FAOSTAT Data Download (", n, " in Total)", sep = ""))
    
    i <- 1
    retry <- 1
    while(i <= n){
      if(retry == 1)
        cat(paste("(", i, "): Downloading variable ", name[i], " ... ",
                  sep = ""))
      if(any(is.na(domainCode[i]), is.na(elementCode[i]))){
        cat("FAIL\n\t Error: domain or element is missing\n")
        results.dt[i, "Success"] <- FALSE
        results.dt[i, "Reason"] <- "domain or element is missing"
      } else {
        ## API
        baseUrl <- paste0("http://fenixservices.fao.org/faostat/api/v1/en/data/")
        if (!domainCode[i] %in% c("PM", "CP")) {
          url <- paste0(baseUrl, domainCode, "?element=", elementCode)
        } else {
          url <- paste0(baseUrl, domainCode, "?month=", elementCode)
        }
        if (!is.null(itemCode)) {
          url <- paste0(url, "&item=", itemCode)
        }
        if (!is.null(yearRange)) {
          url <- paste0(url, "&year=", yearRange)
        }
        if (!is.null(countrySet)) {
          url <- paste0(url, "&area=", countrySet)
        }
        
        ## Download the data
        ## NOTE: the Flag variable sometimes is interpreted as logic by data.table as "F"
        ## is one of the flag codes, but then data.table understands that is not logic and
        ## goes back to character. However, the previos read flags "F" are converted in "0".
        ## For this reason I force Flag to be character.
        tmp <- try(fread(paste0(url[i], "&output_type=csv"), encoding = "UTF-8", 
                         colClasses = c(Flag = "character")))
        
        if(!inherits(tmp, "try-error")){
          ## This was to account sometimes the download is successful, yet
          ## the data frame is empty
          if(NROW(tmp) != 0){
            cat("OK\n")
            results.dt[i, "Success"] <- TRUE
            results.dt[i, "Reason"] <- "Download Successful"
            results.dt[i, "Time"] <- Sys.time()
            
            ## Adjust the encoding of the title
            for (coltmp in colnames(tmp)){
              Encoding(colnames(tmp)) <- "UTF-8"
            }
            
            if (domainCode[i] %in% c("PM", "CP")) {
              names(tmp)[which(names(tmp) == "Months Code")] <- "Element Code"
              names(tmp)[which(names(tmp) == "Months")] <- "Element"
            }
            if (domainCode[i] == "PE") {
              names(tmp)[which(names(tmp) == "ISO Currency Code")] <- "Element Code"
              names(tmp)[which(names(tmp) == "Currency")] <- "Element"
            }
            ## Add the Element List just before the Element Code
            ## NOTE: the element list is the code used for the downloading
            ## while the element code is the code used in dissemination.
            target <- which(names(tmp) == 'Element Code')[1]
            suppressWarnings(tmp[, `Element List` := elementCode[i]])
            tmp <- 
              cbind(tmp[,1:(target-1), with = FALSE], 
                    tmp[, .(`Element List`)], 
                    tmp[, target:(ncol(tmp)-1), with = FALSE])
            
            ## Remove Year Code
            tmp[, `Year Code` := NULL]
            
            ## Add the Name as first column
            tmp[, "Name" := name[i]]
            tmp <- 
              cbind(tmp[, .(`Name`)], 
                    tmp[,1:(ncol(tmp)-1), with = FALSE])
            
            ## Convert to numeric
            ## NOTE: sometimes there are strange symbols that do not allow 
            ## the value to be numeric
            tmp[, Year := as.integer(Year)]
            tmp[, Value := suppressWarnings(as.numeric(Value))]

            ## Country vs Area
            names(tmp)[which(names(tmp) == "Area")] <- "Country"
            names(tmp)[which(names(tmp) %in% c("Area Code", "Country Code"))] <- 
              "FAOST_CODE"
            
            ## Add Note
            if (length(grep("Note", colnames(tmp))) == 0) {
              suppressWarnings(tmp[, Note := ""])
            }
            
            faoData <- rbind(faoData, tmp)
            i <- i + 1
            retry <- 1
          } else {
            tmp <- c("The specified query has no data, consult FAOSTAT")
            cat(paste(tmp, "\n"))
            class(tmp) <- "try-error"
            attr(tmp, "condition") <-
              list(message = tmp, call = NULL)
            i <- i + 1
            retry <- 1
          }
        } else {
          if(retry <=50){
            print(retry)
            retry <- retry + 1
          } else {
            cat("Download fail after 50 tries\n")
            results.dt[i, "Success"] <- FALSE
            results.dt[i, "Reason"] <- attr(tmp, "condition")$message
            i <- i + 1
            retry <- 1
          }
        }
      }
    }
    entity.dt <- arrange(with(faoData, faoData[FAOST_CODE %in%
                                                 FAOcountryProfile[, FAOST_CODE], ]), FAOST_CODE, Year)
    region.dt <- arrange(with(faoData, faoData[!(FAOST_CODE %in%
                                                   FAOcountryProfile[, FAOST_CODE]), ]), FAOST_CODE, Year)
    cat(paste("\n Number of variables successfully downloaded: ",
              sum(results.dt$Success), " out of ", NROW(results.dt), "\n\n", sep = ""))
    if (toDataFrame) {
      list(entity = as.data.frame(entity.dt), aggregates = as.data.frame(region.dt), 
           results = as.data.frame(results.dt))
    } else {
      list(entity = entity.dt, aggregates = region.dt, results = results.dt)
    }
  }

## The following two variables are hard coded
utils::globalVariables(names = c("FAOST_CODE", "Year"))
