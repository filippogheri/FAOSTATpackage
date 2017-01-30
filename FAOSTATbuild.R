###########################################################################
## Title: This script builds the FAOSTAT package
## Created: 09/05/2013
## Updated: 09/07/2014
###########################################################################

# http://fenixservices.fao.org/faostat/api/v1/en/dimensions/FT
# http://fenixservices.fao.org/faostat/api/v1/en/codes/item/PM
# setwd("C:/Users/filip/OneDrive/Documenti/GitHub/FAOSTATpackage/")

# Needed libraries --------------------------------------------------------

library(roxygen2)
library(RJSONIO)
library(utils)
library(knitr)

# Ghost script ------------------------------------------------------------

# Sys.setenv(R_GSCMD='"C:/Program Files/gs/gs9.07/bin/gswin32c.exe"')

# FAOcountryProfile -------------------------------------------------------

tmp <- 
  read.csv(file = "FAOcountryProfileUTF8.csv", header = TRUE,
           stringsAsFactors = FALSE, na.string = "", encoding = "UTF-8")
tmp[tmp == " "] <- NA
FAOcountryProfile <- 
  tmp[, c("FAOST_CODE", "ADM0_CODE", "ISO2_CODE", "ISO2_WB_CODE",
          "ISO3_CODE", "ISO3_WB_CODE", "UN_CODE", "CRS_CODE", "MOTHER_M49_CODE",
          "M49_FAOST_CODE", "FAO_TABLE_NAME", "OFFICIAL_FAO_NAME", 
          "UNOFFICIAL1_NAME", "UNOFFICIAL2_NAME", "UNOFFICIAL3_NAME", 
          "SHORT_NAME", "ABBR_FAO_NAME_SP", "UN_YEAR_START_INFO",
          "UN_YEAR_END_INFO", "FORMER_MEMBER_INFO", "COMPOSITION_INFO", 
          "UN_STATUS_INFO")]
FAOcountryProfile <- data.table(FAOcountryProfile)
save(FAOcountryProfile, file = "FAOcountryProfile.RData")

# FAOmetaTable ------------------------------------------------------------

baseUrl <- "http://fenixservices.fao.org/faostat/api/v1/en/"

## Groups
groupsUrl <- paste0(baseUrl, "groups")
groupCode <- 
  unique(data.frame(groupCode = sapply(fromJSON(groupsUrl, encoding = "UTF-8")$data, 
                                       function(x) x[1]),
                    groupName = sapply(fromJSON(groupsUrl, encoding = "UTF-8")$data, 
                                       function(x) x[2]),
                    stringsAsFactors = FALSE))

## Groups and Domains
domainUrl <- paste0(baseUrl, "groupsanddomains")
domainCode <- 
  unique(data.frame(groupCode = sapply(fromJSON(domainUrl, encoding = "UTF-8")$data, 
                                       function(x) x[1]),
                    domainCode = sapply(fromJSON(domainUrl, encoding = "UTF-8")$data, 
                                       function(x) x[3]),
                    domainName = sapply(fromJSON(domainUrl, encoding = "UTF-8")$data, 
                                       function(x) x[4]),
                    stringsAsFactors = FALSE))
## Indicators from Household Surveys
## Employment indicators
## Development flows to agriculture
## Food aid shipments
domainCode <-
  subset(domainCode, !domainCode %in% c("HS", "OE", "EA", "FA"))

## Domains and Elements
elemCode <- data.frame()
noElemCode <- data.frame()
for(i in 1:NROW(domainCode)){
  tmp <- try(fromJSON(paste0(baseUrl, "codes/element/", domainCode[i, "domainCode"]), 
                      encoding = "UTF-8")$data)
  if(!inherits(tmp, "try-error") & length(tmp) != 0){
    tmp2 <- unique(data.frame(domainCode = domainCode[i, "domainCode"],
                              elementCode = sapply(tmp, function(x) x[1]),
                              elementName = sapply(tmp, function(x) x[2]),
                              stringsAsFactors = FALSE))
    elemCode <- rbind(elemCode, tmp2)
  } else {
    noElemCode <- rbind(noElemCode, domainCode[i, ])
    ## Producer prices - monthly & Consumer price indeces: treating 
    ## month as an element
    if (domainCode[i, "domainCode"] %in% c("PM", "CP")) {
      tmp <- try(fromJSON(paste0(baseUrl, "codes/months/", domainCode[i, "domainCode"]), 
                          encoding = "UTF-8")$data)
      if(!inherits(tmp, "try-error") & length(tmp) != 0){
        tmp2 <- unique(data.frame(domainCode = domainCode[i, "domainCode"],
                                  elementCode = sapply(tmp, function(x) x[1]),
                                  elementName = sapply(tmp, function(x) x[2]),
                                  stringsAsFactors = FALSE))
        elemCode <- rbind(elemCode, tmp2)
      }
    }
    ## Exchange rates - Annual: treating currency as an element
    if (domainCode[i, "domainCode"] %in% c("PE")) {
      tmp <- try(fromJSON(paste0(baseUrl, "codes/currency/", domainCode[i, "domainCode"]), 
                          encoding = "UTF-8")$data)
      if(!inherits(tmp, "try-error") & length(tmp) != 0){
        tmp2 <- unique(data.frame(domainCode = domainCode[i, "domainCode"],
                                  elementCode = sapply(tmp, function(x) x[1]),
                                  elementName = sapply(tmp, function(x) x[2]),
                                  stringsAsFactors = FALSE))
        elemCode <- rbind(elemCode, tmp2)
      }
    }
  }
}

## Domains and Items
itemCode <- data.frame()
noItemCode <- data.frame()
for(i in 1:NROW(domainCode)){
  tmp <- try(fromJSON(paste0(baseUrl, "codes/item/", domainCode[i, "domainCode"]), 
                      encoding = "UTF-8")$data)
  if(!inherits(tmp, "try-error") & length(tmp) != 0){
    tmp2 <- unique(data.frame(domainCode = domainCode[i, "domainCode"],
                              itemCode = sapply(tmp, function(x) x[1]),
                              itemName = sapply(tmp, function(x) x[2]),
                              itemType = sapply(tmp, function(x) x[3]),
                              stringsAsFactors = FALSE))
    itemCode <- rbind(itemCode, tmp2)
  } else {
    noItemCode <- rbind(noItemCode, domainCode[i, ])
  }
}
itemAggCode <- itemCode[itemCode[, "itemType"] == "+", 
                        c("domainCode", "itemCode", "itemName")]
itemGroupCode <- itemCode[itemCode[, "itemType"] == ">", 
                        c("domainCode", "itemCode", "itemName")]
itemCode <- itemCode[itemCode[, "itemType"] == "0", 
                     c("domainCode", "itemCode", "itemName")]

## Areas
areaCode <- data.frame()
noAraCode <- data.frame()
for(i in 1:NROW(domainCode)){
  tmp <- try(fromJSON(paste0(baseUrl, "codes/area/", domainCode[i, "domainCode"]), 
                      encoding = "UTF-8")$data)
  if(!inherits(tmp, "try-error") & length(tmp) != 0){
    tmp2 <- unique(data.frame(domainCode = domainCode[i, "domainCode"],
                              areaCode = sapply(tmp, function(x) x[1]),
                              areaName = sapply(tmp, function(x) x[2]),
                              areaType = sapply(tmp, function(x) x[3]),
                              stringsAsFactors = FALSE))
    areaCode <- rbind(areaCode, tmp2)
  } else {
    noAraCode <- rbind(noAraCode, domainCode[i, ])
    ## Detailed trade matrix, Forestry Trade Flows, Food Aid Shipments
    if (domainCode[i, "domainCode"] %in% c("TM", "FT", "FA")) {
      tmp <- try(fromJSON(paste0(baseUrl, "codes/reporterarea/", domainCode[i, "domainCode"]), 
                          encoding = "UTF-8")$data)
      if(!inherits(tmp, "try-error") & length(tmp) != 0){
        tmp2 <- unique(data.frame(domainCode = domainCode[i, "domainCode"],
                                  areaCode = sapply(tmp, function(x) x[1]),
                                  areaName = sapply(tmp, function(x) x[2]),
                                  areaType = sapply(tmp, function(x) x[3]),
                                  stringsAsFactors = FALSE))
        areaCode <- rbind(areaCode, tmp2)
      }
    }
  }
}
areaAggCode <- areaCode[areaCode[, "areaType"] == "+", 
                        c("domainCode", "areaCode", "areaName")]
areaGroupCode <- areaCode[areaCode[, "areaType"] == ">", 
                          c("domainCode", "areaCode", "areaName")]
areaCode <- areaCode[areaCode[, "areaType"] == "0", 
                     c("domainCode", "areaCode", "areaName")]

## Table
FAOmetaTable <- 
  list(groupTable = groupCode, domainTable = domainCode,
       itemTable = itemCode, itemAggTable = itemAggCode, 
       itemGroupTable = itemGroupCode, elementTable = elemCode,
       areaTable = areaCode, areaAggTable = areaAggCode,
       areaGroupTable = areaGroupCode)
save(FAOmetaTable, file = "FAOmetaTable.RData")

# Building the package ----------------------------------------------------

## Remove the folder if it exists
if(file.exists("./FAOSTAT"))
    unlink("FAOSTAT", recursive = TRUE)

## Build the package
package.skeleton("FAOSTAT", code_files = paste("./Codes/R/",
                                               dir("./Codes/R/",
                                                   pattern = "\\.R$"), sep = ""),
                 force = FALSE)

## Include the data
dir.create("FAOSTAT/data")
file.copy(from = "./FAOcountryProfile.RData",
          to = "FAOSTAT/data/", overwrite = TRUE)
file.copy(from = "./FAOmetaTable.RData",
          to = "FAOSTAT/data/", overwrite = TRUE)
file.copy(from = "./DESCRIPTION", to = "FAOSTAT/",
          overwrite = TRUE)
unlink("./FAOSTAT/Read\\-and\\-delete\\-me")

## Include Demo
dir.create("FAOSTAT/demo")
file.copy(from = "./FAOSTATdemo.R",
          to = "FAOSTAT/demo/", overwrite = TRUE)
cat("FAOSTATdemo      Demonstration for the FAOSTAT package\n",
    file = "FAOSTAT/demo/00Index")

# ## Include tests
dir.create("FAOSTAT/tests")
file.copy("Codes/tests", "FAOSTAT", recursive = TRUE)

## Use roxygen to build the documentation
roxygenize("FAOSTAT")

## Include vignette
# dir.create("./FAOSTAT/vignettes/")
# dir.create("./FAOSTAT/inst/")
# dir.create("./FAOSTAT/inst/doc/")
# file.copy(from = "./Documentation/FAOSTAT.pdf",
#           to = "./FAOSTAT/inst/doc/", overwrite = TRUE)
# file.copy(from = "./Documentation/FAOSTAT.Rnw",
#           to = "./FAOSTAT/vignettes/", overwrite = TRUE)
# file.copy(from = "./Documentation/FAOSTAT.pdf",
#          to = "./FAOSTAT/vignettes/", overwrite = TRUE)

## Build and check the package
system("R CMD INSTALL --build FAOSTAT")
system("R CMD build FAOSTAT")
system("R CMD check --as-cran --timings FAOSTAT")

## Install package from local directory
# library(FAOSTAT)

###########################################################################
## End
###########################################################################
