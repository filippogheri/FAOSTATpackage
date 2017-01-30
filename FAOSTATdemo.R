###########################################################################
## Title: Demo of the FAOSTAT package
## Updated: 15/11/2016
## Notes:
###########################################################################

# Install the package -----------------------------------------------------

# if(!is.element("FAOSTAT", .packages(all.available = TRUE)))
#   install_github(username = "filippogheri", repo = "FAOSTATpackage", 
#                  subdir = "FAOSTAT")
library(FAOSTAT)
# help(package = "FAOSTAT")
# vignette("FAOSTAT", package = "FAOSTAT")
library(data.table)

# FAOsearch function ------------------------------------------------------

## Use the interective function to search the codes.
FAOSTATsearch()
## Use the result of the search to download the data.
test.df <- FAOSTAT(query = .LastSearch)

# FAOSTAT -----------------------------------------------------------------

## Production domain
production.dt <- 
  data.table(varName = c("Grapes_AreaHarv", 
                         "Wine_ProdQuantity", 
                         "CattleBuffaloes_Stocks", 
                         "MilkProducts_Yield",
                         "Yoghurt_ProdQuantity",
                         "Food _GPIN",
                         "Agriculture_NetProdValue"),
             domainCode = c("QC", "QD", "QA", "QL", "QP", "QI", "QV"),
             itemCode = c(560, 564, 1746, "1780>", 891, 2054, 2051),
             elementCode = c(2312, 2510, 2111, 2413, 2510, 432, 154))
production.lst <- with(production.dt,
                       FAOSTAT(name = varName, domainCode = domainCode,
                               itemCode = itemCode, elementCode = elementCode,
                               yearRange = c(2005:2010), countrySet = c(106, 5400)))

## Trade
tradeFlow.dt <- 
  data.table(varName = c("Wine_ImpVal", 
                         "Sawnwood_Imp"),
             domainCode = c("TM", "FT"),
             itemCode = c(564, 1632),
             elementCode = c(2620, 2610))
tradeFlow.lst <- with(tradeFlow.dt,
                      FAOSTATtrade(name = varName, domainCode = domainCode,
                                   itemCode = itemCode, elementCode = elementCode,
                                   yearRange = c(2010), countrySet = c(68, 106)))

# fillCountryCode function ------------------------------------------------


# translateCountryCode function -------------------------------------------


