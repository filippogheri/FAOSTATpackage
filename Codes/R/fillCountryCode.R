##' A function to get country code when not available in data.
##'
##' This function can be useful when a dataset provided does not have
##' a country code available.
##'
##' @param data The data frame to be matched
##' @param country The column name of the data which contains
##' the country name
##' @param outCode The output country code system, defaulted to FAO standard.
##' @export
##'
##' @examples
##' 
##' ## test.dt <- 
##' ##   FAOSTAT(name = c("Grapes_AreaHarv"), domainCode = "QC",
##' ##           itemCode = 560, elementCode = 2312, yearRange = 2010)
##' ## test.dt <- test.dt$entity
##' ## test.dt <- 
##' ##   fillCountryCode(data = test.dt, country = "Country", outCode = "ISO2_CODE")

fillCountryCode = function(data, country, outCode = "FAOST_CODE"){
  unqCountry = unique(data[, country, with = FALSE])
  n = nrow(unqCountry)
  countryCODE = rep(NA, n)
  for(i in 1:n){
    ind = which(as.matrix(FAOcountryProfile[,
      c("OFFICIAL_FAO_NAME", "SHORT_NAME", "FAO_TABLE_NAME",
        "UNOFFICIAL1_NAME", "UNOFFICIAL2_NAME", "UNOFFICIAL3_NAME",
        "ABBR_FAO_NAME_SP")]) ==
      as.character(unqCountry[i]), arr.ind = TRUE)
    which.row = ind[, 1]
    if(length(unique(which.row)) == 1)
      countryCODE[i] = as.character(FAOcountryProfile[unique(which.row), outCode, with = FALSE])
  }
  if(anyDuplicated(na.omit(countryCODE)))
    warning(paste0("Duplicated ", outCode, " matched, double check the data"))
    dt = data.table(unqCountry, countryCODE)
  names(dt) = c(country, outCode)
  if (any(is.na(dt[, outCode, with = FALSE]))) {
    warning(paste("The following entries does not have '",
                  outCode, "' available\n", sep = ""), immediate. = TRUE)
    print(dt[is.na(eval(parse(text = outCode))),])
  }
  merge(x = data, y = dt, by = country, all.x = TRUE)
}

utils::globalVariables(names = c("FAOcountryProfile"))