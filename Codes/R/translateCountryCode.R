##' A function to translate between different country coding systems
##'
##' The function translate any country code scheme to another if both
##' are in the FAOcountryProfile
##'
##' @param data The data frame
##' @param from The name of the old coding system
##' @param to The name of the new coding system
##' @param oldCode The column name of the old country coding scheme
##' @export
##'
##' @examples
##' 
##' ## test.dt <- 
##' ##   FAOSTAT(name = c("Grapes_AreaHarv"), domainCode = "QC",
##' ##           itemCode = 560, elementCode = 2312, yearRange = 2010)
##' ## test.dt <- test.dt$entity
##' ## test.dt <- 
##' ##   translateCountryCode(data = test.dt, from = "FAOST_CODE", 
##' ##                        to = "ISO2_CODE")

translateCountryCode = function (data, from, to, oldCode) {
    cat("\nNOTE: Please make sure that the country are matched according to their definition\n\n")
    if (missing(oldCode))
        oldCode = from
    if (from != to) {
        codeTrans = FAOcountryProfile[, c(from, to), with = FALSE]
        trans.dt = merge(x = codeTrans, y = data, by.x = from,
            by.y = oldCode, all.y = TRUE)
        if (any(is.na(trans.dt[, to, with = FALSE]))) {
            warning(paste("The following entries does not have '",
                to, "' available\n", sep = ""), immediate. = TRUE)
            print(trans.dt[is.na(eval(parse(text = to))), c(from, to, "Country"), with = FALSE])
        }
    }
    else {
        trans.dt = data
    }
    trans.dt
}

utils::globalVariables(names = c("FAOcountryProfile"))