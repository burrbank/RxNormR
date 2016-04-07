# All REST resource we want to wrap
#
# /rxcui/{rxcui}/filter
# /rxcui?idtype
# /rxcui?name
# /allconcepts
# /rxcui/{rxcui}/allndcs
# /rxcui/{rxcui}/allProperties
# /rxcui/{rxcui}/allrelated
# DONE /approximateTerm
# /displaynames
# DONE /drugs
# DONE /idtypes
# /brands
# /rxcui/{rxcui}/ndcs
# /ndcproperties
# /ndcstatus
# /propCategories
# /propnames
# /rxcui/{rxcui}/proprietary
# /rxcui/{rxcui}/related?rela
# /rxcui/{rxcui}/related?tty
# /relatypes
# /rxcui/{rxcui}/properties
# /rxcui/{rxcui}/status
# DONE /version
# /rxcui/{rxcui}/property
# /sourcetypes
# /spellingsuggestions
# DONE /termtypes
NULL

#' Get Drug Information
#'
#' Get the drug products associated with a specified name. The name can be an ingredient, brand name, clinical dose form, branded dose form, clinical drug component, or branded drug component. See \href{http://mor.nlm.nih.gov/download/rxnav/RxNormAPIREST.html#uLink=RxNorm_REST_getDrugs}{RxNorm}.
#'
#' @param drugName An ingredient, brand, clinical dose form, branded dose form, clinical drug component or branded drug component name
#'
#' @return Some results.
#' @export
rx_drugs <- function(drugName) {
  params = list(name = drugName)
  r <- GET(restBaseURL, path = paste0("REST/drugs.json"), query = params)
  parse_results(r)
}

#' Get ID Types in RxNorm
#'
#' Get the valid identifier types of the RxNorm data set.
#'
#' @return Identifier type information.
#' @export
rx_idtypes <- function() {
  r <- GET(restBaseURL, path = paste0("REST/idtypes.json"))
  parse_results(r)
}

#' Get Version of API
#'
#' Get the version of the RxNorm data set. See \href{http://mor.nlm.nih.gov/download/rxnav/RxNormAPIREST.html#uLink=RxNorm_REST_getRxNormVersion}{RxNorm}.
#'
#' @return Version information.
#' @export
rx_version <- function() {
  r <- GET(restBaseURL, path = paste0("REST/version.json"))
  parse_results(r)
}

#' Get RxNorm Term Types
#'
#' Get the valid term types in the RxNorm data set. See \href{http://mor.nlm.nih.gov/download/rxnav/RxNormAPIREST.html#uLink=RxNorm_REST_getTermTypes}{RxNorm}.
#'
#' @return Term type information.
#' @export
rx_termtypes <- function() {
  r <- GET(restBaseURL, path = paste0("REST/termtypes.json"))
  parse_results(r)
}

#' Get RxNorm Approximate Terms
#'
#' Do an approximate match search to determine the strings in the data set that
#' most closely match the search string.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getApproximateMatch}{RxNorm}.
#'
#' @return The returned comment field contains messages about the processing
#' of the operation.
#' @export
rx_approximateTerm <- function(term, maxEntries = 20, option = 0) {
  params <- list(term = term, maxEntries = maxEntries, option = option)
  r <- GET(restBaseURL, path = paste0("REST/approximateTerm.json"), query = params)
  parse_results(r)
}


