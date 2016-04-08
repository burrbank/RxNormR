# All REST resource we want to wrap
#
# /rxcui/{rxcui}/filter
# DONE /rxcui?idtype
# DONE /rxcui?name
# DONE /allconcepts
# /rxcui/{rxcui}/allndcs
# /rxcui/{rxcui}/allProperties
# /rxcui/{rxcui}/allrelated
# DONE /approximateTerm
# DONE /displaynames
# DONE /drugs
# DONE /idtypes
# DONE /brands
# /rxcui/{rxcui}/ndcs
# DONE /ndcproperties
# DONE/ndcstatus
#     Review this one, lmk if the error handeling and date helper are fine.
#     Does date_helpers comment need NULL after it?
# DONE /propCategories
# DONE /propnames
# /rxcui/{rxcui}/proprietary
# /rxcui/{rxcui}/related?rela
# /rxcui/{rxcui}/related?tty
# DONE /relatypes
# /rxcui/{rxcui}/properties
# /rxcui/{rxcui}/status
# DONE /version
# /rxcui/{rxcui}/property
# DONE /sourcetypes
# DONE /spellingsuggestions
# DONE /termtypes
#
#
# Does parse_results change order of results?
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
  params <- list(name = drugName)
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

#' Get RxCUIs from another vocabulary identifier.
#'
#' Search for an identifier from another vocabulary and return the RxCUIs of any
#' concepts which have an RxNorm term as a synonym or have that identifier as an
#' attribute.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_findRxcuiById}{RxNorm}.
#'
#' @param idtype Identifier type. Valid types are AMPID, ANDA, ATC, GCN_SEQNO,
#' GFC, GPPC, HIC_SEQN, MESH, MMSL_CODE, NDA, NDC, NUI, SNOMEDCT, SPL_SET_ID,
#' UMLSCUI, UNII_CODE, and VUID
#' @param id Identifier.
#' @param allsrc Flag indicating whether to consider all identifiers in the
#' RxNorm data set (=1) or only identifiers which are related synonyms to an
#' RxNorm vocabulary term (=0).
#'
#' @return RxCUIs
#' @export
rx_rxcui_idtype <- function(idtype, id, allsrc = 0) {
  params <-list(idtype = idtype, id = id, allsrc = allsrc)
  r <- GET(restBaseURL, path = paste0("REST/rxcui.json"), query = params)
  parse_results(r)
}

#' Get RxCUIs for a ame in RxNorm.
#'
#' Search for a name in the RxNorm data set and return the RxCUIs of any
#' concepts which have that name as an RxNorm term or as a synonym of an RxNorm
#' term. Search options include exact match searching or normalized string
#' search.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_findRxcuiByString}{RxNorm}.
#'
#' @param name Search string.
#' @param srclist List of the source vocabulary names to use. Only used when
#' allsrc is set to 1. See the sourcetypes() example for the valid source
#' vocabularies. If not specified, all sources are used.
#' @param allsrc Flag indicating whether to consider all terms in the RxNorm
#' data set (=1) or only terms which are either from the RxNorm vocabulary or
#' related synonyms from other vocabularies (=0).
#' @param search Number indicating the type of search to be performed. If set to
#' 0 (the default), exact match search will be performed. If set to 1, a
#' normalized string search will be done. When search = 2, then an exact match
#' search will be done, and if no results are found, a normalized search will be
#' done.
#'
#' @return RxCUIs
#' @export
rx_rxcui_name <- function(name, srclist = NULL, allsrc = 0, search = 0) {
  params <- list(name = name, srclist = srclist, allsrc = allsrc,
                 search = search)
  r <- GET(restBaseURL, path = paste0("REST/rxcui.json"), query = params)
  parse_results(r)
}

#' Get RxNorm Approximate Terms
#'
#' Do an approximate match search to determine the strings in the data set that
#' most closely match the search string.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getApproximateMatch}{RxNorm}.
#'
#' @return Approximate terms.
#' @export
rx_approximateTerm <- function(term, maxEntries = 20, option = 0) {
  params <- list(term = term, maxEntries = maxEntries, option = option)
  r <- GET(restBaseURL, path = paste0("REST/approximateTerm.json"), query = params)
  parse_results(r)
}

#' Get RxNorm Concept Information
#'
#' Get concept information for specified term types.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getAllConceptsByTTY}{RxNorm}.
#'
#' @return Content information
#' @export
rx_allconcepts <- function(tty) {
  params <- list(tty = tty)
  r <- GET(restBaseURL, path = paste0("REST/allconcepts.json"), query = params)
  parse_results(r)
}

#' Get RxNav names
#'
#' Gets the names used by RxNav for auto completion. A large list which includes
#' names of ingredients, brands, and branded packs.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getDisplayTerms}{RxNorm}.
#'
#' @return Names of ingredients, brands, and branded packs.
#' @export
rx_displaynames <- function() {
  r <- GET(restBaseURL, path = paste0("REST/displaynames.json"))
  parse_results(r)
}

#' Get brands that contain all specified ingredients.
#'
#' Get the brands that contain all the specified ingredients. Note that the
#' brands returned may contain other ingredients in addition to those specified.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getMultiIngredBrand}{RxNorm}.
#'
#' @param ingredientids list of (or single) ingredient RxCUIs.
#'
#' @return Brand names
#' @export
rx_brands <- function(ingredientids) {
  ingredientids <- paste(unlist(ingredientids), collapse = ' ')
  params <- list(ingredientids = ingredientids)
  r <- GET(restBaseURL, path = paste0("REST/brands.json"), query = params)
  parse_results(r)
}

#' Get the National Drug Code (NDC) properties.
#'
#' This function returns the NDC properties for an RxNorm concept, an NDC or a
#' structured product label. Only NDC properties of active NDCs curated by
#' RxNorm are returned.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getNDCProperties}{RxNorm}.
#'
#' @param id Valid identifiers are NDC 2 segment, NDC 3 segment, NDC11, RxCUI,
#' and SPL_SET_ID
#'
#' @return NDC properties, NDC, or structured product label.
#' @export
rx_ndcproperties <- function(id) {
  params <- list(id = id)
  r <- GET(restBaseURL, path = paste0("REST/ndcproperties.json"), query = params)
  parse_results(r)
}

#' Get the status for a National Drug Code (NDC).
#'
#' This resource returns the NDC status along with history data for the NDC.
#' Possible status values returned are:
#'
#' Active: The NDC is contained in the current data set.
#'
#' Obsolete: The NDC previously existed in RxNorm, but is no longer recognized
#' as active.
#'
#' Alien: The NDC exists in the current data set, but only in a source
#' vocabulary other than RxNorm.
#'
#' Unknown: The NDC is unknown or invalid.
#'
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getNDCStatus}{RxNorm}.
#'
#' @param ndc National Drug Code (NDC).
#' @param start (optional) Starting date used in conjunction with an end date to
#' provide an interval to return history data. If specified, an end date must
#' also be specified.
#' @param end (optional) Ending date used in conjuction with a start date to
#' provide an interval to return history data.If specified, a start date must
#' also be specified.
#' @param history (optional, default:0) 0 = include all history. 1 = include
#' only lastest entry
#'
#' @return NDC status and history data for the NDC.
#' @export
rx_ndcstatus <-function(ndc, start = NULL, end = NULL, history = 0) {
  if(length(start) + length(end) == 1){
    stop("If start or end date are provided, they both must be.")
  }
  start <- date_helper(start)
  end <- date_helper(end)
  params <- list(ndc = as.character(ndc), start = start,
                 end = end, history = history)
  r <- GET(restBaseURL, path = paste0('REST/ndcstatus.json'), query = params)
  parse_results(r)
}

# Convert dates to the appropriate format for rx_ndcstatus
date_helper <- function(input_date){
  if(class(input_date) == "Date"){
    input_date <- format(input_date, '%Y%m')
  }
  input_date
}

#' Get property categories.
#'
#' Return the RxNorm property categories.
#'
#' @return Property categories.
#' @export
rx_propCategories <- function() {
  r <- GET(restBaseURL, path = paste0("REST/propCategories.json"))
  parse_results(r)
}

#' Get property names.
#'
#' Return the valid property names.
#'
#' @return Property names.
#' @export
rx_propnames <- function() {
  r <- GET(restBaseURL, path = paste0("REST/propnames.json"))
  parse_results(r)
}

#' Get relationship names.
#'
#' Return the valid relationship names.
#'
#' @return Relationship names.
#' @export
rx_relatypes <- function() {
  r <- GET(restBaseURL, path = paste0("REST/relatypes.json"))
  parse_results(r)
}

#' Get abbreviated source types.
#'
#' Return vocabulary of abbreviated source types.
#'
#' @return Abbreviated source types.
#' @export
rx_sourcetypes<- function() {
  r <- GET(restBaseURL, path = paste0("REST/sourcetypes.json"))
  parse_results(r)
}

#' Get spelling suggestions for a given term.
#'
#' Return spelling suggestions for a given term. The suggestions are RxNorm
#' terms contained in the current version, listed in decreasing order of
#' closeness to the original phrase.
#'
#' @param name Name for which spelling suggestions are to be generated.
#'
#' @return RxNorm terms.
#' @export
rx_spellingsuggestions <- function(name) {
  params <- list(name = name)
  r <- GET(restBaseURL, path = paste0("REST/spellingsuggestions.json"),
           query = params)
  parse_results(r)
}

# <- function() {
#   params <-
#   r <- GET(restBaseURL, path = paste0())
#   parse_results(r)
# }
#
NULL
