# All REST resource we want to wrap
#
# DONE /rxcui/{rxcui}/filter
# DONE /rxcui?idtype
# DONE /rxcui?name
# DONE /allconcepts
# DONE /rxcui/{rxcui}/allndcs
# DONE /rxcui/{rxcui}/allProperties
# DONE /rxcui/{rxcui}/allrelated
# DONE /approximateTerm
# DONE /displaynames
# DONE /drugs
# DONE /idtypes
# DONE /brands
# DONE /rxcui/{rxcui}/ndcs
# DONE /ndcproperties
# DONE/ndcstatus
#     Review this one, lmk if the error handeling and date helper are fine.
#     Does date_helpers comment need NULL after it?
# DONE /propCategories
# DONE /propnames
# /rxcui/{rxcui}/proprietary
#    Need a valid UMLS license?
# DONE /rxcui/{rxcui}/related?rela
# DONE /rxcui/{rxcui}/related?tty
# DONE /relatypes
# DONE /rxcui/{rxcui}/properties
# DONE /rxcui/{rxcui}/status
# DONE /version
# DONE /rxcui/{rxcui}/property
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

#' Determine if a property exists for a concept
#'
#' Determine if a property exists for a concept and (optionally) matches the
#' specified property value. Returns the RxCUI if the property name matches.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_filterByProperty}{RxNorm}.
#'
#' @param propName Property name
#' @param propValue (optional) Property value.
#'
#' @return RxCUI if the property name matches.
#' @export
rx_filter <- function(rxcui, propName, propValues = "IN"){
  prams <- list(propName = propName, propValues = propValues)
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/filter"),
           query = prams)
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

#' Get the National Drug Codes (NDCs) for the RxNorm concept.
#'
#' The NDCs are returned in the CMS 11-digit NDC derivative form, along with
#' the start and end times (format:"YYYYMM") corresponding to the time interval
#' in which the NDC was associated with the concept.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getAllNDCs}{RxNorm}.
#'
#' @param history (optional) if the value is 1 or not specified, all NDCs, past
#' or present, are returned. A value of 0 indicates only currently associated
#' NDCs with the concept will be returned.
#'
#' @return NDC, along with the start and end times (format:"YYYYMM")
#' @export
rx_allndcs <- function(rxcui, history = 1){
  prams <- list(history = history)
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/allndcs"),
           query = prams)
  parse_results(r)
}

#' Get properties for a specified RxNorm concept.
#'
#' Return the properties for a specified RxNorm concept. Information returned
#' includes property name, value and category
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getAllProperties}{RxNorm}.
#'
#' @param prop Property categories for the properties to be returned.
#'
#' @return Property name, value and category.
#' @export
rx_allProperties <- function(rxcui, prop = "all"){
  prams <- list(prop = prop)
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/allProperties"),
           query = prams)
  parse_results(r)
}

#' Get all the related RxNorm concepts for a given RxNorm identifier.
#'
#' all the related RxNorm concepts for a given RxNorm identifier. This includes
#' concepts of term types "IN", "MIN", "PIN", "BN", "SBD", "SBDC", "SBDF",
#' "SBDG", "SCD", "SCDC", "SCDF", "SCDG", "DF", "DFG", "BPCK" and "GPCK".
#' See \href{https://rxnav.nlm.nih.gov/RxNavViews.html#label:appendix}{default paths}
#' for the paths traveled to get concepts for each term type.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getAllRelatedInfo}{RxNorm}.
#'
#'
#' @return Related RxNorm concepts.
#' @export
rx_allrelated <- function(rxcui){
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/allrelated"))
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
  ingredientids <- paste(ingredientids, collapse = ' ')
  params <- list(ingredientids = ingredientids)
  r <- GET(restBaseURL, path = paste0("REST/brands.json"), query = params)
  parse_results(r)
}

#' Get the active National Drug Codes (NDCs) for the RxNorm concept.
#'
#' Return the active National Drug Codes (NDCs) for the RxNorm concept. Active
#' NDCs are those NDCs present in the current version of RxNorm. Only the NDCs
#' curated by RxNorm (i.e., with SAB = RXNORM) are returned by this function.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getNDCs}{RxNorm}.
#'
#' @return National Drug Codes (NDCs) for the RxNorm concept.
#' @export
rx_ndcs <- function(rxcui){
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/ndcs"))
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
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getPropCategories}{RxNorm}.
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
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getPropNames}{RxNorm}.
#'
#' @return Property names.
#' @export
rx_propnames <- function() {
  r <- GET(restBaseURL, path = paste0("REST/propnames.json"))
  parse_results(r)
}

### Apparently we need a valid UMLS license to use this function. Does that
### mean anything to you Matt?
# rx_proprietary <- function(rxcui){
#   prams <- list(history = history)
#   r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/proprietary"),
#            query = prams)
#   parse_results(r)
# }
NULL

#' Get the related RxNorm identifiers of an RxNorm concept by relational attribute.
#'
#' This function returns the related RxNorm identifiers of an RxNorm concept
#' specified by a relational attribute.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getRelatedByRelationship}{RxNorm}.
#'
#' @param rela A list of the relationship attribute names such as
#' "tradename_of", "has_form", "isa", etc. This field is required. See the
#' {\link{rx_relatypes}} example for the valid relationship attributes.
#'
#' @return Related RxNorm identifiers.
#' @export
rx_related_rela <- function(rxcui, rela){
  prams <- list(rela = paste(rela, collapse = ' '))
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/related"),
           query = prams)
  parse_results(r)
}

#' Get the related RxNorm identifiers of an RxNorm concept by term types.
#'
#' This function returns the related RxNorm identifiers of an RxNorm concept
#' specified by one or more term types.
#' See \href{https://rxnav.nlm.nih.gov/RxNavViews.html#label:appendix}{default paths}
#' for the paths traveled to get concepts for each term type.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getRelatedByType}{RxNorm}.
#'
#' @param tty - a list of one or more RxNorm term types. This field is required.
#' See the {\link{rx_termtypes}} example for the valid term types.
#'
#' @return Related RxNorm identifiers.
#' @export
rx_related_tty <- function(rxcui, tty){
  prams <- list(tty = paste(tty, collapse = ' '))
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/related"),
           query = prams)
  parse_results(r)
}

#' Get relationship names.
#'
#' Return the valid relationship names.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getRelaTypes}{RxNorm}.
#'
#' @return Relationship names.
#' @export
rx_relatypes <- function() {
  r <- GET(restBaseURL, path = paste0("REST/relatypes.json"))
  parse_results(r)
}

#' Get the RxNorm concept properties.
#'
#' Return the RxNorm concept properties: Concept name, Concept identifier
#' (RxCUI), Synonym, RxNorm term type, Language of the term, UMLS CUI,
#' Suppress flag
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getRxConceptProperties}{RxNorm}.
#'
#' @return Concept properties.
#' @export
rx_properties <- function(rxcui){
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/properties"))
  parse_results(r)
}

#' Get the status for a concept.
#'
#' Return the status for a concept. Possible values are:
#' Active (The concept is in the current data set and has a non-obsolete term
#' from the RxNorm vocabulary)
#' Alien (The concept exists in the current data set, but contains only terms
#' from vocabularies other than RxNorm.)
#' Remapped (The concept has been remapped to one or more concepts in the
#' current data set.)
#' Retired (The concept no longer exists in the current data set, or contains
#' only obsolete terms.)
#' Unknown (The concept identifier is invalid.)
#'
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getRxcuiStatus}{RxNorm}.
#'
#' @return Concept status.
#' @export
rx_status <- function(rxcui){
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/status"))
  parse_results(r)
}

#' Get the property values associated with the property name.
#'
#' This function returns the property values associated with the property name.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getRxProperty}{RxNorm}.
#'
#' @param propName the property name. See {\link{rx_propnames}} for the list of
#' valid property names
#'
#' @return Property values associated with the property name.
#' @export
rx_property <- function(rxcui, propName){
  prams <- list(propName = propName)
  r <- GET(restBaseURL, path = paste0("REST/rxcui/", rxcui,"/property"),
           query = prams)
  parse_results(r)
}

#' Get abbreviated source types.
#'
#' Return vocabulary of abbreviated source types.
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getSourceTypes}{RxNorm}.
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
#' See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getSpellingSuggestions}{RxNorm}.
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
