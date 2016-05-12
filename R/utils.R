restBaseURL <- "https://rxnav.nlm.nih.gov/REST/"

# Exhaust the search result over all pages.
exhaust_search <- function(FUN = searchFunction, PARSER = parseFunction, ...) {
  results <- list()
  curPage <- 1
  keepSearching <- TRUE
  while (keepSearching == TRUE) {
    curResult <- PARSER(FUN(..., pageNumber = curPage))
    if (!is.null(curResult)) {
      results <- c(results, list(curResult))
      curPage <- curPage + 1
    } else {
      keepSearching <- FALSE
    }
  }
  unlist(results, recursive = F)
}

# Generic parser function for various response types.
parse_results <- function(result) {
  if(status_code(result) != 200){
    NULL
  } else {
    resContent <- content(result)
    resContent
  }
}