restBaseURL <- "https://rxnav.nlm.nih.gov/REST/"

# Generic parser function for various response types.
parse_results <- function(result) {
  if(status_code(result) != 200){
    NULL
  } else {
    resContent <- content(result)
    resContent
  }
}