#' @importFrom httr POST status_code content GET
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to RxNormR. By using this package you agree to the Terms of Service for the RxNorm API: http://mor.nlm.nih.gov/download/rxnav/TermOfService.html.")
}