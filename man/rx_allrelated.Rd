% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apis.R
\name{rx_allrelated}
\alias{rx_allrelated}
\title{Get all the related RxNorm concepts for a given RxNorm identifier.}
\usage{
rx_allrelated(rxcui)
}
\arguments{
\item{rxcui}{The RxNorm Concept Unique Identifier (RXCUI)}
}
\value{
Related RxNorm concepts.
}
\description{
all the related RxNorm concepts for a given RxNorm identifier. This includes
concepts of term types "IN", "MIN", "PIN", "BN", "SBD", "SBDC", "SBDF",
"SBDG", "SCD", "SCDC", "SCDF", "SCDG", "DF", "DFG", "BPCK" and "GPCK".
See \href{https://rxnav.nlm.nih.gov/RxNavViews.html#label:appendix}{default paths}
for the paths traveled to get concepts for each term type.
See \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getAllRelatedInfo}{RxNorm}.
}

