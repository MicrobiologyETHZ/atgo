% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agi2go.r
\name{agi2go}
\alias{agi2go}
\title{Function to}
\usage{
agi2go(
  agi,
  variants = TRUE,
  relationships = "all",
  aspects = "all",
  evidence = "any"
)
}
\arguments{
\item{agi}{A vector of AGI codes for the A. thaliana genes of interest}

\item{variants}{A logical indicating whether or not isoform variants are included as part of their locus. Default: TRUE}

\item{relationships}{A vector of GO relationships to consider, see http://www.ontobee.org/ontology/RO .  Default: all}

\item{evidence}{A vector of GO evidence codes to consider, see http://geneontology.org/docs/guide-go-evidence-codes/ . Default: any}

\item{aspect}{A vector of GO aspects to consider. F: molecular function; C: cellular component; P: biological process. Default: all}
}
\value{
A list of GO IDs.
}
\description{
Function to
}
\details{

}
\examples{
None
}
\author{
Chris Field \href{mailto:fieldc@ethz.ch}{fieldc@ethz.ch}
}
