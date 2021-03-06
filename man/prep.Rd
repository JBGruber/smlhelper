% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep.R
\name{prep}
\alias{prep}
\title{Prep a dfm doing or don't doing certain preprocessing steps}
\usage{
prep(
  x,
  remove_punct,
  remove_num,
  lowercase,
  stem,
  remove_stop,
  infrequent_terms,
  tfidf,
  use_ngrams,
  stopwords = stopwords::stopwords(language = "en"),
  pb = NULL
)
}
\arguments{
\item{x}{Preferably a corpus object but can contain everything accepted by
quanteda::tokens.}

\item{remove_punct, remove_num, lowercase, stem, remove_stop, infrequent_terms, tfidf, use_ngrams}{Logical. Should a preprocessing step be included or not.}

\item{stopwords}{A character vector of stopwords.}

\item{pb}{A progress_bar environment from the progress package.}
}
\value{
a dfm.
}
\description{
Prep a dfm doing or don't doing certain preprocessing steps
}
