% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep.R
\name{batch_prep}
\alias{batch_prep}
\title{Prepares a list of dfms using different preprocessing steps}
\usage{
batch_prep(
  corp,
  use_ngrams = TRUE,
  stopwords = stopwords::stopwords(language = "en")
)
}
\arguments{
\item{corp}{Preferably a corpus object but can contain everything accepted by
quanteda::tokens.}

\item{use_ngrams}{Logical Should the ngrams step be included?}

\item{stopwords}{A character vector of stopwords.}
}
\value{
A tibble containing a list of dfms and information about
  preprocessing steps
}
\description{
Prepares a list of dfms using different preprocessing steps
}
\details{
Following the notation by Denny and Spirling (2018) the
  preprocessing steps included are:

\itemize{
  \item **P** Punctuation
  \item **N** Numbers
  \item **L** Lowercasing
  \item **S** Stemming
  \item **W** Stopword Removal
  \item **3** n-gram Inclusion
  \item **I** Infrequently Used Terms
  \item **T** tf–idf (term frequency–inverse document frequency) weighting of terms
}
}
