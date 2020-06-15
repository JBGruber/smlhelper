#' Prepares a list of dfms using different preprocessing steps
#'
#' @param corp Preferably a corpus object but can contain everything accepted by
#'   quanteda::tokens.
#' @param use_ngrams Logical Should the ngrams step be included?
#' @param stopwords A character vector of stopwords.
#'
#' @return A tibble containing a list of dfms and information about
#'   preprocessing steps
#' @details Following the notation by Denny and Spirling (2018) the
#'   preprocessing steps included are:
#'
#' \itemize{
#'   \item **P** Punctuation
#'   \item **N** Numbers
#'   \item **L** Lowercasing
#'   \item **S** Stemming
#'   \item **W** Stopword Removal
#'   \item **3** n-gram Inclusion
#'   \item **I** Infrequently Used Terms
#' }
#' @import quanteda
#' @importFrom progress progress_bar
#' @export
batch_prep <- function(corp,
                       use_ngrams = TRUE,
                       stopwords = stopwords::stopwords(language = "en")) {

  sets <- data.frame(expand.grid(list(
    removePunctuation = c(TRUE,FALSE),
    removeNumbers = c(TRUE,FALSE),
    lowercase = c(TRUE,FALSE),
    stem = c(TRUE,FALSE),
    removeStopwords = c(TRUE,FALSE),
    infrequent_terms = c(TRUE, FALSE),
    use_ngrams = if (use_ngrams) c(TRUE, FALSE) else FALSE
  )))

  sets$labels <- apply(sets, 1, function(x) paste(c("P","N","L","S","W","I","3")[x], collapse = "-"))

  if (interactive()) {
    pb <- progress::progress_bar$new(total = nrow(sets),
                                     format = "[:bar] :current/:total (:percent) :eta")
  } else {
    pb <- NULL
  }

  dfms_list <- mapply(
    prep,
    removePunctuation = sets$removePunctuation,
    removeNumbers = sets$removeNumbers,
    lowercase = sets$lowercase,
    stem = sets$stem,
    removeStopwords = sets$removeStopwords,
    infrequent_terms = sets$infrequent_terms,
    use_ngrams = sets$use_ngrams,
    MoreArgs = list(x = corp, pb = pb, stopwords = stopwords)
  )

  names(dfms_list) <- sets$labels

  return(dfms_list)
}

#' Prep a dfm doing or don't doing certain preprocessing steps
#'
#' @param x Preferably a corpus object but can contain everything accepted by
#'   quanteda::tokens.
#' @param removePunctuation,removeNumbers,lowercase,stem,removeStopwords,infrequent_terms,use_ngrams Logical. Should a preprocessing step be included or not.
#' @param pb A progress_bar environment from the progress package.
#' @param stopwords A character vector of stopwords.
#'
#' @return a dfm.
#' @import quanteda
#' @importFrom stopwords stopwords
#' @export
prep <- function(x,
                 removePunctuation,
                 removeNumbers,
                 lowercase,
                 stem,
                 removeStopwords,
                 infrequent_terms,
                 use_ngrams,
                 stopwords = stopwords::stopwords(language = "en"),
                 pb = NULL) {

  if (!is.null(pb)) {
    pb$tick()
  }

  if (!quanteda::is.tokens(x)) x <- quanteda::tokens(x)

  out <- quanteda::tokens(
    x,
    remove_punct = removePunctuation,
    remove_symbols = TRUE,
    remove_numbers = removeNumbers,
    remove_url = TRUE,
    remove_separators = TRUE
  )

  if (removeStopwords) out <- quanteda::tokens_remove(
    out, pattern = stopwords::stopwords(language = "en"),
    valuetype = "fixed"
  )

  if (use_ngrams) out <- quanteda::tokens_ngrams(out, n = 1:3)

  out <- quanteda::dfm(out, tolower = lowercase, stem = stem)

  # discard terms that appear in less than 0.5%–1% of documents (Grimmer 2010;
  # Yano, Smith, and Wilkerson 2012; Grimmer and Stewart 2013);
  if (infrequent_terms) out <- quanteda::dfm_trim(out,
                                                  min_docfreq = 0.01,
                                                  docfreq_type = "prop")

  return(out)
}
