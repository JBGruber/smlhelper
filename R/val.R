#' Batch validate algorithms over a list of dfms
#'
#' @param x A dfm or list of dfms to evaluate.
#' @param y A variable in docvars of the dfms with the class (must be logical
#'   for now).
#' @param set A variable in docvars indicating membership to training or test
#'   set.
#' @param alg A named list of functions containing the algorithms to be
#'   evaluated (see example)
#' @param pred A list of prediction functions (most packages use predict(),
#'   which is the default). Either length of 1 or same length as alg.
#' @param as_matrix A list of logical values indicating if the respective alg
#'   needs the dfm converted to a matrix.
#' @param positive value for the positive class.
#'
#' @import purrr
#' @import tibble
#' @importFrom utils head
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(quanteda)
#' corp <- corpus(c(d1 = "Chinese Beijing Chinese",
#'                  d2 = "Chinese Chinese Shanghai",
#'                  d3 = "Chinese Macao",
#'                  d4 = "Tokyo Japan Chinese",
#'                  d5 = "Chinese Chinese Chinese Tokyo Japan"))
#'
#' docvars(corp, "class") <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
#' docvars(corp, "training") <- c(TRUE, TRUE, TRUE, TRUE, FALSE)
#'
#' test_prep <- batch_prep(corp)
#'
#' batch_validate(x = test_prep[1:3],
#'                y = "class",
#'                set = "training",
#'                alg = list(textmodel_nb = quanteda.textmodels::textmodel_nb,
#'                           textmodel_svm = quanteda.textmodels::textmodel_svm)
#' }
batch_validate <- function(x,
                           y,
                           set = docvars(x, "training"),
                           alg = NULL,
                           pred = predict,
                           as_matrix = FALSE,
                           positive = FALSE) {

  if (!is.list(x)) x <- list(x)
  if (!is.list(alg)) alg <- list(alg)
  if (!is.list(pred)) pred <- list(pred)
  if (!is.list(as_matrix)) as_matrix <- list(as_matrix)

  # force same length
  if (length(alg) > length(pred))
    pred <- head(rep(pred, length(alg)), length(alg))
  if (length(alg) > length(as_matrix))
    as_matrix <- head(rep(as_matrix, length(alg)), length(alg))

  out <- purrr::map(seq_along(alg), function(a) {

    if (interactive()) {
      pb <- progress::progress_bar$new(
        total = length(x),
        format = ":what [:bar] :current/:total (:percent) :eta"
      )
    } else {
      pb <- NULL
    }

    purrr::map(x, .f = val, y = y, set = set, alg = alg[[a]],
               pred = pred[[a]], as_matrix = as_matrix[[a]],
               pb = pb, what = names(alg)[a], positive = positive)
  })
  names(out) <- names(alg)
  if (is.null(names(out))) {
    names(out) <- seq_along(names(out))
  }

  results <- purrr::map(out, ~ purrr::map(.x, ~ .x[["res"]]))
  results2 <- tibble(
    algorithm = rep(names(out), purrr::map_int(out, length)),
    prep = unlist(purrr::map(out, names)),
    x = unlist(results, recursive = FALSE)
  ) %>%
    unnest(x)

  attr(results2, "prediction") <- purrr::map(out, ~ purrr::map(.x, ~ .x[["prediction"]]))

  return(results2)
}


#' Evaluate  an algorithm with one dfm
#'
#' @param x A dfm to evaluate.
#' @param y A variable in docvars of the dfms with the class (must be logical
#'   for now).
#' @param set A variable in docvars indicating membership to training or test
#'   set.
#' @param alg A functions containing the algorithm to be evaluated (see example)
#' @param pred A function used for prediction (most packages use predict(),
#'   which is the default).
#' @param as_matrix Logical. Indicating if alg needs the dfm converted to a
#'   matrix.
#' @param positive value for the positive class.
#' @param pb,what Used to display status bar when used in batch mode.
#'
#' @return A confusion matrix object.
#' @importFrom  stats predict
#' @export
val <- function(x,
                y,
                set = "training",
                alg = NULL,
                pred = stats::predict,
                as_matrix = FALSE,
                positive = TRUE,
                pb = NULL,
                what = NULL) {

  if (!is.null(pb)) {
    pb$tick(tokens = list(what = what))
  }

  training_dfm <- quanteda::dfm_subset(x, docvars(x, set))
  test_dfm <- quanteda::dfm_subset(x, !docvars(x, set))

  train_v <- quanteda::docvars(training_dfm, y)
  true <- quanteda::docvars(test_dfm, y)

  if (as_matrix) {
    training_dfm <- as.matrix(training_dfm)
    test_dfm <- as.matrix(test_dfm)
  }

  model <- alg(training_dfm, train_v)

  # quanteda warns if test_dfm has features not in the model
  pred <- suppressWarnings(as.logical(pred(model, test_dfm)))
  names(pred) <- docnames(test_dfm)
  list(
    #model = model,
    prediction = pred,
    res = confu_mat(pred, true, positive = positive)
  )
}
