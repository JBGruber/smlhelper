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
#'
#' @import purrr
#' @import tibble
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
batch_validate <- function(x, y, set = docvars(x, "training"), alg = NULL, pred = NULL) {

  if (!is.list(alg)) alg <- list(alg)
  if (!is.list(predict)) predict <- list(predict)
  if (!is.list(x)) predict <- list(x)

  out <- purrr::map2(seq_along(alg), predict, function(a, p) {

    if (interactive()) {
      pb <- progress::progress_bar$new(total = length(x),
                                       format = ":what [:bar] :current/:total (:percent) :eta")
    } else {
      pb <- NULL
    }

    purrr::map(x, .f = val, y = y, set = set, alg = alg[[a]], pred = p,  pb = pb, what = names(alg)[a])
  })
  names(out) <- names(alg)

  results <- purrr::map(out, ~ purrr::map(.x, ~ .x[["res"]]))
  results2 <- tibble(
    algorithm = rep(names(out), purrr::map_int(out, length)),
    prep = unlist(purrr::map(out, names)),
    x = unlist(results, recursive = FALSE)
  ) %>%
    unnest(x)

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
                pb = NULL,
                what = NULL) {

  if (!is.null(pb)) {
    pb$tick(tokens = list(what = what))
  }

  training_dfm <- quanteda::dfm_subset(x, docvars(x, set))
  test_dfm <- quanteda::dfm_subset(x, !docvars(x, set))

  model <- alg(training_dfm, docvars(training_dfm, y))
  true <- docvars(test_dfm, y)

  # quanteda warns if test_dfm has features not in the model
  pred <- suppressWarnings(as.logical(pred(model, newdata = test_dfm)))
  list(
    model = model,
    res = confu_mat(pred, true)
  )
}
