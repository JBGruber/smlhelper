#' Flat confusion matrix
#'
#' @param pred Predicted classes.
#' @param true  True classes.
#' @param case_name Name added to the tibble.
#' @param positive Value for the positive class.
#'
#' @return A flat confusion matrix accuracy measures.
#' @import dplyr
#' @import tidyr
#' @importFrom irr kripp.alpha
#' @importFrom cvAUC AUC
#' @export
confu_mat <- function(pred, true, case_name = NULL, positive = TRUE) {

  false_negative <- NULL
  false_positive <- NULL
  name <- NULL
  precision <- NULL
  recall <- NULL
  true_negative <- NULL
  true_positive <- NULL

  kripp_alpha <- irr::kripp.alpha(t(matrix(as.integer(c(pred, true)), ncol = 2)))$value
  pos_prob <- length(true[true == positive]) / length(true)
  neg_prob <- 1 - pos_prob
  base_acc <- max(c(pos_prob, neg_prob))

  if (length(unique(pred)) > 1) {
    AUC <- cvAUC::AUC(as.integer(pred), as.integer(true))
  } else {
    AUC <- NaN
  }

  out <- tibble::tibble(pred, true) %>%
    count(pred, true, .drop = FALSE) %>%
    mutate(name = case_when(
      pred == true &  pred == !positive ~ "true_negative",
      pred == true &  pred == positive ~ "true_positive",
      pred != true &  pred == positive ~ "false_positive",
      pred != true &  pred == !positive ~ "false_negative",
    )) %>%
    rbind(tibble(pred = c(!positive, positive, positive, !positive),
                 true = c(!positive, positive, !positive, positive),
                 n = 0,
                 name = c("true_negative",
                          "true_positive",
                          "false_positive",
                          "false_negative"))) %>%
    group_by(name) %>%
    summarise(n = sum(n), .groups = "drop_last") %>%
    pivot_wider(id_cols = NULL,
                names_from = name,
                values_from = n) %>%
    mutate(accuracy = (true_positive + true_negative) /
             (true_positive + true_negative +
                false_positive + false_negative),
           precision = true_positive / (true_positive + false_positive),
           recall = true_positive / (true_positive + false_negative),
           f1 = 2 * ((precision * recall) / (precision + recall)),
           kripp_alpha = kripp_alpha,
           base_acc = base_acc,
           auc = AUC)
  if (!is.null(case_name)) {
    out <- out %>%
      tibble::add_column(case_name = case_name, .before = "true_negative")
  }


  class(out) <- c("confu_mat", class(out))
  out
}


print.confu_mat <- function(x) {
  x
}
