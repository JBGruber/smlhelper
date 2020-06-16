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
#' @export
confu_mat <- function(pred, true, case_name = NULL, positive = TRUE) {
  false_negative = NULL
  false_positive = NULL
  name = NULL
  precision = NULL
  recall = NULL
  true_negative = NULL
  true_positive = NULL

  out <- tibble::tibble(pred, true) %>%
    count(pred, true, .drop = FALSE) %>%
    mutate(name = case_when(
      pred == true &  pred == FALSE ~ "true_negative",
      pred == true &  pred == TRUE ~ "true_positive",
      pred != true &  pred == TRUE ~ "false_positive",
      pred != true &  pred == FALSE ~ "false_negative",
    )) %>%
    rbind(tibble(pred = c(FALSE, TRUE, TRUE, FALSE),
                 true = c(FALSE, TRUE, FALSE, TRUE),
                 n = 0,
                 name = c("true_negative",
                          "true_positive",
                          "false_positive",
                          "false_negative"))) %>%
    group_by(name) %>%
    summarise(n = sum(n), .groups = "drop_last") %>%
    pivot_wider(id_cols = NULL, names_from = name, values_from = n) %>%
    mutate(accuracy = (true_positive + true_negative) / (true_positive + true_negative +
                                                           false_positive + false_negative),
           precision = true_positive / (true_positive + false_positive),
           recall = true_positive / (true_positive + false_negative),
           f1 = 2 * ((precision * recall) / (precision + recall)))

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

