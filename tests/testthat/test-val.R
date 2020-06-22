context("prep")
# corp <- corpus(c(d1 = "Chinese Beijing Chinese",
#                  d2 = "Chinese Chinese Shanghai",
#                  d3 = "Chinese Macao",
#                  d4 = "Tokyo Japan Chinese",
#                  d5 = "Chinese Chinese Chinese Tokyo Japan"))
#
# docvars(corp, "class") <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
# docvars(corp, "training") <- c(TRUE, TRUE, TRUE, TRUE, FALSE)
#
# test_prep <- batch_prep(corp)
# saveRDS(test_prep, "../test-files/test_prep.Rds")

test_prep <- readRDS("../test-files/test_prep.Rds")

test_that("prep", {
  expect_equal({
      out <- val(x = test_prep[[1]],
                 y = "class",
                 set = "training",
                 alg = quanteda.textmodels::textmodel_nb,
                 pred = predict)
      c(length(out),
        is.list(out),
        ncol(out$res))
    },
    c(2L, 1L, 11L)
  )
})

test_that("batch_prep", {

  expect_equal({
      out <- batch_validate(
        x = test_prep[1:3],
        y = "class",
        set = "training",
        alg = list(
          textmodel_nb = quanteda.textmodels::textmodel_nb,
          textmodel_svm = quanteda.textmodels::textmodel_svm
        ),
        pred = predict,
        as_matrix = FALSE
      )
      c(tibble::is_tibble(out),
        nrow(out),
        ncol(out))
    },
    c(TRUE, 6L, 13L)
  )
})
