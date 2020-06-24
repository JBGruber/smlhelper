context("prep")
corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                 d2 = "Chinese Chinese Shanghai",
                 d3 = "Chinese Macao",
                 d4 = "Tokyo Japan Chinese",
                 d5 = "Chinese Chinese Chinese Tokyo Japan"))

docvars(corp, "class") <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
docvars(corp, "training") <- c(TRUE, TRUE, TRUE, TRUE, FALSE)


test_that("batch_prep", {

  expect_equal({
      out <- batch_prep(corp)
      c(is.list(out),
        is.dfm(out[[1]]),
        length(out),
        nrow(out[[1]]),
        ncol(out[[1]]))
    },
    c(TRUE, TRUE, 256, 5, 20)
  )
})
