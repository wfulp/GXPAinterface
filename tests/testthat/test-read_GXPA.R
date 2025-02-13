test_that("read_GXPA input checking", {
  expect_error(
    get_data_from_gxpa(),
    "series_id can't be missing"
  )
  expect_error(
    get_data_from_gxpa(10),
    "genes can't be missing"
  )
})


test_that("read_GXPA bad url testing", {
  test_link <- "https://geneatlas.redda.bms.com/series_api/series/?format=json&name_only=1"
  expect_error(
    get_info_from_url(get_url = "https://geneatlas.redda.bms.com/se"),
    "Error \\(status=404\\) while reading url: https://geneatlas.redda.bms.com/se"
  )

  expect_error(
    get_info_from_url(get_url = test_link, GXPA_TOKEN = "fgsgvfds"),
    "Error \\(status=401\\) while reading url: https://geneatlas.redda.bms.com/series_api/series/\\?format=json&name_only=1"
  )
})



test_that("read_GXPA output testing", {
  # confirming series results for 41
  test_data_info <-
    data.frame(
      Cd34 = c(9.48, 9.10, 9.55, 5.95, 5.91, 6.01, 9.93, 10.24, 9.85, 5.64),
      Cd33 = c(5.22, 5.22, 5.49, 5.29, 5.29, 5.36, 5.42, 5.11, 5.32, 5.45),
      cell_type = c(
        "hypofunc_CD8", "hypofunc_CD8", "hypofunc_CD8",
        "functional_CD8", "functional_CD8", "functional_CD8",
        "hypofunc_CD8", "hypofunc_CD8", "hypofunc_CD8",
        "functional_CD8"
      )
    )
  rownames(test_data_info) <- c(
    "GSM2107347", "GSM2107348", "GSM2107349", "GSM2107350", "GSM2107351",
    "GSM2107352", "GSM2107353", "GSM2107354", "GSM2107355", "GSM2107356"
  )

  results_plus <- purrr::quietly(
    ~ get_data_from_gxpa(41, c("CD33", "CD34",'aasaa'))[1:10, 1:3]
  )()

  expect_equal(results_plus$result,
    test_data_info,
    ignore_attr = TRUE
  )
  expect_equal(results_plus$messages,
               c('These genes not present:\naasaa\n',
                 'These genes having incorrect case:\nCD33 vs. Cd33\nCD34 vs. Cd34\n')
  )


  # confirming series results for 41
  test_series_info <-
    data.frame(
      id = 41,
      name = "GSE79858 antigen specific hypofunctional T cells",
      organism = "mouse",
      perm_id = 1
    )

  expect_equal(get_series_info_from_gxpa(41)[, -3],
    test_series_info,
    ignore_attr = TRUE
  )
})
