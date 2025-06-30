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
  test_link <- "https://demo.needlegenomics.com/series_api/series/?format=json&name_only=1"
  expect_error(
    get_info_from_url(get_url = "https://demo.needlegenomics.com/se"),
    "Error \\(status=404\\) while reading url: https://demo.needlegenomics.com/se"
  )

  expect_error(
    get_info_from_url(get_url = test_link, GXPA_TOKEN = "fgsgvfds"),
    "Error \\(status=401\\) while reading url: https://demo.needlegenomics.com/series_api/series/\\?format=json&name_only=1"
  )
})



test_that("read_GXPA output testing", {
  # confirming series results for 3
  test_data_info <-
    data.frame(
      TYK2 = c(11.5165, 11.2788, 11.1443, 11.1332, 11.1507, 11.0768, 11.3049,
               10.9641, 11.0645, 10.9338),
      CD34 = c(7.05569, 6.98715, 6.8604, 7.18281, 6.86445, 6.73649, 6.89719,
               6.73338, 7.1622, 6.78285),
      cell_type = "Monocytes"
    )
  rownames(test_data_info) <- c(
    "GSM705287", "GSM705288", "GSM705289", "GSM705290", "GSM705291",
    "GSM705292", "GSM705293", "GSM705294", "GSM705295", "GSM705296"
  )

  results_plus <- purrr::quietly(
    ~ get_data_from_gxpa(3, c("Cd34", "Tyk2",'aasaa'))[1:10, 1:3]
  )()

  expect_equal(results_plus$result,
    test_data_info,
    ignore_attr = TRUE
  )
  expect_equal(results_plus$messages,
               c('These genes not present:\naasaa\n',
                 'These genes having incorrect case:\nCd34 vs. CD34\nTyk2 vs. TYK2\n')
  )


  # confirming series results for 41
  test_series_info <-
    data.frame(
      id = 41,
      name = "GSE60424 blood subsets in disease BRI",
      organism = "human",
      perm_id = 1
    )

  expect_equal(get_series_info_from_gxpa(41)[, -3],
    test_series_info,
    ignore_attr = TRUE
  )
})
