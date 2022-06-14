test_that("input checking", {
  expect_error(get_data_from_gxpa(),
               "series_id can't be missing")
  expect_error(get_data_from_gxpa(10),
               "genes can't be missing")
  expect_error(get_data_from_gxpa(10, 'AA', GXPA_TOKEN = ''),
               "GXPA_TOKEN can't be missing")
  expect_error(get_data_from_gxpa(10, 'AA', GXPA_TOKEN = NA),
               "GXPA_TOKEN can't be missing")
  expect_error(get_data_from_gxpa(10, 'AA', GXPA_TOKEN = NULL),
               "GXPA_TOKEN can't be missing")

  expect_error(get_series_info_from_gxpa(10, GXPA_TOKEN = ''),
               "GXPA_TOKEN can't be missing")
  expect_error(get_series_info_from_gxpa(10, GXPA_TOKEN = NA),
               "GXPA_TOKEN can't be missing")
  expect_error(get_series_info_from_gxpa(10, GXPA_TOKEN = NULL),
               "GXPA_TOKEN can't be missing")

  test_link <- "https://geneatlas.redda.celgene.com/series_api/series/?format=json&name_only=1"
  expect_error(get_info_from_url(test_link, GXPA_TOKEN = ''),
               "GXPA_TOKEN can't be missing")
  expect_error(get_info_from_url(test_link, GXPA_TOKEN = NA),
               "GXPA_TOKEN can't be missing")
  expect_error(get_info_from_url(test_link, GXPA_TOKEN = NULL),
               "GXPA_TOKEN can't be missing")
})


test_that("bad url testing", {
  test_link <- "https://geneatlas.redda.celgene.com/series_api/series/?format=json&name_only=1"
  expect_error(get_info_from_url(get_url = 'fdafaf'),
               "Error \\(status=404\\) while reading url: fdafaf")

  expect_error(get_info_from_url(get_url = test_link, GXPA_TOKEN = 'fgsgvfds'),
               "Error \\(status=401\\) while reading url: https://geneatlas.redda.celgene.com/series_api/series/\\?format=json&name_only=1")

})
