test_that("testing series info creation", {

  # should match
  filename <- tempfile(fileext = ".csv")
  make_series_info_file(
    file = filename,
    series_name = "SU2C_2019",
    series_descript = "Long description",
    default_group = "tumor_site",
    organism = "human",
    expr_units = "RNA seq log2 (FPKM+1)",
    pmid = 31061129,
    default_color = "AR_score_group",
    extra.list = list(
      "Extra_Field" = "Something",
      "Another_one" = 5,
      "Whoops" = NULL
    )
  )
  expect_equal(
    read.csv(filename),
    data.frame(
      key = c(
        "series_name", "series_descript", "default_group", "organism",
        "expr_units", "pmid", "default_color", "Extra_Field", "Another_one"
      ),
      value = c(
        "SU2C_2019", "Long description", "tumor_site", "human",
        "RNA seq log2 (FPKM+1)", 31061129, "AR_score_group", "Something", 5
      )
    )
  )

  # various errors
  expect_error(
    make_series_info_file(),
    'argument "filename" is missing, with no default'
  )
  expect_error(
    make_series_info_file(filename = "fda"),
    'argument "series_name" is missing, with no default'
  )
  expect_error(
    make_series_info_file(
      filename = filename,
      series_name = "SU2C_2019"
    ),
    'argument "series_descript" is missing, with no default'
  )
  expect_error(
    make_series_info_file(filename = NULL),
    "filename, name, descript, default_group, organism, or expr_units is missing, with no default"
  )
})
