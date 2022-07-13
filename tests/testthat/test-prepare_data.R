test_that("testing series info creation", {

  # should match
  filename <- tempfile(fileext = ".csv")
  make_series_info_file(
    filename = filename,
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



test_that("testing check_files_for_GXPA", {
  run_test_checks <- function(test_expr, test_meta, quote = FALSE) {
    test_dir <- tempdir()
    utils::write.table(test_expr,
      file = file.path(test_dir, "expr.expr.txt"),
      sep = "\t",
      quote = quote
    )
    utils::write.csv(test_meta,
      file = file.path(test_dir, "expr.samples.csv")
    )

    check_files_for_GXPA(
      expr_file = file.path(test_dir, "expr.expr.txt"),
      samples_file = file.path(test_dir, "expr.samples.csv")
    )
  }

  test_expr <- data.frame(matrix(1:1000, nrow = 100))
  colnames(test_expr) <- paste0("Sample", 1:10)
  rownames(test_expr) <- paste0("Gene", 1:100)

  test_meta <- data.frame(matrix(rep(letters[1:20], 3), nrow = 10))
  rownames(test_meta) <- colnames(test_expr)
  colnames(test_meta) <- c('_id', paste0("Variable", 2:6))

  # success
  expect_message(
    run_test_checks(test_expr, test_meta),
    "All checks passed"
  )
  # going throw all errors
  expect_error(
    run_test_checks(test_expr[, -1], test_meta),
    "Number of columns of expression data should equal number of rows in metadata"
  )
  bad_meta <- as.matrix(test_meta)
  colnames(bad_meta)[1] <- 'badname'
  expect_error(
    run_test_checks(test_expr, bad_meta),
    'The first column name of the metadata must be an empty string or "_id"'
  )
  expect_error(
    run_test_checks(test_expr, test_meta, quote = TRUE),
    'Detected quotes in all expr data column names or row names. Check to make sure "quote = FALSE" is used when writing expr data'
  )
  bad_expr <- test_expr
  colnames(bad_expr)[1] <- colnames(bad_expr)[2]
  expect_error(
    run_test_checks(bad_expr, test_meta),
    "Detected duplicated header values in expr data"
  )
  bad_meta <- as.matrix(test_meta)
  rownames(bad_meta)[1] <- rownames(bad_meta)[2]
  expect_error(
    run_test_checks(test_expr, bad_meta),
    "duplicate 'row.names' are not allowed"
  )
  colnames(bad_expr)[2] <- colnames(test_expr)[1]
  expect_warning(
    suppressMessages(run_test_checks(bad_expr, test_meta)),
    "expr data and metadata can be linked, but is not currently \\(i.e. different order\\)"
  )
  colnames(bad_expr)[2] <- "Whoops"
  expect_error(
    run_test_checks(bad_expr, test_meta),
    "expr data and metadata can not be linked"
  )
  bad_expr <- test_expr
  rownames(bad_expr)[1] <- toupper(rownames(bad_expr)[2])
  expect_error(
    run_test_checks(bad_expr, test_meta),
    "Some of the gene names are duplicated \\(ignoring case\\)"
  )
  bad_meta <- test_meta
  colnames(bad_meta)[2:4] <- c("fsgs fdfv", "sgs-gdfs", "123fadf")
  expect_error(
    run_test_checks(test_expr, bad_meta),
    "The following variable do not follow standard R variable naming convention:\nfsgs fdfv\nsgs-gdfs\n123fadf"
  )
})
