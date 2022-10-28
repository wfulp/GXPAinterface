test_that("testing send_file_to_session() errors and success", {
  tmp_file <- tempfile()
  utils::write.csv(matrix(rnorm(100), nrow = 10), tmp_file)

  # testing bad params throwing expected errors
  expect_error(
    send_file_to_session(
      session_id = "083UNV",
      filename_local = "Bad_Name.txt",
      file_save_name = "expr.expr.txt"
    ),
    "filename_local file does not exist: Bad_Name.txt"
  )
  expect_error(
    send_file_to_session(
      session_id = "Bad_ID",
      filename_local = tmp_file,
      file_save_name = "expr.expr.txt"
    ),
    "Session ID does not exist: Bad_ID"
  )
  expect_error(
    send_file_to_session(
      session_id = "083UNV",
      filename_local = tmp_file,
      file_save_name = "Bed_Output_Name.txt"
    ),
    "File name not allowed for upload: Bed_Output_Name.txt"
  )
  expect_error(
    send_file_to_session(
      session_id = "083UNV",
      filename_local = tmp_file,
      file_save_name = "expr.expr.txt",
      user_cookie = "BAD_COOKIE"
    ),
    "Did not save file. Need to log in with admin user, or could be a bad user_cookie."
  )

  # success
  expect_message(
    send_file_to_session(
      session_id = "083UNV",
      filename_local = tmp_file,
      file_save_name = "expr.expr.txt"
    ),
    paste0("Successfully uploaded expr.expr.txt\\(from ", tmp_file, "\\)")
  )
})


test_that("testing begin_new_session() errors", {

  results_plus <- purrr::quietly(
    ~ begin_new_session(session_name = "test_GXPAinterface2")
  )()

  expect_equal(
    results_plus$result,
    "083UNV"
  )

  expect_equal(
    results_plus$warnings,
    "did not make new session since one with that name already exists, and its session_id is returned"
  )

  expect_warning(
    begin_new_session(session_name = "abcd", user_cookie = 'bad_cookie'),
    "did not receive message saying session was made"
  )

})


test_that("testing dry_run() errors", {

  expect_error(
    dry_run_session("BAD_ID"),
    "Session not found"
  )

})


test_that("testing dry_run() success", {

  expect_message(
    dry_run_session("70FO1W"),
    "No errors or warnings found in Dry Run"
  )

  expect_message(
    dry_run_session("70FO1W", run_type = 'quick'),
    "No errors or warnings found in Dry Run"
  )


})

test_that("testing remove_session() errors", {

  expect_warning(
    remove_session("BAD_ID"),
    "did not receive confirmation message"
  )

})


test_that("testing load_session() errors", {

  expect_warning(
    load_session("BAD_ID", load_type = 'update_series'),
    "did not receive confirmation message"
  )

  expect_error(
    load_session("083UNV", load_type = 'update_series'),
    "Did not submit load task because that load type is not currently allowed given the state of the database"
  )

})



test_that("testing run of begin_new_session() to remove_session()", {

  results_plus <- purrr::quietly(
    ~ begin_new_session("new_test_session")
  )()

  expect_equal(
    results_plus$messages,
    "Success\n"
  )

  dry_run_plus <- purrr::quietly(
    ~ dry_run_session(results_plus$result)
  )()

  expect_equal(
    dry_run_plus$warnings,
    "Errors found in Dry Run (see above)"
  )

  expect_equal(
    dry_run_plus$messages,
    c("<li>IOERROR (series): samples file missing</li>\n",
   "<li>IOERROR (series): series info file missing</li>\n",
    "<li>IOERROR (series): expr file missing</li>\n",
    "<li>IOERROR (score): samples file missing</li>\n",
    "<li>IOERROR (score): series info file missing</li>\n",
    "<li>IOERROR (score): expr file missing</li>\n",
    "<li>IOERROR (score): score_info file missing</li>\n")
  )

  expect_message(
    remove_session(results_plus$result),
    "Success - session removed\n"
  )
})






test_that("testing loading and deleting series", {
  expect_warning(remove_series('aaaa'),
                 'did not receive confirmation message'
  )

  # Need to remove test series if it exists
  all_dat <- get_series_info_from_gxpa()
  if (any(all_dat$name == 'test_GXPAinterface')) {
    test_series_id <- all_dat$id[all_dat$name == 'test_GXPAinterface']
    suppressMessages(remove_series(test_series_id))
    Sys.sleep(10)
  }

  expect_message(load_session("70FO1W"),
                 "Success - loaded into task queue")

  Sys.sleep(10)

  all_dat <- get_series_info_from_gxpa()
  test_series_id <- all_dat$id[all_dat$name == 'test_GXPAinterface']
  expect_message(remove_series(test_series_id),
                 "Success - series removed")
})




test_that("testing get_GXPA_session_list() errors and success", {

  tmp_list <- get_GXPA_session_list()
  selected_output <- as.list(tmp_list[tmp_list$id == '70FO1W', ])[-(1:2)]

  expect_equal(
    selected_output[-5],
    list(
      perm = "61",
      mod_time = 1666374416,
      mod_time_readable = "2022-10-21 10:10",
      perm_reason = "user is superuser",
      id = "70FO1W"
    )
  )

  # only public data shows if bad token given
  public_list <- get_GXPA_session_list(GXPA_TOKEN = 'Bad_Token')
  selected_public_output <- as.list(
    public_list[public_list$id == 'AQUKTU', ])[-(1:2)]

  expect_false(any(public_list$id == '70FO1W'))
  expect_equal(
    selected_public_output[-5],
    list(
      perm = "0",
      mod_time = 1563990941,
      mod_time_readable = "2019-07-24 10:07",
      perm_reason = "no perm set for session",
      id = "AQUKTU"
    )
  )


})


test_that("testing get_GXPA_session_details() success", {

  tmp_output <- get_GXPA_session_details('70FO1W')
  expect_true(
    all(c("expr.samples.csv",
          "expr.expr.txt",
          "expr.series_info.csv",
          "session_info.txt") %in%
          tmp_output$registered_files))

  expect_equal(
    tmp_output[-7],
    list(
      cur_session = "70FO1W",
      session_type = 'expr',
      session_name = "test_GXPAinterface3",
      cur_step = "task_finished",
      help = ", Use 'dir=1' to see all registered files; 'list_file_types=csv' to list all registered files of certain file types;'expr_details=1' to look up series info, number of samples & features, etc for expr related sessions;",
      # should be task finished based on previous delete session test
      user = list(name = "fulpw", id = 61L),
      details = list(
        num_samples = "28",
        num_genes = "101",
        num_annots = "5",
        series_name = "test_GXPAinterface",
        series_descript = "Long description"
      )
    )
  )


})

