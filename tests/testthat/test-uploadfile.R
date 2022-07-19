test_that("testing send_file_to_session() errors and success", {
  tmp_file <- tempfile()
  utils::write.csv(matrix(rnorm(100), nrow = 10), tmp_file)

  # testing bad params throwing expected errors
  expect_error(
    send_file_to_session(
      session_id = "7AR22L",
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
      session_id = "7AR22L",
      filename_local = tmp_file,
      file_save_name = "Bed_Output_Name.txt"
    ),
    "File name not allowed for upload: Bed_Output_Name.txt"
  )
  expect_error(
    send_file_to_session(
      session_id = "7AR22L",
      filename_local = tmp_file,
      file_save_name = "expr.expr.txt",
      user_cookie = "BAD_COOKIE"
    ),
    "Did not save file. Need to log in with admin user, or could be a bad user_cookie."
  )

  # success
  expect_message(
    send_file_to_session(
      session_id = "7AR22L",
      filename_local = tmp_file,
      file_save_name = "expr.expr.txt"
    ),
    paste0("Successfully uploaded expr.expr.txt\\(from ", tmp_file, "\\)")
  )
})


test_that("testing begin_new_session() errors and success", {

  expect_equal(
    suppressWarnings(
      begin_new_session(session_name = "test_GXPAinterface")),
    "7AR22L"
  )
  expect_warning(
    begin_new_session(session_name = "test_GXPAinterface"),
    "did not make new session since one with that name already exists, and its session_id is returned"
  )

  expect_error(
    begin_new_session(session_name = "abcd", user_cookie = 'bad_cookie'),
    "did not receive message saying session was made"
  )

})



test_that("testing get_GXPA_session_list() errors and success", {

  tmp_list <- get_GXPA_session_list()
  selected_output <- as.list(tmp_list[tmp_list$id == '7AR22L', ])[-(1:2)]

  expect_equal(
    selected_output,
    list(
       mod_time_readable = "2022-06-21 20:06",
      perm_reason = "user is superuser",
      perm = "61",
      type = "expr",
      name = "test_GXPAinterface",
      id = "7AR22L"
    )
  )

  # only public data shows if bad token given
  public_list <- get_GXPA_session_list(GXPA_TOKEN = 'Bad_Token')
  selected_public_output <- as.list(
    public_list[public_list$id == 'AQUKTU', ])[-(1:2)]

  expect_false(any(public_list$id == '7AR22L'))
  expect_equal(
    selected_public_output,
    list(
      mod_time_readable = "2019-07-24 17:07",
      perm_reason = "no perm set for session",
      perm = "0",
      type = "GEO",
      name = "GSE28490",
      id = "AQUKTU"
    )
  )


})


test_that("testing get_GXPA_session_details() success", {

  tmp_output <- get_GXPA_session_details('7AR22L')

  expect_equal(
    tmp_output,
    list(
      help = ", Use 'dir=1' to see all registered files; 'list_file_types=csv' to list all registered files of certain file types;'expr_details=1' to look up series info, number of samples & features, etc for expr related sessions;",
      session_type = "expr",
      cur_step = "base_ready",
      cur_session = "7AR22L",
      user = list(name = "fulpw", id = 61L),
      registered_files = c("cur_step.txt", "expr.expr.txt",
                           "expr.series_info.csv", "session_info.txt"),
      session_name = "test_GXPAinterface",
      details = list(series_descript = "Long description",
                     num_genes = "11",
                     series_name = "SU2C_2019",
                     num_annots = NULL,
                     num_samples = "1")
    )
  )


})

