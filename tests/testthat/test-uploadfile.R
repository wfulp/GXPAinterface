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

