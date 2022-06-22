test_that("get cookie testing", {
  expect_error(
    login_and_get_user_cookie("fdfgvaz"),
    "invalid login info"
  )
  # success
  expect_error(
    login_and_get_user_cookie(),
    NA
  )
})
