#' Get User Cookie for GXPA app
#'
#' Uses GXPA username and password to log into GXPA and get user cookie
#'
#' @param GXPA_username username for GXPA. Default is to use the environment variable
#' `GXPA_username`
#' @param GXPA_password password for GXPA. Default is to use the environment variable
#' `GXPA_password`
#'
#' @return user cookie
#' @details
#' `GXPA_username` and `GXPA_password` can be set by:
#'
#'  - For the current session:
#'    - `Sys.setenv(GXPA_username = "XXXXXXXXXXXXXXXX")`
#'    - `Sys.setenv(GXPA_password = "XXXXXXXXXXXXXXXX")`
#'  - For all sessions:
#'    - `usethis::edit_r_environ()`,
#'    - adding: `GXPA_USERNAME = "XXXXXXXXXXXXXXXX"`
#'    - adding: `GXPA_PASSWORD = "XXXXXXXXXXXXXXXX"`
#' @export
#'
#' @examples
#' login_and_get_user_cookie()
login_and_get_user_cookie <- function(GXPA_username = Sys.getenv("GXPA_USERNAME"),
                                      GXPA_password = Sys.getenv("GXPA_PASSWORD")) {

  # this is the URL for logging in
  login_url <- paste0("https://geneatlas.redda.celgene.com/register/login")

  # need to load the form to get the CSRF token
  resp1 <- httr::GET(login_url)
  csrf_token <- get_cookie_value(resp1, "csrftoken")

  # now log in and then get the user cookie
  resp2 <- httr::POST(login_url,
    httr::add_headers(Referer = login_url),
    body = list(
      username = GXPA_username,
      password = GXPA_password,
      csrfmiddlewaretoken = csrf_token
    )
  )
  # check for errors
  resp_text <- httr::content(resp2, as = "text")
  if (grepl("CSRF token missing or incorrect", resp_text)) {
    stop("CSRF error")
  }
  if (grepl("Not logged in", resp_text)) {
    stop("invalid login info")
  }

  get_cookie_value(resp2, "sessionid")
}

# internal use - get the cookie value
get_cookie_value <- function(resp, name) {
  cook <- resp$cookies
  if (name %in% cook$name) {
    cook[cook$name == name, "value"]
  } else {
    NULL
  }
}
