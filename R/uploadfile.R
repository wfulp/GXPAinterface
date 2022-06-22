#' Push a File to a GXPA Session
#'
#' Importing data for an already opened GXPA session.
#'
#' @param session_id session id from a session that has already been opened in GXPA
#' @param filename_local input file name, with path, to import
#' @param file_save_name file name to be saved as in GXPA. Must match a know name (see details)
#' @param user_cookie user cookie (default is using [login_and_get_user_cookie()])
#'
#' @details  The GXPA session needs to exist, so go to the
#' [GXPA app](https://geneatlas.redda.celgene.com/sessions/) to see open
#' sessions and create a new one if needed. The session id can be found on the
#' main sessions page or in the URL once in the session
#'  (6 uppercase chars or numbers)
#'
#' Common `file_save_name` values are expr.expr.txt, expr.samples.csv,
#' expr.series_info.csv, and expr.score_info.csv, for expression data loading, and
#' GEO.expr.txt, GEO.samples.csv, GEO.series_info.csv, and GEO.score_info.csv,
#' for GEO data loading
#'
#' @return message indicating upload was successful
#' @export
#'
#' @examples
#' \dontrun{
#' tmp_file <- tempfile()
#' utils::write.csv(matrix(rnorm(100), nrow = 10), tmp_file)
#' send_file_to_session(
#'   session_id = "7AR22L",
#'   filename_local = tmp_file,
#'   file_save_name = "expr.expr.txt"
#' )
#' }
send_file_to_session <- function(session_id,
                                 filename_local,
                                 file_save_name,
                                 user_cookie = login_and_get_user_cookie()) {
  if (!file.exists(filename_local)) {
    stop("filename_local file does not exist: ", filename_local)
  }

  # build the URL to access the data
  post_url <- paste0(
    "https://geneatlas.redda.celgene.com/sessions/upload_file",
    "?cur_session=", session_id,
    "&file_save_name=", file_save_name
  )

  # first need to load the form via GET to get the CSRF token
  resp1 <- httr::GET(post_url, httr::set_cookies(sessionid = user_cookie))
  resp1_text <- httr::content(resp1, as = "text")
  if (grepl("Error: file name not allowed", resp1_text)) {
    stop(
      "File name not allowed for upload: ",
      file_save_name
    )
  } else if (grepl("Error in UploadFileSessionView", resp1_text)) {
    stop("Session ID does not exist: ", session_id)
  }

  # get the csrf token
  csrf_token <- get_cookie_value(resp1, "csrftoken")

  resp2 <- httr::POST(
    post_url,
    httr::add_headers(Referer = post_url),
    httr::set_cookies(sessionid = user_cookie),
    body = list(
      file_any = httr::upload_file(filename_local),
      cur_session = session_id,
      file_save_name = file_save_name,
      csrfmiddlewaretoken = csrf_token
    )
  )

  resp2_text <- httr::content(resp2, as = "text")
  if (grepl("Please login.*to view this page", resp2_text, ignore.case = T)) {
    stop("Did not save file. Need to log in with admin user, or could be a bad user_cookie.")
  }
  if (grepl("Uploaded files have been saved", resp2_text, ignore.case = T)) {
    message(
      "Successfully uploaded ", file_save_name, "(from ",
      filename_local, ")"
    )
  } else {
    stop("Did not receive message saying file was saved")
  }
}
