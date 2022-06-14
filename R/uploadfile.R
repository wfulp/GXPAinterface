
# Push a file to a session in the app. The session needs to exist, and so go here
#    https://geneatlas.redda.celgene.com/sessions/
# to go to a session page (or create a new session) and then get
#    the session id in the URL (6 uppercase chars or numbers)
#
# filename_local is the name (w/path if not in currenet dir) of the file you want to upload
# file_save_name is the name of the file in the GXPA session, eg. expr.expr.txt
#              expr.samples.csv, expr.series_info.csv
#
# Usage:
#   # First, get the user_cookie by logging in (using the data in ~/.gxpa)
#   user_cookie = login_and_get_user_cookie()
#      [or if you don't have ~/.gxpa]
#   user_cookie = login_and_get_user_cookie("myusername", "mypassword")
#
#   send_file_to_session("GDFG56",
#                        filename_local="my_expr_file.txt",
#                        file_save_name="expr.expr.txt",
#                        user_cookie=user_cookie)
#
send_file_to_session <- function(session_id,
                                 filename_local, file_save_name,
                                 user_cookie=NULL) {

  # build the URL to access the data
  post_url = paste0('https://geneatlas.redda.celgene.com/sessions/upload_file',
                    '?cur_session=', session_id,
                    '&file_save_name=', file_save_name)

  # first need to load the form via GET to get the CSRF token
  resp1 = httr::GET(post_url, httr::set_cookies(sessionid=user_cookie))

  if(grepl("Error: file name not allowed", httr::content(resp1, as="text"))) {
    stop(paste("Error: file name not allowed for upload:" , file_save_name))
  }

  # get the csrf token
  csrf_token = get_cookie_value(resp1, "csrftoken")

  if (F) {
    resp1$request
    resp1$cookies
    csrf_token
  }

  resp2 = httr::POST(post_url,
                     httr::add_headers(Authorization=paste("Token",GXPA_TOKEN),
                                       Referer=post_url),
                     httr::set_cookies(sessionid=user_cookie),
                     body = list(file_any=httr::upload_file(filename_local),
                                 cur_session=session_id,
                                 file_save_name=file_save_name,
                                 csrfmiddlewaretoken=csrf_token))
  resp2$request
  resp_text = httr::content(resp2, as="text")
  if (grepl("Please login.*to view this page", resp_text, ignore.case = T)) {
    stop("Error: did not save file, need to log in with admin user")
  }
  if (grepl("Uploaded files have been saved", resp_text, ignore.case = T)) {
    write("Success", stderr())
  } else {
    write("Error: did not receive message saying file was saved", stderr())
  }
}
