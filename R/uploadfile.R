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


#' Begin a new GXPA session
#'
#' @description This will use the form on the app located at the URL path of
#'      /sessions/new_session. It will create a new session (staging area)
#'      in order to put the data files prior to loading into the database
#'      portion of the app. After making a new session, then run
#'      `send_file_to_session()` for the required files. You can make sure the
#'      files have no warnings or errors by running `dry_run_session()`. Then
#'      you can run `load_session()` in order to trigger a task on the app
#'      which loads the data from the session and into the app database.
#'
#' @param session_name Required: the name of the new session in the app
#'      (letters, numbers and underscores only)
#' @param session_type The type of session to begin: expr (default), GEO,
#' features, extract, analysis
#' @param user_cookie the cookie string. Can use
#'      `[login_and_get_user_cookie()]` to get it. If you don't provide the cookie
#'      string, then the function will run `[login_and_get_user_cookie()`]. So, if
#'      you provide the cookie then it saves one interaction with the server.
#'
#' @return If successful, the session ID (6 characters) will be returned, and
#'      it can be used for other session functions
#' @export
#'
#' @seealso [dry_run_session()], [load_session()]
#'
#' @examples
#' \dontrun{
#' user_cookie <- login_and_get_user_cookie()
#' begin_new_session("new_test_session", user_cookie = user_cookie)
#' }
begin_new_session <- function(
    session_name,
    session_type = c("expr", "GEO", "features", "extract", "analysis"),
    user_cookie = login_and_get_user_cookie()) {

  session_type <- match.arg(session_type)

  # build the URL to access the data
  post_url <- "https://geneatlas.redda.celgene.com/sessions/new_session"

  # first need to load the form via GET to get the CSRF token
  resp1 <- httr::GET(post_url, httr::set_cookies(sessionid = user_cookie))

  # get the csrf token
  csrf_token <- get_cookie_value(resp1, "csrftoken")

  resp2 <- httr::POST(post_url,
    httr::add_headers(Referer = post_url),
    httr::set_cookies(sessionid = user_cookie),
    body = list(
      new_session_name = session_name,
      session_type = session_type,
      csrfmiddlewaretoken = csrf_token
    )
  )

  resp_text <- httr::content(resp2, as = "text")

  if (grepl(
    paste0("Select a valid choice. ",
           session_type,
           " is not one of the available choices"),
    resp_text
  )) {
    stop(paste0("session_type is not allowed: ", session_type))
  }

  if (grepl(paste0("Name [",
                   session_name,
                   "] has already been used, pick a different name"),
            resp_text,
            fixed = TRUE
  )) {
    warning("did not make new session since one with that name already exists,",
            " and its session_id is returned")
  } else if ((grepl(paste0("Made new session [", session_name, "]"),
                   resp_text,
                   fixed = TRUE)) ||
             (session_type == 'features' &&
              grepl("Made custom session filename",
                    resp_text,
                    fixed = TRUE))) {
    message("Success")
  } else {
    stop("did not receive message saying session was made")
  }

  all.sessions <- get_GXPA_session_list()
  all.sessions[all.sessions$name == session_name, "id"]
}

#' Perform a Dry Run of the session (only works in GXPA version with py3)
#'
#' @description This function will trigger the app to execute a dry run of
#'    loading the session data to the database. If you assign the output to
#'    a variable then it will invisibly return the html output from the dry
#'    run and split any errors or warnings into separate list elements. output
#'    list has $output $errors and $warnings. If you don't assign the output
#'    to a variable then errors and warnings will only be printed to stderr.
#'
#'    After the dry run is successful you can run `load_session()` in order
#'    to trigger a task on the app which loads the data from the session and
#'    into the app database.
#'
#' @param session_id Required: the id of session
#' @param run.type Default 'quick' another choice is 'full' which takes longer
#'    since it checks all the lines of the expression matrix
#' @param user_cookie Optional: the cookie string. Can use
#'      login_and_get_user_cookie() to get it. If you don't provide the cookie
#'      string, then the function will run login_and_get_user_cookie(). So, if
#'      you provide the cookie then it saves one interaction with the server.
#'
#' @return Invisibly returns a list with 3 elements: $output has the full html
#'     output of the dry run. $errors has any errors (also printed to stderr),
#'     and $warnings has any warnings (also printed to stderr)/
#'
#' @examples
#' \dontrun{
#' user_cookie <- login_and_get_user_cookie()
#' tmp <- dry_run_session("VLZHJS", user_cookie = user_cookie)
#' tmp <- dry_run_session("8T3X5G", user_cookie = user_cookie)
#' tmp <- dry_run_session("8P9JD3", user_cookie = user_cookie)
#' }
#'
dry_run_session <- function(session_id,
                            run.type = "quick", # the other option is "full"
                            user_cookie = login_and_get_user_cookie()) {

  # build the URL to access the data and get the csrf token
  post_url <- paste0("https://geneatlas.redda.celgene.com/", "load_expr/dry_run?cur_session=", session_id)
  resp1 <- httr::GET(post_url, httr::set_cookies(sessionid = user_cookie))
  csrf_token <- get_cookie_value(resp1, "csrftoken")

  # submit the dry run
  resp2 <- httr::POST(post_url,
    httr::add_headers(Referer = post_url),
    httr::set_cookies(sessionid = user_cookie),
    body = list(
      cur_session = session_id,
      dryrun_choice = run.type,
      csrfmiddlewaretoken = csrf_token
    )
  )
  resp2$request
  resp_text <- httr::content(resp2, as = "text")
  resp_lines <- unlist(strsplit(resp_text, "\n"))

  out_start <- which(grepl("<h2>Dry run:", resp_lines, fixed = T))
  out_end <- which(grepl("Resume Wizard</a></h2>", resp_lines, fixed = T))
  resp_lines <- resp_lines[out_start:out_end]

  out_lines <- c()
  warn_lines <- c()
  err_lines <- c()
  for (i in 1:length(resp_lines)) {
    cur.line <- resp_lines[i]
    cur.line <- sub("^ *", "", cur.line) # remove leading spaces

    # write(paste("checking: ", cur.line), stderr())
    if (length(cur.line) == 1 & cur.line != "") {
      if (any(grepl("error", cur.line, ignore.case = T))) {
        write(cur.line, stderr())
        err_lines <- c(err_lines, cur.line)
      } else if (any(grepl("warning", cur.line, ignore.case = T))) {
        write(cur.line, stderr())
        warn_lines <- c(warn_lines, cur.line)
      }
      out_lines <- c(out_lines, cur.line)
    }
  }

  invisible(list(output = out_lines, warnings = warn_lines, errors = err_lines))
}

#' Load session data to database
#'
#' @description Run the remote function to upload data from the session to the
#' app database. If this session has not been loaded to the database, then
#' `load.type` must be `upload_new` which is the default. On the other hand, if
#' this session is already matched to one in the database, then several other
#' choices for `load.type` are available: `update_series` `add_new_scores` and
#' `load_coords`. For more information on these options, go to the Load Data
#' page on the app.
#'
#' A message will be shown in red (printed to stderr) to say if the task
#' submission was successul or if there was a problem.
#'
#' @param session_id Required: the id of session
#' @param load.type Default 'upload_new'. See description above for more choices.
#' @param user_cookie Optional: the cookie string. Can use
#'      login_and_get_user_cookie() to get it. If you don't provide the cookie
#'      string, then the function will run login_and_get_user_cookie(). So, if
#'      you provide the cookie then it saves one interaction with the server.
#'
#' @return Invisibly returns the full html output which can be useful in
#' case you didn't get a success message.
#' @export
#'
#' @examples
#' \dontrun{
#' # This session (X1NBJK) on the demo app has fake data and is tiny and easy to test
#' load_session("X1NBJK", load.type = "upload_new")
#' load_session("X1NBJK", load.type = "upload_new")
#' load_session("X1NBJK", load.type = "load_scores")
#' }
#'
load_session <- function(session_id,
                         load.type = "upload_new", # the other option is "update"
                         user_cookie = login_and_get_user_cookie()) {
  if (load.type == "update_series") {
    # use this one if the series in the app has a linked session_id
    load.type <- "update_series_using_session_id_and_sample_ids"

    # some old series don't have a session_id, so this would re-establish a link
    # load.type = "update_series_using_name_and_sample_ids"
  }

  # build the URL to access the data and get the csrf token
  post_url <- paste0("https://geneatlas.redda.celgene.com/", "load_expr/", load.type, "?cur_session=", session_id)
  resp1 <- httr::GET(post_url, httr::set_cookies(sessionid = user_cookie))
  csrf_token <- get_cookie_value(resp1, "csrftoken")

  resp1_text <- httr::content(resp1, as = "text")
  if (grepl("Error: This type of data loading action", resp1_text, fixed = T)) {
    stop(
      "Did not submit load task because that load type is not ",
      "currently allowed given the state of the database"
    )
  }

  # submit the load command
  resp2 <- httr::POST(post_url,
    httr::add_headers(Referer = post_url),
    httr::set_cookies(sessionid = user_cookie),
    body = list(
      cur_session = session_id,
      csrfmiddlewaretoken = csrf_token
    )
  )
  resp2$request
  resp_text <- httr::content(resp2, as = "text")

  if (grepl("Page not found", ignore.case = T, resp_text)) {
    stop("load.type is not an option: ", load.type)
  } else if (grepl("Error: This type of data loading action", resp_text, fixed = T)) {
    write("Error: did not submit load task because that load type is not currently allowed given the state of the database [from POST page] ", stderr())
  } else if (grepl("cannot permit that action:", resp_text, fixed = T)) {
    write("Error: cannot permit that load.type action", stderr())
  } else if (grepl("Messages:.*num_inserted", resp_text)) {
    write("Success - data saved", stderr())
  } else if (grepl("saved into task queue:", resp_text, fixed = T)) {
    write("Success - loaded into task queue", stderr())
  } else {
    write("Error: did not receive confirmation message", stderr())
  }
  invisible(resp_text)
}


#' Get a dataframe of all sessions
#'
#' @description Like going to the page /sessions
#'
#' @return If successful, a dataframe of sessions and info about
#' @export
#'
#' @seealso [dry_run_session()], [load_session()]
#'
#' @examples
#' \dontrun{
#' out.df <- get_GXPA_session_list()
#' out.df[grepl("Blueprint", out.df$name, ignore.case = T), ]
#' session_id <- out.df[grepl("blueprint", out.df$name), "id"]
#' }
get_GXPA_session_list <- function() {
  session_url <- paste0("https://geneatlas.redda.celgene.com/", "remote/api")
  dat <- get_info_from_url(session_url)
  dat$user

  out.df <- NULL
  for (cur.id in names(dat$sessions)) {
    cur <- dat$sessions[[cur.id]]

    # perm is sometimes NULL and that causes as.data.frame to fail
    if (is.null(cur$perm)) cur$perm <- 0

    # make sure everything works ok
    tryCatch(
      {
        cur.df <- as.data.frame(cur)
      },
      error = function(e) message(paste("Cannot convert to dataframe:", cur[[1]]))
    )
    cur.df$id <- cur.id

    if (is.null(out.df)) {
      out.df <- cur.df
    } else {
      tryCatch(
        {
          out.df <- rbind(out.df, cur.df)
        },
        error = function(e) message(paste("Cannot combine with previous:", cur[[1]]))
      )
    }
  }
  head(out.df)
  dim(out.df)

  return(out.df)
}


# get_GXPA_session_details("IKAV1T")
get_GXPA_session_details <- function(session_id) {
  session_url <- paste0(
    "https://geneatlas.redda.celgene.com/",
    "remote/api?expr_details=1&dir=1&cur_session=",
    session_id
  )
  dat <- get_info_from_url(session_url)
  dat$user

  return(dat)
}
