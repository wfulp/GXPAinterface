
# read ~/.gxpa file:
#   GXPA_TOKEN = klhkldfgn
#   USERNAME = bfox
#   PASSWORD = mypass
# code modified from: https://stackoverflow.com/questions/54302007/how-to-extract-data-from-ini-file-in-r
read_config = function(fn="~/.gxpa") {
  if (! file.exists(fn)) {
    write(paste("Warning: could not find config file:", fn ), stderr())
    return(NULL)
  }

  if (as.character(file.mode(fn)) != "600") {
    write("Warning: GXPA config file was readable by others. Changing mode to 600", stderr())
    Sys.chmod(fn, mode = "600")
  }

  lines = readLines(fn)
  ini = list()
  for (l in lines) {
    # skip blank lines
    if (grepl("^\\s*$", l)) next

    # read key/val lines
    if (grepl("^.*=.*$", l)) {
      kv = strsplit(l, "\\s*=\\s*")[[1]]
      ini[[kv[1]]] = kv[2]
    }
  }
  ini
}

# run this to log in and get the user cookie.
# default with no params is to read the config file for username and password
login_and_get_user_cookie <- function(GXPA_username=NULL, GXPA_password=NULL) {
  # read the config file
  ini = read_config(); ini

  # if the username and password weren't passed as params, then use config
  if (is.null(GXPA_username)) GXPA_username = ini$USERNAME
  if (is.null(GXPA_password)) GXPA_password = ini$PASSWORD

  # this is the URL for logging in
  login_url = paste0('https://geneatlas.redda.celgene.com/register/login')

  # need to load the form to get the CSRF token
  resp1 = httr::GET(login_url)
  csrf_token = get_cookie_value(resp1, "csrftoken")

  # now log in and then get the user cookie
  resp2 = httr::POST(login_url,
                     httr::add_headers(Referer=login_url),
                     body = list(username=GXPA_username,
                                 password=GXPA_password,
                                 csrfmiddlewaretoken=csrf_token))
  # check for errors
  resp_text = httr::content(resp2, as="text")
  if (grepl("CSRF token missing or incorrect", resp_text)) stop("CSRF error")
  if (grepl("Not logged in", resp_text)) stop("invalid login info")

  resp2$cookies
  user_cookie = get_cookie_value(resp2, "sessionid"); user_cookie

  return(user_cookie)
}

# internal use - get the cookie value
get_cookie_value <- function(resp, name) {
  cook = resp$cookies; cook
  if (name %in% cook$name) {
    val = cook[cook$name==name, "value"]
  } else {
    val = NULL
  }
  return(val)
}
