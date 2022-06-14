# go to https://geneatlas.redda.celgene.com/browse/settings to get your token
# some datasets require you to login, so be sure to do that before you copy your token

# you can set this variable elsewhere in the code or try this:
#   GXPA_TOKEN = Sys.getenv("GXPA_TOKEN")
# or this (if you make a ~/.gxpa file -- see auth_config.R for more info):
#   ini = read_config()
#   GXPA_TOKEN = ini$GXPA_TOKEN

# use the app to get the series_id of your favorite dataset
# usage:
#   dat=get_data_from_gxpa(151, c("CD33", "CD34")); head(dat)
get_data_from_gxpa <- function(series_id, genes) {
  # build the URL to access the data
  get_url = paste0('https://geneatlas.redda.celgene.com/series_api/series_data_view/', series_id,
                   '?sample_labels=1&transpose=1&no_feat_info=1&name=')
  get_url = paste0(get_url, paste(genes, collapse='&name=')); get_url

  # send the request to the url and parse the results
  dat = get_csv_from_url(get_url)

  # print to stderr if any of the requested genes are missing
  missing.genes = genes[! genes %in% colnames(dat)]
  if (length(missing.genes) > 0) {
    write(paste("These genes not found:", paste(missing.genes, sep=", ")),)
  }

  return(dat)
}

# Usage:
#   get_series_info_from_gxpa(3)
get_series_info_from_gxpa <- function(series_id) {
  # build the URL to access the data
  get_url = paste0('https://geneatlas.redda.celgene.com/series_api/series/',series_id,'?format=json&name_only=1')
  get_url

  dat = get_json_from_url(get_url)

  return(dat)
}

# Usage:
#   dat=get_series_list_from_gxpa()
get_series_list_from_gxpa <- function() {
  # build the URL to access the data
  get_url = paste0('https://geneatlas.redda.celgene.com/series_api/series/?format=json&name_only=1')
  get_url

  dat = get_json_from_url(get_url)

  return(dat$results)
}


# These are used by the other functions


get_csv_from_url <- function(get_url) {
  # grab the data from GXPA
  r=httr::GET(get_url, httr::add_headers(Authorization=paste("Token",GXPA_TOKEN))); r$status_code
  if (r$status_code != 200) stop(paste("Error (status=", r$status_code, ") while reading url:", expr_data_url))

  # get the output content, check if there is an error
  dat.string = httr::content(r, as="text", encoding = "utf-8")
  if (grepl("^Error", dat.string)) stop(dat.string, stderr())

  # the data from GXPA in in CSV format, so convert to a table
  dat = read.csv(text=dat.string, row.names=1, check.names=T, stringsAsFactors=F, sep=",")
  head(dat)
  return(dat)
}


get_json_from_url <- function(get_url) {
  r=httr::GET(get_url, httr::add_headers(Authorization=paste("Token", GXPA_TOKEN)))
  if (r$status_code != 200) stop(paste("Error (status=", r$status_code, ") while reading url:", get_url))

  # get the output content
  dat.string = httr::content(r, as="text", encoding = "utf-8")

  # I should wrap this in a try..except block
  dat=jsonlite::fromJSON(dat.string)
  #dim(dat$results)
  #dat$count
  head(dat)
}
