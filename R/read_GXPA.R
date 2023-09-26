#' Get data from GXPA
#'
#' Get data from GXPA for a given series id and genes
#'
#' @param series_id dataset series id
#' @param genes select genes
#' @param GXPA_TOKEN GXPA API token. Default is to use the environment variable
#' `GXPA_TOKEN`
#' @param server_url GXPA server (default GXPA_SERVER environment variable or
#' https://geneatlas.redda.bms.com/ if missing)
#'
#' @return
#'
#' a data.frame of expression values for selected genes and sample annotation
#' information.
#'
#' @details
#' Go to [GXPA Settings](https://geneatlas.redda.bms.com/browse/settings)
#' to get your token. Some datasets require you to login, so be sure to do
#' that before you copy your token.
#'
#' Token can be set by:
#'
#'  - For the current session:
#'    - `Sys.setenv(GXPA_TOKEN = "XXXXXXXXXXXXXXXX")`
#'  - For all sessions:
#'    - `usethis::edit_r_environ()`,
#'    -  adding: `GXPA_TOKEN = "XXXXXXXXXXXXXXXX"`
#'
#' @export
#'
#' @examples
#' dat <- get_data_from_gxpa(series_id = 41, genes = c("CD33", "CD34"))
#' head(dat)
#'
get_data_from_gxpa <- function(
    series_id,
    genes,
    GXPA_TOKEN = Sys.getenv("GXPA_TOKEN"),
    server_url = Sys.getenv("GXPA_SERVER",
                            "https://geneatlas.redda.bms.com/")) {

  # input catching
  if (missing(series_id)) {
    stop("series_id can't be missing")
  }
  if (missing(genes)) {
    stop("genes can't be missing")
  }
  if (is.null(GXPA_TOKEN) ||
    is.na(GXPA_TOKEN) ||
    GXPA_TOKEN == "") {
    stop("GXPA_TOKEN can't be missing")
  }

  # build the URL to access the data
  get_url <- paste0(
    server_url,
    "series_api/series_data_view/",
    series_id,
    "?sample_labels=1&transpose=1&no_feat_info=1&name=",
    paste0(genes, collapse = "&name=")
  )

  # send the request to the url and parse the results
  dat <- get_info_from_url(get_url, GXPA_TOKEN)

  # note if any of the requested genes are missing
  if (!is.null(genes)) {
    missing.genes <- genes[!toupper(genes) %in% toupper(colnames(dat))]
    if (length(missing.genes) > 0) {
      message(paste0(
        "These genes not present:\n",
        paste0(missing.genes, collapse = "\n")
      ))
    }
    bad_case_genes <- genes[(toupper(genes) %in% toupper(colnames(dat))) &
                              (!genes %in% colnames(dat))]
    if (length(bad_case_genes) > 0) {
      bad_case_genes_dat <- colnames(dat)[match(toupper(bad_case_genes),
                                                toupper(colnames(dat)))]
      message(paste0(
        "These genes having incorrect case:\n",
        paste0(
          paste0(
            bad_case_genes,
            ' vs. ',
            bad_case_genes_dat
          ),
          collapse = "\n")
      ))
    }
  }
  dat
}

#' Get Series Information from GXPA
#'
#' Get series information from GXPA for a all data or a given series id
#'
#' @param series_id dataset series id(s). NULL (default) will output all dataset
#' series information
#' @param GXPA_TOKEN GXPA API token. Default is to use the environment variable
#' `GXPA_TOKEN`
#' @param server_url GXPA server (default GXPA_SERVER environment variable or
#' https://geneatlas.redda.bms.com/ if missing)
#'
#' @return
#'
#' a data.frame of series information for selected datasets, with the following
#' columns:
#'
#' - `id`: series ID
#' - `name`: dataset name ID
#' - `description`: dataset description
#' - `organism`: dataset organism
#' - `perm_id`: dataset perm_id
#'
#' @details
#' Go to [GXPA Settings](https://geneatlas.redda.bms.com/browse/settings)
#' to get your token. Some datasets require you to login, so be sure to do
#' that before you copy your token.
#'
#' Token can be set by:
#'
#'  - For the current session:
#'    - `Sys.setenv(GXPA_TOKEN = "XXXXXXXXXXXXXXXX")`
#'  - For all sessions:
#'    - `usethis::edit_r_environ()`,
#'    -  adding: `GXPA_TOKEN = "XXXXXXXXXXXXXXXX"`
#'
#' @export
#'
#' @examples
#' get_series_info_from_gxpa(41)
#' # Can give a numeric vector
#' get_series_info_from_gxpa(41:43)
#'
#' # Can get all available data
#' all_dat <- get_series_info_from_gxpa()
#' all_dat$name[1:10]
#' nrow(all_dat)
#' table(all_dat$organism)
#'
#' # Can get series ID based on dataset name
#' all_dat$id[all_dat$name == "Beat AML - OHSU"]
#'
get_series_info_from_gxpa <- function(
    series_id = NULL,
    GXPA_TOKEN = Sys.getenv("GXPA_TOKEN"),
    server_url = Sys.getenv("GXPA_SERVER",
                            "https://geneatlas.redda.bms.com/")) {
  if (is.null(GXPA_TOKEN) ||
    is.na(GXPA_TOKEN) ||
    GXPA_TOKEN == "") {
    stop("GXPA_TOKEN can't be missing")
  }

  full_dat <- get_info_from_url(
    paste0(
      server_url,
      "series_api/series/?format=json&name_only=1"
    ),
    GXPA_TOKEN
  )$results

  if (is.null(series_id)) {
    full_dat
  } else {
    full_dat[full_dat$id %in% series_id, ]
  }
}


#' Get webpage information for a GXPA url
#'
#' @param get_url GXPA url
#' @param GXPA_TOKEN GXPA API token. Default is to use the environment variable
#' `GXPA_TOKEN`
get_info_from_url <- function(get_url,
                              GXPA_TOKEN = Sys.getenv("GXPA_TOKEN")) {
  if (is.null(GXPA_TOKEN) ||
    is.na(GXPA_TOKEN) ||
    GXPA_TOKEN == "") {
    stop("GXPA_TOKEN can't be missing")
  }

  # grab the data from GXPA
  r <- httr::GET(
    get_url,
    httr::add_headers(Authorization = paste("Token", GXPA_TOKEN))
  )
  if (r$status_code != 200) {
    stop("Error (status=", r$status_code, ") while reading url: ", get_url)
  }

  # get the output content, check if there is an error
  dat.string <- httr::content(r, as = "text", encoding = "utf-8")
  if (grepl("^Error", dat.string)) {
    stop("Error getting content from url: ", get_url)
  }

  # parse differently if valid JSON data string
  if (jsonlite::validate(dat.string)[[1]]) {
    jsonlite::fromJSON(dat.string)
  } else {
    # the data from GXPA in in CSV format, so convert to a table
    utils::read.csv(
      text = dat.string,
      row.names = 1,
      check.names = TRUE,
      stringsAsFactors = FALSE,
      sep = ","
    )
  }
}
