#' Create Series Info File for GXPA
#'
#' @param filename series info output file name
#' @param series_name series name (can match series name used in GXPA app)
#' @param series_descript series description
#' @param default_group default group to consider for plots
#' (will often be used for x axis variable)
#' @param organism series organism
#' @param expr_units data experimental units
#' @param pmid PubMed id (can be NULL)
#' @param analyst data analyst (can be NULL)
#' @param analyst_comments comments by analyst (can be NULL)
#' @param default_color default color to consider for plots (can be NULL)
#' @param extra.list named list of extra variables to include in the series info
#' file
#'
#' @return writes out csv file of series information to `filename` location
#' @export
#'
#' @examples
#' filename <- tempfile(fileext = ".csv")
#' make_series_info_file(
#'   file = filename,
#'   series_name = "SU2C_2019",
#'   series_descript = "Long description",
#'   default_group = "tumor_site",
#'   organism = "human",
#'   expr_units = "RNA seq log2 (FPKM+1)",
#'   pmid = 31061129,
#'   default_color = "AR_score_group",
#'   extra.list = list(
#'     "Extra_Field" = "Something",
#'     "Another_one" = 5
#'   )
#' )
#' utils::read.csv(filename)
#'
make_series_info_file <- function(filename,
                                  series_name,
                                  series_descript,
                                  default_group,
                                  organism,
                                  expr_units,
                                  pmid = NULL,
                                  analyst = NULL,
                                  analyst_comments = NULL,
                                  default_color = NULL,
                                  extra.list = NULL) {
  if (is.null(filename) ||
    is.null(series_name) ||
    is.null(series_descript) ||
    is.null(default_group) ||
    is.null(organism) ||
    is.null(expr_units)) {
    stop(
      "filename, name, descript, default_group, organism, or expr_units is missing, with no default"
    )
  }

  var_names <- ls(sorted = FALSE)
  var_names <- var_names[!var_names %in% c("filename", "extra.list")]

  out_list <- lapply(var_names, function(xx) {
    eval(parse(text = xx))
  })
  names(out_list) <- var_names

  out_vec <- unlist(out_list)
  if (!is.null(extra.list)) {
    out_vec <- c(out_vec, unlist(extra.list))
  }

  out_table <- data.frame(
    key = names(out_vec),
    value = out_vec
  )
  utils::write.csv(out_table, file = filename, row.names = FALSE)
}


#' Checks Expression and Metadata for Adding to GXPA
#'
#' Performs common checks on the expression data and metadata files.
#' Often run before adding to GXPA
#'
#' @param expr_file expression data file name, with path, to check with metadata
#' @param samples_file metadata data file name, with path, to check with expression data
#'
#' @return message indicating all checks passed
#' @export
#'
#' @details The following check are performed:
#'
#' - number of columns of expression data not equal to number of rows in metadata
#' - check for entirely quoted column names or row names in expr file
#' - duplicated expression data column names
#' - duplicated metadata values in first column
#' - expression data column names that can't link to metadata samples
#'   - Will give warning if linking is possible but currently not matching order
#' - duplicated gene names (ignoring case)
#' - first column name of metadata must be an empty string of `_id`
#' - other column names of metadata must follow standard R variable naming convention
#'
#' @examples
#' \dontrun{
#' check_files_for_GXPA(
#'   expr_file = "expr.expr.txt",
#'   samples_file = "expr.samples.csv"
#' )
#' }
check_files_for_GXPA <- function(expr_file,
                                 samples_file) {
  expr1 <- utils::read.table(expr_file,
                             sep = "\t",
                             header = TRUE,
                             check.names = FALSE,
                             quote = ''
  )
  pd1 <- utils::read.csv(samples_file,
                         check.names = FALSE
  )

  if (ncol(expr1) != nrow(pd1)) {
    stop("Number of columns of expression data should equal number of rows in metadata")
  }

  # Checking sample header starts with "" or "_id"
  if (!colnames(pd1)[1] %in% c("_id", "")) {
    stop('The first column name of the metadata must be an empty string or "_id"')
  }

  # Checking for quoted colnames or rownames
  if (all(grepl('"', colnames(expr1))) || all(grepl('"', colnames(expr1)))) {
    stop('Detected quotes in all expr data column names or row names. ',
         'Check to make sure "quote = FALSE" is used when writing expr data')
  }

  # No duplicated genes expr samples and metadata allowed
  if (any(duplicated(colnames(expr1)))) {
    stop("Detected duplicated header values in expr data")
  }

  if (any(duplicated(pd1[,1]))) {
    stop("Detected duplicated values in metadata sample column")
  }

  # Check expr samples and metadata linking
  if (!all(pd1[,1] == colnames(expr1))) {
    if (any(is.na(match(pd1[,1], colnames(expr1))))) {
      stop("expr data and metadata can not be linked")
    } else {
      warning("expr data and metadata can be linked, but is not currently (i.e. different order)")
    }
  }

  # No duplicated genes allowed
  if (any(duplicated(toupper(rownames(expr1))))) {
    stop("Some of the gene names are duplicated (ignoring case)")
  }

  # metadata colnames must follow standard R variable naming convention
  # except first case
  meta_varnames <- colnames(pd1)[-1]
  if (any(make.names(meta_varnames) != meta_varnames)) {
    stop(
      "The following variable do not follow standard R variable naming convention:\n",
      paste(meta_varnames[make.names(meta_varnames) != meta_varnames],
            collapse = "\n"
      )
    )
  }

  # No gene names are blank or NA
  if (any(rownames(expr1) %in% c("", "<NA>"))) {
    stop('Some of the gene names are blank or "<NA>"')
  }

  message("All checks passed")
}
