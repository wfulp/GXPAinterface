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


under_construction_dont_use_yet___check_files <- function(expr_file, samples_file) {
  expr1 <- utils::read.table(expr_file, sep = "\t", quote = "", header = TRUE)
  pd1 <- utils::read.csv(samples_file, header = TRUE, row.names = 1)

  if (!all(rownames(pd1) == colnames(expr1))) {
    write("Error: Not all metadata rownames are the same as the expr matrix colnames")
  }

  if (any(duplicated(toupper(rownames(expr1))))) {
    write("Error: some of the gene names are duplicated after changing all to upper case")
  }

  colnames.spaces <- colnames(pd1)[grepl(" ", colnames(pd1))]
  if (length(colnames.spaces) > 0) {
    write(paste("Error: some metadata colnames have spaces:", paste(colnames.spaces, collapse = ", ")), stderr())
  }

  colnames.dashes <- colnames(pd1)[grepl("\\-", colnames(pd1))]
  if (length(colnames.dashes) > 0) {
    write(paste("Error: some metadata colnames have dashes:", paste(colnames.dashes, collapse = ", ")), stderr())
  }

  colnames.makenames <- colnames(pd1)[make.names(colnames(pd1)) != colnames(pd1)]
  if (length(colnames.makenames) > 0) {
    write(paste("Error: some metadata colnames change when applying make.names():", paste(colnames.makenames, collapse = ", ")), stderr())
  }
}
