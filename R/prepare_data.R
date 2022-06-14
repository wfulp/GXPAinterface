

make_series_info_file <- function(file,
                                  name, descript, default_group, organism, expr_units,
                                  pmid=NULL, analyst=NULL, analyst_comments=NULL,
                                  default_color=NULL,
                                  extra.list=NULL) {
  out.table = data.frame(
    key=c("series_name", "series_descript", "default_group", "organism", "expr_units"),
    value=c(name, descript, default_group, organism, expr_units),
    stringsAsFactors = F)
  if (! is.null(default_color)) out.table = rbind(out.table, c("default_color", default_color))
  if (! is.null(pmid)) out.table = rbind(out.table, c("pmid", pmid))
  if (! is.null(analyst)) out.table = rbind(out.table, c("analyst", analyst))
  if (! is.null(analyst_comments)) out.table = rbind(out.table, c("analyst_comments", analyst_comments))
  if (! is.null(extra.list)) {
    extra.table = data.frame(key=names(extra.list),
                             value=as.character(unlist(extra.list)),
                             stringsAsFactors = F)
    out.table = rbind(out.table, extra.table)
  }

  write.csv(out.table, file=file, row.names=F)
}


under_construction_dont_use_yet___check_files <- function(expr_file, samples_file) {
  expr1 = read.table(expr_file, sep="\t", quote="", header=T)
  pd1 = read.csv(samples_file, header=T, row.names = 1)

  if (! all(rownames(pd1) == colnames(expr1)))
    write("Error: Not all metadata rownames are the same as the expr matrix colnames")

  if (any(duplicated(toupper(rownames(expr1)))))
    write("Error: some of the gene names are duplicated after changing all to upper case")

  colnames.spaces = colnames(pd1)[grepl(" ", colnames(pd1))]
  if (length(colnames.spaces) > 0)
    write(paste("Error: some metadata colnames have spaces:", paste(colnames.spaces, collapse=", ")), stderr())

  colnames.dashes = colnames(pd1)[grepl("\\-", colnames(pd1))]
  if (length(colnames.dashes) > 0)
    write(paste("Error: some metadata colnames have dashes:", paste(colnames.dashes, collapse=", ")), stderr())

  colnames.makenames = colnames(pd1)[make.names(colnames(pd1)) != colnames(pd1)]
  if (length(colnames.makenames) > 0)
    write(paste("Error: some metadata colnames change when applying make.names():", paste(colnames.makenames, collapse=", ")), stderr())

}
