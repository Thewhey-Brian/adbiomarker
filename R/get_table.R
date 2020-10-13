# function to get table

#' Get Subtable
#'
#' @param table_code Code of subtable ("Cognitive" as "COG")
#' @param file_names List of all files name in path
#' @param path Location where data stored
#' @param par_file Parameters dictionary (could be modified if needed)
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' dat_cog <- get_table("COG", file_names, par_file, path)
#' }
get_table <- function(table_code, file_names, par_file, path) {
  key_name <- filter(par_file, file_code == table_code)[["file_name"]]
  real_name <- file_names[grep(key_name, file_names)]
  location <- paste(path, real_name, sep = "")
  start_row <- filter(par_file, file_code == table_code)[["start_row"]]
  dat <- read.xls(xls = location, skip = start_row - 1)
  return(dat)
}
