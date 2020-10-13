# function to map variables

#' Map Variables
#'
#' @param data_name0 Dataset where the variable from
#' @param table_code0 Table where the variable from
#' @param var_name Name of the variable
#' @param var_file Variables mapping file
#'
#' @return The real name in raw data
#' @export
#'
#' @examples
#' \dontrun{
#' map_var("BIOCARD", "CSF", "date", var_file)
#' }
map_var <- function(data_name0 = "BIOCARD", table_code0, var_name, var_file) {
  var_file <- data.frame(var_file)
  res <- filter(var_file, data_name == data_name0 &
                  table_code == table_code0 &
                  variables == var_name)[["label"]]
  res <- gsub(" ", "\\.", res)
  res <- gsub("\\(", "\\.", res)
  res <- gsub("\\)", "\\.", res)
  return(res)
}
