#' get_assignment
#'
#' Open .Rmd template for weekly lab assignments
#'
#' @param assignment Assignment number (refer to lab activity page if you are unsure which assignment number you need to open)
#' @export

get_assignment <- function(assignment){
  file <- system.file(paste0("assignments/Assignment", assignment, ".Rmd"), package="WILD3810")
  if(file == "")stop(paste0("Assignment ", assignment, " does not exist.
                             Be sure you entered the correct assignment number.
                             If you are still having trouble, contact your TA"))
  file.edit(file)
}
