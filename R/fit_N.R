#' fit_N
#'
#' Convert html slide show into pdf
#' @export

fit_N <- function(){
  file_name <- paste0("file://", normalizePath(paste0(path, ".html")))
  webshot::webshot(file_name, paste0(path, ".pdf"))
}
