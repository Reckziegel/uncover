#' @keywords internal
tbl_to_mtx <- function(x) as.matrix(dplyr::select(x, where(is.numeric)))

#' @keywords internal
make_tidy_names <- function(x) paste0("...", 1:NCOL(x))

#' @keywords internal
has_colnames <- function(x) !is.null(colnames(x))
