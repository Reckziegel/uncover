#' Distribution Selection with AIC Criteria
#'
#' Performs a maximum likelihood estimation for univariate and
#' multivariate generalized hyperbolic distributions and reorder the fitted models
#' based on AIC information criteria.
#'
#' @param .invariant A tabular (non-tidy) time-series.
#' @param .silent A logical. Should the optimization results be printed? The default is \code{TRUE}.
#' @param ... Additional arguments the user may want to pass to \code{\link[ghyp]{fit.ghypuv}}.
#'
#' @return A \code{tibble} with the fitted parameters: 11 rows and 8 columns.
#'
#' @seealso \code{\link[ghyp]{stepAIC.ghyp}}
#'
#' @references Pfaff, Bernhard. Financial risk modelling and portfolio optimization with R.
#' John Wiley & Sons, 2016.
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#' colnames(x) <- colnames(EuStockMarkets)
#'
#' reveal(x)
reveal <- function(.invariant, .silent = TRUE, ...) {
  UseMethod("reveal", .invariant)
}


#' @rdname reveal
#' @export
reveal.default <- function(.invariant, .silent = TRUE, ...) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}


#' @rdname reveal
#' @export
reveal.tbl_df <- function(.invariant, .silent = TRUE, ...) {
  reveal_(.invariant = tbl_to_mtx(.invariant), .silent = .silent, ...)
}


#' @rdname reveal
#' @export
reveal.xts <- function(.invariant, .silent = TRUE, ...) {
  reveal_(.invariant = as.matrix(.invariant), .silent = .silent, ...)
}


#' @rdname reveal
#' @export
reveal.matrix <- function(.invariant, .silent = TRUE, ...) {
  reveal_(.invariant = .invariant, .silent = .silent, ...)
}

#' @rdname reveal
#' @export
reveal.numeric <- function(.invariant, .silent = TRUE, ...) {
  reveal_(.invariant = .invariant, .silent = .silent, ...)
}


#' @keywords internal
reveal_ <- function(.invariant, .silent = TRUE, ...) {

  assertthat::assert_that(assertthat::is.flag(.silent))

  suppressMessages(
    suppressWarnings(
      ghyp::stepAIC.ghyp(data = .invariant, silent = .silent, ...)
    )
  ) |>
    purrr::chuck("fit.table") |>
    tibble::as_tibble()

}
