#' Generate Marginal Distributions
#'
#' This function takes a fitted object from the \code{cma_fit} class and generate
#' new scenarios for the marginal distributions.
#'
#' @param model An object of the \code{cma_fit} class.
#' @param n An \code{integer} with the number of samples to be generated.
#'
#' @return An S3 \code{list} of the class \code{marginal}.
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#' colnames(x) <- colnames(EuStockMarkets)
#'
#' # fit the student t
#' dist <- fit_t(x)
#' dist
#'
#' # generate new margins from the fitted model
#' simulate_margins(dist, 10000)
simulate_margins <- function(model, n) {
  if (inherits(model, "uncover_fit")) {
    x <- as.matrix(ghyp::rghyp(n = n, attributes(model)$ghyp))
    new_marginal(x, model$model)
  } else {
    rlang::abort("`model` must be an object of the `uncover_fit` class.")
  }
}


# New marginals -----------------------------------------------------------


#' Internal vctrs methods
#'
#' @param x A numeric vector.
#' @return No return value, called for side effects.
#' @import vctrs
#' @keywords internal
#' @name uncover-marginal
#NULL

# for compatibility with the S4 system
#methods::setOldClass(c("marginal", "vctrs_vctr"))

#' @rdname uncover-marginal
new_marginal <- function(x, ...) {
  dots <- as.list(...)
  if (NCOL(x) == 1) {
    if (!has_colnames(x)) {
      colnames(x) <- make_tidy_names(x)[[1L]]
    } else {
      colnames(x) <- names(x)
    }
  } else {
    if (!has_colnames(x)) {
      colnames(x) <- make_tidy_names(x)
    } else {
      colnames(x) <- colnames(x)
    }
  }
  tibble::new_tibble(x = tibble::as_tibble(x), nrow = nrow(x), class = "marginal", model = dots$model)

  # vctrs::new_list_of(x     = list(marginal = tibble::as_tibble(x)),
  #                    ptype = double(),
  #                    model = dots$model,
  #                    class = "marginal"
  # )
}

#' @importFrom vctrs obj_print_header
#' @export
# obj_print_header.marginal <- function(x, ...) {
#   cat(crayon::green("# New Margins"))
# }

#' @rdname uncover-marginal
#' @export
# obj_print_data.marginal <- function(x, ...) {
#   cat(crayon::cyan(attributes(x)$model))
#   cat("\n")
#   cat("marginal: <<", crayon::silver("tbl"), NROW(x$marginal), "x", NCOL(x$marginal),">>")
# }
