#' Abort with a scans error
#'
#' @param message A character vector containing the error message and optional
#'   cli bullets.
#' @param ... Additional arguments passed to [cli::cli_abort()].
#' @param class Additional condition subclasses.
#' @param call The execution environment of a currently running function, used
#'   to determine the error call.
#' @param .envir The environment used to evaluate cli expressions in `message`.
#'
#' @noRd
scans_abort <- function(
  message,
  ...,
  class = NULL,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  cli::cli_abort(
    message,
    ...,
    class = c(class, "scans_error"),
    call = call,
    .envir = .envir
  )
}
