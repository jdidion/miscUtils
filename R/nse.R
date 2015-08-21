#' Call a function with only the subset of ...
#' that match its formals.
call.with <- function(fn, ...) {
    d <- list(...)
    do.call(fn, d[names(d) %in% names(formals(fn))])
}