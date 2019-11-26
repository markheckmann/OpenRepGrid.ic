
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ic <- list(
    ic.login = FALSE
  )
  toset <- !(names(op.ic) %in% names(op))
  if (any(toset)) options(op.ic[toset])
  
  invisible()
}