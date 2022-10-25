

.onLoad <- function(libname, pkgname) {
  op.ic <- list(
    # OpenRepGrid.ic.login = FALSE
  )

  op <- options()
  toset <- !(names(op.ic) %in% names(op))
  if (any(toset)) options(op.ic[toset])

  invisible(NULL)
}
