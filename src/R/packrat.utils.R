#' Wrapper around read.dcf to workaround LC_CTYPE bug
#' (see: http://r.789695.n4.nabble.com/Bug-in-read-dcf-all-TRUE-td4690578.html)
#' @param ... All parameter to function read.dcf.
#' @return Same as a read.dcf function.
#' @export 
readDcf <- function(...) {
  loc <- Sys.getlocale('LC_CTYPE')
  on.exit(Sys.setlocale('LC_CTYPE', loc))
  read.dcf(...)
}



#' Read only package entries in the lock file, and do not expand package dependencies
#' Useful when a package + its requirements is of interest, and expansion of
#' sub-dependencies is unnecessary
#' @param file A path to first packrat.lock file.
#' @return The list packrat.lock values.
#' @export 
readLockFilePackages <- function(file) {
  # Drop the first row as it contains lockfile-specific info
  lock <- readDcf(file)[-1, , drop = FALSE]
  result <- apply(lock, 1, function(x) {
    x <- as.list(x)
    list(
      name = x$Package,
      source = x$Source,
      version = x$Version,
      requires = as.character(unlist(strsplit(as.character(x$Requires), ",[[:space:]]*", perl = TRUE))),
      hash = x$Hash
    )
  })
  names(result) <- lock[, "Package"]
  result
}