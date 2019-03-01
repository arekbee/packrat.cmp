

#' compares packrat lock files 
#' @param file1 A path to first packrat.lock file.
#' @param file2 A path to second packrat.lock file.
#' @return The list of same packages and diffrent packages.
#' @export 
cmp.lock <- function(file1, file2) {
    packages1 <- readLockFilePackages(file1) 
    packages2 <- readLockFilePackages(file2)

    pdf1 <- as.data.frame(unlist(packages1))
    pdf2 <- as.data.frame(unlist(packages2))
    
    same.row <- intersect(rownames(pdf1, pdf2))
    diff.row <- setdiff(rownames(pdf1, pdf2))


    return(list(package.diff = diff.row))
}