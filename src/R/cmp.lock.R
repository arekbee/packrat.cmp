

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

    pnames1 <- unique(sapply(intersect(rownames(pdf1), diff.row), function(x) unlist(strsplit(x, split='.', fixed=TRUE))[1]))
    pnames2 <- unique(sapply(intersect(rownames(pdf2), diff.row), function(x) unlist(strsplit(x, split='.', fixed=TRUE))[1]))

    same.row.pdf1 <- pdf1[same.row,]
    same.row.pdf2 <- pdf2[same.row,]
    
    same.row.pd.diff <- same.row.pdf1 != same.row.pdf2 & !is.na(same.row.pdf1) & !is.na(same.row.pdf2)
    res <- cbind(same.row.pdf1, same.row.pdf2)
    rownames(res) <- same.row 
    colnames(res) <- c('file1', 'file2')
    
    return(list(package.file1=pnames1, package.file2=pnames2, package.diff = res[same.row.pd.diff,]))
}