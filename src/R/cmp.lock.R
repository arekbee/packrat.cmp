

cmp.lock <- function(file1, file2) {
    packages1 <- packrat:::readLockFilePackages(file1)
    packages2 <- packrat:::readLockFilePackages(file2)

    pdf1 <- as.data.frame(unlist(packages1))
    pdf2 <- as.data.frame(unlist(packages2))
    
    same.row <- intersect(rownames(pdf1, pdf2))
    diff.row <- setdiff(rownames(pdf1, pdf2))


    return(list(package.diff = diff.row))
}