#'@title Reverse Combination Generator
#'@name revCombnG
#'@author Dale Potter, \email{dale@@piratepress.org}
#'@description Returns the reference integer vector of a combination matrix\cr
#'k is identified from length(x) or length(x[1,]) as appropriate.
#'@param x integer, a single combination as a 'vector' or several (with the same k) as rows in a 'matrix'
#'@param n integer, length(1), <= .Machine$integer.max, size of the set from which combination is generated
#'@return a 'vector' of the requested indices.
#'@examples
#'n<-15
#'l<-revCombnG(rbind(
#' c(2,7),
#' c(3,5),
#' c(3,9),
#' c(10,1),
#' c(6,3),
#' c(5,2)
#' ),n)
#'print(l)
#'k<-2
#'combnG(l,n,k)
#'@export
revCombnG <- function(x, n) {
    if (length(dim(x)) == 0) {
        k <- length(x)
    } else {
        k <- ncol(x)
    }
    if (k == 1) {
        revCombnGenElem <- function(x) {
            return(x)
        }
    }
    if (k == n) {
        revCombnGenElem <- function(x) {
            return(1)
        }
    }
    if (!exists("revCombnGenElem")) {
        p <- getProfile(n, k)
        if (p$indexType == "bigz") {
            revCombnGenElem <- revCombnGenElemGenR(p)
        } else {
            revCombnGenElem <- revCombnGenElemGenC(p)
        }
    }
    revCombnGen <- revCombnGenGen(revCombnGenElem, n)
    revCombnGen(x)
}

revCombnGenGen <- function(FUN, n) {
    function(x) {
        debugCat("revCombnGenGen", n, paste(x, collapse = ","))
        switch(is.valid.combination(x, n), vector(), FUN(sort(x)), sapply(seq(nrow(x)),
            function(y) FUN(sort(x[y, ]))))
    }
}
