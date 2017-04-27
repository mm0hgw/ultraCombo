#'\t@title Reverse Combination Generator Generator
#'\t@name revCombnGG
#'\t@author Dale Potter, \email{dale@@piratepress.org}
#'\t@description Creates a function with a precomputed 
#'\tlook-up table which returns a
#'\treference integer vector when fed with a matrix
#'\tof combinations in rows.
#'\t
#'\t@param n integer < .Machine$integer.max, size of the set from which combination is generated
#'\t@return a 'function (x)' that takes
#'\tcombination matrices and returns reference integers.
#'\t
#'\t@examples
#'\tn<-10
#'\trevCombnGen<-revCombnGG(n)
#'\ti<-cbind(seq(4),5,6,7,8,9,10)
#'\tj<-revCombnGen(i)
#'\tj
#'\tcombnG(j,n,ncol(i))
#'
#'#define game
#'n<-25
#'game<-function(n){
#'\tk<-ceiling(runif(1,0,n))
#'\tout<-vector()
#'\tfor(i in seq(k)){
#'\t\tl<-setdiff(seq(n),out)
#'\t\tout<-c(l[ceiling(runif(1)*length(l))],out)
#'\t}
#'\tout[order(out)]
#'}
#'revCombnGen<-revCombnGG(n)
#'g<-game(n)
#'g
#'i<-revCombnGen(g)
#'k<-length(g)
#'print(i)
#'combnG(i,n,k)
#'stopifnot(sum(g!=.Last.value)==0)
#'@export
revCombnGG <- function(n) {
    debugCat("revCombnGG", n)
    p <- getProfile(n, n%/%2)
    if (p$indexType == "bigz") {
        revCombnGenElem <- revCombnGenElemGenR(p)
    } else {
        revCombnGenElem <- revCombnGenElemGenC(p)
    }
    revCombnGen <- revCombnGenGen(revCombnGenElem, n)
    return(revCombnGen)
}

#'\t@title Reverse Combination Generator
#'\t@name revCombnG
#'\t@author Dale Potter, \email{dale@@piratepress.org}
#'\t@description Returns the reference integer vector of a combination matrix\cr
#'\tk is identified from length(x) or length(x[1,]) as appropriate.
#'\t@param x integer, a single combination as a 'vector' or several (with the same k) as rows in a 'matrix'
#'\t@param n integer, length(1), <= .Machine$integer.max, size of the set from which combination is generated
#'\t@return a 'vector' of the requested indices.
#'\t@examples
#'\tn<-15
#'\tl<-revCombnG(rbind(
#' c(2,7),
#'\t c(3,5),
#'\t c(3,9),
#'\t c(10,1),
#'\t c(6,3),
#' \tc(5,2)
#' ),n)
#'\tprint(l)
#'\tk<-2
#'\tcombnG(l,n,k)
#'\t@export
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
        debugCat("revCombnGen", n, paste(x, collapse = ","))
        switch(is.valid.combination(x, n), vector(), FUN(sort(x)), sapply(seq(nrow(x)), 
            function(y) FUN(sort(x[y, ]))))
    }
}
