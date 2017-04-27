#'\t@title Combination Generator Generator
#'\t@name combnGG
#'\t@author Dale Potter, \email{dale@@piratepress.org}
#'\t@description Creates a generator function that transforms
#'\treference integer vectors into combination matrices.
#'\t
#'\t@inheritParams is.valid.nk
#'\t@return a 'function (x,.combine=rbind)' that takes
#'\treference integers in the range 1:choose(n,k) and returns
#'\tthe requested combinations bound as specified.
#'\t
#'\t@examples
#'\t#define combination
#'\tn<-8
#'\tk<-5
#'\tcombnGen<-combnGG(n,k)
#'\t# generate single combinations
#'\tstopifnot(0==sum(combnGen(1)!=seq(k)))
#'\tstopifnot(0==sum(combnGen(2)!=c(seq(k-1),k+1)))
#'\tstopifnot(0==sum(combnGen(choose(n,k))!=seq(to=n,length.out=k)))
#'\t#define larger combination
#'\tn2<-choose(n,k)
#'\tk2<-9
#'\tcombnGen2<-combnGG(n2,k2)
#'\tstopifnot(0==sum(combnGen2(1)!=seq(k2)))
#'\tstopifnot(0==sum(combnGen2(2)!=c(seq(k2-1),k2+1)))
#'\tstopifnot(0==sum(combnGen2(choose(n2,k2))!=seq(to=n2,length.out=k2)))
#'\t#generate random index
#'\tindex <- ceiling(runif(1)*choose(n2,k2))
#'\t#use index to look up indices
#'\tindices <- combnGen2(index)
#'\t# generate result
#'\tcombnGen(indices)
#'\t@useDynLib ultraCombo
#'\t@export
combnGG <- function(n, k) {
    debugCat("combnGG", n, k)
    p <- getProfile(n, k)
    if (k == 1) {
        combnGenElem <- function(index) {
            return(index)
        }
    }
    if (k == n) {
        combnGenElem <- function(index) {
            return(seq(n))
        }
    }
    if (k != 1 && k != n) {
        if (p$indexType != "bigz") {
            combnGenElem <- combnGenElemGenC(p)
        } else {
            combnGenElem <- combnGenElemGenR(p)
        }
    }
    combnGen <- function(index, .combine = rbind) {
        debugCat("combnGen", n, k)
        switch(is.valid.index(index, n, k), vector(), combnGenElem(index), do.call(.combine, 
            lapply(index, combnGenElem)))
    }
    return(combnGen)
}

#'\t@title Combination Generator
#'\t@name combnG
#'\t@author Dale Potter, \email{dale@@piratepress.org}
#'\t@description Generates combinations from a
#'\treference integer vector. combnG(x,n,k)
#'\tis equivalent to combn(n,k)[,x] but does not
#'\tgenerate the rest of the table.\cr
#'\t@inheritParams is.valid.index
#'\t@inheritParams is.valid.nk
#'\t@param .combine the function with which the requested combinations will be bound.
#'\t@return a 'vector' or 'matrix'
#'\tthe requested combinations bound as specified.
#'\t@examples
#'\tn<-20
#'\tk<-10
#'\tstopifnot(0==sum(combnG(1,n,k)!=seq(k)))
#'\tstopifnot(0==sum(combnG(2,n,k)!=c(seq(k-1),k+1)))
#'\tstopifnot(0==sum(combnG(choose(n,k),n,k)!=seq(to=n,length.out=k)))
#'\tcombnG(ceiling(runif(10)*choose(n,k)),n,k)
#'\t
#'\tif(ultraCombo:::debugFlag==FALSE){
#'\t\tstopifnot(sum(combn(n,k)!=combnG(seq(choose(n,k)),n,k,cbind))==0) 
#'\t}
#'
#'\t@export
combnG <- function(i, n, k, .combine = rbind) {
    combnGen <- combnGG(n, k)
    combnGen(i, .combine)
}
