getProfile <- function(n, k) {
    # debugCat('getProfile:',n,k)
    is.valid.nk(n, k)
    if (k == 1 || k == n) {
        return(list(n = n, k = k, invert = FALSE, imirror = superChoose(n, k) + 1, 
            indexType = "numeric"))
    }
    out <- list()
    if ((out$ch <- superChoose(n, k)) < integer.precision.limit) {
        out$indexType <- "numeric"
    } else {
        if (requireNamespace("gmp")) {
            out$indexType <- "bigz"
        } else {
            stop(paste("Combination too big. Install 'gmp' package to extend range.", 
                "n:", n, "k:", k, "choose(n,k):", choose(n, k), "limit:", integer.precision.limit, 
                sep = " "))
        }
    }
    out$ifun <- function(i) as(i, out$indexType)
    if (k > n%/%2) {
        out$k <- n - k
        out$invert <- TRUE
        out$imirror <- superChoose(n, k) + 1
    } else {
        out$k <- k
        out$invert <- FALSE
        out$imirror <- superChoose(n, k) + 1
    }
    out$n <- n
    # debugCat('getProfile','returning ...')
    return(out)
}
