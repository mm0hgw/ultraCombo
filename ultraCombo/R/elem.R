# Generate an Element handler for combnGen
combnGenElemGenC <- function(p) {
    debugCat("combnGenElemGenC", p$indexType, p$n, p$k)
    function(index) {
        debugCat("combnGenElemC", p$indexType, p$n, p$k, index)
        if (p$invert == TRUE) {
            index <- p$imirror - index
            debugCat("combnGenElemC", "inversed index:", index)
        }
        out <- combnGenElemRcpp(index, p$n, p$k, p$ch)
        if (p$invert == TRUE) {
            out <- setdiff(seq(p$n), out)
        }
        debugCat("combnGenElemC", paste(collapse = ",", out))
        out
    }
}

# Generate an Element handler for revCombnGen
revCombnGenElemGenC <- function(p) {
    debugCat("revCombnGenElemGenC", p$indexType, p$n)
    function(x) {
        debugCat("revCombnGenElemC", p$indexType, p$n, ":", paste(collapse = ",", 
            x))
        k <- length(x)
        invert <- FALSE
        if (k > p$n%/%2) {
            invert <- TRUE
            x <- setdiff(seq(p$n), x)
            debugCat("revCombnGenElemC", p$n, ":", paste(collapse = ",", x))
            k <- length(x)
        }
        out <- revCombnGenElemRcpp(x, p$n, p$ch)
        if (invert == TRUE) {
            out <- p$imirror - out
        }
        debugCat("revCombnGenElemC", "out", out)
        out
    }
}

#'@importFrom gmp as.bigq
combnGenElemR <- function(x,n,k,ch){
	i <- n
	j <- k
	out <- vector('integer',k)
	if(class(ch)=='bigz'){
		ch <- gmp::as.bigq(ch)
		gmp <- TRUE
	}else{
		gmp <- FALSE
	}
	oldch <- ch
	while(j>1){
		while(x > (ch <- oldch * j / i)){
			x <- x - ch
			oldch <- oldch - ch
			i <- i - 1
		}
		out[k-j+1] = n-i+1
		oldch <- ch
		i <- i - 1
		j <- j - 1
	}
	out[k] <- as.integer(n-i+x)
	out
}

combnGenElemGenR <- function(p) {
    debugCat("combnGenElemGenR", p$indexType, p$n, p$k)
    function(index) {
        debugCat("combnGenElemR", p$indexType, p$n, p$k, index)
        if (p$invert == TRUE) {
            index <- p$imirror - index
            debugCat("combnGenElemR", "inversed index:", index)
        }
        out <- combnGenElemR(index, p$n, p$k, p$ch)
        if (p$invert == TRUE) {
            out <- setdiff(seq(p$n), out)
        }
        debugCat("combnGenElemR", paste(collapse = ",", out))
        out
    }
}

#'@importFrom gmp as.bigq as.bigz
revCombnGenElemR <- function(x,n,ch){
	k <- length(x)
	ch <- ch * k / n
	if(class(ch)=='bigz'){
		ch <- gmp::as.bigq(ch)
		gmp <- TRUE
	}else{
		gmp <- FALSE
	}
	out <- x[k] - x[k-1]
	i <- 1
	j <- 1
	while(k-i>0){
		if(x[i]-j == 0){
			ch <- ch * (k-i) / (n-j)
			i <- i + 1
			j <- j + 1
		}else{
			out <- out + ch
			ch <- ch - ch * (k-i) / (n-j)
			j <- j + 1
		}
	}
	if(gmp == TRUE){
		gmp::as.bigz(out)
	}else{
		out
	}
}

revCombnGenElemGenR <- function(p) {
    debugCat("revCombnGenElemGenR", p$indexType, p$n)
    function(x) {
        debugCat("revCombnGenElemR", p$indexType, p$n, ":", paste(collapse = ",", 
            x))
        k <- length(x)
        invert <- FALSE
        if (k > p$n%/%2) {
            invert <- TRUE
            x <- setdiff(seq(p$n), x)
            debugCat("revCombnGenElemR", p$n, ":", paste(collapse = ",", x))
            k <- length(x)
        }
        out <- revCombnGenElemR(x, p$n, p$ch)
        if (invert == TRUE) {
            out <- p$imirror - out
        }
        debugCat("revCombnGenElemR", "out", out)
        out
    }
}
