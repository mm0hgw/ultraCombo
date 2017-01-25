#' createCombo
#'	@description Create a combination object.
#'	@param indices A vector of combination indices.
#'	@inheritParams combnGen::is.valid.nk combnGen::is.valid.index 
#'	@return A list with the following elements:\cr
#'		$i The indices of the combination set\cr
#'		$n The n of the combination set.\cr
#'		$k The k of the combination set.\cr
#'		$Gen A function which generates combinations from the indices
#'	@examples
#'	n1<-10
#'	k1<-5
#'	n2<-choose(n1,k1)
#'	k2<-5
#'	i<-ceiling(runif(1)*choose(n2,k2))
#'	combo<-createCombo(combnGen::combnG(i,n2,k2),n1,k1)
#'	combo
#'	combo$Gen(seq_along(combo$indices))
#'@importFrom combnGen is.valid.nk is.valid.index combnGG 
#' @export
createCombo <- function(
	i,
	n,
	k
){
	is.valid.nk(n,k)
	is.valid.index(i,n,k)
	combnGen<-combnGG(n,k)
	out<-list(i=i,
		len=length(i),
		n=n,
		k=k,
		Gen=function(x)combnGen(i[x])
	)
}

#'is.combo.compatible
#'@export
is.combo.compatible <- function(...){
	l<-list(...)
	n<-sapply(l,function(x)x$n)
	if(0!=sum(n!=n[1])){
		return(FALSE)
	}
	k<-sapply(l,function(x)x$k)
	if(0!=sum(k!=k[1])){
		return(FALSE)
	}
	return(TRUE)	
}

#'setdiff.combo
#'@export
setdiff.combo <- function(a,b){
	if(is.combo.compatible(a,b)){
		n<-a$n
		k<-a$k
		i<-setdiff(a$indices,b$indices)
		return(createCombo(i,n,k))
	}else{
		stop("mismatched input")
	}
}

#'intersect.combo
#'@export
intersect.combo <- function(a,b){
	if(is.combo.compatible(a,b)){
		n<-a$n
		k<-a$k
		i<-intersect(a$indices,b$indices)
		return(createCombo(i,n,k))
	}else{
		stop("mismatched input")
	}
}

#'union.combo
#'@export
union.combo <- function(...){
	l<-list(...)
	if(is.combo.compatible(...)){
		i<-do.call(multiUnion,lapply(l,function(x)x$indices))
		n<-l[[1]]$n
		k<-l[[2]]$k
		return(createCombo(i,n,k))
	}else{
		stop("mismatched input")
	}
}

#'multiUnion
#'@export
multiUnion <- function(...){
	o<-c(...)
	o[!duplicated(o)]
}
