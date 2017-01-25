#' createCombo
#'	@description Create a combination object.
#'	@inheritParams combnGen::is.valid.nk 
#'	@inheritParams combnGen::is.valid.index 
#'	@return A list with the following elements:\cr
#'		$i The indices of the combination set.\cr
#'		$n The n of the combination set.\cr
#'		$k The k of the combination set.\cr
#'		$Gen A function which generates combinations from the indices
#'	@examples
#'	n<-20
#'	k<-10
#'	combo<-createCombo(seq(choose(n,k)),n,k)
#'	object.size(combo)
#'	object.size(combo$Gen(seq(combo$len)))
#'@importFrom combnGen is.valid.nk is.valid.index combnGG
#' @export
createCombo <- function(
	i,
	n,
	k
){
	combnGen::is.valid.nk(n,k)
	combnGen::is.valid.index(i,n,k)
	i<-multiUnion(i)
	if(max(i)<.Machine$integer.max){
		i<-as.integer(i)
	}
	combnGen<-combnGen::combnGG(n,k)
	out<-list(i=i,
		len=length(i),
		n=as.integer(n),
		k=as.integer(k),
		Gen=function(x){
			if(any(c(x<1,x>length(i)))){
				stop("index out of range")
			}
			combnGen(i[x])
		}
	)
	#class(out)<-"combo"
	out
}

#'setdiff.combo
#'@examples
#'a<-createCombo(seq(4),4,2)
#'b<-createCombo(seq(3)+2,4,2)
#'a$i
#'b$i
#'stopifnot(all(c(1,2)==setdiff.combo(a,b)$i))
#'stopifnot(all(c(1,4)==setdiff.combo(a,seq(2)+1)$i))
#'stopifnot(all(c(1,2,6)==setdiff.combo(seq(6),b)$i))
#'@param a,b one combo and one combo or vector
#'@export
setdiff.combo <- function(a,b){
	l<-validateInput(a,b)
	i<-setdiff(l[[1]]$i,l[[2]]$i)
	n<-l[[1]]$n
	k<-l[[1]]$k
	return(createCombo(i,n,k))
}

#'intersect.combo
#'@examples
#'a<-createCombo(seq(4),4,2)
#'b<-createCombo(seq(3)+2,4,2)
#'a$i
#'b$i
#'stopifnot(all(c(3,4)==intersect.combo(a,b)$i))
#'stopifnot(all(c(2,3)==intersect.combo(a,seq(2)+1)$i))
#'stopifnot(all(c(4,5)==intersect.combo(seq(3)+3,b)$i))
#'@inheritParams setdiff.combo
#'@export
intersect.combo <- function(a,b){
	l<-validateInput(a,b)
	i<-intersect(l[[1]]$i,l[[2]]$i)
	n<-l[[1]]$n
	k<-l[[1]]$k
	return(createCombo(i,n,k))
}

#'union.combo
#'@examples
#'a<-createCombo(seq(4),4,2)
#'b<-createCombo(seq(3)+2,4,2)
#'a$i
#'b$i
#'stopifnot(all(seq(5)==union.combo(a,b)$i))
#'stopifnot(all(seq(4)==union.combo(a,seq(2)+1)$i))
#'stopifnot(all(seq(4)+2==union.combo(b,6)$i))
#'stopifnot(all(seq(6)==union.combo(a,b,6)$i))
#'@param ... at least one object, and any amount of combos or vectors
#'@export
union.combo <- function(...){
	l<-validateInput(...)
	i<-do.call(multiUnion,lapply(l,function(x)x$i))
	n<-l[[1]]$n
	k<-l[[1]]$k
	return(createCombo(i,n,k))
}

validateInput<-function(...){
	l<-list(...)
	m<-lapply(l,class)
	lc<-l[m=="list",drop=FALSE]
	if(length(lc)==0){
		stop("no combo in args")
	}
	n<-sapply(lc,function(x)x$n)
	k<-sapply(lc,function(x)x$k)
	if(sum(duplicated(n))!=length(n)-1 ||
		sum(duplicated(k))!=length(k)-1
	){
		stop("input combos disagree on parameters")
	}
	n<-n[1] 
	k<-k[1]
	lv<-l[m!="list",drop=FALSE]
	if(length(lv)!=0){
		l[m!="list"]<-lapply(lv,createCombo,n,k)
	}
	l
}

#'multiUnion
#'@param ... vectors of values
#'@export
multiUnion <- function(...){
	o<-c(...)
	o[!duplicated(o)]
}
