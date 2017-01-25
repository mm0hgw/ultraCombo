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
#'	print(combo)
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
	if(length(i)==0 || max(i)<.Machine$integer.max){
		i<-as.integer(i)
		Gen=function(x){
			if(any(c(x<1,x>length(i)))){
				stop("index out of range")
			}
			combnGen(i[x])
		}
	class(out)<-"combo"
	out
	}
	combnGen<-combnGen::combnGG(n,k)
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
	class(out)<-"combo"
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
#'#stopifnot(all(c(3,4)==intersect.combo(a,b)$i))
#'#stopifnot(all(c(2,3)==intersect.combo(a,seq(2)+1)$i))
#'#stopifnot(all(c(4,5)==intersect.combo(seq(3)+3,b)$i))
#'stopifnot(all(c(4)==intersect.combo(a,b,4)$i))
#'#intersect.combo(a,b,4)$i
#'@inheritParams union.combo
#'@export
intersect.combo <- function(...){
	l<-validateInput(...)
	u<-union.combo(...)
	setdiff.combo(
		u,
		do.call(union.combo,
			lapply(l,setdiff.combo,a=u)
		)
	)
}

#'invert.combo
#'@param a a combo object
#'@examples
#'n<-4
#'k<-2
#'a<-createCombo(vector(),n,k)
#'b<-invert.combo(a)
#'print(b)
#'print(b$i)
#'stopifnot(all(b$i==seq(6)))
#'@importFrom superChoose superChoose
#'@export
invert.combo <- function(a){
	setdiff.combo(seq(superChoose(a$n,a$k)),
		a
	)
}

#'invert.combo
#'@param a a combo object
#'@export
invert.combo <- function(a){
	createCombo(
		setdiff(seq(a$n),
			a$i
		),
		a$n,
		a$k
	)
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
#'@param ... at least one combo object, and any amount of combos or vectors
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
	m<-lapply(l,typeof)
	lc<-l[m=="list",drop=FALSE]
	if(length(lc)==0){
		stop("no combo in args")
	}
	n<-sapply(lc,function(x)x$n)
	k<-sapply(lc,function(x)x$k)
	if(sum(duplicated(n))!=length(n)-1 ||
		sum(duplicated(k))!=length(k)-1
	){
		stop("input combos disagree on n,k parameters")
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

#'slice.combo
#'@param combo a combo object
#'@param slice index of the desired subset of combo object
#'@examples
#'n<-4
#'k<-2
#'combo<-createCombo(seq(choose(n,k)),n,k)
#'slice.combo(combo,seq(3))
#'slice.combo(combo,seq(3)*2)
#'try(slice.combo(combo,0))
#'try(slice.combo(combo,7))
#'@export
slice.combo <- function(combo,slice){
	if(any(c(slice<1,slice>combo$len)))stop("slice values out of range")
	createCombo(combo$i[slice],combo$n,combo$k)
}

#'print.combo
#'@param x a combo object
#'@param ... ignored
#'@examples
#'n<-20
#'k<-10
#'combo<-createCombo(ceiling(runif(100)*choose(n,k)),n,k)
#'print(combo)
#'@importFrom utils object.size
#'@export
print.combo<-function(x,...){
	cat(
		paste(sep="\n",
			paste(sep="","Combo object for n:",x$n," k:",x$k),
			paste("Object contains",x$len,"indices,"),
			paste("and has a memory footprint of",object.size(x),"bytes"),
			""
		)
	)
	invisible(x)
}

#'slice.combo
#'@param combo a combo object
#'@param slice index of the desired subset of combo object
#'@examples
#'n<-4
#'k<-2
#'combo<-createCombo(seq(choose(n,k)),n,k)
#'slice.combo(combo,seq(3))
#'slice.combo(combo,seq(3)*2)
#'try(slice.combo,0)
#'try(slice.combo,7)
#'@export
slice.combo <- function(combo,slice){
	if(any(c(slice<1,slice>combo$len)))stop("slice values out of range")
	createCombo(combo$i[slice],combo$n,combo$k)
}

#'print.combo
#'@param x a combo object
#'@param ... extra arguments
#'@examples
#'n<-20
#'k<-10
#'combo<-createCombo(ceiling(runif(100)*choose(n,k)),n,k)
#'print(combo)
#'@importFrom utils object.size
#'@export
print.combo<-function(x,...){
	print(
		c(
			paste(sep="","Combo object for n:",x$n," k:",x$k),
			paste("Object contains",x$len,"indices."),
		)
		...
	)
	invisible(x)
}
