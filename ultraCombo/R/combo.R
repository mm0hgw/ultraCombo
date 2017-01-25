#' ultraCombo
#'	@description Create a combination object.
#'	@inheritParams is.valid.nk 
#'	@inheritParams is.valid.index 
#'	@return A list with the following elements:\cr
#'		$i The indices of the combination set.\cr
#'		$n The n of the combination set.\cr
#'		$k The k of the combination set.\cr
#'		$Gen A function which generates combinations from the indices
#'	@examples
#'	n<-20
#'	k<-10
#'	combo<-ultraCombo(seq(choose(n,k)),n,k)
#'	print(combo)
#'	object.size(combo$Gen(seq(combo$len)))
#' @export
ultraCombo <- function(
	i,
	n,
	k
){
	is.valid.nk(n,k)
	is.valid.index(i,n,k)
	i<-multiUnion(i)
	if(length(i)==0 || max(i)<.Machine$integer.max){
		i<-as.integer(i)
		Gen=function(x){
			if(any(c(x<1,x>length(i)))){
				stop("index out of range")
			if(any(c(x<1,x>len))){
				stop("index out of range")
			}
			combnGen(i[x])
		}
			}
			combnGen(i[x])
		}
	class(out)<-"combo"
	out
	}
	combnGen<-combnGG(n,k)
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
	class(out)<-"ultraCombo"
	out
}

#'[.ultraCombo
#'@param x a combo object
#'@param i index of the desired subset of combo object
#'@examples
#'n<-4
#'k<-2
#'combo<-ultraCombo(seq(choose(n,k)),n,k)
#'combo[seq(3)]
#'combo[seq(3)*2]
#'try(combo[0])
#'try(combo[7])
#'@export
'[.ultraCombo' <- function(x,i){
	ultraCombo(x$i[i],x$n,x$k)
}

#'print.ultraCombo
#'@param x An 'ultraCombo' to print
#'@param ... ignored
#'@importFrom utils object.size
#'@method print ultraCombo
#'@export
print.ultraCombo<-function(x,...){
	cat(
		paste(sep="\n",
			paste(sep="","ultraCombo object n=",x$n," k=",x$k),
			paste("contains",x$len,"indices,"),
			paste("has size of",object.size(x),"bytes"),
			""
		)
	)
	invisible(x)
}

#'invert.combo
#'@param a a combo object
#'@examples
#'b<-invert.combo(a)
#'print(b)
#'print(b$i)
#'stopifnot(all(b$i==seq(6)))
#'@importFrom superChoose superChoose
#'@export chunk.combo
invert.combo <- function(a){
	setdiff.combo(seq(superChoose(a$n,a$k)),
		a
	)
}

		}
	)
}

	lc<-l[class
	if(length(l)==0){
		stop("no input")
	}
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
#'@export
slice.combo <- function(combo,slice){
	if(any(c(slice<1,slice>combo$len)))stop("slice values out of range")
	createCombo(combo$i[slice],combo$n,combo$k)
}

#'print.combo
#'@param x a combo object
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
#'@param combo a combo object
#'@param ... extra arguments
#'@examples
#'n<-20
#'k<-10
#'combo<-createCombo(ceiling(runif(100)*choose(n,k)),n,k)
#'combo.print(combo)
