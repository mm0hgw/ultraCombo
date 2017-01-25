#' growCombo
#' @description Compute geographically contiguous
#' regions by looped expansion.
#'	@param name a character name of the desired area.
#' @param nb a 'nb' object defining element relationships
#' @param k a 'numeric' dictating the size in elements required, default is 7.
#' @param seeds a numeric vector containing the regions to grow, or if negative, the regions to exclude. Default is 0 (which grows all)
#' @return an 'ultraCombo' containing the expanded regions.
#' @export
growCombo <- function(
	name,
	nb,
	k=7,
	seeds=0
){
#	cat(paste("growCombo",name,"\n"))
	stopifnot(length(name)==1)
	stopifnot(is.character(name))
	stopifnot(inherits(nb,'nb'))
	stopifnot(length(k)==1)
	k <- as.integer(k)
	stopifnot(is.integer(k))
	seeds <- as.integer(seeds)
	stopifnot(is.integer(seeds))
	
	comboName <- paste(sep=":",name,k,paste(collapse=",",seeds))
	n <- length(nb)
	if(seeds==0){
		combo <- ultraCombo(1,n,0)
	}else{
		if(all(seeds>0)){
			combo <- ultraCombo(seeds,n,1)
		}else{
			if(all(seeds<0)){
				combo <- setdiff.combo(
					growCombo(nb=nb,k=k,seeds=0),
					growCombo(nb=nb,k=k,seeds=-seeds)
				)
				return(combo)
			}
		}
	}
	while(combo$k<k){
		print(combo)
		combo<-growComboSingle(
			nb,
			combo
		)
	}
	combo
}

#' growComboSingle
#' @description Expand a set of geographically contiguous regions by one element.
#' @param combo a 'list' object of combination definitions.
#' @param nb a 'nb' object defining element relationships
#' @return a 'numeric' vector of combination indices for k+1
#' @importFrom get.lapply get.lapply get.chunkSize
#' @export
growComboSingle <- function(
	nb,
	combo
){
#	cat(paste("growComboSingle k",combo$k,"\n"))
	stopifnot(inherits(combo,'ultraCombo'))
	stopifnot(inherits(nb,'nb'))
	MCLAPPLYFUN <- get.lapply()
	chunkSize <- get.chunkSize()
	n<-combo$n
	if(length(nb)!=n){
		stop("combo 'n' doesn't match ballot 'n'")
	}
	k<-combo$k
		# trivial cases
	if(k==n-1){
		return(ultraCombo(1,n,n))
	}
	if(k==0){
		return(ultraCombo(seq(n),n,1))
	}
		# define revCombnGen function
	revCombnGen<-revCombnGG(n)
		# lapply over combo chunks
	do.call(union.combo,
		MCLAPPLYFUN(chunk.combo(combo,chunkSize),
			function(combo){
				out<-ultraCombo(vector(),combo$n,combo$k+1)
				y<-1
				while(y <= combo$len){
						# compute combination from index
					x <- combo$Gen(y)
						# compute neighbours of combinations
					w <- setdiff(do.call(multiUnion,nb[x]),c(0,x))
	
						# add neighbours to combination
						# and recompute to indices

					z <- do.call(rbind,
						lapply(w,function(i)c(i,x))
					)
					out <- union.combo(out,revCombnGen(z))
					y<-y+1
				}
				out
			}
		)
	)
}
