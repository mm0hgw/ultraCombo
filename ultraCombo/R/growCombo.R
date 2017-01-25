#' growCombo
#' @description Compute geographically contiguous
#' regions by looped expansion.
#'	@param name a character name of the desired area.
#' @param nb a 'nb' object defining element relationships
#' @param k a 'numeric' dictating the size in elements required, default is 7.
#' @param seeds a numeric vector containing the regions to grow, or if negative, the regions to exclude. Default is 0 (which grows all)
#' @return an 'ultraCombo' containing the expanded regions.
#'@importFrom get.lapply get.lapply get.chunkSize
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
	LAPPLYFUN <- get.lapply()
	chunkSize <- get.chunkSize()

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
			# trivial cases
		if(combo$k==n-1){
			return(ultraCombo(1,n,n))
		}
		if(combo$k==0){
			return(ultraCombo(seq(n),n,1))
		}
			# define revCombnGen function
		revCombnGen<-revCombnGG(n)
			# lapply over combo chunks
		combo <- do.call(union.combo,
			LAPPLYFUN(chunk.combo(combo,chunkSize),
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
	print(combo)
	combo
}
