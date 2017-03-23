dataCombo <- function(
	combo,
	dataObj,
	...
){
	stopifnot(is.ultraCombo(combo))
	
	out <- list(Gen=function(i){
			stopifnot(length(i)==1)
			dataObj[combo$Gen(i),...]
		},
		len=combo$len,
		combo=combo,
		dataObj=dataObj
	)
	class(out) <- 'dataCombo'
	out
}
