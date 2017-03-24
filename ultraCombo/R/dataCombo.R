#'dataCombo
#'@description A method to wrap a data object with an ultraCombo.
#'@param combo an 'ultraCombo'
#'@param dataObj a data object
#'@param FUN a 'function' used to process data after slicing. Default is invisible()
#'@param ... extra args for '['
#'@export
dataCombo <- function(
	combo,
	dataObj,
	FUN=invisible,
	...
){
	stopifnot(is.ultraCombo(combo))
	stopifnot(is.function(FUN))
	
	out <- combo
	out$dGen <-function(i){
		stopifnot(length(i)==1)
		FUN(dataObj[combo$Gen(i),...])
	}
	out$dataObj <- dataObj
	)
	class(out) <- c('dataCombo', class(out))
	out
}