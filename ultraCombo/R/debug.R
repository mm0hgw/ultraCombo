debugFlag<-FALSE

debugLine<-function(...){
	if(debugFlag==TRUE){
		cat(paste(...,"\n"))
	}
}

debugPrint<-function(...){
	if(debugFlag==TRUE){
		print(...)
	}
}
