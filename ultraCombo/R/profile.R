getProfile<-function(n,k){
	debugLine("getProfile:",n,k)
	is.valid.nk(n,k)
	if(k==1||k==n){
		return(NULL)
	}
	if(choose(n,k)<integer.precision.limit){
		out<-list(indexType=class(integer.precision.limit))
	}else{
		if(requireNamespace("gmp")){
			out<-list(indexType="bigz")
		}else{
			stop(paste("Combination too big. Install 'gmp' package to extend range.","n:",n,"k:",k,"choose(n,k):",choose(n,k),"limit:",integer.precision.limit,sep=" "))
		}
	}
	if(k>n%/%2){
		out$k<-n-k
		out$invert<-TRUE
		out$imirror<-superChoose(n,k)+1
	}else{
		out$k<-k
		out$invert<-FALSE
	}
	out$n<-n
	debugLine("getProfile","returning ...")
	return(out)
}
