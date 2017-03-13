#'chunk.combo
#'@param combo an 'ultraCombo' to chunk
#'@param chunkSize maximum number of indices per chunk.
#'@importFrom bit chunk
#'@export
chunk.combo <- function(combo,chunkSize){
	lapply(chunk(from=1,to=combo$len,by=chunkSize),
		function(ch){
			ultraCombo(combo$i[seq(ch[1],ch[2])],
				combo$n,
				combo$k
			)
		}
	)
}

