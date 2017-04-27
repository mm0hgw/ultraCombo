#' integer.precision.limit
#'\t@description A constant. The maximum double value the base system can handle
#'\twith integer precision.\cr 
#'\code{
#'\tmax(.Machine$integer.max,\t
#'\t\t2^.Machine$double.digits-1
#'\t)
#'/}
#'\t@export
integer.precision.limit <- 2^(.Machine$double.digits) - 1
