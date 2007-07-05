# some generic utility functions for explorase

# asserts that 'expr' is true. if it's false, then an error is reported,
# described by the pasting of '...'
assert <- function(expr, ...)
{
   if (!(expr)) {
	   errorMsg <- paste("Assertion failed:", deparse(match.call()[["expr"]]), "is NOT true")
	   if (length(c(...) > 0))
		   errorDialog(...)
	   else errorDialog(paste("Congratulations, you have found a bug in exploRase.",
		   		"Please inform Michael and send him the following error message:",
	   			errorMsg, sep="\n"))
	   stop(errorMsg, call.=FALSE) 
   }
}

# add a variable to the GGobi dataset using a structured naming system
# prefix + sep + [subprefix+subsep+name] + sep + ...
createVarName <- function(names, prefix="", sep=".") {
  if (nchar(paste(names,collapse="")) > 0) # don't end in '.'
    paste(prefix, paste(names, collapse = sep), sep = sep)
  else prefix
}

empty_data_frame <- function(types) {
	l <- as.list(sapply(types, vector))
	structure(l, row.names = character(0), class = "data.frame")
}
