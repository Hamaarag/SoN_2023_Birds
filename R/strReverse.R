strReverse <- function(s) {
	# Reverse a string, but only if we're in RStudio Desktop
	is_posit_desktop <- Sys.getenv("RSTUDIO_PROGRAM_MODE") == "desktop"
	if (is_posit_desktop) {
		s <- sapply(lapply(strsplit(s, NULL), rev), paste, collapse="")
	}
	return(s)
}