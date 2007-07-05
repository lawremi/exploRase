###################### AtGeneSearch interface ######################

# Launch browser to AtGeneSearch URL
browseBtn_cb <- function(button, user.data)
{
   ents <- exp_entitySelection("gene")
   if (length(ents) > 0) { 
	   browseURL(paste(.ATGeneSearchURL, paste(ents, collapse=";"), sep=""))
   }
}
