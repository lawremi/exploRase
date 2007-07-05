# This code is obsolete and broken. Some day it will be rewritten for Cytoscape.
if (FALSE) {
init_fcm_link <- function(fcm_dir = "/home/larman/research/fcmodeler", graph = NULL) 
{
	if (!require(rJava)) # until SOAP stuff arrives or permanately
		stop("cannot run fcmodeler without rJava")
	
	# figure out classpath and start jvm
	fcm_dir <- file.path(fcm_dir, "bin")
	jars <- paste(dir(file.path(fcm_dir, "jars"), full.names=T), collapse=":")
	.jinit(paste(fcm_dir, jars, sep=":"))
	
	# start FCModeler and create external interface
	.jcall("fcmodeler/Main", "V", "main", c(file.path(fcm_dir, "FCModelerConfig.txt"), ""))
	ext <- .jnew("fcmodeler/External")
	
	if (!is.null(graph))
		.jcall(ext, "Z", "openGraph", graph)
	
	data(probe_locus)
	
	ggobi_obj <- asGObject(gg)
	gSignalConnect(ggobi_obj, "identify_point", fcm_identify_cb, ext)
	
	gSignalConnect(.geneView$getSelection(), "changed", fcm_entity_select_cb, ext)
}

fcm_identify_cb <- function(ggobi, plot, id, dataset, ext)
{
	# note that theses are all raw C pointers now, except for id (int)
	# we translated the node ids to locus ids before for linked brushing in ggobi
	# now we have to translate back (or modify the fcmodeler xml)
	probe_id <- .jnull("java/lang/String")
	if (id != -1) {
		g <- bpg(.networks[[1]])
		locus_name <- c(nodesA(g), nodesB(g))[id+1]
		print(locus_name)
		probe_id <- probe_locus[match(locus_name, probe_locus$locus.name)[[1]], "probe.id"]
		if (is.na(probe_id)) {
			.jcall(ext, "Z", "setNodeProp", "id")
			probe_id <- locus_name
		} else {
			.jcall(ext, "Z", "setNodeProp", "nodeName") # want to link by nodename
			probe_id <- toupper(as.character(probe_id))
		}
		print(probe_id)
	}
	.jcall(ext, "Z", "selectNodes", probe_id, F)
}

fcm_entity_select_cb <- function(selection, ext)
{
	ents <- getSelectedEntities()
	.jcall(ext, "Z", "setNodeProp", "nodeName") # want to link by nodename
	ents <- toupper(ents)
	.jcall(ext, "Z", "selectNodes", ents, F)
}
}
