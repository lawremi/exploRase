# test script for exploRase2

# source("/home/larman/research/explorase/inst/examples/test.R")

#ggobi.home<-function(){"C:/Program Files/GGobi"}

library(explorase)

# add debug functions here
#debug(loadGeneInfo)

#library(explorasetree)

data.path <- "/home/larman/research/explorase/data"

# expression data
gene1.dat<-read.table(paste(data.path, "gene1b.txt", sep="/"),header=T)
# experiment info
exp1.dat<-read.table(paste(data.path, "exp1.txt", sep="/"))
# genelists
gl9.dat <- read.table(paste(data.path, "GL9.txt", sep="/"), header=T)
geneList.dat <- list(gl9.dat)
# gene info
data(gene.help)
# network
network <- ggFile("data/biotin-fa.xml")
# start explorase
explorase(gene.expression=gene1.dat,gene.info=gene.help,exp.info=exp1.dat,network = network, gene.list=geneList.dat)
