   ##############################
   # Gene Gobi Tree - to be replaced by something not based on R graphics
   ##############################

ggobiTreeWindow <- function(selectNode_cb, parent = getMainWindow()) {
    win <- gtkWindow("toplevel", show = F)
    win$setTitle("GGobi-Dendogram")
    win$setTransientFor(parent)
    win$setDestroyWithParent(TRUE)

    box <- gtkHButtonBox()
    #box$setDefaultSize(200, 50)
    box$setSpacing(25)
    box$setLayout("edge")
    box$setBorderWidth(10)
    b<-gtkButton("Brush Node")
    box$add(b)
    gSignalConnect(b, "clicked", selectNode_cb)
    c<-gtkButton("End")
    box$Add(c)
    gSignalConnect(c, "clicked", function(w,u=NULL) {
          win$hide()
          win$destroy()
          dev.off()
    })
    win$add(box)
    win
}

##################################################################
# gobitreeco
#
# returns the plotting locations for the nodes of a tree. That is, 
# returns graph coords for intersections of a classification tree 
# dendrogram
#
# input: tree object, optional uniform??
# output: list(x=xcoords, y=ycoords)
# dependencies: gobitree.depth, tree object format
# side effects: none
#######################################################################
gobitreeco <- function (tree, 
            uniform = paste(".Tree.unif", dev.cur(), sep = ".")) 
{
    frame <- tree$frame
    node <- as.numeric(row.names(frame))
    depth <- gobitree.depth(node)
    x <- -depth
    if (exists(uniform)) 
        uniform <- get(uniform)
    else uniform <- 0
    if (uniform) 
        y <- x
    else {
        y <- dev <- frame$dev
        depth <- split(seq(node), depth)
        parent <- match(node%/%2, node)
        sibling <- match(ifelse(node%%2, node - 1, node + 1), 
            node)
        for (i in depth[-1]) y[i] <- y[parent[i]] - dev[parent[i]] + 
            dev[i] + dev[sibling[i]]
    }
    depth <- -x
    leaves <- frame$var == "<leaf>"
    x[leaves] <- seq(sum(leaves))
    depth <- split(seq(node)[!leaves], depth[!leaves])
    left.child <- match(node * 2, node)
    right.child <- match(node * 2 + 1, node)
    for (i in rev(depth)) x[i] <- 0.5 * (x[left.child[i]] + x[right.child[i]])
    list(x = x, y = y)
}

###########################################################################
# gobi.plot.tree
#
# wrapper function to plot a tree structure, with simple lines. 
# 
# inputs: See below
# outputs: none usually used. Can assign list of x and y coords for plotted
#          nodes.
# Dependencies: gobitreepl, tree object structure
# side effects: plots a tree in the motif graphic window, with no lables.
###########################################################################
gobi.plot.tree <- function (x, y = NULL, 
                   type = c("proportional", "uniform"), ...) 
{
    if (!inherits(x, "tree")) 
        stop("Not legitimate tree")
    type <- match.arg(type)
    uniform <- type == "uniform"
    dev <- dev.cur()
    if (dev == 1) 
        dev <- 2
    assign(paste(".Tree.unif", dev, sep = "."), uniform, envir = .GlobalEnv)
    invisible(gobitreepl(gobitreeco(x), node = as.numeric(row.names(x$frame)), 
        ...))
}

###########################################################################
# gobitree.depth
#
# returns the depth of a node within a tree structure. (That is, the number
# of parents to get to the root node, plus one.)
#
# inputs: a vector of node numbers
# outputs. vector of node depths, (numeric) 
# dependencies: none
# side effects: none
###########################################################################
gobitree.depth <- function (nodes) 
{
    depth <- floor(log(nodes, base = 2) + 1e-07)
    as.vector(depth - min(depth))
}


###########################################################################
# gobitreepl
#
# function that actually takes a set of coords from gobitreeco, and plots them
# into the motif window. It also returns the x and y coords of the nodes.
# 
# Inputs: see below
# outputs: list of x and y coords
# dependencies: none
# side effects: plots lines into the motif window
###########################################################################
gobitreepl <- function (xy, node, erase = FALSE, ...) 
{
    x <- xy$x
    y <- xy$y
    parent <- match((node%/%2), node)
    sibling <- match(ifelse(node%%2, node - 1, node + 1), node)
    xx <- rbind(x, x, x[sibling], x[sibling], NA)
    yy <- rbind(y, y[parent], y[parent], y[sibling], NA)
    if (any(erase)) {
        lines(c(xx[, erase]), c(yy[, erase]), col = par("bg"))
        return(x = x[!erase], y = y[!erase])
    }
    plot(range(x), range(y), type = "n", axes = FALSE, xlab = "", 
        ylab = "")
    text(x[1], y[1], "|", ...)
    lines(c(xx[, -1]), c(yy[, -1]), ...)
    list(x = x, y = y)
}

color.click.dn <- function( c.hclust, ids, data, .gobi=ggobi_get() )
  {
  tmp <- .dend.coords( c.hclust )
  c.n  <- length( c.hclust$order )
  c.xy <- tmp$c.xy
  c.cl <- tmp$c.cl
  c.lk <- tmp$c.lk


  # define a function to search the dendrogram for all observations
  # that are in a particular node
  search.merge <- function(i)
    {
    #search left
    if ( c.hclust$merge[i,1] < 0)
      {
      ret.left <- -1 * c.hclust$merge[i,1]
      }
    else
      {
      ret.left <- search.merge(c.hclust$merge[i,1])
      }
    #search right
    if ( c.hclust$merge[i,2] < 0)
      {
      ret.right <- -1 * c.hclust$merge[i,2]
      }
    else
      {
      ret.right <- search.merge(c.hclust$merge[i,2])
      }
    ret <- c(ret.left, ret.right)
    return(ret)
    }

   
  # get a mouse click event
  dex<-identify(x=c.cl[,1], y=c.cl[,2], 
             n=1, plot=FALSE)
  
  # # This code will change as the interface between ggobi and R develops
  # cat("This node is node", dex, fill=TRUE)
  # cat("---------------------------", fill=TRUE)
  # cat("   Its descendants are", obs, fill=TRUE)

  descendants <- search.merge(dex)
  #exp_colorEntities(ids, .backgroundColor)
  #setColors.ggobi(colors = rep(7, nrow(data)), which = show.help[high.list,1],.data="expression.GG")
  exp_colorEntities(ids[descendants])
  #setColors.ggobi(colors = rep(1, length(descendants)), 
  #                  which = show.help[high.list,1][descendants],.data="expression.GG")

  # update ggtree plot
  setup.gobidend(c.hclust)
  }
  
  .dend.coords <- function( c.hclust )
  {
  c.n  <- length( c.hclust$order )
  c.xy <- cbind( order( c.hclust$order ), 0 )    # coordinates for points
  c.cl <- cbind( rep(0,c.n-1), 0 )               # coords for cluster nodes
  c.lk <- matrix( 0, ncol=2, nrow=4*(c.n-1) )    # list of points for cluster
                                                # nodes.
  for(i in 1:(c.n-1) ) {
    ii <- c.hclust$merge[i,1];  jj <- c.hclust$merge[i,2]
    if(ii<0 & jj<0) { c.xy[-ii,2] <- 0
                      c.xy[-jj,2] <- 0
                      c.cl[  i,1] <- ( c.xy[-ii,1] + c.xy[-jj,1] )/2
                      c.cl[  i,2] <- c.hclust$height[i]
                    }
    if(ii<0 & jj>0) { c.xy[-ii,2] <- c.hclust$height[jj]
                      c.cl[  i,1] <- ( c.xy[-ii,1] + c.cl[jj,1] )/2
                      c.cl[  i,2] <- c.hclust$height[i]
                    }
    if(ii>0 & jj<0) { c.xy[-jj,2] <- c.hclust$height[ii]
                      c.cl[  i,1] <- ( c.xy[-jj,1] + c.cl[ii,1] )/2
                      c.cl[  i,2] <- c.hclust$height[i]
                    }
    if(ii>0 & jj>0) { c.cl[  i,1] <- ( c.cl[ii,1] + c.cl[jj,1] )/2
                      c.cl[  i,2] <- c.hclust$height[i]
                    }
    c.lk[4*i - 3, 1] <- ifelse(ii>0, c.cl[ii,1], c.xy[-ii,1])
    c.lk[4*i - 3, 2] <- ifelse(ii>0, c.cl[ii,2], c.xy[-ii,2])
    c.lk[4*i - 2, 1] <- c.cl[i,1]
    c.lk[4*i - 2, 2] <- c.cl[i,2]
    c.lk[4*i - 1, 1] <- ifelse(jj>0, c.cl[jj,1], c.xy[-jj,1])
    c.lk[4*i - 1, 2] <- ifelse(jj>0, c.cl[jj,2], c.xy[-jj,2])
    c.lk[4*i - 0, 1] <- "NA"
    c.lk[4*i - 0, 2] <- "NA"
    }
  return(list(c.xy=c.xy, c.cl=c.cl, c.lk=c.lk))
  }
  
  setup.gobidend <- function( c.hclust, data ) 
  {
  tmp <- .dend.coords( c.hclust )
  c.n  <- length( c.hclust$order )
  c.xy <- tmp$c.xy
  c.cl <- tmp$c.cl
  c.lk <- tmp$c.lk

  plot(rbind(c.xy, c.cl), type="n", axes=FALSE, xlab="", ylab="")
  color <- getGGobiColors()[getGGobiEntityColors(c.hclust$labels)]
  points(c.xy, col = toupper(color), pch = 19)
  lines(c.lk)
  tmp <- seq(2, 4*(c.n-1) - 2, by=4)
  points(c.lk[tmp,] , pch=15)

  # Now, we need to set up the gobi window
#  gobi.instance <- ggobi(data)
#  scatterplot.ggobi(1,2)
  
  # ... and the setup is complete
#  return(gobi.instance)
  }
  
  index <- function (val, vec) 
{
    ret <- NULL
    for (i in 1:length(vec)) {
        if (vec[i] == val) {
            ret <- c(ret, i)
        }
    }
    return(ret)
}

