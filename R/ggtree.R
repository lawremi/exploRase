   ##############################
   # Gene Gobi Tree - to be replaced by something not based on R graphics
   ##############################

ggobiTreeWindow <- function(selectNode_cb, parent = mainWindow) {
    win <- gtkWindow("toplevel", show = F)
    win$setTitle("GGobi-Dendogram")
    win$setTransientFor(parent)
    win$setDestroyWithParent(TRUE)

    box <- gtkHButtonBox()
    #box$setDefaultSize(200, 50)
    box$setSpacing(25)
    box$setLayout("edge")
    box$setBorderWidth(10)
    b<-gtkButton("Select Node")
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

color.click.dn <- function( c.hclust, ids, data, .gobi=getDefaultGGobi() )
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
  exp_colorEntities(ids[descendants], 1)
  #setColors.ggobi(colors = rep(1, length(descendants)), 
  #                  which = show.help[high.list,1][descendants],.data="expression.GG")
  }
  
  color.click.tr <- function( treeobj, data, setaxes=FALSE, n="NA", 
                  .gobi=getDefaultGGobi() )
  {

  # Check to see if we have a node number. If not, get one
  if(n == "NA")
    {
    # first, get the list of clickable coordinates, pare out the leaves
    xcoord <- (gobitreeco(treeobj))$x
    ycoord <- (gobitreeco(treeobj))$y
    xcoord[treeobj$frame$var == "<leaf>"] <- "NA"
    ycoord[treeobj$frame$var == "<leaf>"] <- "NA"
    xcoord <- as.numeric(xcoord)
    ycoord <- as.numeric(ycoord)

    # get a mouse click event
    dex<-identify(x=xcoord, y=ycoord, n=1, plot=FALSE)
    
    # This code will change as the interface between ggobi and R develops
    nodenum <- as.numeric(rownames(treeobj$frame)[dex])
    #  print(c("The node number clicked is ", nodenum))
    }
  else
    {
    nodenum <- n
    }

  # Check the setaxes option
  if(setaxes==TRUE)
    {
    cat("Option 'setaxes' is not yet implemented", fill=TRUE)
    # # find this node's variable name
    # this.var <- as.character((treeobj$frame$var)[nodenum])
    #
    # # find the parent
    # if(nodenum == 1) # This is the root node: no parent
    #   parent.node <- nodenum * 2  # substitute in the left child's node #
    # else
    #   {
    #   if( nodenum%%2 == 0 )
    #     parent.node <- nodenum/2
    #   else
    #     parent.node <- (nodenum-1)/2
    #   }
    # 
    # # find parent's variable name
    # that.var <- as.character((treeobj$frame$var)[parent.node] )
    #
    # # set the mode
    # setMode.ggobi("XYPlot", .gobi=.gobi)
    #
    # # set the axes
    # setPlotVariables.ggobi(this.var, that.var)
    }
  left.child <- nodenum * 2
  right.child <- nodenum*2 + 1

  # Now, we use the matrix that was passed in, and make a 
  # new tree, snipped at the appropriate places. Then we use that 
  # smaller tree to predict everything in the data set, and 
  # color the ones that appear at the sides of the node clicked
  smtree <- snip.tree(treeobj, nodes=c(left.child, right.child))
  smpreds <- predict(smtree, data, type="where")

  left.child.dex <- match(left.child, 
       as.numeric(row.names(smtree$frame)))
  right.child.dex <- match(right.child, 
       as.numeric(row.names(smtree$frame)))

  left.obs <- index(left.child.dex, smpreds)
  right.obs <- index(right.child.dex, smpreds)

  setColors.ggobi(colors = rep(7, length(smpreds)), 
                                     which=1:length(smpreds))
  setColors.ggobi(colors = rep(2, length(left.obs)), which=left.obs)
  setColors.ggobi(colors = rep(9, length(right.obs)), which=right.obs)
  
  } #closes function


  color.node.tr <- function(treeobj, data, nodenum, .gobi=getDefaultGGobi())
  {
  
  # This is exactly the same as color.click, except the node number is 
  # passed in as an argument
  color.click.tr(treeobj=treeobj, data=data, n=nodenum, .gobi=.gobi)

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
  points(c.xy)
  lines(c.lk)
  tmp <- seq(2, 4*(c.n-1) - 2, by=4)
  points(c.lk[tmp,] , pch=15)

  # Now, we need to set up the gobi window
#  gobi.instance <- ggobi(data)
#  scatterplot.ggobi(1,2)
  
  # ... and the setup is complete
#  return(gobi.instance)
  }
  
  setup.gobitree <- function(treeobj, data)
  {
  # a little housecleaning:
  par(mfrow=c(1,1))
  
  # First, plot the tree using the gobi.plot.tree routine 
  # stolen from the tree library.
  gobi.plot.tree (treeobj)
  
  # here is a vector of node numbers That is, if the 5th elem is "1", then the
  # fifth line of the $frame has info for node number 1.
  node <- as.numeric(row.names(treeobj$frame))
  
  # Here, we have coordinates for plotting the nodes. This depends on the 
  # function gobitreeco found elsewhere in the library.
  xcoord <- (gobitreeco(treeobj))$x
  ycoord <- (gobitreeco(treeobj))$y
  xcoord[treeobj$frame$var == "<leaf>"] <- "NA"
  ycoord[treeobj$frame$var == "<leaf>"] <- "NA"
  xcoord <- as.numeric(xcoord)
  ycoord <- as.numeric(ycoord)
  
  # draw the points as big squares, big enough to click on
  points(xcoord, ycoord, pch=15, cex=1.7)
  
  #Now, for the variable labels. These appear just below the nodes themselves
  #Note that we remove the labels for the <leaf> nodes
  label.x <- xcoord  # no change in x position
  label.y <- ycoord - (par("cxy"))[2] # Move down by one line of text
  label.x[treeobj$frame$var == "<leaf>"] <- "NA"
  label.y[treeobj$frame$var == "<leaf>"] <- "NA"
  
  par(adj=0.5)  # Center the text 
  text(x=as.numeric(label.x), y=as.numeric(label.y), 
                 labels=as.character(treeobj$frame$var))

  # Now, define indeces for the children of each node. That is, if 
  # leftchild[3] 
  # is a 6, then the info for the left child of the node described in row 3 
  # is described in row 6. There are no node numbers here, 
  # just location indeces
  leftchild <- match(node*2, node)
  rightchild <- match((node * 2 + 1), node)
  
  # Now, to put the labels on each side of the node, we define the x location 
  # of the text: between the node and it's child. We define one set of 
  # locations
  # for the left children, and one set for the right. The y locations are the
  # same for both sets: a bit above the location of the node.
  # If a node has no left or right leaf, the coords are NA from above
  split.left.x <- (xcoord + xcoord[leftchild] ) / 2
  split.right.x <- (xcoord + xcoord[rightchild] ) / 2
  split.both.y <- ycoord + (par("cxy")[2])*0.7
  
  # Put in the actual text for the points
  text(x=as.numeric(split.left.x), y=as.numeric(split.both.y), 
           label=as.character(treeobj$frame$splits[,1]))
  text(x=as.numeric(split.right.x), y=as.numeric(split.both.y), 
           label=as.character(treeobj$frame$splits[,2]))
  
  # Now that the R window is done, we need to start up a ggobi window.
  gobi.instance <- ggobi(data)
  scatterplot.ggobi(1, 2)
  
  # and that finishes the setup. 

  return(gobi.instance)
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

