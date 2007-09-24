################## Statusbar GUI ###################

getStatusbar <- function() .exp$getStatusbar()
getOpbar <- function() getStatusbar()[[1]]
getTaskbar <- function() getStatusbar()[[2]]

################## Conventional progress reporting ###################

# This is much more literate than the AOP style, in my opinion

ensureUpdate <- function() {
  while(gtkEventsPending())
    gtkMainIteration()
}

printTask <- function(...) {
  cursor <- gdkCursorNew("watch")
  getMainWindow()[["window"]]$setCursor(cursor)
  taskbar <- getTaskbar()
  taskbar$push(taskbar$getContextId("task"), paste("Task:", ...))
  ensureUpdate()
}
printOp <- function(...) {
  opbar <- getOpbar()
  opbar$setText(paste(...))
  ensureUpdate()
}
addProgress <- function(amount) {
  opbar <- getOpbar()
  opbar$setFraction(opbar$getFraction() + amount / 100)
  ensureUpdate()
}
setProgress <- function(value) {
  opbar <- getOpbar()
  opbar$setFraction(value / 100)
  ensureUpdate()
}
clearTask <- function() {
  printTask("Idle")
  printOp("Awaiting your command")
  setProgress(0)
  getMainWindow()[["window"]]$setCursor(NULL)
}
finishTask <- function() {
  setProgress(100)
  clearTask()
}

####################### Experimental AOP-style progress reporting ############
#startTask <- function(ops, name) 
#{
#  # override the caller's environment
#  caller <- sys.function(sys.parent())
#  env <- new.env(environment(caller))
#  environment(caller) <- env
#  # set busy cursor
#  cursor <- gdkCursorNew("watch")
#  getMainWindow()[["window"]]$setCursor(cursor)
#  # configure status bar
#  sBar <- getStatusBar()
#  taskBar <- sBar[[1]]
#  taskId <- taskBar$getContextId("task")
#  taskBar$push(taskId, paste("Task:", name))
#  opBar <- sBar[[2]]
#  opId <- opBar$getContextId("op")
#  opProgress <- sBar[[3]]
#  # install interceptors
#  sapply(ops, function(op) {
#    fun <- get(op$fun, env)
#    environment(fun) <- env
#    assign(op$fun, function(...) {
#      opBar$push(opId, paste("Op:",op$name))
#      desc <- op$description
#      if (is.function(desc))
#        desc <- desc(...)
#      opProgress$setText(desc)
#      retval <- fun(...)
#      weight <- op$weight
#      if (is.function(weight))
#        weight <- weight(...)
#      opProgress$setFraction(opProgress$getFraction() + weight)
#      opBar$pop(opId)
#      return(retval)
#    }, env)
#  })
#}
#stopTask <- function()
#{
#  getStatusbar()[[1]]$pop(taskBar$getContextId("task"))
#  getMainWindow()[["window"]]$setCursor(NULL)
#  caller <- sys.function(sys.parent())
#  environment(caller) <- parent.env(environment(caller))
#}
