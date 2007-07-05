################## Callbacks (from toolbar) #################

drawColorBox_cb <- function(da, event) {
	expRect <- event[["area"]]
	
	gdkDrawRectangle(da[["window"]], da[["style"]][["bgGc"]][[GtkStateType["normal"]+1]], TRUE, 
		expRect[["x"]], expRect[["y"]], expRect[["width"]], expRect[["height"]])

	return(TRUE)
}

selectColor_cb <- function(item, data) {
	data[[1]]$modifyBg("normal", data[[2]])
	data[[1]]$setData("cur-color", data[[2]])
}

colorBtn_cb <- function(button, da) {
    exp_colorEntities(exp_entitySelection(), toGGobiColor(da$getData("cur-color")))
}

# currently clears the color of every entity
clearBtn_cb <- function(button, user.data)
{   
   exp_colorEntities(color=.backgroundColor)
}

updateBtn_cb <- function(button, user.data)
{
   updateColors()
}

################### Creating the brush ###################


addColorsToMenu <- function(menu, button_da, colors)
{
	for (i in length(colors):1) {
		da <- gtkDrawingArea()
		da$setSizeRequest(20, 20)
		da$modifyBg("normal", colors[[i]])
		gSignalConnect(da, "expose_event", drawColorBox_cb)
		menuItem <- gtkImageMenuItem(names(colors)[i])
		gSignalConnect(menuItem, "activate", selectColor_cb, list(button_da, colors[[i]]))
		frm <- gtkFrame()
		frm$add(da)
		frm$setShadowType("in")
		menuItem$setImage(frm)
		menu$attach(menuItem, 0, 1, i, i+1)
	}
}

# custom button for setting and choosing colors
colorMenuToolButton <- function(colors) {
  frame <- gtkFrame()
  frame$setShadowType("etched-out")
  
  da <- gtkDrawingArea()
  da$setSizeRequest(40,20)
  da$modifyBg("normal", colors[[1]])
  da$setData("cur-color", colors[[1]])
  gSignalConnect(da, "expose_event", drawColorBox_cb)
  frame$add(da)
  
  button <- gtkMenuToolButton(frame, "Brush")
  
  colorMenu <- gtkMenu()
  addColorsToMenu(colorMenu, da, colors)
  button$setMenu(colorMenu)
  gSignalConnect(button, "clicked", colorBtn_cb, da)
  
  button
}
