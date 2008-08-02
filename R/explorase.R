# exploRase - explorartory analysis of systems biology data

# exploRase code naming conventions:
#   All functions and GUI objects will be in camelBackCase.
#   All other variables are lower_case_underscore_delimited
#   Public "API" functions are prefixed by 'exp_'
#   All callback functions are postfixed with '_cb'
#   Always keep the 'R' in exploRase lower-case in the code

# FYI:
# All global variables are stored in the '.exp' local namespace

# Start exploRase
# The main function of exploRase, normally invoked without arguments, unless
# one already has data/metadata in the R session to load
#
# @arguments Experimental data to load
# @arguments Entity metadata to load
# @arguments Experimental design information to load
# @arguments The entity type of the data being loaded
# @arguments A Bioconductor "graph" to load (not yet implemented)
# @arguments A list of entity lists to load
# @arguments The GGobi session to use
# @arguments Whether R should quit when exploRase is exited
# @keyword dynamic
explorase <-
function (exp_data = NULL, entity_info = NULL, design_info = NULL, type = "gene", 
  network = NULL, entity_lists = NULL, gobi = ggobi_get(), quit_on_exit = F) 
{
    #### Reset exploRase ####
    
    if (exp_isRunning())
      getMainWindow()$destroy() # kill existing exploRase, if any
    .exp$clear() # ensure previous state is cleared
    
    #### Initialize GGobi ####
    
    if (!is.null(gobi))
      gg <- gobi
    else gg <- ggobi()
    .exp$setGGobi(gg)
    
    # FIXME: we need to be able update this on the fly from GGobi
    .exp$setGGobiColors(toGdkColors(colorscheme(gg)$colors))
    
    #### Initialize exploRase GUI ####
    
    # disable animations (requires GTK 2.10)
    if (is.null(gtkCheckVersion(2,10,0))) {
      settings <- gtkSettingsGetDefault()
      settings$set(gtk_enable_animations = F)
    }
    
    # create tooltips repository
    .exp$setTooltips(gtkTooltips())
    
    # create mainWindow window
    mainWindow <- gtkWindow("toplevel", show = F)
    .exp$setMainWindow(mainWindow)
    mainWindow$setTitle(paste("exploRase", getExploraseVersion()))
    mainWindow$setDefaultSize(950, 450)
    mainWindow$setResizable(TRUE)
    gSignalConnect(mainWindow, "delete-event", MnuExit_cb, quit_on_exit)
    
    #### Create data models and views ####
    
    # Entity Lists
    
    .exp$setListModel(listModel())
    .exp$setListView(listView(getListModel()))
    listViewScroll <- scrollView(getListView())
    
    # Entity Information
    
    entityBook <- gtkNotebook()
    .exp$setEntityBook(entityBook)
    
    if (!(type %in% names(.exploraseTypes)))
      exp_addEntityType(type)
    sapply(names(.exploraseTypes), function(t) # register entity types
      exp_addEntityType(t, .exploraseTypes[t]))
    
    gSignalConnect(entityBook, "switch-page", entityBookSwitchPage_cb)
    
    # Experimental Design
    .exp$setDesignView(designView(getDesignModel(type)))
    designViewScroll <- scrollView(getDesignView())
    
    # Ready to set the entity type
    setEntityType(type)
    
    #### Define GUI actions ####
    
    actionEntries <- list(
      list("FileMenu", NULL, "_File"), 
      list("OpenFile", "gtk-open", "_Import File(s)", "<control>F", 
        "Select an individual file to load", MnuOpenFile_cb),
      list("OpenProject", "gtk-goto-top", "_Open", "<control>O", 
        "Select an individual file to load", MnuOpenProject_cb),
      list("SaveMenu", "gtk-save", "_Export"),
      list("SaveEntityInfo", NULL, "Export Entity _Info", "<alt>I", 
            "Save entity information table", MnuSaveEntityInfo_cb),
      list("SaveEntityLists", NULL, "Export Entity _Lists", "<alt>L", 
            "Save entity lists", MnuSaveEntityLists_cb),
      list("SaveProject", "gtk-goto-bottom", "Save", "<control>R", 
            "Save entity information and entity lists", MnuSaveProject_cb),
      list("Quit", "gtk-quit", "_Quit", "<control>Q", "Exit exploRase", MnuExit_cb), 
      #list("SaveDesignInfo", NULL, "_Design Info", 
      #      "<alt>X", "Save experiment design information", MnuSaveDesignInfo_cb), 
      list("AnalysisMenu", NULL, "_Analysis"), 
      #list("CompareGenotypesMenu", NULL, "Compare _Genotypes"), 
      list("InterestingEntsMenu", NULL, "Find _Difference (two conditions)"), 
      list("SimilarEntsMenu", NULL, "Find _Similar (to selected entity)"), 
      list("ClusterMenu", NULL, "Find _Clusters"), 
      list("Patterns", NULL, "Find Specific _Pattern...", NULL, 
           "Open the pattern finding tool", MnuFindPattern_cb), 
      #list("Correlation", NULL, "Co_rrelation", NULL, 
      #      "Find correlation between conditions", MnuCompareCorr_cb), 
      #list("Covariance", NULL, "Co_variance", NULL, 
      #      "Find covariance between conditions", MnuCompareCov_cb), 
      list("Difference", NULL, "_Subtract", NULL, 
            "Subtract one condition from the other - good start", MnuDiff_cb), 
      list("Regression", NULL, "_Regression", NULL, 
            "Residuals from linear regression of conditions", MnuRegression_cb), 
      list("Angle", NULL, "_Angle", NULL, 
        "Angular distance from diagonal in scatterplot", MnuAngle_cb), 
      list("Mahalanobis", NULL, "_Mahalanobis", NULL, 
         "Distance from center of mass, assuming ellipse in scatterplot", 
         MnuMahalanobis_cb), 
      list("Euclidean", NULL, "_Euclidean", NULL, 
            "Compare by magnitude", MnuEuclidean_cb), 
      list("CorrelationDistance", NULL, "_Correlation", NULL, 
            "Compare by pattern", MnuCorr_cb), 
      list("ZeroCorr", NULL, "Cosine _Angle", NULL, 
            "Compare by pattern and magnitude", MnuZerocorr_cb),
      list("Canberra", NULL, "Can_berra", NULL, 
            "Compare by normalized distance (good for zeros)", MnuCanberra_cb), 
      list("HClust", NULL, "_Hierarchical"),
      list("HClustEuc", NULL, "_Euclidean", NULL, "Cluster by magnitude",
        MnuHClust_euclidean_cb),
      list("HClustCor", NULL, "_Correlation", NULL, "Cluster by pattern",
        MnuHClust_cor_cb),
      list("HClustAngle", NULL, "Cosine _Angle", NULL, 
        "Cluster by pattern and magnitude", MnuHClust_zerocor_cb),
      list("HClustCan", NULL, "Can_berra", NULL,
        "Cluster by Canberra distance (good for zeros)", MnuHClust_canberra_cb),
      list("HClustSpearman", NULL, "_Spearman", NULL, "Cluster by rank pattern",
        MnuHClust_spearman_cb),
      list("DataMenu", NULL, "_Tools", NULL),
      list("SubsetData", NULL, "_Subset/Filter...", NULL, 
           "Subset the experimental data based on various criteria", subset_cb),
      list("AverageReplicates", NULL, "_Average over replicates", NULL, 
           "Create new variables for the means over the replicates", average_cb),
      list("MedianReplicates", NULL, "_Median over replicates", NULL, 
           "Create new variables for the medians over the replicates", median_cb),
      list("StdDevReplicates", NULL, "_Standard deviation over replicates", NULL, 
           "Create new variables for the standard deviations over the replicates", stddev_cb),
      list("LogTransform", NULL, "_Log transform", NULL, 
           "Transform the data to the log scale", log_transform_cb),
      list("ImportEntityInfoMenu", NULL, "Import Entity _Info"),
      list("ClearColors", "gtk-clear", "_Clear Colors", NULL, 
            "Clear colors for selected genes (gray-out)", clearBtn_cb), 
      list("SyncColors", "gtk-refresh", "_Sync Colors", NULL, 
            "Synchronize colors with GGobi", updateBtn_cb), 
      list("CreateEntityList", "gtk-indent", "_Create List", NULL,
            "Create a new entity list from selected rows in entity info table",
            createList_cb),
      list("QuerySources", "gtk-jump-to", "_ATGeneSearch", NULL, 
           "Access external data sources for information about selected entities", 
           browseBtn_cb),
      list("ModelingMenu", NULL, "_Modeling"),
      list("Limma", NULL, "_Linear modeling (limma)", NULL, 
           "Fit a linear model to the data using the Limma package", 
           limma_cb),
      list("Temporal", NULL, "_Temporal modeling", NULL, 
           "Fit a polynomial time model", 
           temporal_cb),
      list("GraphicsMenu", NULL, "_Graphics"),
      list("Parcoords", NULL, "_Parallel coordinates plot", NULL,
           "Create a parallel coordinates plot of the selected samples",
           parcoords_cb),
      list("Scatmat", NULL, "_Scatterplot matrix", NULL,
           "Create a scatterplot matrix of the selected samples",
           scatmat_cb))
           
    ag <- gtkActionGroup("exploRaseActions")
    ag$addActions(actionEntries, mainWindow)
    uiManager <- gtkUIManager()
    .exp$setUIManager(uiManager)
    uiManager$setAddTearoffs(T)
    uiManager$insertActionGroup(ag, 0)
    mainWindow$addAccelGroup(uiManager$getAccelGroup())
    
    #### Create UI for actions ####
    
    uiManager$addUiFromFile(ggFile("ui", "ui.xml"))
    menuBar <- uiManager$getWidget("/MenuBar")
    toolBar <- uiManager$getWidget("/ToolBar")
    
    # Add custom brush button to toolbar
    colorBtn <- colorMenuToolButton(getGGobiColors())
    toolBar$insert(colorBtn, 2)

    # Remember the filled area for this button (to get brush color)
    .exp$setBrushArea(colorBtn$getData("brush-area"))
    
    # Link entity list selection to selection in entity info table
    gSignalConnect(getListView()$getSelection(), "changed", 
        listSelectRow_cb)
    
    # Design details button
    designDetailsButton <- gtkButton("Details...")
    gSignalConnect(designDetailsButton, "clicked", designDetailsButton_cb)
    
    #### Statusbar ####
    
    statusbar <- gtkHBox(F, 0)
    progressBar <- gtkProgressBar()
    statusbar$packStart(progressBar, T, T, 0)
    taskBar <- gtkStatusbar()
    taskBar$setSizeRequest(300, -1)
    statusbar$packStart(taskBar, F, F, 0)
    .exp$setStatusbar(statusbar)
    
    #### Put everything together ####
    
    glVbox <- gtkVBox(FALSE, 0)
    designDetailsButtonBox <- gtkHBox()
    designDetailsButtonBox$packStart(designDetailsButton, TRUE, FALSE, 0)
    glVbox$packStart(designViewScroll, TRUE, TRUE, 0)
    glVbox$packStart(designDetailsButtonBox, FALSE, FALSE, 2)
    designFrame <- gtkFrame("Samples/Treatments")
    designFrame$add(glVbox)
    entityListFrame <- gtkFrame("Lists/Pathways")
    entityListFrame$add(listViewScroll)
    entityFrame <- gtkFrame("Entity Information")
    entityFrame$add(entityBook)
    vpan <- gtkVPaned()
    hpan <- gtkHPaned()
    vpan$pack1(designFrame, TRUE, TRUE)
    vpan$pack2(entityListFrame, TRUE, TRUE)
    hpan$pack1(vpan, TRUE, TRUE)
    hpan$pack2(entityFrame, TRUE, TRUE)
    hpan$setPosition(175)
    mainWindowLayout <- gtkVBox(FALSE, 0)
    mainWindowLayout$packStart(menuBar, FALSE, FALSE, 0)
    mainWindowLayout$packStart(toolBar, FALSE, FALSE, 0)
    mainWindowLayout$packStart(hpan, TRUE, TRUE, 0)
    mainWindowLayout$packStart(statusbar, F, F, 0)
    mainWindow$add(mainWindowLayout)
    
    # work around gtk-fs-home icon 'bug'
    # not known if this still exists in GTK 2.10
    iconset <- mainWindow[["style"]]$lookupIconSet("gtk-home")
    factory <- gtkIconFactoryNew()
    factory$add("gnome-fs-home", iconset)
    factory$addDefault()
      
    #### Load any data passed at startup ####

    
    if (!is.null(entity_info)) { # entity information
        exp_loadInfo(entity_info, type)
    }
    
    if (!is.null(design_info)) { # experimental design information
        exp_loadDesign(design_info, type)
    }

    if (is(exp_data, "ExpressionSet")) {
      exp_loadExpressionSet(exp_data, type)
    } else if (!is.null(exp_data)) { # experimental data
      exp_loadData(exp_data, ent_type = type)
    }
    
    # networks would be loaded here...
    
    if (length(entity_lists) > 0) # entity lists
        exp_loadLists(entity_lists)
    
    #### Display GUI, finally ####
    
    mainWindow$showAll()
    
    # Final configuration of view columns
    # This needs to be done at the end (after realization, I think)
    configureViewColumns(getListView())
    configureViewColumns(getDesignView())
    sapply(getEntityViews(), configureViewColumns)
    
    clearTask()
    printOp("Welcome to exploRase: catalyze your analysis")
}

# Package info reflection
getExploraseVersion <- function() {
	installed.packages()["explorase", "Version"]
}

# Check whether exploRase is running
# Currently a hack that checks if the main window exists
# @value whether exploRase is currently running
# @keyword GUI
exp_isRunning <- function() inherits(getMainWindow(), "GtkWindow")

exp_close <- function() {
  getMainWindow()$destroy()
  close(exp_ggobi())
}

# a project has been started, disable loading of more
projectStarted <- function() {
  getAction("OpenProject")$setSensitive(FALSE)
}
