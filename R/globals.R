# global context containing the internal state of exploRase
.exp <- local({
  .ggobiColors <- NULL # GGobi colorscheme converted to RGB hex strings
  .tooltips <- NULL # the global tooltip context
  .mainWindow <- NULL # the main window of explorase
  .entityBook <- NULL # the notebook holding the entity info views
  .entityModels <- list() # the models for each entity type
  .entityViews <- list() # the views for each entity type
  .designModels <- list() # the models holding experimental design for each type
  .designView <- NULL # the view for the experimental design of current type
  .listModel <- NULL # the model for the entity lists
  .listView <- NULL # the view for the entity lists
  .uiManager <- NULL # the manager for the menus and toolbar
  .datasets <- list() # subsetted experimental data frames for each type
  .gobisets <- list() # TEMPORARY - gone when rggobi supports native data.frames
  .entityType <- NULL # current entity type
  .entityTypes <- list() # a type-named list of entity type labels
  .ggobi <- NULL # the GGobi instance
  .filterModels <- NULL # the models holding filter rules for each type
  .statusbar <- NULL # the bar at the bottom of the screen for progress reporting
  .brushArea <- NULL # the filled rectangle in the brush button
  
  list(
    getGGobiColors = function() .ggobiColors,
    setGGobiColors = function(ggobiColors) .ggobiColors <<- ggobiColors,
  
    getTooltips = function() .tooltips,
    setTooltips = function(tooltips) .tooltips <<- tooltips,
  
    getMainWindow = function() .mainWindow,
    setMainWindow = function(mainWindow) .mainWindow <<- mainWindow,
    
    getEntityBook = function() .entityBook,
    setEntityBook = function(entityBook) .entityBook <<- entityBook,
  
    getEntityModels = function() .entityModels,
    setEntityModel = function(entityModel, ent_type) .entityModels[[ent_type]] <<- entityModel,
  
    getEntityViews = function() .entityViews,
    setEntityView = function(entityView, ent_type) .entityViews[[ent_type]] <<- entityView,
  
    getDesignModels = function() .designModels,
    setDesignModel = function(designModel, ent_type) .designModels[[ent_type]] <<- designModel,
  
    getDesignView = function() .designView,
    setDesignView = function(designView) .designView <<- designView,
  
    getListModel = function() .listModel,
    setListModel = function(listModel) .listModel <<- listModel,
  
    getListView = function() .listView,
    setListView = function(listView) .listView <<- listView,
  
    getUIManager = function() .uiManager,
    setUIManager = function(uiManager) .uiManager <<- uiManager,
  
    getDatasets = function() .datasets,
    setDataset = function(dataset, ent_type) .datasets[[ent_type]] <<- dataset,
    
    getGobisets = function() .gobisets,
    setGobiset = function(gobiset, ent_type) .gobisets[[ent_type]] <<- gobiset,
    
    getEntityType = function() .entityType,
    setEntityType = function(entityType) .entityType <<- entityType,
    
    getEntityTypes = function() .entityTypes,
    addEntityType = function(entityType, typeLabel) .entityTypes[[entityType]] <<- typeLabel,
    
    getGGobi = function() .ggobi,
    setGGobi = function(ggobi) .ggobi <<- ggobi,
    
    getFilterModels = function() .filterModels,
    setFilterModel = function(filterModel, ent_type) .filterModels[[ent_type]] <<- filterModel,
    
    getStatusbar = function() .statusbar,
    setStatusbar = function(statusbar) .statusbar <<- statusbar,

    getBrushArea = function() .brushArea,
    setBrushArea = function(brushArea) .brushArea <<- brushArea,
    
    clear = function() {
      .ggobiColors <<- .tooltips <<- mainWindow <<- .entityBook <<- NULL
      .entityModels <<- .entityViews <<- .designModels <<- list() 
      .designView <<- .listModel <<- .listView <<- .uiManager <<- NULL
      .datasets <<- .gobisets <<- .entityTypes <<- .filterModels <<- list()
      .entityType <<- .ggobi <<- .statusbar <<- NULL
    }
  )
})
