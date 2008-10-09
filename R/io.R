###################### IO GUI #########################

MnuSaveEntityInfo_cb<- function(w, u = NULL)
{
  chooser <- fileSaveDialog("Save Entity Information")
  if (chooser$run() == GtkResponseType["accept"]) {
    type <- exp_entityType()
    filename <- chooser$getFilename()
    filename <- ensureExt(filename, type, "info.csv")
    write.table(unloadInfo(), filename, row.names=F, sep=",")
  }
  chooser$destroy()
}

MnuSaveEntityLists_cb <- function(w, u) {
  selectedButton <- gtkCheckButton("Selected lists only")
  chooser <- folderSelectDialog("Save Entity Lists to a Folder", "save")
  chooser$setExtraWidget(selectedButton)
  if (chooser$run() == GtkResponseType["accept"]) {
    folder <- chooser$getFilename()
    if (selectedButton$getActive())
      list_names <- exp_listSelection()
    else list_names <- getListNames()
    sapply(list_names, function(name) {
      ent_list <- getListMatrix(name)
      write.csv(ent_list,
                paste(file.path(folder, name), "list.csv", sep="."),
                row.names=F)
    })
  }
  chooser$destroy()
}

MnuSaveProject_cb <- function(w, u) {
  chooser <- folderSelectDialog("Save exploRase state to a folder", mode = "save")
  if (chooser$run() == GtkResponseType["accept"]) {
    folder <- chooser$getFilename()
    if (length(dir(folder)) > 0) {
      proceed <- F
      dialog <- gtkMessageDialog(getMainWindow(), "destroy-with-parent", "question", 
        "yes-no", "Files already exist in ", folder, ". If you save the project",
        " there, you might overwrite your data! Are you sure you want to do this? ",
        "If not, click 'No' and save the project to a new empty directory.")
      gSignalConnect(dialog, "response", function(wid, response) {
        proceed <<- response == GtkResponseType["yes"]
        wid$destroy()
      })
      dialog$run()
      if (!proceed) {
        chooser$destroy()
        return()
      }
    }
    info_prefix <- file.path(folder, "explorase")
    printTask("Saving project")
    inc <- 25 / length(which(getNumEntities() > 0))
    sapply(exp_entityTypes()[getNumEntities() > 0], function(ent_type) {
      printOp("Saving", ent_type, "info")
      write.csv(unloadInfo(ent_type),
                paste(info_prefix, ent_type, "info.csv", sep="."),
                row.names=F)
      addProgress(inc)
    })
    printOp("Saving entity lists")
    sapply(getListNames(), function(name) {
      ent_list <- getListMatrix(name)
      write.csv(ent_list,
                paste(file.path(folder, name), "list.csv", sep="."),
                row.names=F)
    })
    addProgress(25)
    gg <- exp_ggobi()
    datasets <- getDatasets()
    sapply(names(datasets), function(name) {
      printOp("Saving", name, "data")
      write.csv(datasets[[name]],
                paste(file.path(folder,"explorase"), name, "data.csv", sep="."),
                row.names=T)
      addProgress(inc)
    })
    sapply(exp_entityTypes()[getNumEntities() > 0], function(ent_type) {
      printOp("Saving", ent_type, "exp. design info")
      write.csv(exp_designFrame(ent_type),
                paste(info_prefix, ent_type, "design.csv", sep="."),
                row.names=F)
      addProgress(inc)
    })
    finishTask()
  }
  chooser$destroy()
}

MnuOpenProject_cb <- function(w, u) {
  chooser <- folderSelectDialog("Open exploRase Projects (folders)")
  chooser$setSelectMultiple(T) 
  if (chooser$run() == GtkResponseType["accept"]) {
    # if any entities or experiments loaded, restart exploRase
    if (any(sapply(c(getEntityModels(), getDesignModels()), nrow) > 0)) {
      exp_close()
      explorase()
    }
    exp_loadFiles(unlist(chooser$getFilenames()))
  }
  chooser$destroy()
}

# works but doesn't look that nice
genPatterns <- function()
{
  types <- getEntityTypes()
  type_names <- names(types)
  types <- sub("s$", "", types)
  p <- paste("*", c(paste(type_names, "data", sep="."),
                    paste(type_names, "info", sep="."),
                    paste(type_names, "design", sep="."), "list"),
             sep=".")
  common <- paste("*", c("csv", "txt"), sep = ".")
  p <- lapply(p, function(glob) c(glob, common))
  p <- c(p, list(paste("*", c("net", "sbml", "xml"), sep = ".")))
  p_names <- c(paste(types, "data"), paste(types, "annotations"), 
    paste(types, "exp. design"), "Entities of interest", "SBML Network")
  names(p) <- p_names
  p
}

MnuOpenFile_cb <- function(w, u) {
  patterns <- genPatterns()
  chooser <- fileOpenDialog("Open exploRase File", patterns)
  chooser$setSelectMultiple(T) 
  if (chooser$run() == GtkResponseType["accept"]) {
    pattern <- patterns[[chooser$getFilter()$getName()]]
    data_type <- entity_type <- NULL
    if (!is.null(pattern)) {
      exts <- findExtensions(pattern[1])
      entity_type <- exts[1]
      data_type <- exts[2]
    }
    exp_loadFiles(unlist(chooser$getFilenames()), data_type, entity_type)
  }
  chooser$destroy()
}

# just for convenience - adds a cancel and open button to dialog
# this also accepts a set of patterns to convert to filters
fileOpenDialog <- function(title, patterns = c(All = "*"),
                           parent = getMainWindow())
{
  d <- gtkFileChooserDialog(title, parent, "open",
                            "gtk-cancel", GtkResponseType["cancel"],
                            "gtk-open", GtkResponseType["accept"])
  all_filter <- gtkFileFilterNew()
  all_filter$setName("All types")
  sapply(unique(unlist(patterns)), all_filter$addPattern)
  d$addFilter(all_filter)
  sapply(names(patterns), function(name) {
    filter <- gtkFileFilterNew()
    filter$setName(name)
    sapply(patterns[[name]], filter$addPattern)
    d$addFilter(filter)
  })
  d
}
# just for convenience - adds a cancel and save button to dialog
fileSaveDialog <- function(title, parent = getMainWindow())
{
	gtkFileChooserDialog(title, parent, "save", "gtk-cancel", GtkResponseType["cancel"],
		"gtk-save", GtkResponseType["accept"])
}
# creates a dialog for selecting a folder
folderSelectDialog <- function(title, mode = "open", parent = getMainWindow())
{
	gtkFileChooserDialog(title, parent, "select-folder", "gtk-cancel", GtkResponseType["cancel"],
		paste("gtk", mode, sep="-"), GtkResponseType["accept"])
}

######################### Loading the files ########################

# Load a project
# Loads a project (a file system directory) into exploRase
# 
# Loads all of the files in a specified directory, using their file extensions
# to determine their purpose.
# 
# @arguments The path to the directory holding the project
# @keyword IO
exp_loadProject <- function(project)
{
  assert(file.info(project)[,"isdir"] == T, "Please choose a directory")
  exp_loadFiles(dir(project, full.names=T))
}

# heuristic to check if format is csv (as opposed to eg tsv)
formatCheck <- function(d, type = "matrix") {
  if (ncol(d) < 2)
    warningDialog("Only ", ncol(d), " columns in ", type, 
      " - is file in CSV (comma-separated value) format?")
}

# Load files
# Loads a set of files into exploRase
#
# If the \code{data_type} is specified, it is assumed that all the files
# are of the given \code{data_type} and \code{entity_type}. Otherwise,
# the types are autodetected based on file extensions.
#
# @arguments The paths to the files to load
# @arguments The data type ("data", "design", "info", "list")
# @arguments The entity type (by default: "gene", "met", "prot"),
# only used if \code{data_type} is specified.
# @keyword IO
exp_loadFiles <- function(filenames, data_type = NULL, entity_type = "gene")
{
  dirs <- file.info(filenames)[,"isdir"]
  sapply(filenames[dirs], exp_loadProject)
  filenames <- filenames[!dirs]
  if (length(filenames) == 0)
    return()
  if (is.null(data_type))
    file_matrix <- cbind(filenames, t(sapply(filenames, findExtensions)))
  else file_matrix <- cbind(filenames, entity_type, data_type)
  printTask("Loading files")
  printOp("Loading entity lists...")
  exp_loadLists(lapply(file_matrix[file_matrix[,3] == "list",1], read.csv, row.names = NULL))
  addProgress(20)
  info_files <- file_matrix[file_matrix[,3] == "info",1]
  inc <- 20 * 1 / length(info_files)
  sapply(info_files, function(f) {
    ent_type <- as.character(file_matrix[f, 2])
    printOp("Loading", ent_type, "information...")
    d <- read.csv(f, row.names = NULL)
    formatCheck(d, "entity information")
    exp_loadInfo(d, ent_type)
    addProgress(inc)
  })
  design_files <- file_matrix[file_matrix[,3] == "design",1]
  inc <- 20 * 1 / length(design_files)
  sapply(design_files, function(f) {
    ent_type <- as.character(file_matrix[f, 2])
    printOp("Loading", ent_type, "design...")
    d <- read.csv(f, row.names = NULL)
    formatCheck(d, "design information")
    exp_loadDesign(d, ent_type)
    addProgress(inc)
  })
  data_files <- file_matrix[file_matrix[,3] == "data",1]
  inc <- 20 * 1 / length(data_files)
  sapply(data_files, function(f) {
    ent_type <- as.character(file_matrix[f, 2])
    printOp("Loading", ent_type, "data...")
    d <- read.csv(f, row.names=1, header=TRUE, check.names = FALSE)
    formatCheck(d, "experimental data")
    exp_loadData(d, sub("\\.data\\.csv$", "", basename(f)), ent_type)
    addProgress(inc)
  })
  #network_files <- file_matrix[file_matrix[,3] == "net",1]
  #inc <- 20 * 1 / length(network_files)
  #printOp("Loading network data...")
  #sapply(network_files, function(f) {
  #  exp_loadNetwork(rsbml_read(f), sub("\\.net$", "", basename(f)))
  #  addProgress(inc)
  #})
  clearTask()
  setEntityType(as.character(file_matrix[1,2]))
}

######################## Utilities #########################

ggFile <- function(...)
{
	file.path(.path.package("explorase"), ...)
}

findExtensions <- function(filename)
{
  # strip off foreign extensions like '.csv'
  filename <- sub(".csv$", "", basename(filename))
  extSplit <- strsplit(filename,"\\.")[[1]]
  extSplit[c(length(extSplit)-1, length(extSplit))]
}

ensureExt <- function(filename, ...)
{
  ext <- paste("", ..., sep=".")
  root <- sub(paste(ext, "$", sep=""), "", filename)
  paste(root, ext, sep="")
}
