# Install script for exploRase

win_gtk_url <- "http://umn.dl.sourceforge.net/sourceforge/gladewin32/gtk-dev-2.8.20-win32-1.exe"
win_ggobi_url <- "http://www.ggobi.org/downloads/ggobi_2.1.4.exe"

gtk_web <- "http://www.gtk.org"
ggobi_web <- "http://www.ggobi.org"

explorase_repos <- "http://www.ggobi.org/r"

install_system_dep <- function(package_name, dep_name, dep_url, dep_web)
{
  if (!require(package_name)) {
    if (is.null(dep_url))
      print(paste("Please install", dep_name, "on your system."))
    else {
      choice <- menu(paste(c("Install", "Do not install"), dep_name), T, 
        paste(dep_name, "may be missing, would you like to install it?"))
      if (choice == 1) {
        if (download.file(dep_url) > 0)
          error("Failed to download ", dep_name)
        else system(file.path(tempdir(), basename(dep_url)))
      } else error("Could not load ", package_name)
    }
    print(paste("Learn more about", dep_name, "at", dep_web))
  }
}

install_explorase <- function(gtk_url = NULL, ggobi_url = NULL) {
  #install.packages(c("rggobi", "limma"), dep = T)
  install.packages("explorase", c(explorase_repos, "@CRAN@"), dep = T)
  
  install_system_dep("RGtk2", "GTK+", gtk_url, gtk_web)
  install_system_dep("rggobi", "GGobi", ggobi_url, ggobi_web)
}

if (.Platform$OS.type == "windows")
  install_explorase(win_gtk_url, win_ggobi_url)
else if (length(grep("darwin", R.version$platform))) 
  #install_explorase(mac_gtk_url, mac_ggobi_url)
  stop("Sorry OS X is not yet supported")
else install_explorase()
