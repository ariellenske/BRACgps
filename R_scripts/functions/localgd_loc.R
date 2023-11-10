###############################################################################
#set path location of a folder stored in the 'R stuff' google drive folder
#2022-12-06
###############################################################################
localgd_loc <- function(foldername) {
  file.path("G:", "My Drive", "Personal", "R stuff", foldername)
}

