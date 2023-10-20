###############################################################################
#Set path location of r output files to a folder in the 
#google drive 'R project outputs' folder 
#2022-12-06
###############################################################################
outputs_loc <- function(outputfolder) {
  file.path("G:", "My Drive", "Personal", "R stuff", "R project outputs", outputfolder)
}
