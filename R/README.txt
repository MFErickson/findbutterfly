Various scripts for managing and analysing the Find the Butterfly website data.


.Renviron: defines some user credential variables that are required for downloading the database contents

resize-butterfly-photos.R: Resizes butterfly photos so they all have similar numbers of non-transparent pixels

build-photo-info-csv.R: Script to update the photo_info.csv file, which lists all the available butterfly and background images

download-data.R: downloads the database of user scores. Relies on the existance of database credentials in the .Renviron file

analysis.R: Reports various stats from the game data

functions.R: Run from analysis.R

dorsal-ventral.R: Run from analysis.R
  
background-anayses.R: Performs some analysis of background characteristics