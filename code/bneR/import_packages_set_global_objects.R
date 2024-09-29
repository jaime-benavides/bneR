# File: a_00_import_packages_set_global_objects.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/21
#
# Contents:
#  0. Prepare
#  1. Load Packages 
#  2. Set Project-Wide Objects
#  3. Load Functions 
#  4. Make CONUS outline

#### ------------ ####
####  0. PREPARE  ####
#### ------------ ####

# 0.a. indicate that the script was run
# although it is intuitive to run this at the end of the script, 
# that creates a recursion with the functions
ran_a_00 <- "ran_a_00"

#### ------------------ ####
####  1. LOAD PACKAGES  ####
#### ------------------ ####

# 1.a. load packages
# Note that nngeo is required but not listed here. nngeo could not be installed in the server 
# where we ran the code, so we manually compiled the package in the server. 
# feel free to use the following code to install nngeo 
# install.packages('nngeo')

# Robbie: Maybe this could be a type of function which loads packages if they're already installed, or if not installs them and then loads
# Robbie: Potentially via something like...

# list.of.packages = X # below

# # check if list of packages is installed. If not, it will install ones not yet installed
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) invisible(install.packages(new.packages,repos = "https://cloud.r-project.org"))

# load packages
# invisible(lapply(list.of.packages, require, character.only = TRUE, quietly=TRUE))

# Robbie: Not necessary but just a thought as would have had to individually load packages otherwise

# Sebastian: I agree. The pacman package has a function that is perfect for this. 
# Solution: I used p_load from the pacman package, which does exactly what you are 
# suggesting. I also leave in your code in case the pacman 

pacman::p_load("tidyverse", "lubridate", "magrittr", "janitor", # tidyverse packages
               "sf", "raster", "rgdal", "sp", "ncdf4", # spatial packages
               "nabor", "units", "methods", "lwgeom", "s2", "data.table", #nngeo requirements
               "foreach", 
               "fst", "FNN",
               "latex2exp",
               "purrr", "furrr", "future", "progress", "progressr", "parallel", #"doSNOW", # efficiency/parallelezation packages
               "mgcv", "splines", "lme4", # stats packages
               "egg", "cowplot", "corrplot", "pals", "colorspace", "ggsci", "scico", "viridis")


# Alternative approach in case pacman can't get installed in Harmattan environment
#list.of.packages <- c("tidyverse", "lubridate", "magrittr", "janitor", # tidyverse packages
#                      "sf", "raster", "rgdal", "sp", "stars", "ncdf4", # spatial packages
#                      "nabor", "units", "methods", "lwgeom", "s2", "data.table", #nngeo requirements
#                      "foreach", 
#                      "fst", "FNN",
#                      "latex2exp",
#                      "purrr", "furrr", "future", "progress", "progressr", "parallel", #"doSNOW", # efficiency/parallelezation packages
#                      "mgcv", "splines", "lme4", # stats packages
#                      "egg", "cowplot", "corrplot", "pals", "colorspace", "ggsci", "scico", "viridis") 
# # check if list of packages is installed. If not, it will install ones not yet installed
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) invisible(install.packages(new.packages,repos = "https://cloud.r-project.org"))

# load packages
# invisible(lapply(list.of.packages, require, character.only = TRUE, quietly=TRUE))


#### ----------------------------- ####
####  2. SET PROJECT-WIDE OBJECTS  ####
#### ----------------------------- ####

# we create certain objects here to stay in the environment so we can call them 
# for various scripts

# 2a. set the projection string 
# US National Atlas Equal Area
# Robbie: can you explain why you used these briefly here?

# Sebastian: Added description. 
# Also updated the CRS (still called Atlas Equal Area)
# as 2163 appears to be depreciated
# also scrapped all references to plotCRS since we cshould use projCRS for everthing 
# for simplicity

# The projCRS, US National Atlas Equal Area, is used for mapping BNE results for CONUS.
# I chose this CRS just based on how the CONUS plots looked. 
# all spatial operations take place using degrees, so CRS choice only impacts visual results
projCRS<- "epsg:9311"
projCRS.ras <- paste0('+init=', projCRS)

#### --------------------- ####
####  3. SOURCE FUNCTIONS  ####
#### --------------------- ####

# 3.a. get the names of all of the scripts that are just functions
# we want both the stable and unstable functions. 
myStableFunctions <- paste0(config$preproc_path, "functions/")
sapply(paste0(myStableFunctions,list.files(path = myStableFunctions)), source)



        
# 3.d. clean up environment    
rm(myStableFunctions)


