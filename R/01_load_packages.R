
# Loading packages --------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("rvest", "tidyverse", "terra", "dplyr", "ggplot2", "sf", "graphics",
       "png", "grid",  "rnaturalearth", "rnaturalearthdata", 
       "rnaturalearthhires", "geobr", "gridGraphics", "gridExtra", "tmap", "ggspatial"))

rm(ipak)

if (!"devtools"%in%installed.packages()){install.packages("devtools")}  
devtools::install_github("andrefaa/ENMTML") 

library(ENMTML)