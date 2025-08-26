library(tidyverse)
library(eyelinker)
rm(list=ls())

#Setting up working dir
#getwd() #if want to verify working dir
currentdir=dirname(rstudioapi::getSourceEditorContext()$path)
pathinput=file.path(currentdir, "data/") #This needs to be set to go inside the output folder of the data viewer project
setwd(pathinput)
pathinput #Check that indeed correct!

#Read in data
fpath = paste(pathinput, "SPG_S01.asc", sep="")
dat <- read.asc(fpath)
str(dat, max.level = 1)

#Get recording info
dplyr::select(dat$info, c(model, mount, sample.rate, cr))
dplyr::select(dat$info, c(left, right, mono, screen.x, screen.y))