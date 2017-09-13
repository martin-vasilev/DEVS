
rm(list= ls())

source("functions/paraFix.R")
source("functions/assign_cond.R")

raw_fix<- paraFix(plot = F, data_list ="C:/Users/mvasilev/Documents/files.txt", ResX = 1024, ResY = 768, maxtrial = 28)
