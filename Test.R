
rm(list= ls())

source("functions/paraFix.R")

raw_fix<- paraFix(plot = T, data_list ="C:/Users/mvasilev/Documents/files.txt", ResX= 1920, ResY=1080, maxtrial = 51, align=FALSE)
