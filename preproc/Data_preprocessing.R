
# Martin R. Vasilev, 2017

rm(list=ls())

source("functions/soundCheck.R")

sound_check<- soundCheck()

# further development:
# was boundary crossed in a saccade?
# how many ms before next fix started?


save(sound_check, file= "preproc/sound_check.Rda")
write.csv(sound_check, "preproc/sound_check.csv")
