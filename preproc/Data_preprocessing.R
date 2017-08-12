
# Martin R. Vasilev, 2017

#rm(list=ls())

source("functions/soundCheck.R")

sound_check<- soundCheck()

# further development:
# was boundary crossed in a saccade?
# how many ms before next fix started?


save(sound_check, file= "preproc/sound_check.Rda")
write.csv(sound_check, "preproc/sound_check.csv")


##########################
#      filter data:
##########################

nobs<- nrow(sound_check)

# remove blinks on critical words:
blinks<- which(sound_check$blink=='Yes')
nblinks<- length(blinks)
sound_check<- sound_check[-blinks,]


# remove sounds played after fixation has started:
infix<- which(sound_check$nextFlag=="EFIX" & sound_check$delFix>5)
infixn<- length(infix)
sound_check<- sound_check[-infix,]

cat(sprintf("%d percent of data excluded due to blinks", (nblinks/nobs)*100))
cat(sprintf("%d percent of data excluded due to in-fixations", (infixn/nobs)*100))
cat(sprintf("%d percent of data remains for analysis", (nrow(sound_check)/nobs)*100))

sound_check<- subset(sound_check, delFix<80)


#######
mean(sound_check$delFix)

hist(sound_check$delFix, xlab= "Time until SFIX flag (in ms)", main= "Sound implementation timing")
abline(v = mean(sound_check$delFix), col= "darkred", lwd=3)
