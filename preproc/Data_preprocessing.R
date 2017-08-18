
# Martin R. Vasilev, 2017

#rm(list=ls())

source("functions/soundCheck.R")

sound_check<- soundCheck()

# further development:
# was boundary crossed in a saccade?
# how many ms before next fix started?
# CHANGE DEFINITION OF HOOK!: crossing is not a hook if next fix is within 1 ppl 


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

cat(sprintf("%f percent of data excluded due to blinks", (nblinks/nobs)*100))
cat(sprintf("%f percent of data excluded due to in-fixations", (infixn/nobs)*100))
cat(sprintf("%f percent of data remains for analysis", (nrow(sound_check)/nobs)*100))

sound_check<- subset(sound_check, delFix<80)


###############################
#   Pre-process fixations:    #
###############################

source("functions/paraFix.R")
source("functions/assign_cond.R")

raw_fix<- paraFix(plot=T)

raw_fix<- assign_cond(sound_check, raw_fix)

#110

save(raw_fix, file= "data/raw_fix.Rda")



