
# Martin R. Vasilev, 2017

rm(list=ls())

source("functions/soundCheck.R")

sound_check<- soundCheck()

sound_check<- subset(sound_check, sound!=1)

source("functions/re_map.R")
sound_check<- re_map(sound_check)

sound_check$word<- NA
for(i in 1:nrow(sound_check)){
  if(sound_check$sound[i]==2){
    sound_check$word[i]<- 5
  }
  if(sound_check$sound[i]==3){
    sound_check$word[i]<- 7
  }
  if(sound_check$sound[i]==4){
    sound_check$word[i]<- 9
  }
  if(sound_check$sound[i]==5){
    sound_check$word[i]<- 11
  }
}


# further development:
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
infix<- which(sound_check$delFix>14)
infixn<- length(infix)
sound_check<- sound_check[-infix,]

cat(sprintf("%f percent of data excluded due to blinks", (nblinks/nobs)*100))
cat(sprintf("%f percent of data excluded due to in-fixations", (infixn/nobs)*100))
cat(sprintf("%f percent of data remains for analysis", (nrow(sound_check)/nobs)*100))

#sound_check<- subset(sound_check, delFix<80)


###############################
#   Pre-process fixations:    #
###############################

source("functions/paraFix.R")
source("functions/assign_cond.R")

raw_fix<- paraFix(plot=F, align = F)
raw_fix<- subset(raw_fix, outsideText==0)

#raw_fix<- assign_cond(sound_check, raw_fix)

raw_fix<- subset(raw_fix, blink==0)
#110

# Merge any fixations <80ms within a character:
source("functions/less80.R")
raw_fix<- less80(raw_fix)
l80<- which(raw_fix$fix_dur<80)
raw_fix<- raw_fix[-l80,]
save(raw_fix, file= "data/raw_fix.Rda")

#source('functions/map_by_pos.R')
#MF<- map_by_pos(raw_fix)

raw_fix$sound<- NA
source("functions/reading_times.R")
FD<- reading_measures(raw_fix)
#FD<- reading_measures(MF)

out<- which(FD$FFD>800 | FD$GD>2000 | FD$TVT>3000)

if(length(out)>0){
  FD<- FD[-out,]
}

FD$keep<- 0
FD$keepN1<- 0

for(i in 1:nrow(FD)){
  a<- which(sound_check$sub== FD$sub[i] & sound_check$item== FD$item[i] & sound_check$word== FD$word[i])
  
  if(length(a)>0){
    FD$keep[i]<- 1
    FD$sound[i]<- sound_check$sound_type[a]
    
    b<- which(FD$item== FD$item[i] & FD$sub== FD$sub[i] & FD$word== FD$word[i]+1)
    if(length(b>0)){
      FD$keepN1[b]<- 1
      FD$sound[b]<- sound_check$sound_type[a]
    }
    
  }
}

TW<- subset(FD, keep==1)
N1<- subset(FD, keepN1==1)

FD<- TW
save(FD, file='data/FD.Rda')
save(N1, file='data/N1.Rda')


#FD$keep<- 0

#for(i in 1:nrow(FD)){
#  a<- which(sound_check$sub== FD$sub[i] & sound_check$item== FD$item[i] & sound_check$word== FD$word[i])
  
#  if(length(a)>0){
##    FD$keep[i]<- 1
#    FD$sound[i]<- sound_check$sound_type[a]
#  }
#}


#source("functions/nFixRG.R")
#FixRG<- nFixRG(raw_fix)
#FixRG<- nFixRG(MF)
#save(FixRG, file='data/FixRG.Rda')



#tw<- subset(raw_fix, is.element(word, c(3,5,7,9,11)))
#tw<- subset(FD, is.element(word, c(3,5,7,9,11)))

# Do fixations on non-target words as a function of experimental block



DesFix<- melt(sound_check, id=c('sub', 'item', 'cond', 'sound_type'), 
                           measure=c("N1", "N2"), na.rm=TRUE)
mFix<- cast(DesFix, sound_type ~ variable
                        ,function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))



DesFix<- melt(TW, id=c('sub', 'item', 'cond', 'sound'), 
              measure=c("FFD", "GD", "TVT"), na.rm=TRUE)
mTW<- cast(DesFix, sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))


DesLen<- melt(sound_check, id=c('sub', 'item', 'cond', 'sound_type'), 
              measure=c("N1len", "N2len"), na.rm=TRUE)
mLen<- cast(DesLen, sound_type ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

DesReg<- melt(sound_check, id=c('sub', 'item', 'cond', 'sound_type'), 
              measure=c("N1reg", "N2reg"), na.rm=TRUE)
mReg<- cast(DesReg, sound_type ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

DesN1<- melt(N1, id=c('sub', 'item', 'cond', 'sound'), 
              measure=c("FFD", "GD", "TVT"), na.rm=TRUE)
mN1<- cast(DesN1, sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))
