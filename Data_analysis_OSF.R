# Martin R. Vasilev, 2018

# SLC: silence (cond 1)
# STD: standard sound (cond 2)
# DEV: deviant sound (cond 3)

rm(list=ls())



load("data/sound_check.Rda")

### sound timing:
m<- subset(sound_check, onTarget=="Yes")

mean(m$delFix)
sd(m$delFix)
###### Sound timing plots:

png('Plots/sound_timing.png', width = 6000, height = 2500, units = "px", res=600, type="cairo")

layout(mat = matrix(c(1,2),nrow = 1,ncol = 2,byrow = TRUE))
par(mar=c(4,4,4,1))


## panel a:

hist(sound_check$delFix, xlab= "Sound onset delay (in ms)",
     main= "a", cex.axis=1.1,cex.lab=1.1, cex.main=1.6, font.lab=2, 
     family= "serif", col="lightgreen", freq=FALSE, breaks=30, xlim= c(-50, 20)) # , xlim= c(-20, 20)
m<- subset(sound_check, delFix>-40)
abline(v = mean(m$delFix, na.rm=T), col= "darkred", lwd=3)
curve(dnorm(x, mean=mean(sound_check$delFix, na.rm=T), 
            sd=sd(sound_check$delFix, na.rm=T)), add=TRUE, col="darkblue", lwd=2) 

## panel b:

s2<- subset(sound_check, sound_type!="SLC")
hist(s2$ISI-50, xlab= "ISI between two consecutive sounds (in ms)", cex.axis=1.1,
     cex.lab=1.1,cex.main=1.6,font.lab=2, 
     main= "b", family= "serif", col="lightgreen", freq=FALSE, breaks=30, xlim= c(0, 3000))
abline(v = mean(s2$ISI-50, na.rm=T), col= "darkred", lwd=3)
curve(dnorm(x, mean=mean(s2$ISI-50, na.rm=T), sd=sd(s2$ISI-50, na.rm=T)), add=TRUE, col="darkblue", lwd=2)

dev.off()

round((length(which(sound_check$delFix< -50))/ nrow(sound_check))*100, 1)

round((length(which(s2$ISI> 3000))/ nrow(sound_check))*100, 1)

####
range(sound_check$delFix)



############# Fixations on target word: plot

load("data/FD.Rda")

library(reshape)
#FD<- subset(FD,!is.na(sound))
FD$sound<- as.factor(FD$sound)
#tw<- subset(FD, is.element(word, c(3,5,7,9,11)))

DesFix<- melt(FD, id=c('sub', 'item', 'cond', 'sound'), 
              measure=c("FFD", "SFD", "GD", "TVT"), na.rm=TRUE)
mFix<- cast(DesFix, sound ~ variable
            , function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))


library(ggplot2)

db<- data.frame(c(mFix$FFD_M, mFix$SFD_M, mFix$GD_M, mFix$TVT_M), c(mFix$FFD_SD, mFix$SFD_SD, mFix$GD_SD, mFix$TVT_SD),
                c(rep("FFD",3), rep("SFD",3),  rep("GD",3), rep("TVT",3)), 
                rep(c("Deviant", "Silence", "Standard"),4))

colnames(db)<- c("Mean", "SD", "Measure", "Sound")
db$SE<- db$SD/sqrt(length(unique(FD$sub)))

db$Sound<- factor(db$Sound, levels= c("Silence", "Standard", "Deviant"))

limits <- aes(ymax = db$Mean + db$SE, ymin=db$Mean - db$SE)

Dplot<- ggplot(data= db, aes(x=Sound, y= Mean, color=Measure, fill= Measure, group= Measure, shape= Measure, linetype= Measure))+ 
  scale_fill_brewer(palette="Dark2")+ 
  scale_colour_brewer(palette="Dark2")+
  theme_bw() + theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
                     axis.line = element_line(colour = "black", size=1),
                     panel.border = element_rect(colour = "black", size=1, fill = NA))+
  geom_line(size=2)+
  geom_point(size=7)+ 
  xlab("\n Background sound")+ ylab("Mean fixation duration (in ms)")+ 
  theme(legend.position= "bottom", legend.title=element_text(size=20,
                                                             face="bold", family="serif"),
        legend.text=element_text(size=20,family="serif"),legend.key.width=unit(2,"cm"),
        legend.key.height=unit(1,"cm"), strip.text=element_text(size=20, family="serif"),
        title=element_text(size=20, family="serif"),
        axis.title.x = element_text(size=20, face="bold", family="serif"),
        axis.title.y = element_text(size=20, face="bold", family="serif"), 
        axis.text=element_text(size=20, family="serif"), 
        panel.border = element_rect(linetype = "solid", colour = "black"), 
        legend.key = element_rect(colour = "#000000", size=1)) +geom_ribbon(limits, alpha=0.07, colour=NA)

ggsave(Dplot, filename = "Plots/TW.png", width = 7.2, height=7, dpi = 300, units = "in")



##### Survival plots:
dbFD<- subset(FD, sound!="SLC")

a<- ecdf(dbFD$FFD[dbFD$sound=="STD"])
d<- sort(unique(dbFD$FFD[dbFD$sound=="STD"]))
dp<- NULL

b<- ecdf(dbFD$FFD[dbFD$sound=="DEV"])
d2<- sort(unique(dbFD$FFD[dbFD$sound=="DEV"]))
dp2<- NULL

for(i in 1:length(d)){
  dp[i]<- (1- a(d[i]))*100 
}

for(i in 1:length(d2)){
  dp2[i]<- (1- b(d2[i]))*100
}

png('Plots/FFD_SRV.png', width = 6000, height = 5000, units = "px", res=600, type="cairo")
plot(d, dp, main= "", type= "l", lwd= 1.2,
     xlab= "First fixation duration [FFD]",
     ylab= "Survival (%)", family="serif",
     cex.lab=1.6, cex.axis= 1.6, pch=16,
     font.lab=2, xlim= c(100, 800))
abline(h = 0, lty=2, col="#706E6E")
abline(h = 100, lty=2, col="#706E6E")
#par(new=F)
points(d, dp, pch= 16, cex= 0.5)

lines(d2, dp2, col= "darkred")
points(d2, dp2, pch= 16, col= "darkred", cex= 0.5)

abline(v= 180, col="dark orange", lwd= 2)

legend(600, 85, legend=c("Standard", "Deviant"),
       col=c("black", "darkred"), lwd=2.5, cex=1.4)

dev.off()



### Comprehension:
load('data/q.Rda')

DesComp<- melt(q, id=c('subject', 'item', 'questcond'), 
               measure=c("accuracy"), na.rm=TRUE)
mQ<- cast(DesComp, subject ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

mQ2<- cast(DesComp, questcond ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

round(mean(q$accuracy),2)
round(sd(q$accuracy),2)
mQ2<- round(mQ2, 2)

q$sentcond<- as.factor(q$sentcond)
q$sentcond<- factor(q$sentcond, levels= c("2", "3", "1"))
#levels(q$sentcond)
contrasts(q$sentcond)

summary(glmer(accuracy ~ sentcond + (1|subject)+ (1|item),  family= binomial, data= q))



##########
##### Target word LMMs:

FD$sound<- as.factor(FD$sound)
FD$sound<- factor(FD$sound, levels= c("STD", "DEV", "SLC"))
contrasts(FD$sound)

library(lme4)

summary(mSFD<-lmer(log(SFD) ~ sound +  (sound|sub)+ (1|item), data=FD, REML=T))
summary(mFFD<-lmer(log(FFD) ~ sound +  (sound|sub)+ (1|item) , data=FD, REML=T))
summary(mGD<-lmer(log(GD) ~ sound+ (sound|sub)+ (sound|item), data=FD, REML=T))
summary(mTVT<-lmer(log(TVT) ~ sound +  (sound|sub)+ (sound|item), data=FD, REML=T))


###### lexical frequency modulation analyses:
library(readr)
freq <- read_delim("freq.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
freq$len<- nchar(freq$Critical_word)
FD$Zipf<- NULL
FD$freq<- NULL
FD$length<- NULL

for(i in 1:nrow(FD)){
  a<- which(freq$item== FD$item[i] & freq$word== FD$word[i])
  FD$Zipf[i]<- freq$Zipf[a]
  FD$freq[i]<- freq$Freq_mil[a]
  FD$length[i]<- freq$len[a]
}

FD$freq<- log(FD$freq) # log-transform frequency
FD$freq<- scale(FD$freq) # centre it to avoid multicollinearity issues

####

FD$word<- as.factor(FD$word)

summary(freqFFD<-lmer(log(FFD) ~  sound*freq+
                        (sound|sub) +(1|item),data=FD, REML=T))

summary(freqSFD<-lmer(log(SFD) ~  sound*freq+
                        (sound|sub) +(1|item),data=FD, REML=T))

summary(freqGD<-lmer(log(GD) ~  sound*freq+
                       (sound|sub) +(sound|item),data=FD, REML=T))

summary(freqTVT<-lmer(log(TVT) ~  sound*freq+
                        (sound|sub) +(1|item),data=FD, REML=T))
# TVT does not converge with a random slope for items


######### Next saccade after playing a sound:
DesReg<- melt(sound_check, id=c('sub', 'item', 'cond', 'sound_type'), 
              measure=c("N1reg", "N2reg"), na.rm=TRUE)
mReg<- cast(DesReg, sound_type ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

sound_check$sound_type<- as.factor(sound_check$sound_type)
sound_check$sound_type<- factor(sound_check$sound_type, levels= c("STD", "DEV", "SLC"))
contrasts(sound_check$sound_type)

summary(glmer(N1reg ~ sound_type + (sound_type|sub)+ (1|item), data=sound_check, family= binomial))



##### Global reading

load("data/raw_fix.Rda")

# Mean sentence reading time:
DesSRT<- melt(raw_fix, id=c('sub', 'item', 'cond'), 
              measure=c("fix_dur"), na.rm=TRUE)
SRT<- cast(DesSRT, cond+sub+item ~ variable
            ,function(x) c(M=signif(sum(x),3)))

DesSRT2<- melt(SRT, id=c('sub', 'item', 'cond'), 
              measure=c("fix_dur"), na.rm=TRUE)
mSRT<- cast(DesSRT2, cond ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

# fixation duration:
DesFix<- melt(raw_fix, id=c('sub', 'item', 'cond'), 
              measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, cond ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

### Total Number of fixations:
source("functions/nFix.R")
GenFix<- nFix(raw_fix)

GenFix<- subset(GenFix, nfixAll<60)

DesGen<- melt(GenFix, id=c('sub', 'item', 'cond'), 
              measure=c("nfix1", "nfix2", "nfixAll"), na.rm=TRUE)
mGen<- cast(DesGen, cond ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

# saccade length:

raw_fix$sacc_len<- NA

for (i in 1:nrow(raw_fix)){
  if(i>1){
    if(raw_fix$item[i]==raw_fix$item[i-1]){
      raw_fix$sacc_len[i]<- abs((raw_fix$xPos[i]-raw_fix$xPos[i-1])/14)
    } else{
    raw_fix$sacc_len[i]<-NA 
   } 
  }
}

DesSacc<- melt(raw_fix, id=c('sub', 'item', 'cond'), 
              measure=c("sacc_len"), na.rm=TRUE)
mSacc<- cast(DesSacc, cond ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))



# LMMs: Global reading:
raw_fix$sound<- NULL

for(i in 1:nrow(raw_fix)){
  if(raw_fix$cond[i]=="1"){
    raw_fix$sound[i]<- "SLC"
  }
  if(raw_fix$cond[i]=="2"){
    raw_fix$sound[i]<- "STD"
  }
  if(raw_fix$cond[i]=="3"){
    raw_fix$sound[i]<- "DEV"
  }
}

library(lme4)
raw_fix$sound<- as.factor(raw_fix$sound)
raw_fix$sound<- factor(raw_fix$sound, levels= c("STD", "DEV", "SLC"))
contrasts(raw_fix$sound)

# fixation duration:
# does not converge with slope for items
summary(mFD<-lmer(log(fix_dur) ~ sound +  (sound|sub)+ (1|item), data=raw_fix, REML=T))

# saccade length:
# does not converge with random slopes
summary(mSL<-lmer(log(sacc_len) ~ sound +  (0+sound|sub)+ (1|item), data=raw_fix, REML=T))


# Sentence reading time:
SRT$sound<- NULL
for(i in 1:nrow(SRT)){
  if(SRT$cond[i]=="1"){
    SRT$sound[i]<- "SLC"
  }
  if(SRT$cond[i]=="2"){
    SRT$sound[i]<- "STD"
  }
  if(SRT$cond[i]=="3"){
    SRT$sound[i]<- "DEV"
  }
}

SRT$sound<- as.factor(SRT$sound)
SRT$sound<- factor(SRT$sound, levels= c("STD", "DEV", "SLC"))
contrasts(SRT$sound)

summary(mRT<-lmer(log(fix_dur) ~ sound + (0+sound|sub)+ (1|item), data=SRT, REML=T))


# number of fixations:
GenFix$sound<- NULL
for(i in 1:nrow(GenFix)){
  if(GenFix$cond[i]=="1"){
    GenFix$sound[i]<- "SLC"
  }
  if(GenFix$cond[i]=="2"){
    GenFix$sound[i]<- "STD"
  }
  if(GenFix$cond[i]=="3"){
    GenFix$sound[i]<- "DEV"
  }
}

GenFix$sound<- as.factor(GenFix$sound)
GenFix$sound<- factor(GenFix$sound, levels= c("STD", "DEV", "SLC"))
contrasts(GenFix$sound)

summary(mGEN<-lmer(nfixAll ~  sound+ (0+sound|sub)+ (1|item),
                      data=GenFix, REML=F))
