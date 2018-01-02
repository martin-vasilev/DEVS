
rm(list=ls())

load("data/sound_check.Rda")


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
  #scale_fill_manual(values=c(ColorBlind[6:7]))+
  #scale_colour_manual(values=c(ColorBlind[6:7]))+
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



##### Density plots:
dbFD<- subset(FD, sound!="SLC")
#levels(dbFD$Sound)<- droplevels(dbFD$Sound)

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

png('Plots/FFD_SRV.png', width = 8000, height = 4000, units = "px", res=600, type="cairo")
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

abline(v= 180, col="dark orange", cex= 1.2)

legend(640, 85, legend=c("Standard", "Deviant"),
       col=c("black", "darkred"), lwd=1.7, cex=1.4)

dev.off()
##########

png('Plots/FFD_CDF.png', width = 8000, height = 4000, units = "px", res=600, type="cairo")
plot(ecdf(dbFD$FFD[dbFD$sound=="STD"]), cex=0.7, main= "", xlab= "First fixation duration [FFD]",
     ylab= "Cumulative distribution function (FFD)", family="serif", cex.lab=1.6, cex.axis= 1.6,
     font.lab=2, xlim= c(0, 800))
plot(ecdf(dbFD$FFD[dbFD$sound=="DEV"]), add=T, col= "darkred", cex=0.7)

legend(665, 0.25, legend=c("Standard", "Deviant"),
       col=c("black", "darkred"), lwd=4.5, cex=1.4)

dev.off()


#### Survival plots:
std<- ecdf(dbFD$FFD[dbFD$sound=="STD"])
dev<-  ecdf(dbFD$FFD[dbFD$sound=="DEV"]) 

S_std<-NULL
S_dev<- NULL
rng<- 80:800


for(i in 1:length(rng)){
  S_std[i]<- 1- std(rng[i])
  S_dev[i]<- 1- dev(rng[i])
}

png('Plots/Survival.png', width = 6000, height = 3000, units = "px", res=600, type="cairo")
plot(rng, S_std, main= "", type= "l", lwd= 1.2,
     xlab= "First fixation duration [FFD]",
     ylab= "Survival (%)", family="serif",
     cex.lab=1.6, cex.axis= 1.6, pch=16,
     font.lab=2)
par(new=TRUE)
plot(rng, S_dev, col= "darkred", lwd=1.2, axes= FALSE, xlab= '', ylab= '', type= 'l')
abline(v= 180, col= "darkorange")
# 219 original DPA
legend(640, 0.95, legend=c("Standard", "Deviant"),
       col=c("black", "darkred"), lwd=4.5, cex=1.4)

dev.off()



E1<- ggplot(dbFD, aes(x=FFD,group=sound, color=sound, fill= sound))+geom_density(alpha=.15, size=1)+
  theme_bw() + theme(panel.grid.major = element_line(colour = "#BDBDBD", size=0.7),
                     axis.line = element_line(colour = "black", size=1), panel.border = element_rect(colour = "black",
                     size=1.5, fill = NA))+ ggtitle("Experiment 1")+ ylab("Probability density")+
  xlab("First fixation duration (in ms)")+
  #scale_x_continuous(breaks = pretty_breaks(n = 5))+
  # scale_x_continuous(breaks = c(0,100,200,300,400,500,600,700,800,900))+
  theme(legend.position= "bottom",legend.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.key.width=unit(2,"cm"), legend.key.height=unit(1,"cm"), strip.text=element_text(size=20), 
        title=element_text(size=20), axis.title.x = element_text(size=20, face="bold"), 
        axis.title.y = element_text(size=20, face="bold"), plot.title=element_text(hjust=0.5),
        axis.text=element_text(size=20), panel.border = element_rect(linetype = "solid", colour = "black"))#+
  #facet_grid(.~ Measure, scales = "free") + theme(strip.text.x = element_text(size = 16,
  #face="bold"), strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
  #legend.key = element_rect(colour = "#000000", size=1))



##### TW LMMS:
FD$sound<- as.factor(FD$sound)
FD$sound<- factor(FD$sound, levels= c("STD", "DEV", "SLC"))
contrasts(FD$sound)

library(lme4)

summary(mSFD<-lmer(log(SFD) ~ sound +  (sound|sub)+ (1|item), data=FD, REML=T))
summary(mFFD<-lmer(log(FFD) ~ sound +  (sound||sub)+ (1|item) , data=FD, REML=T))
summary(mGD<-lmer(log(GD) ~ sound+ (sound|sub)+ (sound|item), data=FD, REML=T))
summary(mTVT<-lmer(log(TVT) ~ sound +  (sound|sub)+ (sound|item), data=FD, REML=T))

###################

FD2<- FD

FD2$GD2<- FD2$GD- FD2$FFD +0.01
FD2$TVT2<- FD2$TVT- FD2$GD+0.01

summary(mGD2<-lmer(log(GD2) ~ sound+ (sound|sub)+ (sound|item), data=FD2, REML=T))
summary(mTVT2<-lmer(log(TVT2) ~ sound+ (sound|sub)+ (sound|item), data=FD2, REML=T))

# Fixations counting towards GD and TVT are not statistically significant on their own
# 



########
# Skipping probability on the next word:

sub<- NULL
item<- NULL
skip<- NULL
cond<- NULL
sound<- NULL
word<- NULL

for (i in 1:nrow(sound_check)){
  a<- subset(raw_fix, sub== sound_check$sub[i] & item== sound_check$item[i] & word== sound_check$word[i]+1)
  n<- subset(a, intrasent_regr==0)
  
  if(nrow(n)>0){
    skip[i]<- 0
  } else{
    skip[i]<- 1
  }
  
  sub[i]<- sound_check$sub[i]
  item[i]<- sound_check$item[i]
  cond[i]<- sound_check$cond[i]
  sound[i]<- sound_check$sound_type[i]
  word[i]<- sound_check$word[i]+1
}

FX<- data.frame(sub, item, cond, sound, word, skip)

FX$pos<- NULL
for(i in 1:nrow(FX)){
  FX$pos[i]
}


save(FX, file= "data/FX.Rda")


DesSkip<- melt(FX, id=c('sub', 'item', 'cond', 'sound', 'word'), 
              measure=c("skip"), na.rm=TRUE)
mSkip<- cast(DesSkip, sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

FX$sound<- as.factor(FX$sound)
FX$sound<- factor(FX$sound, levels= c("STD", "DEV", "SLC"))
contrasts(FX$sound)

summary(glmer(skip ~ sound + (1|sub)+ (1|item), data=FX, family= binomial))


######### Next saccade:
DesReg<- melt(sound_check, id=c('sub', 'item', 'cond', 'sound_type'), 
              measure=c("N1reg", "N2reg"), na.rm=TRUE)
mReg<- cast(DesReg, sound_type ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

sound_check$sound_type<- as.factor(sound_check$sound_type)
sound_check$sound_type<- factor(sound_check$sound_type, levels= c("STD", "DEV", "SLC"))
contrasts(sound_check$sound_type)

summary(glmer(N1reg ~ sound_type + (sound_type|sub)+ (1|item), data=sound_check, family= binomial))




### Where do they land after regression?

R<- subset(sound_check, N1reg==1)
R$sacc_len<-  (R$nextFix- R$N1x)/14
R$prevWords<- NULL

for(i in 1:nrow(R)){
  if(!is.na(R$N1x[i])){
    if(R$N1x[i]< R$regionS[i]){
      R$prevWords[i]<- 1
    } else{
      R$prevWords[i]<- 0
    }
  }

}

DesLen<- melt(R, id=c('sub', 'item', 'cond', 'sound_type'), 
              measure=c("sacc_len"), na.rm=TRUE)
mLen<- cast(DesLen, sound_type ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))
R$sound_type<- as.factor(R$sound_type)
R$sound_type<- factor(R$sound_type, levels= c("STD", "DEV", "SLC"))
contrasts(R$sound_type)

summary(lmer(sacc_len ~ sound_type + (1|sub)+ (1|item), data=R, REML=T))

###### Next fixation:

summary(lmer(log(N2) ~ sound_type + (1|sub)+ (1|item),
             data=sound_check, REML=T))

############### length:

DesLen<- melt(sound_check, id=c('sub', 'item', 'cond', 'sound_type'), 
              measure=c("N1len", "N2len"), na.rm=TRUE)
mLen<- cast(DesLen, sound_type ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))
summary(glmer(N1len ~ sound_type + (sound_type|sub)+ (1|item), data=sound_check, family= poisson, REML=T))
summary(lmer(N2len ~ sound_type + (sound_type|sub)+ (1|item), data=sound_check, REML=T))

##### Global reading

load("data/raw_fix.Rda")

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

# n fix:
### Total nFix:
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


###### lexical frequency:
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

FD$freq<- log(FD$freq)

#summary(freqFFD<-lmer(log(FFD) ~  freq+ sound+ freq:sound+ (freq+sound|sub)+ (freq|item),
#                      data=FD, REML=T))
#summary(freqSFD<-lmer(log(SFD) ~ freq+ freq:sound+ (freq+sound|sub)+ (freq|item),
#                      data=FD, REML=T))
#summary(freqGD<- lmer(log(GD) ~  freq+ freq:sound+ (freq+sound|sub)+ (freq|item),
#                      data=FD, REML=T))
#summary(freqTVT<-lmer(log(TVT) ~ freq+ freq:sound+ (freq|sub)+ (freq|item),
#                      data=FD, REML=T))


####

FD$word<- as.factor(FD$word)

summary(freqFFD<-lmer(log(FFD) ~  freq+ sound+ sound: freq+
                        (sound|sub) +(1|item),data=FD, REML=T))

summary(freqSFD<-lmer(log(SFD) ~  freq+ sound+ sound: freq+
                        (sound|sub) +(1|item),data=FD, REML=T))

summary(freqGD<-lmer(log(GD) ~  freq+ sound+ sound: freq+
                        (sound|sub) +(1|item),data=FD, REML=T))

summary(freqTVT<-lmer(log(TVT) ~  freq+ sound+ sound: freq+
                        (sound|sub) +(1|item),data=FD, REML=T))

library(effects)


plot(effect(c('freq:sound'),freqFFD), family='serif', main= "Lexical frequency x Sound [FFD]", 
     xlab= "log(Lexical frequency)", cex = 2.4)

plot(effect(c('freq:sound'),freqSFD), family='serif', main= "Lexical frequency x Sound [SFD]", 
     xlab= "log(Lexical frequency)")

plot(effect(c('freq:sound'),freqGD), family='serif', main= "Lexical frequency x Sound [SFD]", 
     xlab= "log(Lexical frequency)")

plot(effect(c('freq:sound'),freqTVT), family='serif', main= "Lexical frequency x Sound [SFD]", 
     xlab= "log(Lexical frequency)")




################

FixN$refix<- NULL

for(i in 1:nrow(FixN)){
  if(FixN$nfix1[i]>1){
    FixN$refix[i]<- 1
  }else{
    FixN$refix[i]<- 0
  }
}


DesRefix<- melt(FixN, id=c('sub', 'item', 'cond'), 
               measure=c("refix"), na.rm=TRUE)
mRefix<- cast(DesRefix, cond ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))


summary(glmer(refix ~ sound + (1|sub)+ (1|item), data=FixN, family= binomial))


### Percent of single fixation cases during first pass reading
(length(which(FD$FFD== FD$SFD))/nrow(subset(FD, !is.na(FFD))))*100
