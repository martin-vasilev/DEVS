
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

png('Plots/FFD_CDF.png', width = 8000, height = 4000, units = "px", res=600, type="cairo")
plot(ecdf(dbFD$FFD[dbFD$sound=="STD"]), cex=0.7, main= "", xlab= "First fixation duration [FFD]",
     ylab= "Cumulative distribution function (FFD)", family="serif", cex.lab=1.6, cex.axis= 1.6,
     font.lab=2, xlim= c(0, 800))
plot(ecdf(dbFD$FFD[dbFD$sound=="DEV"]), add=T, col= "darkred", cex=0.7)

legend(665, 0.25, legend=c("Standard", "Deviant"),
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

DesGen<- melt(GenFix, id=c('sub', 'item', 'cond'), 
              measure=c("nfix1", "nfix2", "nfixAll"), na.rm=TRUE)
mGen<- cast(DesGen, cond ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

# saccade length:

raw_fix$sacc_len<- NULL

for (i in 1:nrow(raw_fix)){
  if(i>1){
    if(raw_fix$item[i]==raw_fix$item[i-1]){
      raw_fix$sacc_len[i]<- abs((raw_fix$xPos[i]-raw_fix$xPos[i-1])/14)
    }
  }else{
    raw_fix$sacc_len[i]<-NA 
  }
}

DesSacc<- melt(raw_fix, id=c('sub', 'item', 'cond'), 
              measure=c("sacc_len"), na.rm=TRUE)
mSacc<- cast(DesSacc, cond ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))
