
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
