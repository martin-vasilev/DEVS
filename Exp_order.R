
rm(list= ls())

load('data/FD.Rda')

data= subset(FD, cond==3)
a<- NULL; b<- NULL; c<- NULL
data$order= NA
DEV= NULL

library(readr)

for(i in 1:48){
  a<- subset(data, sub==i)  
  b <- read_delim(paste("design/P", toString(i), ".txt", sep= ""), 
                        " ", escape_double = FALSE, trim_ws = TRUE)
  b<- subset(b, sound=="DEV")
  b$seq<- 1:nrow(b)
  
  for(j in 1:nrow(a)){
    c<- which(b$item== a$item[j])
    a$order[j]<- b$seq[c]
  }
  
  DEV<- rbind(DEV, a)
}

######################################################################################
#rm(list= ls())

load('data/FD.Rda')

data= subset(FD, cond==2)
a<- NULL; b<- NULL; c<- NULL
data$order= NA
STD= NULL

library(readr)

for(i in 1:48){
  a<- subset(data, sub==i)  
  b <- read_delim(paste("design/P", toString(i), ".txt", sep= ""), 
                  " ", escape_double = FALSE, trim_ws = TRUE)
  b<- subset(b, sound=="STD")
  b$seq<- 1:nrow(b)
  
  for(j in 1:nrow(a)){
    c<- which(b$item== a$item[j])
    a$order[j]<- b$seq[c]
  }
  
  STD<- rbind(STD, a)
}


rm(data)

data<- rbind(DEV, STD)

####

DEV$bin<- NA

for(i in 1:nrow(DEV)){
  if(is.element(DEV$order[i], c(1,2,3,4,5))){
    DEV$bin[i]<- 1
  }
  
  if(is.element(DEV$order[i], c(6,7,8,9,10))){
    DEV$bin[i]<- 2
  }
  
  if(is.element(DEV$order[i], c(11,12,13,14,15))){
    DEV$bin[i]<- 3
  }
  
  if(is.element(DEV$order[i], c(16,17,18,19,20))){
    DEV$bin[i]<- 4
  }
  
  if(is.element(DEV$order[i], c(21,22,23,24,25))){
    DEV$bin[i]<- 5
  }
  
  if(is.element(DEV$order[i], c(26,27,28,29,30))){
    DEV$bin[i]<- 6
  }
  
  if(is.element(DEV$order[i], c(31,32,33,34,35))){
    DEV$bin[i]<- 7
  }
  
  if(is.element(DEV$order[i], c(36,37,38,39,40))){
    DEV$bin[i]<- 8
  }
  
}



STD$bin<- NA

for(i in 1:nrow(STD)){
  if(is.element(STD$order[i], c(1,2,3,4,5))){
    STD$bin[i]<- 1
  }
  
  if(is.element(STD$order[i], c(6,7,8,9,10))){
    STD$bin[i]<- 2
  }
  
  if(is.element(STD$order[i], c(11,12,13,14,15))){
    STD$bin[i]<- 3
  }
  
  if(is.element(STD$order[i], c(16,17,18,19,20))){
    STD$bin[i]<- 4
  }
  
  if(is.element(STD$order[i], c(21,22,23,24,25))){
    STD$bin[i]<- 5
  }
  
  if(is.element(STD$order[i], c(26,27,28,29,30))){
    STD$bin[i]<- 6
  }
  
  if(is.element(STD$order[i], c(31,32,33,34,35))){
    STD$bin[i]<- 7
  }
  
  if(is.element(STD$order[i], c(36,37,38,39,40))){
    STD$bin[i]<- 8
  }
  
}


library(reshape)
DesDEV<- melt(DEV, id=c('sub', 'item', 'word', 'order', 'bin'), 
               measure=c("GD"), na.rm=TRUE)
mDEV<- cast(DesDEV, bin ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

DesSTD<- melt(STD, id=c('sub', 'item', 'word', 'order', 'bin'), 
              measure=c("GD"), na.rm=TRUE)
mSTD<- cast(DesSTD, bin ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

m<- data.frame(mDEV[,1])
m$diff<- mDEV$GD_M- mSTD$GD_M
m$SD<- mDEV$GD_SD- mSTD$GD_SD





