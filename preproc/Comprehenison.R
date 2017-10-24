library(readr)
q <- read_delim("preproc/raw_data/q.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
q<- subset(q, item<121)

library(reshape)

DesComp<- melt(q, id=c('subject', 'item', 'questcond'), 
             measure=c("accuracy"), na.rm=TRUE)
mQ<- cast(DesComp, subject ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

mQ2<- cast(DesComp, questcond ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

save(q, file= "data/q.Rda")