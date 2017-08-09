
# Martin R. Vasilev, 2017

library(readr)

design<- NULL

for(i in 1:30){
  t <- read_delim(paste("design/P", toString(i), ".txt", sep=''), 
                       " ", escape_double = FALSE, trim_ws = TRUE)
  
  t$sub<- i
  design<- rbind(design, t)
}

table(design$item, design$sound)
table(design$item, design$pos)
table(design$sound, design$pos)


a<- data.frame(table(design$item, design$sound, design$pos))

range(a$Freq)

