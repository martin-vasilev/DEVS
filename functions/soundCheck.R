
# Martin R. Vasilev, 2017

soundCheck<- function(list_asc = "preproc/files.txt", maxtrial=120, nsounds=5, ppl=14, ResX=1920, soundLatency= 14){
  
  trial_info<- function(file, maxtrial, data){ # extracts information for processing trials
    ### get trial names:
    ID<- which(grepl('TRIALID', file));
    trial_text<- file[ID]
    trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
    #trials<- trials[which(letter!="P" &  letter!="F")] # remove practice items and questions
    trials<- gsub(" ", "", trials)
    # sometimes there is an extra empty space that can mess up detection of duplicates
    
    ### get condition:
    I<- unlist(gregexpr(pattern ='I',trials)) # start of item info
    cond<- as.numeric(substr(trials, 2, I-1)) # extract condition number
    
    ### get item:
    D<- unlist(gregexpr(pattern ='D',trials)) # start of dependent info
    item<- as.numeric(substr(trials, I+1, D-1)) # extract condition number
    depend<- as.numeric(substr(trials, nchar(trials), nchar(trials)))
    
    ### get sequence:
    #seq<- 1:length(trials)
    
    ### get start & end times
    start<- which(grepl('DISPLAY ON', file))
    end <- which(grepl('DISPLAY OFF', file))
    
    duplicated<- trials[duplicated(trials)]
    
    if(length(duplicated)>0){ # if there were aborted trials..
     # message(paste(" Diplicated trial", duplicated, "for file:", data, "\n"))
     # message("Analysing only last attempt at the trial!")
      
      toBeRemoved<- NULL
      uniqueDupl<- unique(duplicated)
      dup_rem<- NULL
      
      for(i in 1:length(uniqueDupl)){
        dup_rem_T<- which(trials==uniqueDupl[i])
        dup_rem<- c(dup_rem, dup_rem_T[1:length(dup_rem_T)-1])


      } # end of i
      
      start<- start[-dup_rem]
       #end<- end[-dup_rem]
      cond<- cond[-dup_rem]
      item<- item[-dup_rem]
      # seq<- seq[-toBeRemoved]
      depend<- depend[-dup_rem]
      ID<- ID[-dup_rem]
    } # end of aborted conditional
    
    trial_db<- data.frame(cond, item, depend, start, end, ID)
    trial_db<- subset(trial_db, depend==0 & item< maxtrial+1)
    trial_db$seq<- 1:nrow(trial_db)
    
    ###
    
    
    # trials<- trials[which(!is.element(trials, duplicated))]
    
    return(trial_db)
  }
  
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  
  get_x<- function(string, where=2){as.numeric(unlist(strsplit(string, "\t"))[1:2])[where]}
  
  # get type of sound that was played (STD, DEV)
  get_type<- function(string){substr(string, nchar(string)-2, nchar(string))} 

  # get location of critical region in pixels  
  get_regions<- function(string){
    string<- substr(string, unlist(gregexpr(pattern ='@', string)), nchar(string))
    string<- substr(string, 3, nchar(string))
    string<- unlist(strsplit(string, " "))
    string<- as.numeric(string)
    
    return(string)
  }
  
  get_boundary<- function(string){string<- substr(string,1, nchar(string)-2); return(get_num(string))}
  
  asc<- readLines(list_asc, warn= F)
  
  
  ##################
  #   processing   #
  ##################
  
  data<- NULL
  temp<- data.frame(sub=NA, item=NA, cond=NA, seq=NA, trialStart= NA, trialEnd= NA, sound= NA, sound_type=NA, regionS= NA, regionE=NA,
                    regionN1= NA,tBnd= NA,tSFIX=NA, ISI=NA, nextFlag= NA, delBnd=NA, delFix=NA, prevFix=NA, nextFix=NA, prevGood=NA, onTarget=NA,
                    inRegion=NA, hook= NA, blink=NA)
  
  
  for(i in 1:length(asc)){ # for each subject..
    
    cat("\n"); cat(sprintf("Loading data file: %s", asc[i]))
    dataF<- readLines(asc[i]) # load asc file;
    cat(". Done"); cat("\n")
    trial_db<- suppressMessages(trial_info(dataF, maxtrial, asc[i])) # get info about trials)
    
    ntrials<- nrow(trial_db)
    cat(sprintf("Processing trial: "));
    
    for(j in 1:ntrials){
      cat(toString(j)); cat(" ")
      db<- trial_db[j,]
      trialF<- dataF[db$start:db$end]
      
      # sound type:
      soundTypes<- which(grepl('PLAY SOUND', trialF))
      soundTypes<- trialF[soundTypes]
      soundTypes<- get_type(soundTypes)
     
      # critical word region
      regions<- which(grepl('CRITICAL', dataF[db$ID:db$start]))
      regions<- dataF[db$ID-1+regions]
      
      # boundary crossed stamp:
      bnds<- which(grepl('BOUNDARY CROSSED', trialF))
      bnds<- trialF[bnds]
      
      if(length(bnds)==0){
        next
      }
      
      for(k in 1:length(bnds)){
        # generic info about trial:
        temp$sub<- i
        temp$item<- trial_db$item[j]
        temp$cond<- trial_db$cond[j]
        temp$seq<- trial_db$seq[j]
        temp$trialStart<- get_num(trialF[1])
        temp$trialEnd<- get_num(trialF[length(trialF)])
        temp$sound<- k
        temp$sound_type<- soundTypes[k]
        if(temp$cond==1){
          temp$sound_type<- "SLC"
        }
        temp$regionS<- get_regions(regions[k])[1]
        temp$regionE<- get_regions(regions[k])[2]
        
        if(k!=nsounds){
          temp$regionN1<- get_regions(regions[k+1])[1]
        } else{
          temp$regionN1<- ResX
        }
        
        temp$tBnd<- get_boundary(bnds[k])
        
        # delay- triggering boundary:
        s<- which(grepl(toString(temp$tBnd), trialF))[1]
        allsac<- which(grepl('SSAC', trialF[1:s]))
        if(length(allsac)>0){
          allsac<- allsac[length(allsac)]  # critical saccade is always last one
          #get_num(trialF[allsac])
          crit_sacc<- trialF[allsac:s]
          crit_sacc<- crit_sacc[3:length(crit_sacc)-1]
          xpos<- NA
          
          for(l in 1:length(crit_sacc)){
            xpos[l]<- get_x(crit_sacc[l])
          }
          
          delStamp<- get_x(crit_sacc[which(xpos>= temp$regionS)[1]], 1)
          temp$delBnd<- temp$tBnd- delStamp
        } else{
          temp$delBnd<- NA
        }
        
        
        ####
        # previous fixation:
        allfix<- which(grepl('EFIX', trialF[1:s]))
        allfix<- allfix[length(allfix)]
        allfix<- trialF[allfix]
        if(length(allfix)>0){
          temp$prevFix<- as.numeric(unlist(strsplit(allfix, "\t"))[4])
        } else{
          temp$prevFix<- NA
        }
        
        
        ####
        # next fixation:
        nextfix<- which(grepl('EFIX', trialF[s:length(trialF)]))
        nextfix3<- nextfix[3]
        nextfix2<- nextfix[2]
        nextfix<- nextfix[1] # always next fix
        nextfix<- trialF[s+nextfix-1]
        nextfix2<- trialF[s+nextfix2-1]
        nextfix3<- trialF[s+nextfix3-1]
        temp$nextFix<- as.numeric(unlist(strsplit(nextfix, "\t"))[4])
        temp$N1<- as.numeric(unlist(strsplit(nextfix, "\t"))[3])
        temp$N2<- as.numeric(unlist(strsplit(nextfix2, "\t"))[3])
        temp$N1x<- as.numeric(unlist(strsplit(nextfix2, "\t"))[4])
        temp$N2x<- as.numeric(unlist(strsplit(nextfix3, "\t"))[4])
        temp$N1len<- (abs(temp$N1x- temp$nextFix))/ppl
        temp$N2len<- (abs(temp$N2x- temp$N1x))/ppl
        
        # Regression after hearing sound
        if(!is.na(temp$N1x) & !is.na(temp$nextFix)){
          if(temp$N1x< temp$nextFix){
            temp$N1reg<- 1
          }else{
            temp$N1reg<- 0
          }
        }

        if(!is.na(temp$N2x) & !is.na(temp$N1x)){
          if(temp$N2x< temp$N1x){
            temp$N2reg<- 1
          }else{
            temp$N2reg<- 0
          }
        }
        
        #####
        # Time between crossing boundary and SFIX flag:
        nextSFIX<- which(grepl('SFIX', trialF[s:length(trialF)]))
        prevSFIX<- which(grepl('SFIX', trialF[1:s]))
        nextSFIX<- nextSFIX[1] # always next fix
        prevSFIX<- prevSFIX[length(prevSFIX)] # always last one
        
        nextSFIX<- trialF[s+nextSFIX-1]
        prevSFIX<- trialF[1+prevSFIX-1]
        
        nextSFIX<- get_num(nextSFIX)
        prevSFIX<- get_num(prevSFIX)
        
        
        if(length(nextSFIX)>0 & length(prevSFIX)>0){
          if(!is.na(nextSFIX) & !is.na(prevSFIX)){
            if(nextSFIX-temp$tBnd< temp$tBnd-prevSFIX){
              temp$tSFIX<- nextSFIX
            }else{
              temp$tSFIX<- prevSFIX
            }
          }
        }
        
        
        #temp$tSFIX<- get_num(nextSFIX)
        
        # delay:
        temp$delFix<-  temp$tBnd- temp$tSFIX
        temp$delFix<- temp$delFix+soundLatency
        
        
        ####
        # Time from previous sound:
        if (temp$sound>1){
          temp$ISI= temp$tBnd- prevSound
        }else{
          temp$ISI<-NA
        }
        
        
        
        
        #####
        # What was the next flag after crossing the boundary?
        nextESACC<- which(grepl('ESACC' , trialF[s:length(trialF)]))
        nextEFIX<- which(grepl('EFIX' , trialF[s:length(trialF)]))
        
        if(!is.na(nextESACC[1]) & !is.na(nextEFIX[1])){
          if(nextESACC[1]<nextEFIX[1]){
            type<- 'ESACC'
            stamp<- nextESACC[1]
          } else{
            type<- 'EFIX'
            stamp<- nextEFIX[1]
          }
          temp$nextFlag<- type
        }

        
        ###
        # previous fixation not on empty space?
        if(!is.na(temp$prevFix)){
          if(round(temp$prevFix)< temp$regionS-1){ #ppl/2
            temp$prevGood<- "Yes"
          } else{
            temp$prevGood<- "No"
          }
        }else{
          temp$prevGood<- NA
        }
        
        ####
        # Next fixation on critical word?
        if(!is.na(temp$nextFix)){
          if(round(temp$nextFix)>= temp$regionS & round(temp$nextFix)<= temp$regionE){
            temp$onTarget<- "Yes"
          }else{
            temp$onTarget<- "No"
          }
        }

        
        ####
        # Next fixation in critical region?
        if(!is.na(temp$nextFix)){
          if(round(temp$nextFix)< temp$regionN1 & round(temp$nextFix)>= temp$regionS-1){ # ppl/2
            # -ppl because fix is still in region if on the space before the critical word
            temp$inRegion<- "Yes"
          } else{
            temp$inRegion<- "No"
          }
        }

        
        ###
        # Hook- boundary crossing?
        if(!is.na(temp$nextFix)){
          if(round(temp$nextFix)<= temp$regionS -1){ # ppl/2
            temp$hook<- "Yes"
          }else{
            temp$hook<- "No"
          }
        }

        
        ######
        # Target word blink?
        allblinks<- which(grepl('SBLINK' , trialF))
        allblinks<- allblinks-1 # get previous stamp that contains x pixel location
        allblinks<- trialF[allblinks]
        xposB<- NA
        
        for(l in 1:length(allblinks)){
          xposB[l]<- get_x(allblinks[l])
        }
        
        targetBlink<- which(xposB >= temp$regionS-1 & xposB<= temp$regionE)# ppl/2
        if(length(targetBlink)>0){
          temp$blink<- "Yes"
        } else{
          temp$blink<- "No"
        }
        
        ###########
        prevSound<- temp$tBnd
        
        
        
        # add to dataframe:
        data<- rbind(data, temp)
      } # end of k loop
      
      
    } # end of j loop
  } # end if i loop
  
  return(data)
}
