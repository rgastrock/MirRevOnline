source('ana/shared.R')
source('ana/su&fa2020online.R')
source('ana/qualtricsdata.R')

# Mirror generalization: These participants complete Part 2 of the experiment (i.e. those who did Fall data, came back)
# Target Locations at 30, 60, 300, 330, 120, 150 degrees
# Trials: Use one hand (R or L) - 20 trials mirrored at 30 and 60
#                                 20 trials mirrored at 300 and 330
#                                 20 trials mirrored at 120 and 150
#                                 20 trials mirrored at 30 and 60
#       : Switch to other hand  - 20 trials mirrored at 30 and 60
#                                 20 trials washout at 30 and 60

# pre-processing----
# Use only data from people who also did the Non-instructed experiment during Fall 2020 (Part 1)
getNoPartOneParticipants <- function(){
  
  #get list of id's from Fall data
  qualtdat <- read.csv('data/mirrorreversal-fall/data/processed/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  ppqualt <- qualtdat$id[-c(1:2)]
  
  #get all filenames in generalizaton data
  datafilenames <- list.files('data/mirrorgeneralization-master/data', pattern = '*.csv')
  ppdel <- c()
  #ppgen <- c()
  #fdat <- c()
  for (datafilenum in c(1:length(datafilenames))){
    
    filename <- sprintf('data/mirrorgeneralization-master/data/%s', datafilenames[datafilenum])
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),filename))
    dat <- handleOneFile(filename = filename)
    ppdat <- unique(dat$participant)
    
    # if(ppdat == '221302'){
    #   fdat <- c(fdat, filename)
    # }
    # 
    # ppgen <- c(ppgen, ppdat)
    
    if(ppdat %in% ppqualt == FALSE){
      ppdel <- c(ppdel, ppdat)
    }
  }
  
  return(ppdel)
 
}