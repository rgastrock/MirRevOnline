source('ana/shared.R')
source('ana/su&fa2020online.R')

# Qualtrics data contains participant answers to survey questions as part of the sensorimotor battery of tasks
# We would want to add in mirror reversal behavioral data (movement time, path length) to the Qualtrics output for further analyses

# Check: Whether Qualtrics participants are the same as what we have already analyzed (Summer data set for now)
getSUDataQualtricsMatch <- function(set = 'su2020'){
  
  #participant list from behavioral data
  
  datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
  
  
  
  dataoutput<- c() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    
    #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- getParticipantCircularAligned(filename = datafilename)
    ppname <- unique(adat$participant)
    
    dataoutput <- c(dataoutput, ppname)
  }
  
  
  #participant list from Qualtrics data
  #this list also contains people who will be removed due to performance in mirror reversal task or other data cleaning procedures
  #we use a list generated to keep track of removed participants
  
  qualt <- read.csv('data/mReversalNewAlpha3-master/data/SU_QualtricsData.csv', stringsAsFactors = F)
  ppdel <- read.csv('data/mReversalNewAlpha3-master/data/pplist_removed.csv', stringsAsFactors = F)
  
  
  ppdel <- ppdel$participant
  ppqualt <- qualt$Please.enter.your.URPP.number.
  for(pp in ppdel){
    if (pp %in% ppqualt){
      qualt <- qualt[-which(qualt$Please.enter.your.URPP.number. == pp),] #d column does not always match participant id in data file
    }
  }
  
  #IMPORTANT: Summer data - These participants have mirror data but no qualtrics data:
  # 215797, Tiffany, Victoria, Yue Hu
  dataoutput <- dataoutput[-which(dataoutput == '215797')]
  dataoutput <- dataoutput[-which(dataoutput == 'Tiffany')]
  dataoutput <- dataoutput[-which(dataoutput == 'Victoria')]
  dataoutput <- dataoutput[-which(dataoutput == 'Yue Hu')]
  
  #check if the two match: will only print TRUE if they do
  print(unique(dataoutput %in% qualt$Please.enter.your.URPP.number.[-1]))
  
  allpp <- data.frame(dataoutput, qualt$Please.enter.your.URPP.number.[-1])
  return(allpp)
  #use below to figure out who else is not included in the already analyzed data, if necessary
  #qualtoutput <- qualt$Please.enter.your.URPP.number.[-1]
  #ab <- qualtoutput[which(qualtoutput %in% dataoutput == FALSE)]
}
#do similar function for Fall Data

#Function below will generate csv file of Qualtric data that contains the participants we have not removed and have data for
getQualtricsData <- function(set){
  if(set=='su2020'){
    qualt <- read.csv('data/mReversalNewAlpha3-master/data/processed/SU_QualtricsData.csv', stringsAsFactors = F)
    ppdel <- read.csv('data/mReversalNewAlpha3-master/data/processed/pplist_removed.csv', stringsAsFactors = F)
  } else if (set=='fa2020'){
    qualt <- read.csv('data/mirrorreversal-fall/data/FA_QualtricsData.csv', stringsAsFactors = F)
    ppdel <- read.csv('data/mirrorreversal-fall/data/pplist_removed.csv', stringsAsFactors = F)
  }
  
  ppdel <- ppdel$participant
  ppqualt <- qualt$Please.enter.your.URPP.number.
  for(pp in ppdel){
    if (pp %in% ppqualt){
      qualt <- qualt[-which(qualt$Please.enter.your.URPP.number. == pp),]
    }
  }
  
  if(set=='su2020'){
    write.csv(qualt, file='data/mReversalNewAlpha3-master/data/processed/SU_Qualtrics_ParticipantList.csv', row.names = F)
  } else if(set=='fa2020'){
    write.csv(qualt, file='data/mirrorreversal-fall/data/processed/FA_Qualtrics_ParticipantList.csv', row.names = F)
  }
}

# Movement time data: Per participant, get last 40 trials. Then take its mean and SD.
getMeanAndStdevMT <- function(set, step = 2){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  participant <- c()#create place holder
  meanMT <- c()
  sdMT <- c()
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    dat <- handleOneMTFile(filename = datafilename, step = step)
    
    #then get only the last 40 trials of the mirror reversal (trials 71 to 110)
    dat <- dat[which(dat$taskno == 2),]
    dat <- tail(dat, n = 40)
    
    #get variables for participant, mean, SD of measure
    ppname <- unique(dat$participant)
    mean <- mean(dat$time)
    stdev <- sd(dat$time)
    
    participant <- c(participant, ppname)
    meanMT <- c(meanMT, mean)
    sdMT <- c(sdMT, stdev)
    
  }
  
  dfmt <- data.frame(participant, meanMT, sdMT)
  
  #IMPORTANT: Summer data - These participants have mirror data but no qualtrics data:
  # 215797, Tiffany, Victoria, Yue Hu
  dfmt <- dfmt[-which(dfmt$participant == '215797'),]
  dfmt <- dfmt[-which(dfmt$participant == 'Tiffany'),]
  dfmt <- dfmt[-which(dfmt$participant == 'Victoria'),]
  dfmt <- dfmt[-which(dfmt$participant == 'Yue Hu'),]
  
  return(dfmt)
  
}

# Path Length data: Per participant, get last 40 trials. Then take its mean and SD.
getMeanAndStdevPL <- function(set, step = 2){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  participant <- c()#create place holder
  meanPL <- c()
  sdPL <- c()
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    dat <- handleOneFilePathLength(filename = datafilename, step = step)
    
    #then get only the last 40 trials of the mirror reversal (trials 71 to 110)
    dat <- dat[which(dat$taskno == 2),]
    dat <- tail(dat, n = 40)
    
    #get variables for participant, mean, SD of measure
    ppname <- unique(dat$participant)
    mean <- mean(dat$path_length)
    stdev <- sd(dat$path_length)
    
    participant <- c(participant, ppname)
    meanPL <- c(meanPL, mean)
    sdPL <- c(sdPL, stdev)
    
  }
  
  dfpl <- data.frame(participant, meanPL, sdPL)
  
  #IMPORTANT: Summer data - These participants have mirror data but no qualtrics data:
  # 215797, Tiffany, Victoria, Yue Hu
  dfpl <- dfpl[-which(dfpl$participant == '215797'),]
  dfpl <- dfpl[-which(dfpl$participant == 'Tiffany'),]
  dfpl <- dfpl[-which(dfpl$participant == 'Victoria'),]
  dfpl <- dfpl[-which(dfpl$participant == 'Yue Hu'),]
  
  return(dfpl)
  
}

# After, add a column to Qualtrics data matching the MT and PL data for each participant
getQualtMirData <- function(set){
  #get data
  if(set=='su2020'){
    qualt <- read.csv('data/mReversalNewAlpha3-master/data/processed/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    MTdat <- getMeanAndStdevMT(set = set, step = 2)
    PLdat <- getMeanAndStdevPL(set = set, step = 2)
  } else if (set=='fa2020'){
    qualt <- read.csv('data/mirrorreversal-fall/data/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    MTdat <- getMeanAndStdevMT(set = set, step = 2)
    PLdat <- getMeanAndStdevPL(set = set, step = 2)
  }
  
  #can put together behavioral data
  behavdat <- cbind(MTdat, PLdat[,2:3])
  
  #add empty columns to qualtrics data
  qualt$meanMT <- rep(NA, nrow(qualt))
  qualt$sdMT <- rep(NA, nrow(qualt))
  qualt$meanPL <- rep(NA, nrow(qualt))
  qualt$sdPL <- rep(NA, nrow(qualt))
  
  qualtpp <- qualt$Please.enter.your.URPP.number.[-1] #remove the extra header row
  for (ppname in qualtpp){
    ppdat <- qualt[which(qualt$Please.enter.your.URPP.number. == ppname),]
    bdat <- behavdat[which(behavdat$participant == ppname),-1] #can remove participant column
    
    ppdat$meanMT <- bdat$meanMT
    ppdat$sdMT <- bdat$sdMT
    ppdat$meanPL <- bdat$meanPL
    ppdat$sdPL <- bdat$sdPL
    
    qualt[which(qualt$Please.enter.your.URPP.number. == ppname),] <- ppdat
  }
  
  if(set=='su2020'){
    write.csv(qualt, file='data/mReversalNewAlpha3-master/data/processed/SU_Qualtrics_Mirror.csv', row.names = F)
  } else if(set=='fa2020'){
    write.csv(qualt, file='data/mirrorreversal-fall/data/processed/FA_Qualtrics_Mirror.csv', row.names = F)
  }
}



















