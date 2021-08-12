source('ana/shared.R')
source('ana/su&fa2020online.R')
source('ana/qualtricsdata.R')

#pre-processing----

#Function to handle one participant. Outputs a df with relevant information across trials
handleOneCtrlFile <- function(filename) {
  
  # if the file can't be read, return empty list for now
  df <- NULL
  try(df <- read.csv(filename, stringsAsFactors = F), silent = TRUE)
  if (is.null(df)) {
    return(list())
  }
  
  # set up vectors for relevant data:
  trialno <- c()            #trialNum
  targetangle_deg <- c()
  trialtype <-c()              #trialsType
  reachdeviation_deg <- c()
  taskno <- c()             #trialsNum
  participant <- c()
  
  # remove empty lines:
  df <- df[which(!is.na(df$trialsNum)),]
  #df <- df[which(df$trialsNum == 2),]
  
  # loop through all trials
  #plot(x,y,type='l',col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
  for (trialnum in c(1:dim(df)[1])) {
    
    x <- convertCellToNumVector(df$trialMouse.x[trialnum])
    y <- convertCellToNumVector(df$trialMouse.y[trialnum])
    s <- convertCellToNumVector(df$step[trialnum])
    m <- df$trialsType[trialnum]
    a <- df$targetangle_deg[trialnum]
    p <- df$participant[trialnum]
    
    # remove stuff that is not step==2
    step2idx = which(s == 2)
    x <- x[step2idx]
    y <- y[step2idx]
    
    # plot(x,y,type='l',col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
    # lines(c(0,1),c(0,0),col='black')
    # points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
    
    # get first point beyond some distance (home-target is 40% of height of participant's screen)
    # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
    d <- sqrt(x^2 + y^2)
    idx <- which(d > .08)[1]
    x <- x[idx]
    y <- y[idx]
    
    #points(x,y,col='red')
    
    # get angular deviation of reach from target angle:
    rotcoords <- rotateTrajectory(x,y,-a)
    x <- rotcoords[1]
    y <- rotcoords[2]
    
    rd <- (atan2(y, x) / pi) * 180
    
    
    #text(0,-0.1,sprintf('%0.3f',rd))
    
    # store in vectors:
    trialno <- c(trialno, trialnum)
    targetangle_deg <- c(targetangle_deg, a)
    trialtype <-c(trialtype, m)
    reachdeviation_deg <- c(reachdeviation_deg, rd)
    taskno <- c(taskno, df$trialsNum[trialnum])
    participant <- c(participant, p)
  }
  
  # vectors as data frame columns:
  dfrd <- data.frame(trialno, targetangle_deg, trialtype, reachdeviation_deg, taskno, participant)
  
  return(dfrd)
}

#Use experimental data for those that also have qualtrics data
getCtrlMirWithoutQualtrics <- function(){
  
  #participant list from behavioral data
  
  datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
  
  
  
  dataoutput<- c() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- handleOneCtrlFile(filename = datafilename)
    ppname <- unique(adat$participant)
    
    dataoutput <- c(dataoutput, ppname)
  }
  
  #read in Qualtrics sheet
  qualt <- read.csv('data/controlmironline-master/qualtrics/ControlMir-SU2021-Part1_June 30, 2021_11.29.csv', stringsAsFactors = F)
  #find which of our dataoutput (pp with data) have qualtrics data as well
  ppqualt <- qualt$id[-c(1:2)]
  pp_no_qualt <- dataoutput[which(dataoutput %in% ppqualt == FALSE)]
  
  
  return(pp_no_qualt)
  #function returns nothing if all data we have also have corresponding Qualtrics data
}

#Function below will generate a new csv file of Qualtrics data that contains only participants that also have experimental data
getCtrlMirQualtricsData <- function(){
  datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
  
  
  
  dataoutput<- c() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- handleOneCtrlFile(filename = datafilename)
    ppname <- unique(adat$participant)
    
    dataoutput <- c(dataoutput, ppname)
  }
  
  qualt <- read.csv('data/controlmironline-master/qualtrics/ControlMir-SU2021-Part1_June 30, 2021_11.29.csv', stringsAsFactors = F)
  
  ndataoutput <- data.frame()
  for (pp in dataoutput){
    if(pp %in% qualt$id){
      ndat <- qualt[which(qualt$id == pp),]
    }
    
    if (prod(dim(ndataoutput)) == 0){
      ndataoutput <- ndat
    } else {
      ndataoutput <- rbind(ndataoutput, ndat)
    }
  }
  
  row1qualt <- qualt[1,]
  alldat <- rbind(row1qualt, ndataoutput)
  write.csv(alldat, file='data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', row.names = F)
  
}

#Funtion collects hand responses for checking (i.e. did they use appropriate hand for task)
getCtrlHandMatches <- function(){
  
  datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
 
  allresp <- data.frame()
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    df <- read.csv(datafilename, stringsAsFactors = F)
    ppname <- unique(df$participant)
    keyresp <- df$intrResp.keys[which(df$intrResp.keys != "")] #remove empty strings
    keyresp <- paste(keyresp, collapse=",") #collapse as one string
    ppresp <- data.frame(ppname, keyresp) #collect with participant id
    
    allresp <- rbind(allresp, ppresp)
  }
  
  ctrlmirdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  
  qualtresp <- data.frame()
  for(pp in allresp$ppname){
    subdat <- ctrlmirdat[which(ctrlmirdat$id == pp),]
    ppname <- pp
    handedness <- subdat$Q2.5 #what is their handedness
    comphand <- subdat$Q3.3 #which hand they typically use for controlling mouse
    handresp <- subdat$Q8.2 #response to hand switching
    qualtppresp <- data.frame(ppname, handedness, comphand, handresp)
    
    qualtresp <- rbind(qualtresp, qualtppresp)
  }
  
  handmatches <- merge(allresp, qualtresp, by='ppname')
  
  write.csv(handmatches, file='data/controlmironline-master/data/processed/HandMatches.csv', row.names = F)
  #These were then manually inspected to see any mismatches
}

#Learning rates----

#gather reach deviations for all participants, across all trials
getParticipantLearningCtrl <- function(filename){
  
  dat <- handleOneCtrlFile(filename = filename)
  dat$circ_rd <- as.circular(dat$reachdeviation_deg, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  targetdist <- c()
  for (target in dat$targetangle_deg){
    if (target %in% c(5, 175, 355)){
      dist <- 'far'
      targetdist <- c(targetdist, dist)
    } else if (target %in% c(45, 135, 315)){
      dist <- 'mid'
      targetdist <- c(targetdist, dist)
    } else if (target %in% c(85, 95, 275)){
      dist <- 'near'
      targetdist <- c(targetdist, dist)
    }
  }
  dat$targetdist <- targetdist
  
  return(dat)
}

getGroupLearningCtrl <- function(groups = c('far', 'mid', 'near')){
  #group is either 'far', 'mid', 'near' in relation to mirror
  for(group in groups){
    datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      adat <- getParticipantLearningCtrl(filename = datafilename)
      # per target location, get reachdev for corresponding trials
      
      trial <- c(1:length(adat$trialno))
      #adat$trialno <- trial
      for (triali in trial){
        trialdat <- adat[which(adat$trialno == triali),]
        #set reachdev to NA if not the target location we want
        if (trialdat$targetdist != group){
          trialdat$circ_rd <- NA
        }
        adat[triali,] <- trialdat
      }
      ppreaches <- adat$circ_rd #get reach deviations column from learning curve data
      ppdat <- data.frame(trial, ppreaches)
      
      ppname <- unique(adat$participant)
      names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- ppdat
      } else {
        dataoutput <- cbind(dataoutput, ppreaches)
        names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
      }
    }
    
    
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/controlmironline-master/data/processed/%s_LearningCtrl.csv', group), row.names = F)
  }
}


getGroupLearningCtrlConfInt <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_LearningCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    
    
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      
      if(length(unique(circ_subdat)) == 1){ #deal with trials with no data at all
        citrial <- as.numeric(c(NA,NA,NA))
      } else{
        citrial <- getCircularConfidenceInterval(data = circ_subdat)
        citrial <- as.numeric(citrial)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }

      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_LearningCtrl_CI.csv', group), row.names = F) 
      
    }
  }
}

plotLearningCtrl <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/controlmironline-master/doc/fig/Fig1_LearningCtrl.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,178), ylim = c(-10,185), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of learning per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 10, 90, 170), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 46, 67, 97, 127, 157, 177)) #tick marks for x axis
  axis(2, at = c(0, 10, 50, 90, 130, 170)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_LearningCtrl_CI.csv', group))
    
    #split up data set for plotting purposes
    groupconfidenceAligned <- groupconfidence[1:45,]
    groupconfidenceLeftAligned <- groupconfidence[46:66,]
    groupconfidenceMirrored <- groupconfidence[67:156,]
    groupconfidenceRAE <- groupconfidence[157:177,]
    
    colourscheme <- getCtrlColourScheme(groups = group)
    #plot Aligned Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceAligned[,1]
    upper <- groupconfidenceAligned[,3]
    mid <- groupconfidenceAligned[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:45), rev(c(1:45))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:45), y = mid,col=col,lty=1)
    
    #plot Left Aligned Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceLeftAligned[,1]
    upper <- groupconfidenceLeftAligned[,3]
    mid <- groupconfidenceLeftAligned[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(46:66), rev(c(46:66))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(46:66), y = mid,col=col,lty=1)
    
    #plot Mirrored Data
    lower <- groupconfidenceMirrored[,1]
    upper <- groupconfidenceMirrored[,3]
    mid <- groupconfidenceMirrored[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(67:156), rev(c(67:156))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(67:156), y = mid,col=col,lty=1)
    
    #plot Wahout Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceRAE[,1]
    upper <- groupconfidenceRAE[,3]
    mid <- groupconfidenceRAE[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(157:177), rev(c(157:177))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(157:177), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(60,25,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
  
  
}