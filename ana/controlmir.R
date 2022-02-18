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

getAngularReachDevsCI <- function(data, group, resamples = 1000){
  
  #CI's generated for far target are very wide, given the negative or positive directions in circular values
  #To fix this, we can generate angular reach deviations using x and y coordinates instead
  #We bootstrap with replacement, so that we can generate lower, mid, upper values for CI
  data <- data[which(is.finite(data))]
  samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
  BS <- c()
  for (irow in 1:nrow(samplematrix)){
    subdat <- samplematrix[irow,]
    #convert reach deviations from degrees to radians
    degtorad <- (subdat / 180) * pi
    #sin of radians values will be y values, cos will be x
    yvals <- sin(degtorad)
    xvals <- cos(degtorad)
    #summation of all y and x values to be passed on to atan2, then converted to degrees
    y <- sum(yvals)
    x <- sum(xvals)
    rd <- (atan2(y,x) / pi) * 180
    
    # BS should have as much as resamples (i.e. 1000)
    BS <- as.numeric(c(BS, rd))
  }
  #wide CI's are generated for far group after atan2 (i.e. -178 should be the same as 182 in a 2D plot)
  #to fix for this, we add 360 for bootstrapped values below -90 for only the far group
  for (angleidx in 1:length(BS)){
    angle <- BS[angleidx]
    if (group == 'far' && angle < -90){
      BS[angleidx] <- angle + 360
    }
  }
  
  return(quantile(BS, probs = c(0.025, 0.50, 0.975)))
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

#group according to targets----

#gather reach deviations for all participants, across all trials
getParticipantLearningCtrl <- function(filename){
  
  dat <- handleOneCtrlFile(filename = filename)
  dat$circ_rd <- as.circular(dat$reachdeviation_deg, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  targetdist <- c()
  
  for (target in dat$targetangle_deg){
    #group targets by how far each one is from mirror (far, mid, near)
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

#Aligned data for both hands----
getAlignedGroupLearningCtrl <- function(groups = c('far', 'mid', 'near')){
  #group is either 'far', 'mid', 'near' in relation to mirror
  for(group in groups){
    datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      adat <- getParticipantLearningCtrl(filename = datafilename)
      adat <- adat[which(adat$taskno == 1 | adat$taskno == 2),] #get only aligned data for both hands
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
    write.csv(dataoutput, file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), row.names = F)
  }
}

getAlignedGroupLearningCtrlCI <- function(groups = c('far', 'mid', 'near')){
  
  for(group in groups){
    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    trialno <- data$trial
    
    confidence <- data.frame()
    
    for(trial in trialno){
      subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      citrial <- getAngularReachDevsCI(data = subdat, group = group)
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_CI.csv', group), row.names = F)
    }
  }
}

getAlignedGroupLearningCtrlCircularCI <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    
    
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

      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_Circular_CI.csv', group), row.names = F) 
      
    }
  }
}

plotAlignedCtrl <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/controlmironline-master/doc/fig/Fig1A_AlignedCtrl.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,67), ylim = c(-30,185), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Aligned reaches", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 10, 90, 170), v = c(45, 66), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30, 40, 46, 56, 66)) #tick marks for x axis
  axis(2, at = c(-30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_CI.csv', group))
    
    #split up data set for plotting purposes
    groupconfidenceAligned <- groupconfidence[1:45,]
    groupconfidenceLeftAligned <- groupconfidence[46:66,]
    #groupconfidenceMirrored <- groupconfidence[67:156,]
    #groupconfidenceRAE <- groupconfidence[157:177,]
    
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
    
  }
  
  #add legend
  legend(5,145,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

plotAlignedCtrlCircular <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/controlmironline-master/doc/fig/Fig1A_AlignedCtrlCircular.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,67), ylim = c(-30,185), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Aligned reaches", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 10, 90, 170), v = c(45, 66), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30, 40, 46, 56, 66)) #tick marks for x axis
  axis(2, at = c(-30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_Circular_CI.csv', group))
    
    #split up data set for plotting purposes
    groupconfidenceAligned <- groupconfidence[1:45,]
    groupconfidenceLeftAligned <- groupconfidence[46:66,]
    #groupconfidenceMirrored <- groupconfidence[67:156,]
    #groupconfidenceRAE <- groupconfidence[157:177,]
    
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
    
  }
  
  #add legend
  legend(5,145,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#Mirrored data----
#baseline biases removed, sign flip if target in other quadrants
getMirroredParticipantLearningCtrl <- function(filename){
  
  dat <- getParticipantLearningCtrl(filename = filename)
  adat <- dat[which(dat$taskno == 1),] #get only aligned data for first hand
  mdat <- dat[which(dat$taskno == 3),] #mirrored data
  
  biases <- aggregate(circ_rd ~ targetangle_deg, data= adat, FUN = median.circular)
  #get only biases for locations used in mirrored (quad 1: 5, 45, 85)
  biases <- biases[which(biases$targetangle_deg == c(5, 45, 85)),]
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'circ_rd'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    mdat$circ_rd[which(mdat$targetangle_deg == target)] <- mdat$circ_rd[which(mdat$targetangle_deg == target)] - bias
    
  }
  return(mdat)
}

getMirroredGroupLearningCtrl <- function(groups = c('far', 'mid', 'near')){
  #group is either 'far', 'mid', 'near' in relation to mirror
  for(group in groups){
    datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      mdat <- getMirroredParticipantLearningCtrl(filename = datafilename)
      # per target location, get reachdev for corresponding trials
      
      trial <- c(1:length(mdat$trialno))
      #adat$trialno <- trial
      for (triali in trial){
        #trialdat <- mdat[which(mdat$trialno == triali),]
        trialdat <- mdat[triali,]
        #set reachdev to NA if not the target location we want
        if (trialdat$targetdist != group){
          trialdat$circ_rd <- NA
        }
        mdat[triali,] <- trialdat
      }
      ppreaches <- mdat$circ_rd #get reach deviations column from learning curve data
      ppdat <- data.frame(trial, ppreaches)
      
      ppname <- unique(mdat$participant)
      names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- ppdat
      } else {
        dataoutput <- cbind(dataoutput, ppreaches)
        names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
      }
    }
    
    
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), row.names = F)
  }
}

getMirroredGroupLearningCtrlCI <- function(groups = c('far', 'mid', 'near')){
  
  for(group in groups){
    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    trialno <- data$trial
    
    confidence <- data.frame()
    
    for(trial in trialno){
      subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      citrial <- getAngularReachDevsCI(data = subdat, group = group)
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl_CI.csv', group), row.names = F)
    }
  }
}

getMirroredGroupLearningCtrlCircularCI <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    
    
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
      
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl_Circular_CI.csv', group), row.names = F) 
      
    }
  }
}

plotMirCtrl <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/controlmironline-master/doc/fig/Fig1B_MirCtrl.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-65,225), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Mirrored reaches", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 10, 90, 170), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90)) #tick marks for x axis
  axis(2, at = c(-60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl_CI.csv', group))
    
    colourscheme <- getCtrlColourScheme(groups = group)
    #plot Mir Data
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:90), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(65,-15,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

plotMirCtrlCircular <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/controlmironline-master/doc/fig/Fig1B_MirCtrlCircular.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Mirrored reaches", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 10, 90, 170), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90)) #tick marks for x axis
  axis(2, at = c(-170, -130, -90, -60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl_Circular_CI.csv', group))
    
    colourscheme <- getCtrlColourScheme(groups = group)
    #plot Mir Data
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:90), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(65,-90,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#Washout data----
#remove baseline biases
getRAEParticipantLearningCtrl <- function(filename){
  
  dat <- getParticipantLearningCtrl(filename = filename)
  adat <- dat[which(dat$taskno == 1),] #get only aligned data for first hand
  mdat <- dat[which(dat$taskno == 4),] #washout data
  
  biases <- aggregate(circ_rd ~ targetangle_deg, data= adat, FUN = median.circular)
  #get only biases for locations used in mirrored (quad 1: 5, 45, 85)
  biases <- biases[which(biases$targetangle_deg == c(5, 45, 85)),]
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'circ_rd'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    mdat$circ_rd[which(mdat$targetangle_deg == target)] <- mdat$circ_rd[which(mdat$targetangle_deg == target)] - bias
    
  }
  return(mdat)
}

getRAEGroupLearningCtrl <- function(groups = c('far', 'mid', 'near')){
  #group is either 'far', 'mid', 'near' in relation to mirror
  for(group in groups){
    datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      mdat <- getRAEParticipantLearningCtrl(filename = datafilename)
      # per target location, get reachdev for corresponding trials
      
      trial <- c(1:length(mdat$trialno))
      #adat$trialno <- trial
      for (triali in trial){
        #trialdat <- mdat[which(mdat$trialno == triali),]
        trialdat <- mdat[triali,]
        #set reachdev to NA if not the target location we want
        if (trialdat$targetdist != group){
          trialdat$circ_rd <- NA
        }
        mdat[triali,] <- trialdat
      }
      ppreaches <- mdat$circ_rd #get reach deviations column from learning curve data
      ppdat <- data.frame(trial, ppreaches)
      
      ppname <- unique(mdat$participant)
      names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- ppdat
      } else {
        dataoutput <- cbind(dataoutput, ppreaches)
        names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
      }
    }
    
    
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), row.names = F)
  }
}

getRAEGroupLearningCtrlCI <- function(groups = c('far', 'mid', 'near')){
  
  for(group in groups){
    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    trialno <- data$trial
    
    confidence <- data.frame()
    
    for(trial in trialno){
      subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      citrial <- getAngularReachDevsCI(data = subdat, group = group)
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_CI.csv', group), row.names = F)
    }
  }
}

getRAEGroupLearningCtrlCircularCI <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    
    
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
      
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_Circular_CI.csv', group), row.names = F) 
      
    }
  }
}

plotRAECtrl <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/controlmironline-master/doc/fig/Fig1C_RAECtrl.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,22), ylim = c(-30,185), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Washout trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 10, 90, 170), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 5, 10, 15, 21)) #tick marks for x axis
  axis(2, at = c(-30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_CI.csv', group))
    
    colourscheme <- getCtrlColourScheme(groups = group)
    #plot Mir Data
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:21), rev(c(1:21))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:21), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(5,145,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

plotRAECtrlCircular <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/controlmironline-master/doc/fig/Fig1C_RAECtrlCircular.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,22), ylim = c(-30,185), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Washout trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 10, 90, 170), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 5, 10, 15, 21)) #tick marks for x axis
  axis(2, at = c(-30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_Circular_CI.csv', group))
    
    colourscheme <- getCtrlColourScheme(groups = group)
    #plot Mir Data
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:21), rev(c(1:21))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:21), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(5,145,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#plot all learning rate trials----
plotAllTasksCtrl <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg'){
    svglite(file='data/controlmironline-master/doc/fig/Fig1_LearningCtrl.svg', width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(0,178), ylim = c(-65,225), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 10, 90, 170), v = c(45, 66, 156), col = 8, lty = 2)
  axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
  axis(2, at = c(-60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
  
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_CI.csv', group))
    groupconfidenceLC <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl_CI.csv', group))
    groupconfidenceRAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_CI.csv', group))
    
    
    
    colourscheme <- getCtrlColourScheme(groups = group)
    #plot Aligned Data
    #take only first, last and middle columns of file
    
    #split up data set for plotting purposes
    groupconfidenceAligned <- groupconfidence[1:45,]
    groupconfidenceLeftAligned <- groupconfidence[46:66,]
    
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
    lower <- groupconfidenceLC[,1]
    upper <- groupconfidenceLC[,3]
    mid <- groupconfidenceLC[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(67:156), rev(c(67:156))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(67:156), y = mid,col=col,lty=1)
    
    #plot Washout Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceRAE[,1]
    upper <- groupconfidenceRAE[,3]
    mid <- groupconfidenceRAE[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(157:177), rev(c(157:177))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(157:177), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(5,145,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

plotAllTasksCtrlCircular <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg'){
    svglite(file='data/controlmironline-master/doc/fig/Fig1_LearningCtrlCircular.svg', width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(0,178), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 10, 90, 170), v = c(45, 66, 156), col = 8, lty = 2)
  axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
  axis(2, at = c(-170, -130, -90, -60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
  
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_Circular_CI.csv', group))
    groupconfidenceLC <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl_Circular_CI.csv', group))
    groupconfidenceRAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_Circular_CI.csv', group))
    
    
    
    colourscheme <- getCtrlColourScheme(groups = group)
    #plot Aligned Data
    #take only first, last and middle columns of file
    
    #split up data set for plotting purposes
    groupconfidenceAligned <- groupconfidence[1:45,]
    groupconfidenceLeftAligned <- groupconfidence[46:66,]
    
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
    lower <- groupconfidenceLC[,1]
    upper <- groupconfidenceLC[,3]
    mid <- groupconfidenceLC[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(67:156), rev(c(67:156))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(67:156), y = mid,col=col,lty=1)
    
    #plot Washout Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceRAE[,1]
    upper <- groupconfidenceRAE[,3]
    mid <- groupconfidenceRAE[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(157:177), rev(c(157:177))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(157:177), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(5,145,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getBlockedMirCtrl <- function(group, blockdefs) {
  
  curves <- read.csv(sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv',group), stringsAsFactors=FALSE)  
  curves <- curves[,-1] #remove trial rows
  N <- dim(curves)[2]
  
  blocked <- array(NA, dim=c(N,length(blockdefs)))
  
  for (ppno in c(1:N)) {
    
    for (blockno in c(1:length(blockdefs))) {
      #for each participant, and every three trials, get the mean
      blockdef <- blockdefs[[blockno]]
      blockstart <- blockdef[1]
      blockend <- blockstart + blockdef[2] - 1
      samples <- curves[blockstart:blockend,ppno]
      blocked[ppno,blockno] <- mean(samples, na.rm=TRUE)
      
    }
    
  }
  
  return(blocked)
  
}

plotBlockedMirCtrl <- function(target='inline', groups = c('far', 'mid', 'near')) {
  
  if (target == 'svg') {
    svglite(file='data/controlmironline-master/doc/fig/Fig1F_BlockedLearningCtrl.svg', width=14, height=9, pointsize=14, system_fonts=list(sans='Arial'))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  
  
  # # # # # # # # # #
  # panel A: Learning Curves for all groups across all trials
  plotAllTasksCtrl()
  #mtext('A', side=3, outer=TRUE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  # # # # # # # # # #
  # panel B: individual participants in the first trial set
  plot(c(0,4),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,3.5),ylim=c(-20, 200),xlab='Mir Trials 1 - 3',ylab='Angular reach deviation (°)',xaxt='n',yaxt='n',bty='n',main='',font.main=1, cex.lab=1.10)

  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  abline(h = c(10,90,170), col = 8, lty = 2)
  
  blockdefs <- list(c(1,3))
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  groupno <- 0
  
  for (group in groups) {
    
    groupno <- groupno + 1 #counter for group, so that we can refer to it in x coordinates
    blocked <- getBlockedMirCtrl(group, blockdefs)
    colourscheme <- getCtrlColourScheme(group=group)
    #get 2.5, 50, 97.5% CIs
    meandist <- getAngularReachDevsCI(data = blocked, group = group)
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
    points(x=groupno,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
    
  }
  
  
  axis(side=1, at=c(1,2,3),labels=c('far','mid','near'),cex.axis=1.13)
  axis(side=2, at=c(0,10,90,170),labels=c('0','10','90','170'),cex.axis=1.13, las=2)
  
  
  # # # # # # # # # #
  # panel C: individual participants in the second trial set
  plot(c(0,4),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,3.5),ylim=c(-20, 200),xlab='Mir Trials 4 - 6',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1, cex.lab=1.10)
  
  mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  abline(h = c(10,90,170), col = 8, lty = 2)
  
  blockdefs <- list(c(4,3))
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  groupno <- 0
  
  for (group in groups) {
    
    groupno <- groupno + 1 #counter for group, so that we can refer to it in x coordinates
    blocked <- getBlockedMirCtrl(group, blockdefs)
    colourscheme <- getCtrlColourScheme(group=group)
    #get 2.5, 50, 97.5% CIs
    meandist <- getAngularReachDevsCI(data = blocked, group = group)
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
    points(x=groupno,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
    
  }
  
  
  axis(side=1, at=c(1,2,3),labels=c('far','mid','near'),cex.axis=1.13)
  axis(side=2, at=c(0,10,90,170),labels=c('0','10','90','170'),cex.axis=1.13, las=2)
  
  
  # # # # # # # # # #
  # panel D: individual participants in the last trial set
  plot(c(0,4),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,3.5),ylim=c(-20, 200),xlab='Mir Trials 76 - 90',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1, cex.lab=1.10)
  
  mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  abline(h = c(10,90,170), col = 8, lty = 2)
  
  blockdefs <- list(c(76,15))
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  groupno <- 0
  
  for (group in groups) {
    
    groupno <- groupno + 1 #counter for group, so that we can refer to it in x coordinates
    blocked <- getBlockedMirCtrl(group, blockdefs)
    colourscheme <- getCtrlColourScheme(group=group)
    #get 2.5, 50, 97.5% CIs
    meandist <- getAngularReachDevsCI(data = blocked, group = group)
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
    points(x=groupno,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
    
  }
  
  
  axis(side=1, at=c(1,2,3),labels=c('far','mid','near'),cex.axis=1.13)
  axis(side=2, at=c(0,10,90,170),labels=c('0','10','90','170'),cex.axis=1.13, las=2)
  
  
  if (target == 'svg') {
    dev.off()
  }
  
}


# circular density distributions-----
plotAlignedCtrlCircFreq <- function(groups = c('far', 'mid', 'near')){
  
  for(group in groups){

    dat <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    pdf(sprintf("data/controlmironline-master/doc/fig/Distribution_%s_AlignedCtrl.pdf", group))

    triallist <- c(1:66)
    #triallist <- c(1,2,90)
    #triallist <- c(1,2,3,4,89,90)
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,2:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #print(mean.circular(subdat, na.rm=T))
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      if(group == 'far'){
        plot(distsubdat, main = sprintf('Far target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(170, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'mid'){
        plot(distsubdat, main = sprintf('Mid target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(90, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('Near target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(10, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      }
    }
    dev.off()
    
  }
}

plotMirroredCtrlCircFreq <- function(groups = c('far', 'mid', 'near')){
  
  for(group in groups){
    
    dat <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    pdf(sprintf("data/controlmironline-master/doc/fig/Distribution_%s_MirCtrl.pdf", group))
    
    triallist <- c(1:90)
    #triallist <- c(1,2,90)
    #triallist <- c(1,2,3,4,89,90)
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,2:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #print(mean.circular(subdat, na.rm=T))
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      if(group == 'far'){
        plot(distsubdat, main = sprintf('Far target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(170, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'mid'){
        plot(distsubdat, main = sprintf('Mid target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(90, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('Near target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(10, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      }
    }
    dev.off()
    
  }
}

plotRAECtrlCircFreq <- function(groups = c('far', 'mid', 'near')){
  
  for(group in groups){
    
    dat <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    pdf(sprintf("data/controlmironline-master/doc/fig/Distribution_%s_RAECtrl.pdf", group))
    
    triallist <- c(1:21)
    #triallist <- c(1,2,90)
    #triallist <- c(1,2,3,4,89,90)
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,2:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #print(mean.circular(subdat, na.rm=T))
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      if(group == 'far'){
        plot(distsubdat, main = sprintf('Far target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(170, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'mid'){
        plot(distsubdat, main = sprintf('Mid target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(90, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('Near target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(10, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      }
    }
    dev.off()
    
  }
}

#Movement times----
handleOneMTCtrlFile <- function(filename, step = 2) {
  
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
  taskno <- c()             #trialsNum
  participant <- c()
  time <- c()
  
  # remove empty lines:
  df <- df[which(!is.na(df$trialsNum)),]
  #df <- df[which(df$trialsNum == 2),]
  
  # loop through all trials
  #plot(x,y,type='l',col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
  for (trialnum in c(1:dim(df)[1])) {
    
    s <- convertCellToNumVector(df$step[trialnum])
    m <- df$trialsType[trialnum]
    a <- df$targetangle_deg[trialnum]
    p <- df$participant[trialnum]
    t <- convertCellToNumVector(df$trialMouse.time[trialnum])
    
    # remove stuff that is not step==2
    stepidx <- which(s == step)
    t <- t[stepidx]
    startt <- t[1]
    endt <- t[length(t)]
    mt <- endt - startt
    
    # store in vectors:
    trialno <- c(trialno, trialnum)
    targetangle_deg <- c(targetangle_deg, a)
    trialtype <-c(trialtype, m)
    taskno <- c(taskno, df$trialsNum[trialnum])
    participant <- c(participant, p)
    time <- c(time, mt)
  }
  
  # vectors as data frame columns:
  dfrd <- data.frame(trialno, targetangle_deg, trialtype, taskno, participant, time)
  
  #add in group identifiers
  targetdist <- c()
  for (target in dfrd$targetangle_deg){
    #group targets by how far each one is from mirror (far, mid, near)
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
  dfrd$targetdist <- targetdist
  
  return(dfrd)
}

getGroupCtrlMT <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      alldat <- handleOneMTCtrlFile(filename = datafilename)
      # per target location, get reachdev for corresponding trials
      
      trial <- c(1:length(alldat$trialno))
      alldat$trialno <- trial
      for (triali in trial){
        trialdat <- alldat[which(alldat$trialno == triali),]
        #set reachdev to NA if not the target location we want
        if (trialdat$targetdist != group){
          trialdat$time <- NA
        }
        alldat[triali,] <- trialdat
      }
      ppmt <- alldat$time #get reach deviations column from learning curve data
      ppdat <- data.frame(trial, ppmt)
      
      ppname <- unique(alldat$participant)
      names(ppdat)[names(ppdat) == 'ppmt'] <- ppname
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- ppdat
      } else {
        dataoutput <- cbind(dataoutput, ppmt)
        names(dataoutput)[names(dataoutput) == 'ppmt'] <- ppname
      }
    }
    
    #outlier removal
    for (trialno in dataoutput$trial){
      #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
      ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
      #print(max(ndat, na.rm=T))
      trialmu <- mean(ndat, na.rm = TRUE)
      trialsigma <- sd(ndat, na.rm = TRUE)
      #print(trialsigma)
      trialclip <- abs(trialmu) + (trialsigma * 2)
      
      ndat[which(abs(ndat) > trialclip)] <- NA
      
      dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
    }
    
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime.csv', group), row.names = F)
    #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
  }
}

getGroupCtrlMTCI <- function(groups = c('far','mid', 'near'), type = 't'){
  for(group in groups){

    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    confidence <- data.frame()
    
    
    for (trial in trialno){
      
      cireaches <- data1[which(data$trial == trial), ]
      
      if(length(unique(cireaches)) == 1){ #deal with trials with no data at all
        citrial <- as.numeric(c(NA,NA,NA))
      } else{
        if (type == "t"){
          cireaches <- cireaches[!is.na(cireaches)]
          citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
        } else if(type == "b"){
          citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
        }
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime_CI.csv', group), row.names = F) 
    }
  }
}

plotCtrlMT <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  #but we can save plot as svg file
  if (target=='svg'){
    svglite(file='data/controlmironline-master/doc/fig/Fig2_MovementTime.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(0,178), ylim = c(-0.2,11), 
       xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  abline(h = c(0, 1), v = c(45, 66, 156), col = 8, lty = 2)
  axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
  axis(2, at = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), las = 2) #tick marks for y axis
  
  for(group in groups){
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime_CI.csv', group))
    
    
    #split up data set for plotting purposes
    groupconfidenceAligned <- groupconfidence[1:45,]
    groupconfidenceLeftAligned <- groupconfidence[46:66,]
    groupconfidenceLC <- groupconfidence[67:156,]
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
    lower <- groupconfidenceLC[,1]
    upper <- groupconfidenceLC[,3]
    mid <- groupconfidenceLC[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(67:156), rev(c(67:156))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(67:156), y = mid,col=col,lty=1)
    
    #plot Washout Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceRAE[,1]
    upper <- groupconfidenceRAE[,3]
    mid <- groupconfidenceRAE[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(157:177), rev(c(157:177))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(157:177), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(5,10,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Path Length-----
handleOnePLCtrlFile <- function(filename, step = 2) {
  
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
  taskno <- c()             #trialsNum
  participant <- c()
  path_length <- c()
  
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
    stepidx <- which(s == step)
    x <- x[stepidx]
    y <- y[stepidx]
    
    #path length calculated below
    pl <- c()
    for (i in 1:length(stepidx)){
      if (i == 1){
        sampx <- x[i]
        sampy <- y[i]
        absvec <- sqrt(((sampx)^2)+((sampy)^2))
      } else {
        sampx <- x[i] - x[i-1]
        sampy <- y[i] - y[i-1]
        absvec <- sqrt(((sampx)^2)+((sampy)^2))
      }
      pl <- c(pl,absvec)
    }
    pathlength <- sum(pl)
    
    # store in vectors:
    trialno <- c(trialno, trialnum)
    targetangle_deg <- c(targetangle_deg, a)
    trialtype <-c(trialtype, m)
    taskno <- c(taskno, df$trialsNum[trialnum])
    participant <- c(participant, p)
    path_length <- c(path_length, pathlength)
  }
  
  # vectors as data frame columns:
  dfrd <- data.frame(trialno, targetangle_deg, trialtype, taskno, participant, path_length)
  
  #add in group identifiers
  targetdist <- c()
  for (target in dfrd$targetangle_deg){
    #group targets by how far each one is from mirror (far, mid, near)
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
  dfrd$targetdist <- targetdist
  
  return(dfrd)
}

getGroupCtrlPL <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      alldat <- handleOnePLCtrlFile(filename = datafilename)
      # per target location, get reachdev for corresponding trials
      
      trial <- c(1:length(alldat$trialno))
      alldat$trialno <- trial
      for (triali in trial){
        trialdat <- alldat[which(alldat$trialno == triali),]
        #set reachdev to NA if not the target location we want
        if (trialdat$targetdist != group){
          trialdat$path_length <- NA
        }
        alldat[triali,] <- trialdat
      }
      pppath <- alldat$path_length #get reach deviations column from learning curve data
      ppdat <- data.frame(trial, pppath)
      
      ppname <- unique(alldat$participant)
      names(ppdat)[names(ppdat) == 'pppath'] <- ppname
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- ppdat
      } else {
        dataoutput <- cbind(dataoutput, pppath)
        names(dataoutput)[names(dataoutput) == 'pppath'] <- ppname
      }
    }
    
    #outlier removal
    for (trialno in dataoutput$trial){
      #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
      ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
      #print(max(ndat, na.rm=T))
      trialmu <- mean(ndat, na.rm = TRUE)
      trialsigma <- sd(ndat, na.rm = TRUE)
      #print(trialsigma)
      trialclip <- abs(trialmu) + (trialsigma * 2)
      
      ndat[which(abs(ndat) > trialclip)] <- NA
      
      dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
    }
    
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/controlmironline-master/data/processed/%s_PathLength.csv', group), row.names = F)
    #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
  }
}

getGroupCtrlPLCI <- function(groups = c('far','mid', 'near'), type = 't'){
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_PathLength.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    confidence <- data.frame()
    
    
    for (trial in trialno){
      
      cireaches <- data1[which(data$trial == trial), ]
      
      if(length(unique(cireaches)) == 1){ #deal with trials with no data at all
        citrial <- as.numeric(c(NA,NA,NA))
      } else{
        if (type == "t"){
          cireaches <- cireaches[!is.na(cireaches)]
          citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
        } else if(type == "b"){
          citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
        }
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_PathLength_CI.csv', group), row.names = F) 
    }
  }
}

plotCtrlPL <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  #but we can save plot as svg file
  if (target=='svg'){
    svglite(file='data/controlmironline-master/doc/fig/Fig3_PathLength.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(0,178), ylim = c(-0.2,4), 
       xlab = "Trial", ylab = "Path Length (monitor scale)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  abline(h = c(0, 0.4), v = c(45, 66, 156), col = 8, lty = 2)
  axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
  axis(2, at = c(0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4), las = 2) #tick marks for y axis
  
  for(group in groups){
    groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_PathLength_CI.csv', group))
    
    
    #split up data set for plotting purposes
    groupconfidenceAligned <- groupconfidence[1:45,]
    groupconfidenceLeftAligned <- groupconfidence[46:66,]
    groupconfidenceLC <- groupconfidence[67:156,]
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
    lower <- groupconfidenceLC[,1]
    upper <- groupconfidenceLC[,3]
    mid <- groupconfidenceLC[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(67:156), rev(c(67:156))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(67:156), y = mid,col=col,lty=1)
    
    #plot Washout Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceRAE[,1]
    upper <- groupconfidenceRAE[,3]
    mid <- groupconfidenceRAE[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(157:177), rev(c(157:177))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(157:177), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(5,4,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Heatmaps and Individual data plots----
plotIndividualCtrl <- function(groups = c('far', 'mid', 'near'), target='inline'){
  
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig1D_%s_IndividualAllTasks.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    
    data<- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE)
    data_AL <- data[1:45,]
    data_LeftAL <- data[46:66,]
    data_MIR<- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE)
    data_RAE<- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), check.names = FALSE)
    
    plot(NA, NA, xlim = c(0,178), ylim = c(-200,225), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Individual rate of learning (%s target)", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    if (group == 'far'){
      abline(h = c(0, 170), col = 8, lty = 2) #creates horizontal dashed lines through y
    } else if (group == 'mid'){
      abline(h = c(0, 90), col = 8, lty = 2)
    } else if (group == 'near'){
      abline(h = c(0, 10), col = 8, lty = 2)
    }
    abline(v = c(45, 66, 156), col = 8, lty = 2)
    axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
    axis(2, at = c(-170, -130, -90, -60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
    
    #aligned trials
    mean_AL <- c()
    for (triali in data_AL$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(data_AL[triali,2:ncol(data_AL)])
      X <- rep(triali, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_AL <- c(mean_AL, Ymean)
    }
    lines(x=c(1:45), y=mean_AL, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_CI.csv', group))
    circmean_AL <- dat_CI[1:45,2]
    lines(x=c(1:45), y=circmean_AL, col='red', lw=2)
    
    #left aligned trials
    mean_LeftAL <- c()
    for (triali in data_LeftAL$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(data_LeftAL[which(data_LeftAL$trial == triali),2:ncol(data_LeftAL)]) #used which here because left aligned is from same file as aligned
      X <- rep(triali, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_LeftAL <- c(mean_LeftAL, Ymean)
    }
    lines(x=c(46:66), y=mean_LeftAL, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_CI.csv', group))
    circmean_AL <- dat_CI[46:66,2]
    lines(x=c(46:66), y=circmean_AL, col='red', lw=2)
    
    #mirrored trials
    mean_MIR <- c()
    for (triali in data_MIR$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(data_MIR[triali,2:ncol(data_MIR)])
      X <- rep(triali + 66, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_MIR <- c(mean_MIR, Ymean)
    }
    lines(x=c(67:156), y=mean_MIR, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl_CI.csv', group))
    circmean_MIR <- dat_CI[,2]
    lines(x=c(67:156), y=circmean_MIR, col='red', lw=2)
    
    
    #washout trials
    mean_RAE <- c()
    for (triali in data_RAE$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(data_RAE[triali,2:ncol(data_RAE)])
      X <- rep(triali + 156, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_RAE <- c(mean_RAE, Ymean)
    }
    lines(x=c(157:177), y=mean_RAE, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_CI.csv', group))
    circmean_RAE <- dat_CI[,2]
    lines(x=c(157:177), y=circmean_RAE, col='red', lw=2)
    
    
    legend(0,-160,legend=c('circular mean','numeric mean'),
           col=c('red','orange'),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  } #end for loop
} 

plotCtrlHeatmaps <- function(groups = c('far', 'mid', 'near'), target = 'inline'){
  for(group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig1E_%s_Heatmap.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    dat<- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE)
    data_AL <- dat[1:45,]
    data_LeftAL <- dat[46:66,]
    data_MIR<- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE)
    data_RAE<- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), check.names = FALSE)
    data <- rbind(data_AL, data_LeftAL, data_MIR, data_RAE)
    
    interval <- seq(-200, 200, 10) #group ang devs in bins of 10 degrees each, -200 and 200 due to some values above 180
    alldat <- c()
    for(triali in 1:length(data$trial)){
      subdat <- data[triali, 2:ncol(data)]
      subdat <- na.omit(as.numeric(subdat)) #only want to count those without NA values
      #identify which bin the value corresponds to
      binfreq <- c()
      for(numi in subdat){
        freq <- findInterval(numi, interval, left.open = TRUE) #left.open means interval is from -190 to -199.9, -180 to -189.9, etc.
        binfreq <- c(binfreq, freq)
      }
      
      #identify counts/frequency per bin
      yint <- seq(0,40,1) #bins go from 0 to 40, because we go with groups of 10 degrees. 0 and 40 are any values outside of (-200, 200)
      bincount <- c()
      for(bini in yint){
        count <- sum(binfreq == bini)
        bincount <- c(bincount, count)
      }
      
      trial <- rep(triali, length(yint))
      ndat <- data.frame(trial, yint, bincount)
      
      #append new trials
      alldat <- rbind(alldat, ndat)
    }
    
    #add column converting bin number to angles
    alldat$angles <- rep(interval, len = length(alldat$yint))
    
    #plot heatmap (use levelplot from lattice package)
    X <- alldat$trial
    Y <- alldat$angles
    Z <- alldat$bincount
    col <- colorRampPalette(brewer.pal(9, "PuBu"))
    xscale <- list(at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177), cex = 1.5) #tick marks for x-axis
    yscale <- list(at = c(-170, -130, -90, -60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), cex = 1.5) #tick marks for y-axis
    ckey <- list(labels = list(cex = 1.5)) #for colour key
    fig <- levelplot(Z~X*Y, main = list(sprintf("%s target: Heatmap of angular reach deviations (bin size = 10°)", group), cex = 1.5), xlab = list('Trial', cex = 1.5), ylab = list('Angular reach deviation (°)', cex = 1.5),
                     colorkey = ckey, col.regions = col,
                     scales = list(tck = c(1,0), x = xscale, y = yscale),
                     panel = function(...){
                       panel.levelplot(...)
                       panel.abline(v = c(45, 66, 156), col = 8, lty = 2)
                       if(group == 'far'){
                         panel.abline(h = c(0, 170), col = 8, lty = 2)
                       } else if (group == 'mid'){
                         panel.abline(h = c(0, 90), col = 8, lty = 2)
                       } else if (group == 'near'){
                         panel.abline(h = c(0, 10), col = 8, lty = 2)
                       }
                     })
    print(fig)
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

# Comparing devices used: Mouse vs trackpad----

getDeviceCtrlCI<- function(groups = c('far', 'mid', 'near'), device, task){
  
  for(group in groups){
    #get qualtrics response to device used
    qualtdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q6.4 == device),]
    ppqualt <- devqualt$id
    
    if(task == "Aligned"){
      #keep only data of pp from this list
      #Aligned
      dat_AL <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
      trial <- dat_AL$trial
      ndat <- dat_AL[,which(colnames(dat_AL) %in% ppqualt)]
      data <- cbind(trial, ndat)
    } else if(task == "Mirror"){
      #Mirror
      dat_MR <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE)
      trial <- dat_MR$trial
      ndat <- dat_MR[,which(colnames(dat_MR) %in% ppqualt)]
      data <- cbind(trial, ndat)
    } else if(task == "RAE"){
      #RAE
      dat_RAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), check.names = FALSE)
      trial <- dat_RAE$trial
      ndat <- dat_RAE[,which(colnames(dat_RAE) %in% ppqualt)]
      data <- cbind(trial, ndat)
    }
    
    #get CIs
    trialno <- data$trial
    
    confidence <- data.frame()
    
    for(trial in trialno){
      subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      citrial <- getAngularReachDevsCI(data = subdat, group = group)
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_%sCtrl_CI_%s.csv', group, task, device), row.names = F)
      
    }
  }
}

getDeviceCtrlCircularCI<- function(groups = c('far', 'mid', 'near'), device, task){
  
  for(group in groups){
    #get qualtrics response to device used
    qualtdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q6.4 == device),]
    ppqualt <- devqualt$id
    
    if(task == "Aligned"){
      #keep only data of pp from this list
      #Aligned
      dat_AL <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
      trial <- dat_AL$trial
      ndat <- dat_AL[,which(colnames(dat_AL) %in% ppqualt)]
      data <- cbind(trial, ndat)
    } else if(task == "Mirror"){
      #Mirror
      dat_MR <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE)
      trial <- dat_MR$trial
      ndat <- dat_MR[,which(colnames(dat_MR) %in% ppqualt)]
      data <- cbind(trial, ndat)
    } else if(task == "RAE"){
      #RAE
      dat_RAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), check.names = FALSE)
      trial <- dat_RAE$trial
      ndat <- dat_RAE[,which(colnames(dat_RAE) %in% ppqualt)]
      data <- cbind(trial, ndat)
    }
    
    #get CIs
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
      
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_%sCtrl_Circular_CI_%s.csv', group, task, device), row.names = F)
      
    }
  }
}

plotDeviceCtrl <- function(groups = c('far', 'mid', 'near'), devices = c('Mouse','Trackpad'), target='inline') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig4_%s_DeviceCtrl.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    plot(NA, NA, xlim = c(0,178), ylim = c(-200,200), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    if(group == 'far'){
      abline(h = c(0, 170), v = c(45, 66, 156), col = 8, lty = 2)
    } else if(group == 'mid'){
      abline(h = c(0, 90), v = c(45, 66, 156), col = 8, lty = 2)
    } else if(group == 'near'){
      abline(h = c(0, 10), v = c(45, 66, 156), col = 8, lty = 2)
    }
    
    axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
    axis(2, at = c(-170, -130, -90, -60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
    
    for(device in devices){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_CI_%s.csv', group, device))
      groupconfidenceAligned <- groupconfidence[c(1:45),]
      groupconfidenceLeftAligned <- groupconfidence[c(46:66),]
      groupconfidenceLC <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirrorCtrl_CI_%s.csv', group, device))
      groupconfidenceRAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_CI_%s.csv', group, device))
      
      
      
      colourscheme <- getDeviceColourScheme(devices = device)
      
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:45), rev(c(1:45))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(1:45), y = na.omit(mid),col=col,lty=1)
      
      #plot Left(switch hand) Aligned data
      #take only first, last and middle columns of file
      lower <- groupconfidenceLeftAligned[,1]
      upper <- groupconfidenceLeftAligned[,3]
      mid <- groupconfidenceLeftAligned[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(46:66), rev(c(46:66))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(46:66), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(67:156), rev(c(67:156))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(67:156), y = na.omit(mid),col=col,lty=1)
      
      #plot Washout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(157:177), rev(c(157:177))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(157:177), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(1,-90,legend=c('Mouse','Trackpad'),
           col=c(colourscheme[['Mouse']][['S']],colourscheme[['Trackpad']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
}

plotDeviceCtrlCircular <- function(groups = c('far', 'mid', 'near'), devices = c('Mouse','Trackpad'), target='inline') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig4_%s_DeviceCtrlCircular.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    plot(NA, NA, xlim = c(0,178), ylim = c(-200,200), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    if(group == 'far'){
      abline(h = c(0, 170), v = c(45, 66, 156), col = 8, lty = 2)
    } else if(group == 'mid'){
      abline(h = c(0, 90), v = c(45, 66, 156), col = 8, lty = 2)
    } else if(group == 'near'){
      abline(h = c(0, 10), v = c(45, 66, 156), col = 8, lty = 2)
    }
    
    axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
    axis(2, at = c(-170, -130, -90, -60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
    
    for(device in devices){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_Circular_CI_%s.csv', group, device))
      groupconfidenceAligned <- groupconfidence[c(1:45),]
      groupconfidenceLeftAligned <- groupconfidence[c(46:66),]
      groupconfidenceLC <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirrorCtrl_Circular_CI_%s.csv', group, device))
      groupconfidenceRAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_Circular_CI_%s.csv', group, device))
      
      
      
      colourscheme <- getDeviceColourScheme(devices = device)
      
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:45), rev(c(1:45))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(1:45), y = na.omit(mid),col=col,lty=1)
      
      #plot Left(switch hand) Aligned data
      #take only first, last and middle columns of file
      lower <- groupconfidenceLeftAligned[,1]
      upper <- groupconfidenceLeftAligned[,3]
      mid <- groupconfidenceLeftAligned[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(46:66), rev(c(46:66))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(46:66), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(67:156), rev(c(67:156))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(67:156), y = na.omit(mid),col=col,lty=1)
      
      #plot Washout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(157:177), rev(c(157:177))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(157:177), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(1,-90,legend=c('Mouse','Trackpad'),
           col=c(colourscheme[['Mouse']][['S']],colourscheme[['Trackpad']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
}

getDeviceCtrlMTCI<- function(groups = c('far', 'mid', 'near'), device, type = 't'){
  
  for(group in groups){
    #get qualtrics response to device used
    qualtdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q6.4 == device),]
    ppqualt <- devqualt$id
    
    
    dat <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    trial <- dat$trial
    ndat <- dat[,which(colnames(dat) %in% ppqualt)]
    data <- cbind(trial, ndat)
    
    
    #get CIs
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    confidence <- data.frame()
    
    
    for (trial in trialno){
      
      cireaches <- data1[which(data$trial == trial), ]
      
      if(length(unique(cireaches)) == 1){ #splitting by device creates way too small a sample. Some trials will only have NA, or 1 or 2 values (but we need to create CIs for 3 or more values)
        citrial <- as.numeric(c(NA,NA,NA)) #only a problem with Mouse device
      } else if(length(unique(cireaches)) == 2 | length(unique(cireaches)) == 3){ #potential fix of just getting the mean of these 1 or 2 values and setting that as upper and lower bounds
        cireaches <- cireaches[!is.na(cireaches)]
        cireaches <- mean(cireaches)
        citrial <- as.numeric(c(cireaches, cireaches, cireaches))
      } else{
        if (type == "t"){
          cireaches <- cireaches[!is.na(cireaches)]
          citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
        } else if(type == "b"){
          citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
        }
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime_CI_%s.csv', group, device), row.names = F) 
    }
  }
}

plotDeviceCtrlMT <- function(groups = c('far', 'mid', 'near'), devices = c('Mouse','Trackpad'), target='inline') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig4A_%s_DeviceMT.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    plot(NA, NA, xlim = c(0,178), ylim = c(-0.2,11), 
         xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf('%s target', group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    abline(h = c(0, 1), v = c(45, 66, 156), col = 8, lty = 2)
    axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
    axis(2, at = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), las = 2) #tick marks for y axis
    
    for(device in devices){
      groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime_CI_%s.csv', group, device))
      
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:45,]
      groupconfidenceLeftAligned <- groupconfidence[46:66,]
      groupconfidenceLC <- groupconfidence[67:156,]
      groupconfidenceRAE <- groupconfidence[157:177,] 
      
      colourscheme <- getDeviceColourScheme(devices = device)
      
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:45), rev(c(1:45))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(1:45), y = mid,col=col,lty=1)
      
      #plot Left Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceLeftAligned[,1]
      upper <- groupconfidenceLeftAligned[,3]
      mid <- groupconfidenceLeftAligned[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(46:66), rev(c(46:66))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(46:66), y = mid,col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(67:156), rev(c(67:156))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(67:156), y = mid,col=col,lty=1)
      
      #plot Washout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial number, y depends on values of bounds
      polygon(x = c(c(157:177), rev(c(157:177))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(157:177), y = mid,col=col,lty=1)
    }
    
    #add legend
    legend(0,6,legend=c('Mouse','Trackpad'),
           col=c(colourscheme[['Mouse']][['S']],colourscheme[['Trackpad']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
}

getDeviceCtrlPLCI<- function(groups = c('far', 'mid', 'near'), device, type = 't'){
  
  for(group in groups){
    #get qualtrics response to device used
    qualtdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q6.4 == device),]
    ppqualt <- devqualt$id
    
    
    dat <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_PathLength.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    trial <- dat$trial
    ndat <- dat[,which(colnames(dat) %in% ppqualt)]
    data <- cbind(trial, ndat)
    
    
    #get CIs
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    confidence <- data.frame()
    
    
    for (trial in trialno){
      
      cireaches <- data1[which(data$trial == trial), ]
      
      if(length(unique(cireaches)) == 1){ #splitting by device creates way too small a sample. Some trials will only have NA, or 1 or 2 values (but we need to create CIs for 3 or more values)
        citrial <- as.numeric(c(NA,NA,NA)) #only a problem with Mouse device
      } else if(length(unique(cireaches)) == 2 | length(unique(cireaches)) == 3){ #potential fix of just getting the mean of these 1 or 2 values and setting that as upper and lower bounds
        cireaches <- cireaches[!is.na(cireaches)]
        cireaches <- mean(cireaches)
        citrial <- as.numeric(c(cireaches, cireaches, cireaches))
      } else{
        if (type == "t"){
          cireaches <- cireaches[!is.na(cireaches)]
          citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
        } else if(type == "b"){
          citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
        }
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_PathLength_CI_%s.csv', group, device), row.names = F) 
    }
  }
}

plotDeviceCtrlPL <- function(groups = c('far', 'mid', 'near'), devices = c('Mouse','Trackpad'), target='inline') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig4B_%s_DevicePL.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    plot(NA, NA, xlim = c(0,178), ylim = c(-0.2,4), 
         xlab = "Trial", ylab = "Path Length (monitor scale)", frame.plot = FALSE, #frame.plot takes away borders
         main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    abline(h = c(0, 0.4), v = c(45, 66, 156), col = 8, lty = 2)
    axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
    axis(2, at = c(0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4), las = 2) #tick marks for y axis
    
    for(device in devices){
      groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_PathLength_CI_%s.csv', group, device))
      
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:45,]
      groupconfidenceLeftAligned <- groupconfidence[46:66,]
      groupconfidenceLC <- groupconfidence[67:156,]
      groupconfidenceRAE <- groupconfidence[157:177,] 
      
      colourscheme <- getDeviceColourScheme(devices = device)
      
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:45), rev(c(1:45))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(1:45), y = mid,col=col,lty=1)
      
      #plot Left Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceLeftAligned[,1]
      upper <- groupconfidenceLeftAligned[,3]
      mid <- groupconfidenceLeftAligned[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(46:66), rev(c(46:66))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(46:66), y = mid,col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(67:156), rev(c(67:156))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(67:156), y = mid,col=col,lty=1)
      
      #plot Washout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial number, y depends on values of bounds
      polygon(x = c(c(157:177), rev(c(157:177))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(157:177), y = mid,col=col,lty=1)
    }
    
    #add legend
    legend(0,2.5,legend=c('Mouse','Trackpad'),
           col=c(colourscheme[['Mouse']][['S']],colourscheme[['Trackpad']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
}

#Comparing sexes: Males vs females----
getSexCtrlCI<- function(groups = c('far', 'mid', 'near'), sex, task){
  
  for(group in groups){
    #get qualtrics response to device used
    qualtdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q2.2 == sex),] #one answered prefer not to say
    ppqualt <- devqualt$id
    
    if(task == "Aligned"){
      #keep only data of pp from this list
      #Aligned
      dat_AL <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
      trial <- dat_AL$trial
      ndat <- dat_AL[,which(colnames(dat_AL) %in% ppqualt)]
      data <- cbind(trial, ndat)
    } else if(task == "Mirror"){
      #Mirror
      dat_MR <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE)
      trial <- dat_MR$trial
      ndat <- dat_MR[,which(colnames(dat_MR) %in% ppqualt)]
      data <- cbind(trial, ndat)
    } else if(task == "RAE"){
      #RAE
      dat_RAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), check.names = FALSE)
      trial <- dat_RAE$trial
      ndat <- dat_RAE[,which(colnames(dat_RAE) %in% ppqualt)]
      data <- cbind(trial, ndat)
    }
    
    #get CIs
    trialno <- data$trial
    
    confidence <- data.frame()
    
    for(trial in trialno){
      subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      citrial <- getAngularReachDevsCI(data = subdat, group = group)
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_%sCtrl_CI_%s.csv', group, task, sex), row.names = F)
      
    }
  }
}

getSexCtrlCircularCI<- function(groups = c('far', 'mid', 'near'), sex, task){
  
  for(group in groups){
    #get qualtrics response to device used
    qualtdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q2.2 == sex),] #one answered prefer not to say
    ppqualt <- devqualt$id
    
    if(task == "Aligned"){
      #keep only data of pp from this list
      #Aligned
      dat_AL <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
      trial <- dat_AL$trial
      ndat <- dat_AL[,which(colnames(dat_AL) %in% ppqualt)]
      data <- cbind(trial, ndat)
    } else if(task == "Mirror"){
      #Mirror
      dat_MR <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE)
      trial <- dat_MR$trial
      ndat <- dat_MR[,which(colnames(dat_MR) %in% ppqualt)]
      data <- cbind(trial, ndat)
    } else if(task == "RAE"){
      #RAE
      dat_RAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl.csv', group), check.names = FALSE)
      trial <- dat_RAE$trial
      ndat <- dat_RAE[,which(colnames(dat_RAE) %in% ppqualt)]
      data <- cbind(trial, ndat)
    }
    
    #get CIs
    trialno <- data$trial
    
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      
      if(length(unique(circ_subdat)) == 1){ #deal with trials with no data at all
        citrial <- as.numeric(c(NA,NA,NA))
      } else{
        citrial <- getCircularConfidenceInterval(data = circ_subdat) #this automatically sets lower and upper same as mean if not enough values
        citrial <- as.numeric(citrial)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_%sCtrl_Circular_CI_%s.csv', group, task, sex), row.names = F)
      
    }
  }
}

plotSexCtrl <- function(groups = c('far', 'mid', 'near'), sexes = c('Male','Female'), target='inline') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig5_%s_SexCtrl.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    plot(NA, NA, xlim = c(0,178), ylim = c(-200,200), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    if(group == 'far'){
      abline(h = c(0, 170), v = c(45, 66, 156), col = 8, lty = 2)
    } else if(group == 'mid'){
      abline(h = c(0, 90), v = c(45, 66, 156), col = 8, lty = 2)
    } else if(group == 'near'){
      abline(h = c(0, 10), v = c(45, 66, 156), col = 8, lty = 2)
    }
    
    axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
    axis(2, at = c(-170, -130, -90, -60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
    
    for(sex in sexes){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_CI_%s.csv', group, sex))
      groupconfidenceAligned <- groupconfidence[c(1:45),]
      groupconfidenceLeftAligned <- groupconfidence[c(46:66),]
      groupconfidenceLC <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirrorCtrl_CI_%s.csv', group, sex))
      groupconfidenceRAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_CI_%s.csv', group, sex))
      
      
      
      colourscheme <- getSexColourScheme(sexes = sex)
      
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:45), rev(c(1:45))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(1:45), y = na.omit(mid),col=col,lty=1)
      
      #plot Left(switch hand) Aligned data
      #take only first, last and middle columns of file
      lower <- groupconfidenceLeftAligned[,1]
      upper <- groupconfidenceLeftAligned[,3]
      mid <- groupconfidenceLeftAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(46:66), rev(c(46:66))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(46:66), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(67:156), rev(c(67:156))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(67:156), y = na.omit(mid),col=col,lty=1)
      
      #plot Washout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(157:177), rev(c(157:177))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(157:177), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(1,-90,legend=c('Male','Female'),
           col=c(colourscheme[['Male']][['S']],colourscheme[['Female']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
}

plotSexCtrlCircular <- function(groups = c('far', 'mid', 'near'), sexes = c('Male','Female'), target='inline') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig5_%s_SexCtrlCircular.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    plot(NA, NA, xlim = c(0,178), ylim = c(-200,200), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    if(group == 'far'){
      abline(h = c(0, 170), v = c(45, 66, 156), col = 8, lty = 2)
    } else if(group == 'mid'){
      abline(h = c(0, 90), v = c(45, 66, 156), col = 8, lty = 2)
    } else if(group == 'near'){
      abline(h = c(0, 10), v = c(45, 66, 156), col = 8, lty = 2)
    }
    
    axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
    axis(2, at = c(-170, -130, -90, -60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), las = 2) #tick marks for y axis
    
    for(sex in sexes){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl_Circular_CI_%s.csv', group, sex))
      groupconfidenceAligned <- groupconfidence[c(1:45),]
      groupconfidenceLeftAligned <- groupconfidence[c(46:66),]
      groupconfidenceLC <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirrorCtrl_Circular_CI_%s.csv', group, sex))
      groupconfidenceRAE <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_RAECtrl_Circular_CI_%s.csv', group, sex))
      
      
      
      colourscheme <- getSexColourScheme(sexes = sex)
      
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:45), rev(c(1:45))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(1:45), y = na.omit(mid),col=col,lty=1)
      
      #plot Left(switch hand) Aligned data
      #take only first, last and middle columns of file
      lower <- groupconfidenceLeftAligned[,1]
      upper <- groupconfidenceLeftAligned[,3]
      mid <- groupconfidenceLeftAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(46:66), rev(c(46:66))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(46:66), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(67:156), rev(c(67:156))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(67:156), y = na.omit(mid),col=col,lty=1)
      
      #plot Washout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(157:177), rev(c(157:177))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(157:177), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(1,-90,legend=c('Male','Female'),
           col=c(colourscheme[['Male']][['S']],colourscheme[['Female']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
}

getSexCtrlMTCI<- function(groups = c('far', 'mid', 'near'), sex, type = 't'){
  
  for(group in groups){
    #get qualtrics response to device used
    qualtdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q2.2 == sex),]
    ppqualt <- devqualt$id
    
    
    dat <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    trial <- dat$trial
    ndat <- dat[,which(colnames(dat) %in% ppqualt)]
    data <- cbind(trial, ndat)
    
    
    #get CIs
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    confidence <- data.frame()
    
    
    for (trial in trialno){
      
      cireaches <- data1[which(data$trial == trial), ]
      
      if(length(unique(cireaches)) == 1){ #splitting by device creates way too small a sample. Some trials will only have NA, or 1 or 2 values (but we need to create CIs for 3 or more values)
        citrial <- as.numeric(c(NA,NA,NA)) #only a problem with Males
      } else if(length(unique(cireaches)) == 2 | length(unique(cireaches)) == 3){ #potential fix of just getting the mean of these 1 or 2 values and setting that as upper and lower bounds
        cireaches <- cireaches[!is.na(cireaches)]
        cireaches <- mean(cireaches)
        citrial <- as.numeric(c(cireaches, cireaches, cireaches))
      } else{
        if (type == "t"){
          cireaches <- cireaches[!is.na(cireaches)]
          citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
        } else if(type == "b"){
          citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
        }
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime_CI_%s.csv', group, sex), row.names = F) 
    }
  }
}

plotSexCtrlMT <- function(groups = c('far', 'mid', 'near'), sexes = c('Male','Female'), target='inline') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig5A_%s_SexMT.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    plot(NA, NA, xlim = c(0,178), ylim = c(-0.2,11), 
         xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf('%s target', group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    abline(h = c(0, 1), v = c(45, 66, 156), col = 8, lty = 2)
    axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
    axis(2, at = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), las = 2) #tick marks for y axis
    
    for(sex in sexes){
      groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MovementTime_CI_%s.csv', group, sex))
      
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:45,]
      groupconfidenceLeftAligned <- groupconfidence[46:66,]
      groupconfidenceLC <- groupconfidence[67:156,]
      groupconfidenceRAE <- groupconfidence[157:177,] 
      
      colourscheme <- getSexColourScheme(sexes = sex)
      
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:45), rev(c(1:45))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(1:45), y = mid,col=col,lty=1)
      
      #plot Left Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceLeftAligned[,1]
      upper <- groupconfidenceLeftAligned[,3]
      mid <- groupconfidenceLeftAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(46:66), rev(c(46:66))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(46:66), y = mid,col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(67:156), rev(c(67:156))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(67:156), y = mid,col=col,lty=1)
      
      #plot Washout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial number, y depends on values of bounds
      polygon(x = c(c(157:177), rev(c(157:177))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(157:177), y = mid,col=col,lty=1)
    }
    
    #add legend
    legend(0,6,legend=c('Male','Female'),
           col=c(colourscheme[['Male']][['S']],colourscheme[['Female']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
}

getSexCtrlPLCI<- function(groups = c('far', 'mid', 'near'), sex, type = 't'){
  
  for(group in groups){
    #get qualtrics response to device used
    qualtdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q2.2 == sex),]
    ppqualt <- devqualt$id
    
    
    dat <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_PathLength.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    trial <- dat$trial
    ndat <- dat[,which(colnames(dat) %in% ppqualt)]
    data <- cbind(trial, ndat)
    
    
    #get CIs
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    confidence <- data.frame()
    
    
    for (trial in trialno){
      
      cireaches <- data1[which(data$trial == trial), ]
      
      if(length(unique(cireaches)) == 1){ #splitting by device creates way too small a sample. Some trials will only have NA, or 1 or 2 values (but we need to create CIs for 3 or more values)
        citrial <- as.numeric(c(NA,NA,NA)) #only a problem with Males
      } else if(length(unique(cireaches)) == 2 | length(unique(cireaches)) == 3){ #potential fix of just getting the mean of these 1 or 2 values and setting that as upper and lower bounds
        cireaches <- cireaches[!is.na(cireaches)]
        cireaches <- mean(cireaches)
        citrial <- as.numeric(c(cireaches, cireaches, cireaches))
      } else{
        if (type == "t"){
          cireaches <- cireaches[!is.na(cireaches)]
          citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
        } else if(type == "b"){
          citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
        }
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/controlmironline-master/data/processed/%s_PathLength_CI_%s.csv', group, sex), row.names = F) 
    }
  }
}

plotSexCtrlPL <- function(groups = c('far', 'mid', 'near'), sexes = c('Male','Female'), target='inline') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmironline-master/doc/fig/Fig5B_%s_SexPL.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    plot(NA, NA, xlim = c(0,178), ylim = c(-0.2,4), 
         xlab = "Trial", ylab = "Path Length (monitor scale)", frame.plot = FALSE, #frame.plot takes away borders
         main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    abline(h = c(0, 0.4), v = c(45, 66, 156), col = 8, lty = 2)
    axis(1, at = c(1, 25, 46, 55, 67, 95, 125, 157, 165, 177)) #tick marks for x axis
    axis(2, at = c(0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4), las = 2) #tick marks for y axis
    
    for(sex in sexes){
      groupconfidence <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_PathLength_CI_%s.csv', group, sex))
      
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:45,]
      groupconfidenceLeftAligned <- groupconfidence[46:66,]
      groupconfidenceLC <- groupconfidence[67:156,]
      groupconfidenceRAE <- groupconfidence[157:177,] 
      
      colourscheme <- getSexColourScheme(sexes = sex)
      
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:45), rev(c(1:45))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(1:45), y = mid,col=col,lty=1)
      
      #plot Left Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceLeftAligned[,1]
      upper <- groupconfidenceLeftAligned[,3]
      mid <- groupconfidenceLeftAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(46:66), rev(c(46:66))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(46:66), y = mid,col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(67:156), rev(c(67:156))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(67:156), y = mid,col=col,lty=1)
      
      #plot Washout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial number, y depends on values of bounds
      polygon(x = c(c(157:177), rev(c(157:177))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(157:177), y = mid,col=col,lty=1)
    }
    
    #add legend
    legend(0,2.5,legend=c('Male','Female'),
           col=c(colourscheme[['Male']][['S']],colourscheme[['Female']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
}

#Statistics-----
# First, we focus on ALIGNED trials: both hands
# Angular reach devs are not comparable across target locations
# We can use percentage of compensation instead

getAlignedGroupPercentCompensation <- function(groups = c('far', 'mid', 'near')){
  
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_AlignedCtrl.csv', group), check.names = FALSE)
    trialno <- data$trial
    
    for(trial in trialno){
      subdat <- as.numeric(data[trial, 2:length(data)])
      for (angleidx in 1:length(subdat)){
        angle <- subdat[angleidx]
        if(!is.na(angle) && group == 'far'){
          subdat[angleidx] <- (angle/170)*100 #full compensation for far targets is 170 deg
        } else if (!is.na(angle) && group == 'mid'){
          subdat[angleidx] <- (angle/90)*100 #full compensation for mid targets is 90 deg
        } else if(!is.na(angle) && group == 'near'){
          subdat[angleidx] <- (angle/10)*100 #full compensation for near targets is 10 deg
        }
      }
      data[trial, 2:length(data)] <- subdat
    }
    write.csv(data, file=sprintf('data/controlmironline-master/data/statistics/%s_Aligned_PercentCompensation.csv', group), row.names = F) 
  }
}

#blockdefs <- list('first'=c(1,9),'second'=c(10,9),'last'=c(37,9))
#hand is either 'trained' for the first one they use, or 'untrained' for the hand they switch to
getAlignedBlockedLearningAOV <- function(groups = c('far', 'mid', 'near'), blockdefs, hand) {
  
  LCaov <- data.frame()
  for(group in groups){
    curves <- read.csv(sprintf('data/controlmironline-master/data/statistics/%s_Aligned_PercentCompensation.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
    curves <- curves[,-1] #remove trial rows
    participants <- colnames(curves)
    N <- length(participants)
    
    #blocked <- array(NA, dim=c(N,length(blockdefs)))
    
    target <- c()
    participant <- c()
    block <- c()
    percentcomp <- c()
    
    for (ppno in c(1:N)) {
      
      pp <- participants[ppno]
      
      for (blockno in c(1:length(blockdefs))) {
        #for each participant, and every 9 trials, get the mean
        blockdef <- blockdefs[[blockno]]
        blockstart <- blockdef[1]
        blockend <- blockstart + blockdef[2] - 1
        samples <- curves[blockstart:blockend,ppno]
        samples <- mean(samples, na.rm=TRUE)
        
        target <- c(target, group)
        participant <- c(participant, pp)
        block <- c(block, names(blockdefs)[blockno])
        percentcomp <- c(percentcomp, samples)
      }
    }
    LCBlocked <- data.frame(target, participant, block, percentcomp)
    LCaov <- rbind(LCaov, LCBlocked)
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$hand <- hand
  return(LCaov)
  
}

#check target by block within each aligned period for each hand
alignedLearningANOVA <- function(hands = c('trained', 'untrained')) {
  for(hand in hands){
    if(hand == 'trained'){
      blockdefs <- list('first'=c(1,9),'second'=c(10,9),'last'=c(37,9))
    } else if(hand == 'untrained'){
      blockdefs <- list('first'=c(46,3),'second'=c(49,3),'last'=c(64,3))
    }
    
    LC4aov <- getAlignedBlockedLearningAOV(blockdefs=blockdefs, hand=hand)                      
    
    #looking into interaction below:
    interaction.plot(LC4aov$target, LC4aov$block, LC4aov$percentcomp)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=percentcomp, within= c(block, target), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Compensation during aligned trials across targets and blocks, %s hand:\n', hand))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}

#follow up: main effect of target with trained hand
trainedHandComparisonMeans <- function(hand='trained'){
  blockdefs <- list('first'=c(1,9),'second'=c(10,9),'last'=c(37,9))
  LC4aov <- getAlignedBlockedLearningAOV(blockdefs=blockdefs, hand=hand) 
  
  LC4aov <- aggregate(percentcomp ~ target* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","percentcomp",LC4aov,within="target")
  
  cellmeans <- emmeans(secondAOV,specs=c('target'))
  print(cellmeans)
  
}

trainedHandComparisons <- function(hand='trained', method='bonferroni'){
  blockdefs <- list('first'=c(1,9),'second'=c(10,9),'last'=c(37,9))
  LC4aov <- getAlignedBlockedLearningAOV(blockdefs=blockdefs, hand=hand) 
  
  LC4aov <- aggregate(percentcomp ~ target* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","percentcomp",LC4aov,within="target")
  
  #specify contrasts
  #levels of target are: far, mid, near
  farvsmid <- c(-1,1,0)
  farvsnear <- c(-1,0,1)
  midvsnear <- c(0,-1,1)
  
  contrastList <- list('Far vs. Mid'=farvsmid, 'Far vs. Near'=farvsnear, 'Mid vs. Near'=midvsnear)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('target')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
trainedHandComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- trainedHandComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#compare target and block across hands (3x3x2)
getAlignedBlockedLearningAOV2Hands <- function(handA, handB){
  LC4aov <- c()
  hands <- c(handA, handB)
  for(hand in hands){
    if(hand == 'trained'){
      blockdefs <- list('first'=c(1,9),'second'=c(10,9),'last'=c(37,9))
    } else if(hand == 'untrained'){
      blockdefs <- list('first'=c(46,3),'second'=c(49,3),'last'=c(64,3))
    }
    
    data <- getAlignedBlockedLearningAOV(blockdefs=blockdefs, hand=hand)
    LC4aov <- rbind(LC4aov, data)
  }
  
  #need to make some columns as factors for ANOVA
  LC4aov$target <- as.factor(LC4aov$target)
  LC4aov$block <- as.factor(LC4aov$block)
  LC4aov$hand <- factor(LC4aov$hand, levels = c(hands[1], hands[2])) #keeps order consistent with others
  return(LC4aov)
}

alignedLearningANOVA2Hands <- function(handA, handB) {
  
  LC4aov <- getAlignedBlockedLearningAOV2Hands(handA=handA, handB=handB)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$target, LC4aov$quadrant, LC4aov$percentcomp)
  #interaction.plot(LC4aov$block, LC4aov$quadrant, LC4aov$percentcomp)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=percentcomp, within= c(target, block, hand), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat(sprintf('Quadrants %s and %s:\n', quadrantA, quadrantB))
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

# main effect: block, hand, interaction: targetxblock, targetXhand
# targetxblock does violates sphericity and is not significant under GG
# hand can be explained by targetxhand interaction
# follow ups for main effect of block and interaction of targetxhand

#main effect of block
blockHandComparisonMeans <- function(handA = 'trained', handB = 'untrained'){
  LC4aov <- getAlignedBlockedLearningAOV2Hands(handA=handA, handB=handB)  
  
  #interaction.plot(LC4aov$block, LC4aov$hand, LC4aov$percentcomp)
  
  LC4aov <- aggregate(percentcomp ~ block* participant, data=LC4aov, FUN=mean) #this will be mean for each block, regardless of target and hand
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","percentcomp",LC4aov,within="block")
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

blockHandComparisons <- function(handA = 'trained', handB = 'untrained', method='bonferroni'){
  LC4aov <- getAlignedBlockedLearningAOV2Hands(handA=handA, handB=handB)  
  
  #interaction.plot(LC4aov$block, LC4aov$hand, LC4aov$percentcomp)
  
  LC4aov <- aggregate(percentcomp ~ block* participant, data=LC4aov, FUN=mean) #this will be mean for each block, regardless of target and hand
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","percentcomp",LC4aov,within="block")
  
  #specify contrasts
  #levels of target are: far, mid, near
  firstvssecond <- c(-1,1,0)
  firstvslast <- c(-1,0,1)
  secondvslast <- c(0,-1,1)
  
  contrastList <- list('First block vs. Second block'=firstvssecond, 'First block vs. Last block'=firstvslast, 'Second block vs. Last block'=secondvslast)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
blockHandComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- blockHandComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#interaction target x hand
handComparisonMeans <- function(handA = 'trained', handB = 'untrained'){
  LC4aov <- getAlignedBlockedLearningAOV2Hands(handA=handA, handB=handB)  
  
  LC4aov <- aggregate(percentcomp ~ target* hand* participant, data=LC4aov, FUN=mean) #regardless of block, the mean for every target within each hand
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","percentcomp",LC4aov,within=c("target", "hand"))
  
  cellmeans <- emmeans(secondAOV,specs=c('target', 'hand'))
  print(cellmeans)
  
}

handComparisons <- function(handA = 'trained', handB = 'untrained', method='bonferroni'){
  LC4aov <- getAlignedBlockedLearningAOV2Hands(handA=handA, handB=handB)   
  
  interaction.plot(LC4aov$target, LC4aov$hand, LC4aov$percentcomp)
  
  LC4aov <- aggregate(percentcomp ~ target* hand* participant, data=LC4aov, FUN=mean) #regardless of target, the mean for every block within each quadrant
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","percentcomp",LC4aov,within=c("target", "hand"))
  
  #specify contrasts
  #levels of target are: far, mid, near
  far_trainedvsfar_untrained <- c(-1,0,0,1,0,0)
  mid_trainedvmid_untrained <- c(0,-1,0,0,1,0)
  near_trainedvsnear_untrained <- c(0,0,-1,0,0,1)
  mid_trainedvsnear_trained <- c(0,-1,1,0,0,0) #we know this had a difference from the earlier test
  
  contrastList <- list('Far_trained vs. Far_untrained'=far_trainedvsfar_untrained, 'Mid_trained vs. Mid_untrained'=mid_trainedvmid_untrained, 'Near_trained vs. Near_untrained'=near_trainedvsnear_untrained,
                       'Mid_trained vs. Near_trained'=mid_trainedvsnear_trained)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('target', 'hand')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
handComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- handComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

# Next, we focus on MIRROR REVERSED TRIALS

#get far reach devs corrected
getMirrorCorrectedFarAngDevs <- function(group = 'far'){
  
  data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
  
  trialno <- data$trial
  #postrials <- c(1:21, 64:126)
  
  for(trial in trialno){
    subdat <- as.numeric(data[trial, 2:length(data)])
    
    for (angleidx in 1:length(subdat)){
      angle <- subdat[angleidx]
      if (group == 'far' && angle < -90 && !is.na(angle)){
        subdat[angleidx] <- angle + 360
      }
    }
    
    data[trial, 2:length(data)] <- subdat
  }
  return(data)
}
# convert to percent of compensation
getMirrorGroupPercentCompensation <- function(groups = c('far', 'mid', 'near')){
  
  for(group in groups){
    #far group
    if (group == 'far'){
      data <- getMirrorCorrectedFarAngDevs()
      trialno <- data$trial
      #postrials <- c(1:21, 64:126)
      
      for(trial in trialno){
        subdat <- as.numeric(data[trial, 2:length(data)])
        for (angleidx in 1:length(subdat)){
          angle <- subdat[angleidx]
          if (!is.na(angle)){
            subdat[angleidx] <- (angle/170)*100 #full compensation for far targets is 170 deg
          }
        }
        data[trial, 2:length(data)] <- subdat
      }
    } else {
      data <- read.csv(file=sprintf('data/controlmironline-master/data/processed/%s_MirCtrl.csv', group), check.names = FALSE)
      trialno <- data$trial
      #postrials <- c(1:21, 64:126)
      
      for(trial in trialno){
        subdat <- as.numeric(data[trial, 2:length(data)])
        for (angleidx in 1:length(subdat)){
          angle <- subdat[angleidx]
          if (!is.na(angle) && group == 'mid'){
            subdat[angleidx] <- (angle/90)*100 #full compensation for mid targets is 90 deg
          } else if(!is.na(angle) && group == 'near'){
            subdat[angleidx] <- (angle/10)*100 #full compensation for near targets is 10 deg
          }
        }
        data[trial, 2:length(data)] <- subdat
      }
    }
    write.csv(data, file=sprintf('data/controlmironline-master/data/statistics/%s_Mirror_PercentCompensation.csv', group), row.names = F) 
    
  }
  
}
#run tests
getMirrorBlockedLearningAOV <- function(groups = c('far', 'mid', 'near'), blockdefs) {
  
  LCaov <- data.frame()
  for(group in groups){
    curves <- read.csv(sprintf('data/controlmironline-master/data/statistics/%s_Mirror_PercentCompensation.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
    curves <- curves[,-1] #remove trial rows
    participants <- colnames(curves)
    N <- length(participants)
    
    #blocked <- array(NA, dim=c(N,length(blockdefs)))
    
    target <- c()
    participant <- c()
    block <- c()
    percentcomp <- c()
    
    for (ppno in c(1:N)) {
      
      pp <- participants[ppno]
      
      for (blockno in c(1:length(blockdefs))) {
        #for each participant, and every three trials, get the mean
        blockdef <- blockdefs[[blockno]]
        blockstart <- blockdef[1]
        blockend <- blockstart + blockdef[2] - 1
        samples <- curves[blockstart:blockend,ppno]
        samples <- mean(samples, na.rm=TRUE)
        
        target <- c(target, group)
        participant <- c(participant, pp)
        block <- c(block, names(blockdefs)[blockno])
        percentcomp <- c(percentcomp, samples)
      }
    }
    LCBlocked <- data.frame(target, participant, block, percentcomp)
    LCaov <- rbind(LCaov, LCBlocked)
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  return(LCaov)
  
}

#3x3 anova (target x block)
mirrorANOVA <- function() {
  
  
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  
  
  LC4aov <- getMirrorBlockedLearningAOV(blockdefs=blockdefs)                  
  
  #looking into interaction below:
  #interaction.plot(LC4aov$target, LC4aov$block, LC4aov$percentcomp)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=percentcomp, within= c(target, block), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  #cat(sprintf('Quadrant %s:\n', quadrant))
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

# MirrorComparisonMeans <- function(){
#   blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
#   
#   
#   LC4aov <- getMirrorBlockedLearningAOV(blockdefs=blockdefs)  
#   secondAOV <- aov_ez("participant","percentcomp",LC4aov,within=c("target", "block"))
#   
#   cellmeans <- emmeans(secondAOV,specs=c('target', 'block'))
#   print(cellmeans)
#   
# }

# MirrorComparisons <- function(method='sidak'){
#   blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
#   
#   
#   LC4aov <- getMirrorBlockedLearningAOV(blockdefs=blockdefs)  
#   secondAOV <- aov_ez("participant","percentcomp",LC4aov,within=c("target", "block"))
#   
#   #specify contrasts
#   #levels of target are: far, mid, near
#   far1vsfar2 <- c(-1,0,0,1,0,0,0,0,0)
#   far1vsfar3 <- c(-1,0,0,0,0,0,1,0,0)
#   far2vsfar3<- c(0,0,0,-1,0,0,1,0,0)
#   
#   contrastList <- list('Far first vs Far second'=far1vsfar2, 'Far first vs Far last'=far1vsfar3, 'Far second vs Far last'=far2vsfar3)
#   
#   comparisons<- contrast(emmeans(secondAOV,specs=c('target', 'block')), contrastList, adjust=method)
#   
#   print(comparisons)
#   
# }





