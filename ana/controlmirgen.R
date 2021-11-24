source('ana/shared.R')
source('ana/su&fa2020online.R')
source('ana/qualtricsdata.R')
source('ana/controlmir.R')

#pre-processing----
getParticipantMatchedParts <- function(){
  #This function states participants who are not listed in Part 1, returns null if all data in part 2 also have part 1
  #get list of id's from Part 1 data
  qualtdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  ppqualt <- qualtdat$id[-c(1)]
  
  #get all filenames in generalizaton/part 2 data
  datafilenames <- list.files('data/controlmirgenonline-master/data', pattern = '*.csv')
  ppdel <- c()
  #ppgen <- c()
  #fdat <- c()
  for (datafilenum in c(1:length(datafilenames))){
    
    filename <- sprintf('data/controlmirgenonline-master/data/%s', datafilenames[datafilenum])
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),filename))
    dat <- handleOneCtrlFile(filename = filename)
    ppdat <- unique(dat$participant)
    
    if(ppdat %in% ppqualt == FALSE){
      ppdel <- c(ppdel, ppdat)
    }
  }
  
  return(ppdel)
  
}

getCtrlMirGenQualtricsData <- function(){
  datafilenames <- list.files('data/controlmirgenonline-master/data', pattern = '*.csv')
  
  
  
  dataoutput<- c() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/controlmirgenonline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- handleOneCtrlFile(filename = datafilename)
    ppname <- unique(adat$participant)
    
    dataoutput <- c(dataoutput, ppname)
  }
  
  qualt <- read.csv('data/controlmirgenonline-master/qualtrics/ControlMir-SU2021-Part2_June 30, 2021_11.29.csv', stringsAsFactors = F)
  
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
  write.csv(alldat, file='data/controlmirgenonline-master/qualtrics/CtrlMirGen_Qualtrics_ParticipantList.csv', row.names = F)
  
}

getCtrlGenHandMatches <- function(){
  #check hand matches both within and across sessions
  datafilenames <- list.files('data/controlmirgenonline-master/data', pattern = '*.csv')
  
  #within: check if the key response they entered during the experiment, matches their qualtrics response
  allresp <- data.frame()
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/controlmirgenonline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    df <- read.csv(datafilename, stringsAsFactors = F)
    ppname <- unique(df$participant)
    p2_keyresp <- df$intrResp.keys[which(df$intrResp.keys != "")] #remove empty strings
    p2_keyresp <- paste(p2_keyresp, collapse=",") #collapse as one string
    ppresp <- data.frame(ppname, p2_keyresp) #collect with participant id
    
    allresp <- rbind(allresp, ppresp)
  }
  
  ctrlmirdat <- read.csv('data/controlmirgenonline-master/qualtrics/CtrlMirGen_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  
  qualtresp <- data.frame()
  for(pp in allresp$ppname){
    subdat <- ctrlmirdat[which(ctrlmirdat$id == pp),]
    ppname <- pp
    p2_handresp <- subdat$Q5.2 #response to hand switching
    qualtppresp <- data.frame(ppname, p2_handresp)
    
    qualtresp <- rbind(qualtresp, qualtppresp)
  }
  
  p2_handmatches <- merge(allresp, qualtresp, by='ppname')
  
  #across: check if the trained hand is consistent across sessions
  p1_ctrlmirdat <- read.csv('data/controlmironline-master/qualtrics/CtrlMir_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  
  p1_qualtresp <- data.frame()
  for(pp in allresp$ppname){
    subdat <- p1_ctrlmirdat[which(p1_ctrlmirdat$id == pp),]
    ppname <- pp
    p1_handedness <- subdat$Q2.5 #what is their handedness
    p1_comphand <- subdat$Q3.3 #which hand they typically use for controlling mouse
    p1_handresp <- subdat$Q8.2 #response to hand switching
    p1_qualtppresp <- data.frame(ppname, p1_handedness, p1_comphand, p1_handresp)
    
    p1_qualtresp <- rbind(p1_qualtresp, p1_qualtppresp)
  }
  
  handmatches <- merge(p1_qualtresp, p2_handmatches, by='ppname')
  
  write.csv(handmatches, file='data/controlmirgenonline-master/data/processed/HandMatches.csv', row.names = F)
  #These were then manually inspected to see any mismatches
}

# Time between Part 1 and Part 2----
getCtrlDateOneFile <- function(filename){
  
  #each participant would have a date of completion in their raw file
  # if the file can't be read, return empty list for now
  df <- NULL
  try(df <- read.csv(filename, stringsAsFactors = F), silent = TRUE)
  if (is.null(df)) {
    return(list())
  }
  
  date <- unique(df$date)
  id <- unique(df$participant)
  
  # vectors as data frame columns:
  dfid <- data.frame(id, date)
  
  return(dfid)
}

getCtrlGroupDates<- function(sets = c('part1', 'part2')){
  
  for (set in sets){
    if (set == 'part1'){
      datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
    } else if (set == 'part2'){
      datafilenames <- list.files('data/controlmirgenonline-master/data', pattern = '*.csv')
    }
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      if (set == 'part1'){
        datafilename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      } else if (set == 'part2'){
        datafilename <- sprintf('data/controlmirgenonline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      }
      
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      alldat <- getCtrlDateOneFile(filename = datafilename)
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- alldat
      } else {
        dataoutput <- rbind(dataoutput, alldat)
      }
    }
    #return(dataoutput)
    if (set == 'part1'){
      write.csv(dataoutput, file=sprintf('data/controlmironline-master/data/processed/%sDate.csv', set), row.names = F)
    } else if (set == 'part2'){
      write.csv(dataoutput, file=sprintf('data/controlmirgenonline-master/data/processed/%sDate.csv', set), row.names = F)
    }
  }
}

getCtrlMatchGroupDates <- function(){
  
  part1dat <- read.csv(file='data/controlmironline-master/data/processed/part1Date.csv')
  part2dat <- read.csv(file='data/controlmirgenonline-master/data/processed/part2Date.csv')
  
  dat <- merge(part1dat, part2dat, by.x = 'id', by.y = 'id')
  colnames(dat) <- c('id', 'part1_date', 'part2_date')
  
  dat$days <- as.numeric(as.Date(dat$part2_date) - as.Date(dat$part1_date))
  
  #dat[which(dat$days == 0),] #remove those who did it within a few hours
  
  return(dat)
  
}

plotCtrlDaysApart <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/controlmirgenonline-master/doc/fig/Fig0_HistDaysApart.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  dat <- getCtrlMatchGroupDates()
  width <- max(dat$days) - min(dat$days) #breaks is how many days are accounted for by each bar, so width here would be 1 day per bar
  
  hist(dat$days, breaks = width, main = 'Histogram for number of days between Parts 1 and 2',
       xlab = 'Days', ylab = 'Frequency of participants', axes=FALSE, xlim=c(0,20),ylim=c(0,45))
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
  axis(2, at = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 45), las=2) #tick marks for y axis
  
  cat(sprintf('mean: %s days apart \n',mean(dat$days)))
  cat(sprintf('sd: %s days apart \n',sd(dat$days)))
  cat(sprintf('median: %s days apart \n',median(dat$days)))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Baseline correction/ Part 2 Learning Rates & Washout----

getAlignedGroupLearningCtrlGen <- function(){
  #get part 1 aligned reach data for participants in part 2
  # participant list is based off of participants who did part 2 (less than part 1)
  qualtdat <- read.csv('data/controlmirgenonline-master/qualtrics/CtrlMirGen_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  ppqualt <- qualtdat$id[-c(1)]
  
  #get all filenames in part 1
  datafilenames <- list.files('data/controlmironline-master/data', pattern = '*.csv')
  part1dat <- data.frame()
  for (datafilenum in c(1:length(datafilenames))){
    
    filename <- sprintf('data/controlmironline-master/data/%s', datafilenames[datafilenum])
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),filename))
    dat <- handleOneCtrlFile(filename = filename)
    ppdat <- unique(dat$participant)
    
    if(ppdat %in% ppqualt){
      adat <- getParticipantLearningCtrl(filename = filename) #grab part 1 data
      adat <- adat[which(adat$taskno == 1 | adat$taskno == 2),] #grab the aligned for both hands
    }
    part1dat <- rbind(part1dat, adat)
  }
  #return(part1dat)
  write.csv(part1dat, file='data/controlmirgenonline-master/data/processed/Part1_Aligned.csv', row.names = F)
}

getMirroredParticipantLearningCtrlGen <- function(filename){
  #part 2 data
  dat <- getParticipantLearningCtrl(filename = filename)
  ppid <- unique(dat$participant) #to easily subset part 1 data later
  #split part 2 into tasks 1 (trained hand) and 2 (untrained hand) to baseline correct separately
  task1dat <- dat[which(dat$taskno == 1),]
  task2dat <- dat[which(dat$taskno == 2),]
  
  #part 1 data
  #can grab all part 1 data from previous function
  adat <- read.csv('data/controlmirgenonline-master/data/processed/Part1_Aligned.csv', stringsAsFactors = F)
  #convert data to circular
  adat$circ_rd <- as.circular(adat$circ_rd, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  adat <- adat[which(adat$participant == ppid),] #get only aligned data for participant
  #split part 1 (baseline) into tasks 1 (trained hand) and 2 (untrained hand) to baseline correct separately
  task1adat <- adat[which(adat$taskno == 1),]
  task2adat <- adat[which(adat$taskno == 2),]
  
  #Task 1: remove biases
  biases <- aggregate(circ_rd ~ targetangle_deg, data= task1adat, FUN = median.circular)
  #get only biases for locations used in mirrored (quad 1: 5, 45, 85)
  #biases <- biases[which(biases$targetangle_deg == c(5, 45, 85)),]
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'circ_rd'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    task1dat$circ_rd[which(task1dat$targetangle_deg == target)] <- task1dat$circ_rd[which(task1dat$targetangle_deg == target)] - bias
    
  }
  
  #Task 2: remove biases
  biases <- aggregate(circ_rd ~ targetangle_deg, data= task2adat, FUN = median.circular)
  #get only biases for locations used in mirrored (quad 1: 5, 45, 85)
  #biases <- biases[which(biases$targetangle_deg == c(5, 45, 85)),]
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'circ_rd'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    task2dat$circ_rd[which(task2dat$targetangle_deg == target)] <- task2dat$circ_rd[which(task2dat$targetangle_deg == target)] - bias
    
  }
  #combine baseline corrected tasks
  ndat <- rbind(task1dat, task2dat)
  return(ndat)
}
  
getMirroredGroupLearningCtrlGen <- function(groups = c('far', 'mid', 'near')){
  #group is either 'far', 'mid', 'near' in relation to mirror
  for(group in groups){
    datafilenames <- list.files('data/controlmirgenonline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmirgenonline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      mdat <- getMirroredParticipantLearningCtrlGen(filename = datafilename)
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
    write.csv(dataoutput, file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen.csv', group), row.names = F)
  }
}
  
getMirroredGroupLearningCtrlGenCI <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    
    
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
      
      write.csv(confidence, file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen_CI.csv', group), row.names = F) 
      
    }
  }
}
  
plotLearningCtrlGen <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/controlmirgenonline-master/doc/fig/Fig1_LearningCtrlGen.svg', width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,127), ylim = c(-185,185), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  lim <- par('usr')
  rect(85, lim[3]-1, 126, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(0), v = c(21, 42, 63, 84, 105), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  
  #we could color code the dashed lines at perfect compensation, but washout needs to be grey
  perfnear <- rep(10, 105) #add 5 points to either side to extend the line
  lines(x = c(1:105), y = perfnear, col = '#ff8200ff', lty = 2)
  
  perfmid <- rep(90, 105) #add 5 points to either side to extend the line
  lines(x = c(1:105), y = perfmid, col = '#e51636ff', lty = 2)
  
  perffar <- rep(170, 105) #add 5 points to either side to extend the line
  lines(x = c(1:105), y = perffar, col = '#c400c4ff', lty = 2) 
  #then add grey lines before trials
  greynear <- rep(10, 7) #7 is however many the x axis values are
  lines(x = c(-5:1), y = greynear, col = 8, lty = 2) #5 x values before 0
  greymid <- rep(90, 7) #7 is however many the x axis values are
  lines(x = c(-5:1), y = greymid, col = 8, lty = 2) #5 x values before 0
  greyfar <- rep(170, 7) #7 is however many the x axis values are
  lines(x = c(-5:1), y = greyfar, col = 8, lty = 2)
  #grey lines at washout
  greynear <- rep(10, 27) 
  lines(x = c(105:131), y = greynear, col = 8, lty = 2) 
  greymid <- rep(90, 27) 
  lines(x = c(105:131), y = greymid, col = 8, lty = 2) 
  greyfar <- rep(170, 27) 
  lines(x = c(105:131), y = greyfar, col = 8, lty = 2) 
  
  axis(1, at = c(1, 22, 43, 64, 85, 106, 126)) #tick marks for x axis
  axis(2, at = c(-170, -130, -90, -50, -10, 0, 10, 50, 90, 130, 170), las = 2) #tick marks for y axis
  axis(3, at = c(10, 32, 53, 74, 95, 116), labels = c('Q1', 'Q4', 'Q2', 'Q1', 'Q1', 'Q1'), line = -2, tick = FALSE) #tick marks for x axis
  
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen_CI.csv', group))
    
    #split up data set for plotting purposes
    groupconfidenceQ1 <- groupconfidence[1:21,]
    groupconfidenceQ4 <- (groupconfidence[22:42,])*-1 #sign flip because correction is in negative direction
    groupconfidenceQ2 <- (groupconfidence[43:63,])*-1 #sign flip
    groupconfidenceQ1A <- groupconfidence[64:84,]
    groupconfidenceSQ1 <- groupconfidence[85:105,]
    groupconfidenceWQ1 <- groupconfidence[106:126,]
    
    colourscheme <- getCtrlColourScheme(groups = group)
    #plot Q1 Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceQ1[,1]
    upper <- groupconfidenceQ1[,3]
    mid <- groupconfidenceQ1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:21), rev(c(1:21))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:21), y = mid,col=col,lty=1)
    
    #plot Q4 Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceQ4[,1]
    upper <- groupconfidenceQ4[,3]
    mid <- groupconfidenceQ4[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(22:42), rev(c(22:42))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(22:42), y = mid,col=col,lty=1)
    
    #plot Q2 Data
    lower <- groupconfidenceQ2[,1]
    upper <- groupconfidenceQ2[,3]
    mid <- groupconfidenceQ2[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(43:63), rev(c(43:63))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(43:63), y = mid,col=col,lty=1)
    
    #plot Q1A Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceQ1A[,1]
    upper <- groupconfidenceQ1A[,3]
    mid <- groupconfidenceQ1A[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(64:84), rev(c(64:84))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(64:84), y = mid,col=col,lty=1)
    
    #plot SQ1 Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceSQ1[,1]
    upper <- groupconfidenceSQ1[,3]
    mid <- groupconfidenceSQ1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(85:105), rev(c(85:105))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(85:105), y = mid,col=col,lty=1)
    
    #plot WQ1 Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceWQ1[,1]
    upper <- groupconfidenceWQ1[,3]
    mid <- groupconfidenceWQ1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(106:126), rev(c(106:126))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(106:126), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(106,-90,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#circular density distributions----
plotCtrlGenCircFreq <- function(groups = c('far', 'mid', 'near')){
  
  for(group in groups){
    
    dat <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    pdf(sprintf("data/controlmirgenonline-master/doc/fig/Distribution_%s_MirCtrlGen.pdf", group))
    
    #Quad 1
    triallist <- c(1:21)
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
    
    #Quad 4
    triallist <- c(22:42)
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
        perfcomp <- as.circular(-170, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
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
        perfcomp <- as.circular(-90, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
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
        perfcomp <- as.circular(-10, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
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
    
    #Quad 2
    triallist <- c(43:63)
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
        perfcomp <- as.circular(-170, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
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
        perfcomp <- as.circular(-90, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
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
        perfcomp <- as.circular(-10, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
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
    
    #Quad 1 top up
    triallist <- c(64:84)
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
    
    #Quad 1: Switch hands
    triallist <- c(85:105)
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,2:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #print(mean.circular(subdat, na.rm=T))
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      if(group == 'far'){
        plot(distsubdat, main = sprintf('Far target, switch hand: Trial %s', triali), plot.type = 'circle', 
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
        plot(distsubdat, main = sprintf('Mid target, switch hand: Trial %s', triali), plot.type = 'circle', 
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
        plot(distsubdat, main = sprintf('Near target, switch hand: Trial %s', triali), plot.type = 'circle', 
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
    
    #Quad 1: washout
    triallist <- c(106:126)
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,2:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #print(mean.circular(subdat, na.rm=T))
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      if(group == 'far'){
        plot(distsubdat, main = sprintf('Far target, switch hand (washout): Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        #nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        #arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('perfect compensation'),
               col=c('#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'mid'){
        plot(distsubdat, main = sprintf('Mid target, switch hand (washout): Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        #nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        #arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('perfect compensation'),
               col=c('#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('Near target, switch hand (washout): Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        #nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        #arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('perfect compensation'),
               col=c('#00FF00'),
               lty=1,bty='n',cex=1)
      }
    }
    
    dev.off()
    
  }
}

#movement time----
# handleOneMTCtrlFile() can be used for this data set too (this function is in controlmir.R and is sourced here)

getGroupCtrlGenMT <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    datafilenames <- list.files('data/controlmirgenonline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmirgenonline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
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
      ppmt <- alldat$time #get MT
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
    write.csv(dataoutput, file=sprintf('data/controlmirgenonline-master/data/processed/%s_MovementTime.csv', group), row.names = F)
    #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
  }
}

getGroupCtrlGenMTCI <- function(groups = c('far','mid', 'near'), type = 't'){
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MovementTime.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
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
      write.csv(confidence, file=sprintf('data/controlmirgenonline-master/data/processed/%s_MovementTime_CI.csv', group), row.names = F) 
    }
  }
}

plotCtrlGenMT <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  #but we can save plot as svg file
  if (target=='svg'){
    svglite(file='data/controlmirgenonline-master/doc/fig/Fig2_MovementTime.svg', width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(0,127), ylim = c(0, 7), 
       xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  lim <- par('usr')
  rect(85, lim[3]-1, 126, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(0, 1), v = c(21, 42, 63, 84, 105), col = 8, lty = 2)
  axis(1, at = c(1, 22, 43, 64, 85, 106, 126)) #tick marks for x axis
  axis(2, at = c(0, 1, 2, 3, 4, 5, 6, 7), las = 2) #tick marks for y axis
  axis(3, at = c(10, 32, 53, 74, 95, 116), labels = c('Q1', 'Q4', 'Q2', 'Q1', 'Q1', 'Q1'), line = -2, tick = FALSE) #tick marks for x axis
  
  for(group in groups){
    groupconfidence <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MovementTime_CI.csv', group))
    
    
    #split up data set for plotting purposes
    groupconfidenceQ1<- groupconfidence[1:21,]
    groupconfidenceQ4 <- groupconfidence[22:42,]
    groupconfidenceQ2 <- groupconfidence[43:63,]
    groupconfidenceQ1A <- groupconfidence[64:84,]
    groupconfidenceSQ1 <- groupconfidence[85:105,]
    groupconfidenceWQ1 <- groupconfidence[106:126,]
    
    colourscheme <- getCtrlColourScheme(groups = group)
    
    #Q1
    #take only first, last and middle columns of file
    lower <- groupconfidenceQ1[,1]
    upper <- groupconfidenceQ1[,3]
    mid <- groupconfidenceQ1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:21), rev(c(1:21))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:21), y = mid,col=col,lty=1)
    
    #Q4
    #take only first, last and middle columns of file
    lower <- groupconfidenceQ4[,1]
    upper <- groupconfidenceQ4[,3]
    mid <- groupconfidenceQ4[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(22:42), rev(c(22:42))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(22:42), y = mid,col=col,lty=1)
    
    #plot Q2
    lower <- groupconfidenceQ2[,1]
    upper <- groupconfidenceQ2[,3]
    mid <- groupconfidenceQ2[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(43:63), rev(c(43:63))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(43:63), y = mid,col=col,lty=1)
    
    #Q1A
    #take only first, last and middle columns of file
    lower <- groupconfidenceQ1A[,1]
    upper <- groupconfidenceQ1A[,3]
    mid <- groupconfidenceQ1A[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(64:84), rev(c(64:84))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(64:84), y = mid,col=col,lty=1)
    
    #SQ1
    #take only first, last and middle columns of file
    lower <- groupconfidenceSQ1[,1]
    upper <- groupconfidenceSQ1[,3]
    mid <- groupconfidenceSQ1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(85:105), rev(c(85:105))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(85:105), y = mid,col=col,lty=1)
    
    #WQ1
    #take only first, last and middle columns of file
    lower <- groupconfidenceWQ1[,1]
    upper <- groupconfidenceWQ1[,3]
    mid <- groupconfidenceWQ1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(106:126), rev(c(106:126))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(106:126), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(105,6,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#path length-----
#handleOnePLCtrlFile() is sourced here

getGroupCtrlGenPL <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    datafilenames <- list.files('data/controlmirgenonline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmirgenonline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
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
    write.csv(dataoutput, file=sprintf('data/controlmirgenonline-master/data/processed/%s_PathLength.csv', group), row.names = F)
    #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
  }
}

getGroupCtrlGenPLCI <- function(groups = c('far','mid', 'near'), type = 't'){
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_PathLength.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
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
      write.csv(confidence, file=sprintf('data/controlmirgenonline-master/data/processed/%s_PathLength_CI.csv', group), row.names = F) 
    }
  }
}

plotCtrlGenPL <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  #but we can save plot as svg file
  if (target=='svg'){
    svglite(file='data/controlmirgenonline-master/doc/fig/Fig3_PathLength.svg', width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(0,127), ylim = c(-0.2, 2), 
       xlab = "Trial", ylab = "Path length (monitor scale)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  lim <- par('usr')
  rect(85, lim[3]-1, 126, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(0, 0.4), v = c(21, 42, 63, 84, 105), col = 8, lty = 2)
  axis(1, at = c(1, 22, 43, 64, 85, 106, 126)) #tick marks for x axis
  axis(2, at = c(0, .5, 1, 1.5, 2), las = 2) #tick marks for y axis
  axis(3, at = c(10, 32, 53, 74, 95, 116), labels = c('Q1', 'Q4', 'Q2', 'Q1', 'Q1', 'Q1'), line = -2, tick = FALSE) #tick marks for x axis
  
  for(group in groups){
    groupconfidence <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_PathLength_CI.csv', group))
    
    
    #split up data set for plotting purposes
    groupconfidenceQ1<- groupconfidence[1:21,]
    groupconfidenceQ4 <- groupconfidence[22:42,]
    groupconfidenceQ2 <- groupconfidence[43:63,]
    groupconfidenceQ1A <- groupconfidence[64:84,]
    groupconfidenceSQ1 <- groupconfidence[85:105,]
    groupconfidenceWQ1 <- groupconfidence[106:126,]
    
    colourscheme <- getCtrlColourScheme(groups = group)
    
    #Q1
    #take only first, last and middle columns of file
    lower <- groupconfidenceQ1[,1]
    upper <- groupconfidenceQ1[,3]
    mid <- groupconfidenceQ1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:21), rev(c(1:21))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:21), y = mid,col=col,lty=1)
    
    #Q4
    #take only first, last and middle columns of file
    lower <- groupconfidenceQ4[,1]
    upper <- groupconfidenceQ4[,3]
    mid <- groupconfidenceQ4[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(22:42), rev(c(22:42))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(22:42), y = mid,col=col,lty=1)
    
    #plot Q2
    lower <- groupconfidenceQ2[,1]
    upper <- groupconfidenceQ2[,3]
    mid <- groupconfidenceQ2[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(43:63), rev(c(43:63))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(43:63), y = mid,col=col,lty=1)
    
    #Q1A
    #take only first, last and middle columns of file
    lower <- groupconfidenceQ1A[,1]
    upper <- groupconfidenceQ1A[,3]
    mid <- groupconfidenceQ1A[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(64:84), rev(c(64:84))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(64:84), y = mid,col=col,lty=1)
    
    #SQ1
    #take only first, last and middle columns of file
    lower <- groupconfidenceSQ1[,1]
    upper <- groupconfidenceSQ1[,3]
    mid <- groupconfidenceSQ1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(85:105), rev(c(85:105))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(85:105), y = mid,col=col,lty=1)
    
    #WQ1
    #take only first, last and middle columns of file
    lower <- groupconfidenceWQ1[,1]
    upper <- groupconfidenceWQ1[,3]
    mid <- groupconfidenceWQ1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial number, y depends on values of bounds
    polygon(x = c(c(106:126), rev(c(106:126))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(106:126), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(105,1.75,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Heatmaps and Individual data plots----
plotIndividualCtrlGen <- function(groups = c('far', 'mid', 'near'), target='inline'){
  
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmirgenonline-master/doc/fig/Fig1A_%s_IndividualAllTasks.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    
    data<- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen.csv', group), check.names = FALSE)
    
    dataQ1 <- data[1:21,]
    dataQ4 <- data[22:42,]
    dataQ2 <- data[43:63,]
    dataQ1A <- data[64:84,]
    dataSQ1 <- data[85:105,]
    dataWQ1 <- data[106:126,]
    
    plot(NA, NA, xlim = c(0,127), ylim = c(-185,185), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Individual rate of learning (%s target)", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    lim <- par('usr')
    rect(85, lim[3]-1, 126, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
    if (group == 'far'){
      abline(h = c(-170, 0, 170), col = 8, lty = 2) #creates horizontal dashed lines through y
    } else if (group == 'mid'){
      abline(h = c(-90, 0, 90), col = 8, lty = 2)
    } else if (group == 'near'){
      abline(h = c(-10, 0, 10), col = 8, lty = 2)
    }
    abline(v = c(21, 42, 63, 84, 105), col = 8, lty = 2)
    axis(1, at = c(1, 22, 43, 64, 85, 106, 126)) #tick marks for x axis
    axis(2, at = c(-170, -130, -90, -50, -10, 0, 10, 50, 90, 130, 170), las = 2) #tick marks for y axis
    axis(3, at = c(10, 32, 53, 74, 95, 116), labels = c('Q1', 'Q4', 'Q2', 'Q1', 'Q1', 'Q1'), line = -2, tick = FALSE) #tick marks for x axis
    
    #Q1
    mean_Q1 <- c()
    for (triali in dataQ1$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(dataQ1[triali,2:ncol(dataQ1)])
      X <- rep(triali, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_Q1 <- c(mean_Q1, Ymean)
    }
    lines(x=c(1:21), y=mean_Q1, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen_CI.csv', group))
    circmean_Q1 <- dat_CI[1:21,2]
    lines(x=c(1:21), y=circmean_Q1, col='red', lw=2)
    
    #Q4
    mean_Q4 <- c()
    for (triali in dataQ4$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(dataQ4[which(dataQ4$trial == triali),2:ncol(dataQ4)]) 
      X <- rep(triali, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_Q4 <- c(mean_Q4, Ymean)
    }
    lines(x=c(22:42), y=mean_Q4, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen_CI.csv', group))
    circmean_Q4 <- dat_CI[22:42,2]
    lines(x=c(22:42), y=circmean_Q4, col='red', lw=2)
    
    #Q2
    mean_Q2 <- c()
    for (triali in dataQ2$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(dataQ2[which(dataQ2$trial == triali),2:ncol(dataQ2)]) 
      X <- rep(triali, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_Q2 <- c(mean_Q2, Ymean)
    }
    lines(x=c(43:63), y=mean_Q2, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen_CI.csv', group))
    circmean_Q2 <- dat_CI[43:63,2]
    lines(x=c(43:63), y=circmean_Q2, col='red', lw=2)
    
    
    #Q1A
    mean_Q1A <- c()
    for (triali in dataQ1A$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(dataQ1A[which(dataQ1A$trial == triali),2:ncol(dataQ1A)]) 
      X <- rep(triali, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_Q1A <- c(mean_Q1A, Ymean)
    }
    lines(x=c(64:84), y=mean_Q1A, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen_CI.csv', group))
    circmean_Q1A <- dat_CI[64:84,2]
    lines(x=c(64:84), y=circmean_Q1A, col='red', lw=2)
    
    
    #SQ1
    mean_SQ1 <- c()
    for (triali in dataSQ1$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(dataSQ1[which(dataSQ1$trial == triali),2:ncol(dataSQ1)]) 
      X <- rep(triali, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_SQ1 <- c(mean_SQ1, Ymean)
    }
    lines(x=c(85:105), y=mean_SQ1, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen_CI.csv', group))
    circmean_SQ1 <- dat_CI[85:105,2]
    lines(x=c(85:105), y=circmean_SQ1, col='red', lw=2)
    
    
    #WQ1
    mean_WQ1 <- c()
    for (triali in dataWQ1$trial){
      #plot all points (numeric values, not circular)
      Y <- as.numeric(dataWQ1[which(dataWQ1$trial == triali),2:ncol(dataWQ1)]) 
      X <- rep(triali, length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col = alpha('blue', 0.1))
      
      #plot line indicating mean of data points as numeric values
      Y <- as.numeric(Y)
      Ymean <- mean(Y, na.rm = T)
      mean_WQ1 <- c(mean_WQ1, Ymean)
    }
    lines(x=c(106:126), y=mean_WQ1, col='orange', lw=2)
    #plot line indicating mean of data points as circular values
    dat_CI <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen_CI.csv', group))
    circmean_WQ1 <- dat_CI[106:126,2]
    lines(x=c(106:126), y=circmean_WQ1, col='red', lw=2)
    
    
    legend(0,-50,legend=c('circular mean','numeric mean'),
           col=c('red','orange'),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  } #end for loop
}

plotCtrlGenHeatmaps <- function(groups = c('far', 'mid', 'near'), target = 'inline'){
  for(group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/controlmirgenonline-master/doc/fig/Fig1B_%s_Heatmap.svg', group), width=14, height=8, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    data<- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_MirCtrlGen.csv', group), check.names = FALSE)
    
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
    xscale <- list(at = c(1, 22, 43, 64, 85, 106, 126), cex = 1.5) #tick marks for x-axis
    yscale <- list(at = c(-170, -130, -90, -60, -30, -20, -10, 0, 10, 20, 30, 60, 90, 130, 170), cex = 1.5) #tick marks for y-axis
    ckey <- list(labels = list(cex = 1.5)) #for colour key
    fig <- levelplot(Z~X*Y, main = list(sprintf("%s target: heatmap of angular reach deviations (bin size = 10°)", group), cex = 1.5), xlab = list('Trial', cex = 1.5), ylab = list('Angular reach deviation (°)', cex = 1.5),
                     colorkey = ckey, col.regions = col,
                     scales = list(tck = c(1,0), x = xscale, y = yscale),
                     panel = function(...){
                       panel.levelplot(...)
                       panel.abline(v = c(21, 42, 63, 84, 105), col = 8, lty = 2)
                       if(group == 'far'){
                         panel.abline(h = c(-170, 0, 170), col = 8, lty = 2)
                       } else if (group == 'mid'){
                         panel.abline(h = c(-90, 0, 90), col = 8, lty = 2)
                       } else if (group == 'near'){
                         panel.abline(h = c(-10, 0, 10), col = 8, lty = 2)
                       }
                     })
    print(fig)
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

