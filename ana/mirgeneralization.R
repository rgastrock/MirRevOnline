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

# learning rates across trials----
# Plot learning rates across all trials, including those that use the opposite hand

getParticipantLearningGen <- function(filename){
  
  #first, implement baseline correction
  #get Aligned biases
  dat <- handleOneFile(filename = filename)
  dat$circ_rd <- as.circular(dat$reachdeviation_deg, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  # this data set does not have an aligned baseline block for Part 2
  # adat <- dat[which(dat$taskno == 1), ]
  # # use cleaned baseline data (reaches in correct quadrant)
  # for (trialno in adat$trialno){
  #   #go through each trial, replace outlier values with NA
  #   subadat <- adat[trialno,]
  #   if (subadat$targetangle_deg == '30'){
  #     subadat$circ_rd[which(subadat$circ_rd < -30 | subadat$circ_rd > 60)] <- NA
  #   } else if (subadat$targetangle_deg == '60'){
  #     subadat$circ_rd[which(subadat$circ_rd < -60 | subadat$circ_rd > 30)] <- NA
  #   }
  #   adat[trialno, ] <- subadat
  # }
  # 
  # biases <- aggregate(circ_rd ~ targetangle_deg, data= adat, FUN = median.circular) 
  # 
  # mdat <- dat[which(dat$taskno == 2),]
  # 
  # for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
  #   
  #   target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
  #   bias<- biases[biasno, 'circ_rd'] #get corresponding reachdev or bias
  #   
  #   #subtract bias from reach deviation for rotated session only
  #   mdat$circ_rd[which(mdat$targetangle_deg == target)] <- mdat$circ_rd[which(mdat$targetangle_deg == target)] - bias
  #   
  # }
  # return(mdat)
  return(dat)
}

getGroupLearningGen <- function(){
  

  datafilenames <- list.files('data/mirrorgeneralization-master/data', pattern = '*.csv')
  
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){

    datafilename <- sprintf('data/mirrorgeneralization-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getParticipantLearningGen(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(mdat$trialno))
    targetangle_deg <- mdat$targetangle_deg
    ppreaches <- mdat$circ_rd #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, targetangle_deg, ppreaches)
    
    ppname <- unique(mdat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  write.csv(dataoutput, file='data/mirrorgeneralization-master/data/processed/LearningGen.csv', row.names = F)
}

getGroupLearningGenCI <- function(){

    data <- read.csv(file='data/mirrorgeneralization-master/data/processed/LearningGen.csv', check.names = FALSE) #check.names allows us to keep pp id as headers
    trialno <- data$trial
    
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 3:length(data)]) #get just the values, then make the circular again
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

      write.csv(confidence, file='data/mirrorgeneralization-master/data/processed/LearningGen_CI.csv', row.names = F) 
      
    }
  
}

plotLearningGen <- function(groups = c('far', 'near'), target='inline') {
  #groups are far from mirror axis (30, 330, 150), or near mirror axis (60, 300, 120)
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/mirrorgeneralization-master/doc/fig/Fig1_LearningGen.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,121), ylim = c(-160,160), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  lim <- par('usr')
  rect(81, lim[3]-1, 120, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(-120, -60, 0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #abline(v= c(20, 40, 60, 80, 100), col = 8, lty = 2)
  axis(1, at = c(1, 21, 41, 61, 81, 101, 120)) #tick marks for x axis
  axis(3, at = c(10, 30, 50, 70, 90, 110), labels = c('30°/60°', '300°/330°', '120°/150°', '30°/60°', '30°/60°', '30°/60°'), line = -2, tick = FALSE) #tick marks for x axis
  axis(2, at = c(-150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150), las = 2) #tick marks for y axis
  
  
  #read in CI file
  groupconfidence <- read.csv(file='data/mirrorgeneralization-master/data/processed/LearningGen_CI.csv')
  # we want to color code plot according to target distance from mirror axis (far and near groups)
  # append far and near identifiers to CI data
  dat <- read.csv(file='data/mirrorgeneralization-master/data/processed/LearningGen.csv')
  dat <- dat$targetangle_deg
  groupconfidence$targetangle_deg <- dat
  targetdist <- c()
  for (target in groupconfidence$targetangle_deg){
    if (target %in% c(30, 330, 150)){
      dist <- 'far'
      targetdist <- c(targetdist, dist)
    } else if (target %in% c(60, 300, 120)){
      dist <- 'near'
      targetdist <- c(targetdist, dist)
    }
  }
  groupconfidence$targetdist <- targetdist
  
  for(group in groups){
    colourscheme <- getOnlineGenColourScheme(groups = group)
    
    #plot Hand 1, Quadrant 1
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(1,20,2)
    } else if (group == 'near'){
      x <- seq(2,20,2)
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 4
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(22,40,2)
    } else if (group == 'near'){
      x <- seq(21,40,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 2
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(42,60,2)
    } else if (group == 'near'){
      x <- seq(41,60,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 1 for a second time
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(61,80,2)
    } else if (group == 'near'){
      x <- seq(62,80,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 2, Quadrant 1
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(81,100,2)
    } else if (group == 'near'){
      x <- seq(82,100,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 2, Quadrant 1 WASHOUT
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(101,120,2)
    } else if (group == 'near'){
      x <- seq(102,120,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
   
  }

  #add legend
  legend(80,-120,legend=c('target far from mirror','target near mirror'),
         col=c(colourscheme[['far']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
  
  
}

plotLearningGenSignFlip <- function(groups = c('far', 'near'), target='inline') {
  #groups are far from mirror axis (30, 330, 150), or near mirror axis (60, 300, 120)
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/mirrorgeneralization-master/doc/fig/Fig1_LearningGenSignFlip.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,121), ylim = c(-10,160), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  lim <- par('usr')
  rect(81, lim[3]-1, 120, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #abline(h = 60, col = 8, lty = 2)
  #abline(h = 120, col = 8, lty = 2)
  #we could color code the dashed lines at perfect compensation, but washout needs to be grey
  perfnear <- rep(60, 100) #add 5 points to either side to extend the line
  lines(x = c(1:100), y = perfnear, col = '#005de4ff', lty = 2)
  perffar <- rep(120, 100) #add 5 points to either side to extend the line
  lines(x = c(1:100), y = perffar, col = '#e51636ff', lty = 2) 
  #then add grey lines
  greynear <- rep(60, 7) #7 is however many the x axis values are
  lines(x = c(-5:1), y = greynear, col = 8, lty = 2) #5 x values before 0
  greyfar <- rep(120, 7) #7 is however many the x axis values are
  lines(x = c(-5:1), y = greyfar, col = 8, lty = 2)
  
  greynear <- rep(60, 26) #26 is however many the x axis values are
  lines(x = c(100:125), y = greynear, col = 8, lty = 2) #4 x values after 121
  greyfar <- rep(120, 26) #26 is however many the x axis values are
  lines(x = c(100:125), y = greyfar, col = 8, lty = 2) #4 x values after 121
  
  axis(1, at = c(1, 21, 41, 61, 81, 101, 120)) #tick marks for x axis
  axis(3, at = c(10, 30, 50, 70, 90, 110), labels = c('30°/60°', '300°/330°', '120°/150°', '30°/60°', '30°/60°', '30°/60°'), line = -2, tick = FALSE) #tick marks for x axis
  axis(2, at = c(0, 30, 60, 90, 120, 150), las = 2) #tick marks for y axis
  
  
  #read in CI file
  groupconfidence <- read.csv(file='data/mirrorgeneralization-master/data/processed/LearningGen_CI.csv')
  #we would want to implement a sign flip for blocks with negative values (blocks 2 and 3)
  #But we would want to keep washout the same
  groupconfidence[21:60,] <- ((groupconfidence[21:60,])*-1)
  
  
  # we want to color code plot according to target distance from mirror axis (far and near groups)
  # append far and near identifiers to CI data
  dat <- read.csv(file='data/mirrorgeneralization-master/data/processed/LearningGen.csv')
  dat <- dat$targetangle_deg
  groupconfidence$targetangle_deg <- dat
  targetdist <- c()
  for (target in groupconfidence$targetangle_deg){
    if (target %in% c(30, 330, 150)){
      dist <- 'far'
      targetdist <- c(targetdist, dist)
    } else if (target %in% c(60, 300, 120)){
      dist <- 'near'
      targetdist <- c(targetdist, dist)
    }
  }
  groupconfidence$targetdist <- targetdist
  
  for(group in groups){
    colourscheme <- getOnlineGenColourScheme(groups = group)
    
    #plot Hand 1, Quadrant 1
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(1,20,2)
    } else if (group == 'near'){
      x <- seq(2,20,2)
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 4
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(22,40,2)
    } else if (group == 'near'){
      x <- seq(21,40,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 2
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(42,60,2)
    } else if (group == 'near'){
      x <- seq(41,60,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 1 for a second time
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(61,80,2)
    } else if (group == 'near'){
      x <- seq(62,80,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 2, Quadrant 1
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(81,100,2)
    } else if (group == 'near'){
      x <- seq(82,100,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 2, Quadrant 1 WASHOUT
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(101,120,2)
    } else if (group == 'near'){
      x <- seq(102,120,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
  }
  
  #add legend
  legend(61,30,legend=c('target far from mirror','target near mirror'),
         col=c(colourscheme[['far']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
  
  
}

# movement time across trials----
getGroupGenMT<- function(step){
  
  datafilenames <- list.files('data/mirrorgeneralization-master/data', pattern = '*.csv')
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){

    datafilename <- sprintf('data/mirrorgeneralization-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    alldat <- handleOneMTFile(filename = datafilename, step = step)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(alldat$trialno))
    targetangle_deg <- alldat$targetangle_deg
    ppreaches <- alldat$time #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, targetangle_deg, ppreaches)
    
    ppname <- unique(alldat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  #outlier removal
  for (trialno in dataoutput$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dataoutput[trialno, 3:ncol(dataoutput)])
    #print(max(ndat, na.rm=T))
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)

    ndat[which(abs(ndat) > trialclip)] <- NA

    dataoutput[trialno, 3:ncol(dataoutput)] <- ndat
  }
  
  #return(dataoutput)
  write.csv(dataoutput, file='data/mirrorgeneralization-master/data/processed/MTGen.csv', row.names = F)
  
  #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput

}

getGroupGenMTCI <- function(type){
  
  data <- read.csv(file='data/mirrorgeneralization-master/data/processed/MTGen.csv', check.names = FALSE) #check.names allows us to keep pp id as headers
  trialno <- data$trial
  
  data1 <- as.matrix(data[,3:dim(data)[2]])
  confidence <- data.frame()
  
  for(trial in trialno){
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
    
    write.csv(confidence, file='data/mirrorgeneralization-master/data/processed/MTGen_CI.csv', row.names = F) 
    
  }
  
}

plotMTGen <- function(groups = c('far', 'near'), target='inline') {
  #groups are far from mirror axis (30, 330, 150), or near mirror axis (60, 300, 120)
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/mirrorgeneralization-master/doc/fig/Fig2_MTGen.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,121), ylim = c(0,3.5), 
       xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Movement time across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  lim <- par('usr')
  rect(81, lim[3]-1, 120, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(1), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 21, 41, 61, 81, 101, 120)) #tick marks for x axis
  axis(3, at = c(10, 30, 50, 70, 90, 110), labels = c('30°/60°', '300°/330°', '120°/150°', '30°/60°', '30°/60°', '30°/60°'), line = -2, tick = FALSE) #tick marks for x axis
  axis(2, at = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), las=2) #tick marks for y axis
  
  #read in CI file
  groupconfidence <- read.csv(file='data/mirrorgeneralization-master/data/processed/MTGen_CI.csv')
  # we want to color code plot according to target distance from mirror axis (far and near groups)
  # append far and near identifiers to CI data
  dat <- read.csv(file='data/mirrorgeneralization-master/data/processed/MTGen.csv')
  dat <- dat$targetangle_deg
  groupconfidence$targetangle_deg <- dat
  targetdist <- c()
  for (target in groupconfidence$targetangle_deg){
    if (target %in% c(30, 330, 150)){
      dist <- 'far'
      targetdist <- c(targetdist, dist)
    } else if (target %in% c(60, 300, 120)){
      dist <- 'near'
      targetdist <- c(targetdist, dist)
    }
  }
  groupconfidence$targetdist <- targetdist
  
  for(group in groups){
    colourscheme <- getOnlineGenColourScheme(groups = group)
    
    #plot Hand 1, Quadrant 1
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(1,20,2)
    } else if (group == 'near'){
      x <- seq(2,20,2)
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 4
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(22,40,2)
    } else if (group == 'near'){
      x <- seq(21,40,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 2
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(42,60,2)
    } else if (group == 'near'){
      x <- seq(41,60,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 1 for a second time
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(61,80,2)
    } else if (group == 'near'){
      x <- seq(62,80,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 2, Quadrant 1
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(81,100,2)
    } else if (group == 'near'){
      x <- seq(82,100,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 2, Quadrant 1 WASHOUT
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(101,120,2)
    } else if (group == 'near'){
      x <- seq(102,120,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
  }
  
  #add legend
  legend(80,0.5,legend=c('target far from mirror','target near mirror'),
         col=c(colourscheme[['far']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
  
  
}

# Path length across trials----
getGroupGenPL<- function(step){
  
  datafilenames <- list.files('data/mirrorgeneralization-master/data', pattern = '*.csv')
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/mirrorgeneralization-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    alldat <- handleOneFilePathLength(filename = datafilename, step = step)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(alldat$trialno))
    targetangle_deg <- alldat$targetangle_deg
    ppreaches <- alldat$path_length#get reach deviations column from learning curve data
    ppdat <- data.frame(trial, targetangle_deg, ppreaches)
    
    ppname <- unique(alldat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  #outlier removal
  for (trialno in dataoutput$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dataoutput[trialno, 3:ncol(dataoutput)])
    #print(max(ndat, na.rm=T))
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)

    ndat[which(abs(ndat) > trialclip)] <- NA

    dataoutput[trialno, 3:ncol(dataoutput)] <- ndat
  }
  
  #return(dataoutput)
  write.csv(dataoutput, file='data/mirrorgeneralization-master/data/processed/PLGen.csv', row.names = F)
  
  #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
  
}

getGroupGenPLCI <- function(type){
  
  data <- read.csv(file='data/mirrorgeneralization-master/data/processed/PLGen.csv', check.names = FALSE) #check.names allows us to keep pp id as headers
  trialno <- data$trial
  
  data1 <- as.matrix(data[,3:dim(data)[2]])
  confidence <- data.frame()
  
  for(trial in trialno){
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
    
    write.csv(confidence, file='data/mirrorgeneralization-master/data/processed/PLGen_CI.csv', row.names = F) 
    
  }
  
}

plotPLGen <- function(groups = c('far', 'near'), target='inline') {
  #groups are far from mirror axis (30, 330, 150), or near mirror axis (60, 300, 120)
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/mirrorgeneralization-master/doc/fig/Fig3_PLGen.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,121), ylim = c(0,2), 
       xlab = "Trial", ylab = "Path length (monitor scale)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Path length across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  lim <- par('usr')
  rect(81, lim[3]-1, 120, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(0.4), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 21, 41, 61, 81, 101, 120)) #tick marks for x axis
  axis(3, at = c(10, 30, 50, 70, 90, 110), labels = c('30°/60°', '300°/330°', '120°/150°', '30°/60°', '30°/60°', '30°/60°'), line = -2, tick = FALSE) #tick marks for x axis
  axis(2, at = c(0, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2), las=2) #tick marks for y axis
  
  #read in CI file
  groupconfidence <- read.csv(file='data/mirrorgeneralization-master/data/processed/PLGen_CI.csv')
  # we want to color code plot according to target distance from mirror axis (far and near groups)
  # append far and near identifiers to CI data
  dat <- read.csv(file='data/mirrorgeneralization-master/data/processed/PLGen.csv')
  dat <- dat$targetangle_deg
  groupconfidence$targetangle_deg <- dat
  targetdist <- c()
  for (target in groupconfidence$targetangle_deg){
    if (target %in% c(30, 330, 150)){
      dist <- 'far'
      targetdist <- c(targetdist, dist)
    } else if (target %in% c(60, 300, 120)){
      dist <- 'near'
      targetdist <- c(targetdist, dist)
    }
  }
  groupconfidence$targetdist <- targetdist
  
  for(group in groups){
    colourscheme <- getOnlineGenColourScheme(groups = group)
    
    #plot Hand 1, Quadrant 1
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(1,20,2)
    } else if (group == 'near'){
      x <- seq(2,20,2)
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 4
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(22,40,2)
    } else if (group == 'near'){
      x <- seq(21,40,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 2
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(42,60,2)
    } else if (group == 'near'){
      x <- seq(41,60,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 1, Quadrant 1 for a second time
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(61,80,2)
    } else if (group == 'near'){
      x <- seq(62,80,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 2, Quadrant 1
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(81,100,2)
    } else if (group == 'near'){
      x <- seq(82,100,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
    #plot Hand 2, Quadrant 1 WASHOUT
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == 'far'){
      x <- seq(101,120,2)
    } else if (group == 'near'){
      x <- seq(102,120,2) #near is the first trial
    }
    
    polygon(x = c(x, rev(x)), y = c(lower[x], rev(upper[x])), border=NA, col=col)
    #polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = mid[x],col=col,lty=1)
    
  }
  
  #add legend
  legend(80,0.4,legend=c('target far from mirror','target near mirror'),
         col=c(colourscheme[['far']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
  
  
}