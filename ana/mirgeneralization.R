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
  #This function states participants who have NO part 1 data, returns null if all data in part 2 also have part 1
  #get list of id's from Fall data
  qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  ppqualt <- qualtdat$id[-c(1)]
  
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
  
  #first, implement baseline correction - this is commented out because other targets do not have baseline, we baseline correct for retention plot below
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
    svglite(file='data/mirrorgeneralization-master/doc/fig/Fig1A_LearningGen.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
    svglite(file='data/mirrorgeneralization-master/doc/fig/Fig1_LearningGen.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,121), ylim = c(-20,150), 
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
  axis(2, at = c(-15, 0, 15, 30, 60, 90, 120), las = 2) #tick marks for y axis
  
  
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

# Time between Part 1 and Part 2----
getDateOneFile <- function(filename){
  
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

getGroupDates<- function(sets = c('part1', 'part2')){
  
  for (set in sets){
    if (set == 'part1'){
      datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
    } else if (set == 'part2'){
      datafilenames <- list.files('data/mirrorgeneralization-master/data', pattern = '*.csv')
    }
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      if (set == 'part1'){
        datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      } else if (set == 'part2'){
        datafilename <- sprintf('data/mirrorgeneralization-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      }
      
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      alldat <- getDateOneFile(filename = datafilename)
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- alldat
      } else {
        dataoutput <- rbind(dataoutput, alldat)
      }
    }
    #return(dataoutput)
    if (set == 'part1'){
      write.csv(dataoutput, file=sprintf('data/mirrorreversal-fall/data/processed/%sDate.csv', set), row.names = F)
    } else if (set == 'part2'){
      write.csv(dataoutput, file=sprintf('data/mirrorgeneralization-master/data/processed/%sDate.csv', set), row.names = F)
    }
  }
}

getMatchGroupDates <- function(){
  
  part1dat <- read.csv(file='data/mirrorreversal-fall/data/processed/part1Date.csv')
  part2dat <- read.csv(file='data/mirrorgeneralization-master/data/processed/part2Date.csv')
  
  dat <- merge(part1dat, part2dat, by.x = 'id', by.y = 'id')
  colnames(dat) <- c('id', 'part1_date', 'part2_date')
  
  dat$days <- as.numeric(as.Date(dat$part2_date) - as.Date(dat$part1_date))
  
  #dat[which(dat$days == 0),] #remove those who did it within a few hours
  
  return(dat)
  
}

plotDaysApart <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/mirrorgeneralization-master/doc/fig/Fig4_HistDaysApart.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  dat <- getMatchGroupDates()
  width <- max(dat$days) - min(dat$days) #breaks is how many days are accounted for by each bar, so width here would be 1 day per bar
  
  hist(dat$days, breaks = width, main = 'Histogram for number of days between Parts 1 and 2',
       xlab = 'Days', ylab = 'Frequency of participants', axes=FALSE)
  axis(1, at = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130))
  axis(2, at = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 45), las=2) #tick marks for y axis
  
  cat(sprintf('mean: %s days apart \n',mean(dat$days)))
  cat(sprintf('sd: %s days apart \n',sd(dat$days)))
  cat(sprintf('median: %s days apart \n',median(dat$days)))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Retention across Part 1 and Part 2----

#baseline and first 20 trials of part 1, with baseline corrected first 20 trials of part 2
#use only participants who have completed both parts

getPart1Files <- function(){
  # this function helps to list all files of participants that have also completed part 2
  #use the files already saved for the date function to identify id's
  part1dat <- read.csv(file='data/mirrorreversal-fall/data/processed/part1Date.csv')
  part2dat <- read.csv(file='data/mirrorgeneralization-master/data/processed/part2Date.csv')
  
  ppid <- part1dat$id[which(part1dat$id %in% part2dat$id)]
  
  datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  datfiles <- c()
  
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    alldat <- getDateOneFile(filename = datafilename)
    
    if(alldat$id %in% ppid){
      datfiles <- c(datfiles,datafilename)
    }
  }
  #return(datfiles)
  datfiles <- data.frame(datfiles)
  write.csv(datfiles, file='data/mirrorreversal-fall/data/processed/pplist_matched_part2.csv', row.names = F)
}

getRetentionAligned <- function(group){
  

  datafilenames <- read.csv('data/mirrorreversal-fall/data/processed/pplist_matched_part2.csv', stringsAsFactors = F)
  datafilenames <- datafilenames$datfiles
  
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){

    datafilename <- sprintf('%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- getParticipantCircularAligned(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(adat$trialno))
    adat$trialno <- trial
    for (triali in trial){
      trialdat <- adat[which(adat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$reachdeviation_deg <- NA
      }
      adat[triali,] <- trialdat
    }
    ppreaches <- adat$reachdeviation_deg #get reach deviations column from learning curve data
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
  return(dataoutput)
}

getRetentionAlignedOutlierRemoval <- function(groups = c('30', '60')){
  for(group in groups){
    #get aligned data for specific group
    data <- getRetentionAligned(group = group)
    #set outlier values (ie out of quadrant) to NA, this depends on target location
    for (trialno in data$trial){
      #go through each trial, replace outlier values with NA
      ndat <- as.numeric(data[trialno, 2:ncol(data)])
      if (group == '30'){
        ndat[which(ndat < -30 | ndat > 60)] <- NA
      } else if (group == '60'){
        ndat[which(ndat < -60 | ndat > 30)] <- NA
      }
      data[trialno, 2:ncol(data)] <- ndat
    }
    #return(data)
    write.csv(data, file=sprintf('data/mirrorreversal-fall/data/processed/%s_Retention_Aligned.csv', group), row.names = F)
  }

}

getRetentionAlignedCI <- function(groups = c('30','60')){
  for(group in groups){
    # use cleaned aligned trials (i.e. only trials with reaches in correct quadrant)
    data <- read.csv(sprintf('data/mirrorreversal-fall/data/processed/%s_Retention_Aligned.csv', group), stringsAsFactors = F)
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
      
      #citrial <- getCircularConfidenceInterval(data = circ_subdat)
      #citrial <- as.numeric(citrial)
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_Retention_Aligned_CI.csv', group), row.names = F) 
      
    }
  }
}

getRetentionMirror<- function(groups = c('30', '60')){
  for(group in groups){
    datafilenames <- read.csv('data/mirrorreversal-fall/data/processed/pplist_matched_part2.csv', stringsAsFactors = F)
    datafilenames <- datafilenames$datfiles
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      
      datafilename <- sprintf('%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      mdat <- getParticipantCircularLC(filename = datafilename) #this function contains baseline correction procedure
      # per target location, get reachdev for corresponding trials
      
      trial <- c(1:length(mdat$trialno))
      mdat$trialno <- trial
      for (triali in trial){
        trialdat <- mdat[which(mdat$trialno == triali),]
        #set reachdev to NA if not the target location we want
        if (trialdat$targetangle_deg != group){
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
    write.csv(dataoutput, file=sprintf('data/mirrorreversal-fall/data/processed/%s_Retention_Mirror.csv', group), row.names = F)
  }
  
}

getRetentionMirrorCI <- function(groups = c('30','60')){
  for(group in groups){
    data <- read.csv(sprintf('data/mirrorreversal-fall/data/processed/%s_Retention_Mirror.csv', group), stringsAsFactors = F)
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

      write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_Retention_Mirror_CI.csv', group), row.names = F) 
      
    }
  }
}

#For part 2 data, the baseline correction should depend on part 1 aligned data
getParticipantPart2Retention <- function(filename){
  
  #read in aligned data, which already cleaned for baseline reaches
  dat30 <- read.csv('data/mirrorreversal-fall/data/processed/30_Retention_Aligned.csv', stringsAsFactors = F, check.names = FALSE)
  dat60 <- read.csv('data/mirrorreversal-fall/data/processed/60_Retention_Aligned.csv', stringsAsFactors = F, check.names = FALSE)
  
  part2dat <- getParticipantLearningGen(filename=filename)
  ppid <- unique(part2dat$participant)
  
  bias30 <- dat30[,which(colnames(dat30) == ppid)]
  bias30 <- as.circular(bias30, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  bias30 <- median.circular(bias30, na.rm=TRUE)
  bias60 <- dat60[,which(colnames(dat60) == ppid)]
  bias60 <- as.circular(bias60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  bias60 <- median.circular(bias60, na.rm=TRUE)
  targetangle_deg <- c(30,60)
  circ_rd <- c(bias30, bias60)
  
  biases <- data.frame(targetangle_deg,circ_rd)
  
  mdat <- part2dat[1:20,] #only first 20 trials for retention
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'circ_rd'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    mdat$circ_rd[which(mdat$targetangle_deg == target)] <- mdat$circ_rd[which(mdat$targetangle_deg == target)] - bias
    
  }
  return(mdat)
}

getGroupPart2Retention <- function(groups=c('30','60')){
  for(group in groups){
    datafilenames <- list.files('data/mirrorgeneralization-master/data', pattern = '*.csv')
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      
      datafilename <- sprintf('data/mirrorgeneralization-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      mdat <- getParticipantPart2Retention(filename = datafilename)
      # per target location, get reachdev for corresponding trials
      
      trial <- c(1:length(mdat$trialno))
      mdat$trialno <- trial
      for (triali in trial){
        trialdat <- mdat[which(mdat$trialno == triali),]
        #set reachdev to NA if not the target location we want
        if (trialdat$targetangle_deg != group){
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
    write.csv(dataoutput, file=sprintf('data/mirrorgeneralization-master/data/processed/%s_Retention_Mirror.csv', group), row.names = F) 
  }
}

getGroupPart2RetentionCI <- function(groups = c('30','60')){
  for(group in groups){
    data <- read.csv(sprintf('data/mirrorgeneralization-master/data/processed/%s_Retention_Mirror.csv',group), stringsAsFactors = F, check.names = FALSE)
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

      write.csv(confidence, file=sprintf('data/mirrorgeneralization-master/data/processed/%s_Retention_Mirror_CI.csv', group), row.names = F) 
      
    }
  }
}

plotRetention <- function(groups = c('30', '60'), target='inline') {

  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/mirrorgeneralization-master/doc/fig/Fig5_Retention.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,66), ylim = c(-10,160), #plus 5 on x axis
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #lim <- par('usr')
  #rect(81, lim[3]-1, 120, lim[4]+1, border = "#ededed", col = "#ededed") #xleft, ybottom, x right, ytop; light grey hex code
  abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #abline(v= c(20, 40, 60, 80, 100), col = 8, lty = 2)
  axis(1, at = c(1, 10, 21, 30, 40)) #tick marks for x axis
  nums <- c(1,10,20)
  axis(1, at = c(46,55, 65), labels = nums)
  axis(3, at = c(10, 30, 55), labels = c('aligned', 'mirror: part 1', 'mirror: part 2'), line = -2, tick = FALSE) #tick marks for x axis
  axis(2, at = c(0, 30, 60, 90, 120, 150), las = 2) #tick marks for y axis
  
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidenceAligned <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_Retention_Aligned_CI.csv', group))
    groupconfidencePart1 <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_Retention_Mirror_CI.csv', group))
    groupconfidencePart1 <- groupconfidencePart1[1:20,]
    groupconfidencePart2 <- read.csv(file=sprintf('data/mirrorgeneralization-master/data/processed/%s_Retention_Mirror_CI.csv', group))
    
    
    
    colourscheme <- getOnlineColourScheme(groups = group)
    #plot Aligned Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceAligned[,1]
    upper <- groupconfidenceAligned[,3]
    mid <- groupconfidenceAligned[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:20), rev(c(1:20))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:20), y = mid,col=col,lty=1)
    
    #plot Mirrored Data
    lower <- groupconfidencePart1[,1]
    upper <- groupconfidencePart1[,3]
    mid <- groupconfidencePart1[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(21:40), rev(c(21:40))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(21:40), y = mid,col=col,lty=1)
    
    #plot Wahout Data
    #take only first, last and middle columns of file
    lower <- groupconfidencePart2[,1]
    upper <- groupconfidencePart2[,3]
    mid <- groupconfidencePart2[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if (group == '30'){
      x <- seq(46,65,2) #plus 5 to create space for part 2
    } else if (group == '60'){
      x <- seq(46,65,2) #plus 5 to create space for part 2
    }
    
    
    polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
    
    col <- colourscheme[[group]][['S']]
    lines(x = x, y = na.omit(mid),col=col,lty=1)
  }
  
  #add legend
  legend(46,30,legend=c('30° target','60° target'),
         col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
  
  
}

#density plots -----
plotPart2Density <- function(groups = c('far', 'near')){
  
  for(group in groups){
    data <- read.csv(file='data/mirrorgeneralization-master/data/processed/LearningGen.csv', check.names = FALSE)

    pdf(sprintf("data/mirrorgeneralization-master/doc/fig/Part2Density_%s.pdf", group))

    
    #current fix for summer data being non-randomized and not counterbalanced
    #triallist <- dat$trial
    #triallist <- c(1:90)
    #triallist <- c(1,2,90)
    #triallist <- c(1:20)
    
    if(group == 'far'){
      n <- seq(1,20,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(2,20,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    #Quad 1
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      #prefer the plot to have a small circle, and emphasize the density
      #Xsub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #Ysub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #plot(Xsub, Ysub, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', shrink=1.5, tol = .01)

      if(group == 'far'){
        plot(distsubdat, main = sprintf('30° Target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #points.circular(rd, pch = 15, col = 'red')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.5)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('60° Target: Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      }
      # axis(1, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 330, 300, 360))
      # axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    }
    
    #Quad4
    if(group == 'far'){
      n <- seq(22,40,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(21,40,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      #prefer the plot to have a small circle, and emphasize the density
      #Xsub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #Ysub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #plot(Xsub, Ysub, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', shrink=1.5, tol = .01)

      if(group == 'far'){
        plot(distsubdat, main = sprintf('330° Target: Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(-120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 120, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('300° Target: Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(-60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 60, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      }
      # axis(1, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 330, 300, 360))
      # axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    }
    
    #Quad2
    if(group == 'far'){
      n <- seq(42,60,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(41,60,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      #prefer the plot to have a small circle, and emphasize the density
      #Xsub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #Ysub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #plot(Xsub, Ysub, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', shrink=1.5, tol = .01)
      
      if(group == 'far'){
        plot(distsubdat, main = sprintf('150° Target: Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(-120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 120, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('120° Target: Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(-60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 60, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      }
      # axis(1, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 330, 300, 360))
      # axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    }
    
    #Quad1 top up
    if(group == 'far'){
      n <- seq(61,80,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(62,80,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      #prefer the plot to have a small circle, and emphasize the density
      #Xsub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #Ysub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #plot(Xsub, Ysub, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', shrink=1.5, tol = .01)
     
      if(group == 'far'){
        plot(distsubdat, main = sprintf('30° Target: Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 120, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('60° Target: Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 60, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      }
      # axis(1, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 330, 300, 360))
      # axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    }
    
    #Quad1 switch hand
    if(group == 'far'){
      n <- seq(81,100,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(82,100,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      #prefer the plot to have a small circle, and emphasize the density
      #Xsub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #Ysub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #plot(Xsub, Ysub, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', shrink=1.5, tol = .01)
     
      if(group == 'far'){
        plot(distsubdat, main = sprintf('30° Target, switch hand: Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 120, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('60° Target, switch hand: Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 60, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation','perfect compensation'),
               col=c('#FF0000','#00FF00'),
               lty=1,bty='n',cex=1)
      }
      # axis(1, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 330, 300, 360))
      # axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    }
    
    #Quad1 switch hand, washout
    if(group == 'far'){
      n <- seq(101,120,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(102,120,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      #prefer the plot to have a small circle, and emphasize the density
      #Xsub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #Ysub <- as.circular(NA, type='angles', units ='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      #plot(Xsub, Ysub, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', shrink=1.5, tol = .01)

      if(group == 'far'){
        plot(distsubdat, main = sprintf('30° Target, switch hand (washout): Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 120, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation required'),
               col='#00FF00',
               lty=1,bty='n',cex=1)
      } else if (group == 'near'){
        plot(distsubdat, main = sprintf('60° Target, switch hand (washout): Trial %s', triali), plot.type = 'circle',
             shrink=1.5, points.plot = TRUE, points.col=4, col=4)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#00FF00')
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=0.85)
        #lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.3)
        #abline(v = 60, col = 8, lty = 2)
        legend(-1.5,-1.25,legend=c('no compensation required'),
               col='#00FF00',
               lty=1,bty='n',cex=1)
      }
      # axis(1, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 330, 300, 360))
      # axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    }
    
    dev.off()
    
  }

}

#Move throughs: identifying participant frequency and trials with move throughs----
# we can indicate 1 = move through, 0 = no move through for all trials in generalization experiment
getAllMoveThroughs <- function(){
  
  
  datafilenames <- list.files('data/mirrorgeneralization-master/data', pattern = '*.csv')
  
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/mirrorgeneralization-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- handleOneFileCheck(filename = datafilename) #reuse function from su&fa2020online
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(mdat$trialno))
    ppreaches <- mdat$step1_samp #get reach deviations column from learning curve data
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
  write.csv(dataoutput, file='data/mirrorgeneralization-master/data/processed/TrialMoveThroughs.csv', row.names = F)
}

getTotalMoveThroughs <- function(){
  
  
  dat1 <- read.csv('data/mirrorgeneralization-master/data/processed/TrialMoveThroughs.csv', check.names = FALSE)
  
  #current fix for summer data being non-randomized and not counterbalanced
  #triallist <- dat$trial
  triallist <- c(1:120)
  
  trial <- c()
  ppno <- c()
  for(triali in triallist){
    subdat1 <- dat1[which(dat1$trial == triali),]
    subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
    ppthrough <- sum(subdat1[which(subdat1 == 1)])
    
    trial <- c(trial, triali)
    ppno <- c(ppno, ppthrough)
  }
  ppmovethrough <- data.frame(trial,ppno)
  
  return(ppmovethrough)
}

plotTotalMoveThroughs <- function(target='inline'){
  
  
  if (target=='svg') {
    svglite(file='data/mirrorgeneralization-master/doc/fig/Fig6_TotalMoveThroughs.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  data <- getTotalMoveThroughs()
  barplot(data$ppno~data$trial, xlab = 'Trial', ylab = 'Frequency of Participants',
          main = 'Participants with move throughs')#, axes = FALSE, #axisnames=FALSE,
  #ylim = c(-1,61))
  #axis(1, at = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90))
  #axis(2, at = c(0, 2, 4, 6, 8, 10, 20, 30, 60)) #tick marks for y axis
  if(target=='svg'){
    dev.off()
  }
  
  
}

#Retention: move throughs----
#first 20 trials can either be based on baseline corrected data from part 1 or not.
#this section uses aligned portion of Part 1 for baseline corrected reach deviations.
plotRetentionDensityMoveThroughs <- function(groups = c('30', '60')){
  
  # we can use the outputs of 30_Retention_Mirror.csv and corresponding 60 degree file for reachdevs
  dat30 <- read.csv('data/mirrorgeneralization-master/data/processed/30_Retention_Mirror.csv', check.names = FALSE)
  dat60 <- read.csv('data/mirrorgeneralization-master/data/processed/60_Retention_Mirror.csv', check.names = FALSE)
  dats1 <- read.csv('data/mirrorgeneralization-master/data/processed/TrialMoveThroughs.csv', check.names = FALSE)
  
  for(group in groups){


    pdf(sprintf("data/mirrorgeneralization-master/doc/fig/%s_RetentionDistbyStep1.pdf", group))
    
    
    #current fix for summer data being non-randomized and not counterbalanced
    #triallist <- dat$trial
    triallist <- c(1:20)
    
    
    if(group == '30'){
      n <- triallist[seq(1,length(triallist),2)]
      dat <- dat30[which(dat30$trial %in% n),]
      triallist <- dat$trial
    } else if (group == '60'){
      n <- triallist[seq(2,length(triallist),2)]
      dat <- dat60[which(dat60$trial %in% n),]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,2:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      subdat1 <- dats1[which(dats1$trial == triali),]
      subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
      subdatall <- data.frame(subdat1, subdat)
      
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)

      if(group == '30'){
        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
        
      } else if (group == '60'){
        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      }
    }
    dev.off()
    
  }
}
#compare these plots with fig 5_retention
#it seems like exploration does not necessarily split participants into reaching towards the correct direction or not (unlike fall)
#so retention in fig 5 is likely true retention, but this does not explain why they start off higher (i.e. compensate even more)


#Generalization: move throughs----
# this contains move through density plots for all trials in generalization
# first 20 trials is not baseline corrected (because taken as a group, this all occured in part 2)

plotDensityMoveThroughs <- function(groups = c('far', 'near')){
  
  for(group in groups){
    data <- read.csv(file='data/mirrorgeneralization-master/data/processed/LearningGen.csv', check.names = FALSE)
    dats1 <- read.csv('data/mirrorgeneralization-master/data/processed/TrialMoveThroughs.csv', check.names = FALSE)
    
    pdf(sprintf("data/mirrorgeneralization-master/doc/fig/%s_DistbyStep1.pdf", group))
    
    
    #current fix for summer data being non-randomized and not counterbalanced
    #triallist <- dat$trial
    #triallist <- c(1:90)
    #triallist <- c(1,2,90)
    #triallist <- c(1:20)
    
    if(group == 'far'){
      n <- seq(1,20,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(2,20,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    #Quad 1
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      subdat1 <- dats1[which(dats1$trial == triali),]
      subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
      subdatall <- data.frame(subdat1, subdat)
      
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      
      if(group == 'far'){
        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('30° Target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
        
        
      } else if (group == 'near'){
        
        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('60° Target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      }

    }
    
    #Quad4
    if(group == 'far'){
      n <- seq(22,40,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(21,40,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      subdat1 <- dats1[which(dats1$trial == triali),]
      subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
      subdatall <- data.frame(subdat1, subdat)
      
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      
      if(group == 'far'){

        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('330° Target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(-120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      } else if (group == 'near'){

        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('300° Target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(-60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      }

    }
    
    #Quad2
    if(group == 'far'){
      n <- seq(42,60,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(41,60,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      subdat1 <- dats1[which(dats1$trial == triali),]
      subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
      subdatall <- data.frame(subdat1, subdat)
      
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      
      if(group == 'far'){

        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('150° Target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(-120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      } else if (group == 'near'){

        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('120° Target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(-60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      }

    }
    
    #Quad1 top up
    if(group == 'far'){
      n <- seq(61,80,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(62,80,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      subdat1 <- dats1[which(dats1$trial == triali),]
      subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
      subdatall <- data.frame(subdat1, subdat)
      
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      
      if(group == 'far'){
        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('30° Target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      } else if (group == 'near'){

        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('60° Target: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      }

    }
    
    #Quad1 switch hand
    if(group == 'far'){
      n <- seq(81,100,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(82,100,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      subdat1 <- dats1[which(dats1$trial == triali),]
      subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
      subdatall <- data.frame(subdat1, subdat)
      
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      
      if(group == 'far'){

        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('30° Target, Switch hand: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      } else if (group == 'near'){
        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('60° Target, Switch hand: Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        perfcomp <- as.circular(60, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
        arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
               col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      }

    }
    
    #Quad1 switch hand, washout
    if(group == 'far'){
      n <- seq(101,120,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    } else if (group == 'near'){
      n <- seq(102,120,2)
      dat <- data[which(data$trial %in% n),]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,3:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      subdat1 <- dats1[which(dats1$trial == triali),]
      subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
      subdatall <- data.frame(subdat1, subdat)
      
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      
      if(group == 'far'){
        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('30° Target, Switch hand (washout): Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation required', 'with exploration', 'withoutexploration'),
               col=c('#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      } else if (group == 'near'){
        #tcl, pos and neg values are for each label
        plot(distsubdat, main = sprintf('60° Target, Switch hand (washout): Trial %s', triali), plot.type = 'circle', 
             shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
        dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
        polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
        
        nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        arrows.circular(nocomp, length = 0, angle = 0, col = '#00FF00')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025)
        
        legend(-1.75,-1.25,legend=c('no compensation required', 'with exploration', 'withoutexploration'),
               col=c('#00FF00', '#ff8200ff', '#c400c4ff'),
               lty=1,bty='n',cex=1, ncol=2)
      }

    }
    
    dev.off()
    
  }
  
}

#it seems like exploration does not necessarily split participants into reaching towards the correct direction or not (unlike fall)
#move throughs are really only existent on trial 1, and first trial after switching hands
#this might show that transfer across target locations seems to be more generalization than fast learning (because without exploration,
# the reaches produce a mean deviation in expected direction)
#Participants with move throughs for hand switching is more distributed (both correct and incorrect sides). This might
# show that there is transfer, because exploration of workspace does not necessarily correspond to correct reaches immediately.

# function below just does the same thing but only for first trial and first trial after switching hands
plotTrialOneDensityMoveThroughs <- function(target='inline', trials = c(1, 81)){
  for (triali in trials){
    if (target=='svg'){
      svglite(file=sprintf('data/mirrorgeneralization-master/doc/fig/Fig7_Trial%s_DistributionbyStep1.svg', triali), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    data <- read.csv(file='data/mirrorgeneralization-master/data/processed/LearningGen.csv', check.names = FALSE)
    dats1 <- read.csv('data/mirrorgeneralization-master/data/processed/TrialMoveThroughs.csv', check.names = FALSE)
    
    
    
    
    #current fix for summer data being non-randomized and not counterbalanced
    #triallist <- dat$trial
    #triallist <- c(1:90)
    #triallist <- c(1,2,90)
    #triallist <- c(1,81)
    
    
    subdat <- data[which(data$trial == triali),]
    subdat <- as.numeric(subdat[,3:ncol(subdat)])
    subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
    subdat1 <- dats1[which(dats1$trial == triali),]
    subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
    subdatall <- data.frame(subdat1, subdat)
    
    distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
    
    #tcl, pos and neg values are for each label
    plot(distsubdat, main = sprintf('30° Target: Trial %s', triali), plot.type = 'circle', 
         shrink=1.5, col= '#A9A9A9ff', tcl.text = 0.25) 
    dencurve <- lines(distsubdat, points.plot=FALSE, col='#A9A9A9ff', shrink=1.5)
    polygon(dencurve$x, dencurve$y, col = alpha('#A9A9A92f', 0.10), border = NA)
    
    nocomp <- as.circular(0, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
    perfcomp <- as.circular(120, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
    arrows.circular(nocomp, length = 0, angle = 0, col = '#FF0000')
    arrows.circular(perfcomp, length = 0, angle = 0, col = '#00FF00')
    
    movethrough <- subdatall[which(subdatall$subdat1 == 1),]
    movethrough <- movethrough$subdat
    points.circular(movethrough, pch = 1, col = '#ff8200ff', next.points = -.075) #+.025
    
    nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
    nonthrough <- nonthrough$subdat
    points.circular(nonthrough, pch = 1, col = '#c400c4ff', next.points = -.025) #.075
    
    legend(-1.75,-1.25,legend=c('no compensation','perfect compensation', 'with exploration', 'withoutexploration'),
           col=c('#FF0000','#00FF00', '#ff8200ff', '#c400c4ff'),
           lty=1,bty='n',cex=1, ncol=2)
    
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}



















