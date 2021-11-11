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
    dat <- handleOneFile(filename = filename)
    ppdat <- unique(dat$participant)
    
    if(ppdat %in% ppqualt == FALSE){
      ppdel <- c(ppdel, ppdat)
    }
  }
  
  return(ppdel)
  
}


#Learning rates----

getParticipantLearningCtrlGen <- function(filename){
  
  #first, implement baseline correction - this is commented out because other targets do not have baseline, we baseline correct for retention plot below
  #get Aligned biases
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

getGroupLearningCtrlGen <- function(groups = c('far', 'mid', 'near')){
  #group is either 'far', 'mid', 'near'
  for(group in groups){
    datafilenames <- list.files('data/controlmirgenonline-master/data', pattern = '*.csv')
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/controlmirgenonline-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      adat <- getParticipantLearningCtrl(filename = datafilename)
      # per target location, get reachdev for corresponding trials
      
      trial <- c(1:length(adat$trialno))
      adat$trialno <- trial
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
    write.csv(dataoutput, file=sprintf('data/controlmirgenonline-master/data/processed/%s_LearningCtrlGen.csv', group), row.names = F)
  }
}

getGroupLearningCtrlGenConfInt <- function(groups = c('far', 'mid', 'near')){
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_LearningCtrlGen.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
    
    
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
      
      write.csv(confidence, file=sprintf('data/controlmirgenonline-master/data/processed/%s_LearningCtrlGen_CI.csv', group), row.names = F) 
      
    }
  }
}

plotLearningCtrlGen <- function(groups = c('far', 'mid', 'near'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/controlmirgenonline-master/doc/fig/Fig1_LearningCtrlGen.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,127), ylim = c(-185,185), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of learning per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-170, -90, -10, 0, 10, 90, 170), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 22, 43, 64, 85, 106, 126)) #tick marks for x axis
  axis(2, at = c(-170, -130, -90, -50, -10, 0, 10, 50, 90, 130, 170)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/controlmirgenonline-master/data/processed/%s_LearningCtrlGen_CI.csv', group))
    
    #split up data set for plotting purposes
    groupconfidenceQ1 <- groupconfidence[1:21,]
    groupconfidenceQ4 <- groupconfidence[22:42,]
    groupconfidenceQ2 <- groupconfidence[43:63,]
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
  legend(60,25,legend=c('far target','mid target', 'near target'),
         col=c(colourscheme[['far']][['S']],colourscheme[['mid']][['S']],colourscheme[['near']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
  
  
}