source('ana/shared.R')
source('ana/su&fa2020online.R')
source('ana/qualtricsdata.R')
source('ana/controlmir.R')
source('ana/controlmirgen.R')

#data analyzed here is similar to su&fa2020online,
#but I am creating this separate script to keep analyses consistent with controlmir and controlmirgen
#where we use a different method for calculating confidence intervals and create different plots
#Multiple functions however will be sourced from su&fa2020online.R

#Aligned-----
#Note that baseline reaches here have been cleaned (i.e. had to reach in correct quadrant), such that participants reaching all over the workspace were removed
#as this was taken as evidence that they did not do the task correctly. Mirror trials baseline correction are based off of this cleaned data.
# This step is omitted in control studies as the targets used are closer to the edge of each quadrant.

#However, this would mean that there are NA values in the aligned trials. Here we will plot the uncorrected values for baseline only and its corresponding statistics.

getGroupAlignedMirOnline <- function(groups = c('30','60'), set='fa2020'){
  
  for(group in groups){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
    
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      adat <- getParticipantCircularAligned(filename = datafilename)
      # per target location, get reachdev for corresponding trials
      
      trial <- c(1:length(adat$trialno))
      adat$trialno <- trial
      for (triali in trial){
        trialdat <- adat[which(adat$trialno == triali),]
        #set reachdev to NA if not the target location we want
        if (trialdat$targetangle_deg != group){
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
    
    write.csv(dataoutput, file=sprintf('data/mironline-master/data/processed/%s_CircularAligned.csv', group), row.names = F)
    
    
    #Note: multiple files have no step 2 or have many trials without step 2
    #These participant files have been removed
    #check for any more NA values:
    #names(which(colSums(is.na(dataoutput))>0))
  }
}

getGroupAlignedMirOnlineCI <- function(groups = c('30', '60')){
  
  for(group in groups){
    data <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_CircularAligned.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_Aligned_CI.csv', group), row.names = F)
    }
  }
}

plotAlignedMirOnline <- function(groups = c('30', '60'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/mironline-master/doc/fig/Fig1A_AlignedMirOnline.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,21), ylim = c(-20,20), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Aligned reaches", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 
  axis(1, at = c(1, 5, 10, 15, 20)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5, 10, 15)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_Aligned_CI.csv', group))
    
    colourscheme <- getOnlineColourScheme(groups = group)
    #plot Aligned Data
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:20), rev(c(1:20))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:20), y = mid,col=col,lty=1)
    
  }
  
  #add legend
  legend(15,-10,legend=c('30° target','60° target'),
         col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#Mirrored----
getGroupMirroredMirOnlineCI <- function(groups = c('30', '60')){
  
  for(group in groups){
    data <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularLC.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_Mirrored_CI.csv', group), row.names = F)
    }
  }
}

plotMirroredMirOnline <- function(groups = c('30', '60'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/mironline-master/doc/fig/Fig1B_MirroredMirOnline.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-10,135), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Mirrored reaches", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(0, 30, 60, 90, 120)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_Mirrored_CI.csv', group))
    
    colourscheme <- getOnlineColourScheme(groups = group)
    #plot Aligned Data
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
  legend(60,25,legend=c('30° target','60° target'),
         col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#Washout----
getGroupWashoutMirOnlineCI <- function(groups = c('30', '60')){
  
  for(group in groups){
    data <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularRAE.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
    
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_Washout_CI.csv', group), row.names = F)
    }
  }
}

plotWashoutMirOnline <- function(groups = c('30', '60'), target='inline') {
  
  
  if (target=='svg') {
    svglite(file='data/mironline-master/doc/fig/Fig1C_WashoutMirOnline.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,21), ylim = c(-20,20), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Washout", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 5, 10, 15, 20)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5, 10, 15)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_Washout_CI.csv', group))
    
    colourscheme <- getOnlineColourScheme(groups = group)
    #plot Aligned Data
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:20), rev(c(1:20))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(1:20), y = mid,col=col,lty=1)
    
  }
  
  #add legend
  legend(15,-10,legend=c('30° target','60° target'),
         col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#All tasks----
plotMirOnlineAllTasks <- function(groups = c('30', '60'), target='inline') {
  
  #but we can save plot as svg file
  if (target=='svg'){
    svglite(file='data/mironline-master/doc/fig/Fig1_MirOnlineAllTasks.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,131), ylim = c(-20,140), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(v= c(20, 110), col = 8, lty = 2)
  axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
  axis(2, at = c(-15, 0, 15, 30, 60, 90, 120)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidenceAligned <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_Aligned_CI.csv', group))
    groupconfidenceLC <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_Mirrored_CI.csv', group))
    groupconfidenceRAE <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_Washout_CI.csv', group))
    
    
    
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
    lower <- groupconfidenceLC[,1]
    upper <- groupconfidenceLC[,3]
    mid <- groupconfidenceLC[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(21:110), rev(c(21:110))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(21:110), y = mid,col=col,lty=1)
    
    #plot Wahout Data
    #take only first, last and middle columns of file
    lower <- groupconfidenceRAE[,1]
    upper <- groupconfidenceRAE[,3]
    mid <- groupconfidenceRAE[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(111:130), rev(c(111:130))), y = c(lower, rev(upper)), border=NA, col=col)
    col <- colourscheme[[group]][['S']]
    lines(x = c(111:130), y = mid,col=col,lty=1)
  }
  
  #add legend
  legend(80,0,legend=c('30° target','60° target'),
         col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#Percent Compensation----
getPercentagesMirOnline <- function(groups = c('30', '60')){
  
  for(group in groups){
    
    data <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularLC.csv', group), check.names = FALSE)
    data$trial <- seq(1,length(data$trial),1)
    
    trialno <- data$trial
    #postrials <- c(1:21, 64:126)
    
    for(trial in trialno){
      subdat <- as.numeric(data[trial, 2:length(data)])
      
      for (angleidx in 1:length(subdat)){
        angle <- subdat[angleidx]
        if (!is.na(angle) && group == '30'){
          subdat[angleidx] <- (angle/120)*100 #full compensation for 30 is 120
        } else if(!is.na(angle) && group == '60'){
          subdat[angleidx] <- (angle/60)*100 #full compensation for 60 is 60
        }
      }
      
      data[trial, 2:length(data)] <- subdat
    }
    
    write.csv(data, file=sprintf('data/mironline-master/data/statistics/%s_PercentCompensation.csv', group), row.names = F) 
    
  }
  
}

getBlockedPercentages<- function(group, blockdefs) {
  
  curves <- read.csv(sprintf('data/mironline-master/data/statistics/%s_PercentCompensation.csv',group), stringsAsFactors=FALSE, check.names = FALSE) 
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

plotBlockedMirOnline <- function(target='inline', groups = c('30', '60')) {
  
  if (target == 'svg') {
    svglite(file='data/mironline-master/doc/fig/Fig1D_BlockedMirOnline.svg', width=14, height=9, pointsize=14, system_fonts=list(sans='Arial'))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  
  
  # # # # # # # # # #
  # panel A: Learning Curves for all groups across all trials
  plotMirOnlineAllTasks()
  #mtext('A', side=3, outer=TRUE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  # # # # # # # # # #
  # panel B: First trial set - use percentage of compensation
  plot(c(0,3),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,2.5),ylim=c(-10, 200),xlab='Mirror trials 1 - 3',ylab='Amount of compensation (%)',xaxt='n',yaxt='n',bty='n',main='',font.main=1, cex.lab=1.10)
  
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  abline(h = c(0, 100), col = 8, lty = 2)
  
  blockdefs <- list(c(1,3))
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  groupno <- 0
  
  for (group in groups) {
    
    groupno <- groupno + 1 #counter for group, so that we can refer to it in x coordinates
    blocked <- getBlockedPercentages(group, blockdefs)
    colourscheme <- getOnlineColourScheme(group=group)
    #get bootstrapped 2.5, 50, 97.5% CIs of percentages
    meandist <- getConfidenceInterval(data=blocked, method='b')
    #meandist <- getAngularReachDevsCI(data = blocked, group = group)
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
    points(x=groupno,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
    
  }
  
  
  axis(side=1, at=c(1,2),labels=c('30° target','60° target'),cex.axis=1.13)
  axis(side=2, at=c(0,50, 100, 150),labels=c('0','50','100','150'),cex.axis=1.13, las=2)
  
  
  # # # # # # # # # #
  # panel C: Second trial set
  plot(c(0,3),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,2.5),ylim=c(-10, 200),xlab='Mirror Trials 4 - 6',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1, cex.lab=1.10)
  
  mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  abline(h = c(0, 100), col = 8, lty = 2)
  
  blockdefs <- list(c(4,3))
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  groupno <- 0
  
  for (group in groups) {
    
    groupno <- groupno + 1 #counter for group, so that we can refer to it in x coordinates
    blocked <- getBlockedPercentages(group, blockdefs)
    colourscheme <- getOnlineColourScheme(group=group)
    #get 2.5, 50, 97.5% CIs
    meandist <- getConfidenceInterval(data=blocked, method='b')
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
    points(x=groupno,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
    
  }
  
  
  axis(side=1, at=c(1,2),labels=c('30° target','60° target'),cex.axis=1.13)
  axis(side=2, at=c(0,50, 100, 150),labels=c('0','50','100','150'),cex.axis=1.13, las=2)
  
  
  # # # # # # # # # #
  # panel D: Last trial set
  plot(c(0,3),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,2.5),ylim=c(-10, 200),xlab='Mirror Trials 76 - 90',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1, cex.lab=1.10)
  
  mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  abline(h = c(0, 100), col = 8, lty = 2)
  
  blockdefs <- list(c(76,15))
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  groupno <- 0
  
  for (group in groups) {
    
    groupno <- groupno + 1 #counter for group, so that we can refer to it in x coordinates
    blocked <- getBlockedPercentages(group, blockdefs)
    colourscheme <- getOnlineColourScheme(group=group)
    #get 2.5, 50, 97.5% CIs
    meandist <- getConfidenceInterval(data=blocked, method='b')
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=c(meandist[[1]], meandist[[3]]),col=col) #lower and upper CI
    points(x=groupno,y=meandist[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
    
  }
  
  
  axis(side=1, at=c(1,2),labels=c('30° target','60° target'),cex.axis=1.13)
  axis(side=2, at=c(0,50, 100, 150),labels=c('0','50','100','150'),cex.axis=1.13, las=2)
  
  
  if (target == 'svg') {
    dev.off()
  }
  
}


#Statistics (Learning)----
#Aligned trials
getAlignedBlockedMirOnlineAOV <- function(groups = c('30', '60'), blockdefs) {
  #when analyzing angular deviations, we'd need to transform into circular values, so that stats are closer to what we need
  #circular values are similar to the way we calculate CI's in plots, raw ang devs will distort means
  LCaov <- data.frame()
  for(group in groups){
    curves <- read.csv(sprintf('data/mironline-master/data/processed/%s_CircularAligned.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
    curves <- curves[,-1] #remove trial rows
    curves <- as.circular(curves, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
    participants <- colnames(curves)
    N <- length(participants)
    
    #blocked <- array(NA, dim=c(N,length(blockdefs)))
    
    target <- c()
    participant <- c()
    block <- c()
    angdev <- c()
    
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
        angdev <- c(angdev, samples)
      }
    }
    angdev <- as.circular(angdev, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
    LCBlocked <- data.frame(target, participant, block, angdev)
    LCaov <- rbind(LCaov, LCBlocked)
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  return(LCaov)
  
}

#check target by block within each aligned period for each hand
alignedMirOnlineANOVA <- function() {

    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))

    
    
    LC4aov <- getAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)                      
    
    #looking into interaction below:
    interaction.plot(LC4aov$target, LC4aov$block, LC4aov$angdev)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=angdev, within= c(block, target), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Angular reach deviations during aligned trials across targets and blocks: \n'))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

#target effect, as we see in plot. No interaction, no block effect

#Mirror trials
getMirrorBlockedMirOnlineAOV <- function(groups = c('30', '60'), blockdefs) {
  
  LCaov <- data.frame()
  for(group in groups){
    curves <- read.csv(sprintf('data/mironline-master/data/statistics/%s_PercentCompensation.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
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

mirOnlineANOVA <- function() {
  
  
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  
  
  LC4aov <- getMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                  
  
  #looking into interaction below:
  interaction.plot(LC4aov$target, LC4aov$block, LC4aov$percentcomp)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=percentcomp, within= c(target, block), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  #cat(sprintf('Quadrant %s:\n', quadrant))
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#interaction not significant after GG, but there is a target and block effect
#target shows more compensation for 60, as seen in plots
#block can be investigated
MirOnlineComparisonMeans <- function(){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getMirrorBlockedMirOnlineAOV(blockdefs=blockdefs) 
  
  LC4aov <- aggregate(percentcomp ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","percentcomp",LC4aov,within=c("block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

MirOnlineComparisons <- function(method='bonferroni'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getMirrorBlockedMirOnlineAOV(blockdefs=blockdefs) 
  
  LC4aov <- aggregate(percentcomp ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","percentcomp",LC4aov,within=c("block"))
  
  #specify contrasts
  #levels of target are: far, mid, near
  B1vsB2 <- c(-1,1,0)
  B1vsB3 <- c(-1,0,1)
  B2vsB3 <- c(0,-1,1)
  
  contrastList <- list('First vs second block'=B1vsB2, 'First vs last block'=B1vsB3, 'Second vs last block'=B2vsB3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
MirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- MirOnlineComparisons(method=method)
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
#first block lowest, higher second block, then dips for last block

#Washout
getRAEBlockedMirOnlineAOV <- function(groups = c('30', '60'), blockdefs) {
  
  LCaov <- data.frame()
  for(group in groups){
    #use the one from su&fa2020, since unlike baseline, this required no cleaning (but biases use for correction are from cleaned data)
    curves <- read.csv(sprintf('data/mirrorreversal-fall/data/processed/%s_CircularRAE.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
    curves <- curves[,-1] #remove trial rows
    curves <- as.circular(curves, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
    participants <- colnames(curves)
    N <- length(participants)
    
    #blocked <- array(NA, dim=c(N,length(blockdefs)))
    
    target <- c()
    participant <- c()
    block <- c()
    angdev <- c()
    
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
        angdev <- c(angdev, samples)
      }
    }
    angdev <- as.circular(angdev, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
    LCBlocked <- data.frame(target, participant, block, angdev)
    LCaov <- rbind(LCaov, LCBlocked)
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  return(LCaov)
  
}

RAEMirOnlineANOVA <- function() {
  
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  
  
  
  LC4aov <- getRAEBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  
  #looking into interaction below:
  interaction.plot(LC4aov$target, LC4aov$block, LC4aov$angdev)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=angdev, within= c(block, target), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat(sprintf('Angular reach deviations during washout trials across targets and blocks: \n'))
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

#target effect, 30 degrees is generally lower, but no block effect nor interaction

#compare with baseline
RAEBaselineANOVA <- function() {
  
  blockdefs <- list('first'=c(1,3), 'second'=c(4,3),'last'=c(18,3))
  LC_aligned <- getAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)
  LC_aligned$session <- 'baseline'
  
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC_washout <- getRAEBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC_washout$session <- 'washout'
  
  LC4aov <- rbind(LC_aligned, LC_washout)
  LC4aov$block <- factor(LC4aov$block, levels = c('first', 'second', 'last'))
  LC4aov$session <- factor(LC4aov$session, levels = c('baseline', 'washout'))
  #looking into interaction below:
  #interaction.plot(LC4aov$target, LC4aov$block, LC4aov$angdev)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=angdev, within= c(block, target, session), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Comparing angular reach deviations during washout trials with aligned trials across targets and blocks, trained hand:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}
#no effect of session, but a session by target interaction
RAEBaselineComparisonMeans <- function(){
  blockdefs <- list('first'=c(1,3), 'second'=c(4,3),'last'=c(18,3))
  LC_aligned <- getAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)
  LC_aligned$session <- 'baseline'
  
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC_washout <- getRAEBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC_washout$session <- 'washout'
  
  LC4aov <- rbind(LC_aligned, LC_washout)
  LC4aov$block <- factor(LC4aov$block, levels = c('first', 'second', 'last'))
  LC4aov$session <- factor(LC4aov$session, levels = c('baseline', 'washout'))
  
  LC4aov <- aggregate(angdev ~ target* session* participant, data=LC4aov, FUN=mean) #regardless of target, the mean for every block within each quadrant
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","angdev",LC4aov,within=c("target", "session"))
  
  cellmeans <- emmeans(secondAOV,specs=c('target', 'session'))
  print(cellmeans)
  
}

RAEBaselineComparisons <- function(method='bonferroni'){
  blockdefs <- list('first'=c(1,3), 'second'=c(4,3),'last'=c(18,3))
  LC_aligned <- getAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)
  LC_aligned$session <- 'baseline'
  
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC_washout <- getRAEBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC_washout$session <- 'washout'
  
  LC4aov <- rbind(LC_aligned, LC_washout)
  LC4aov$block <- factor(LC4aov$block, levels = c('first', 'second', 'last'))
  LC4aov$session <- factor(LC4aov$session, levels = c('baseline', 'washout'))
  
  LC4aov <- aggregate(angdev ~ target* session* participant, data=LC4aov, FUN=mean) #regardless of target, the mean for every block within each quadrant
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","angdev",LC4aov,within=c("target", "session"))
  
  #specify contrasts
  #levels of target are: far, mid, near
  baseline30vswashout30 <- c(-1,0,1,0)
  baseline60vswashout60  <- c(0,-1,0,1)
  
  contrastList <- list('Baseline_30 vs. Washout_30'=baseline30vswashout30, 'Baseline_60 vs. Washout_60'=baseline60vswashout60)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('target', 'session')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
RAEBaselineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- RAEBaselineComparisons(method=method)
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



#Statistics (Movement Time)----
