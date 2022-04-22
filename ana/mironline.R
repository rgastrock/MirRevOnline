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
    print(group)
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
    
    
    
    dataoutput<- data.frame() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
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
        samples <- getAngularReachDevsStats(data=samples)
        #samples <- samples[[2]]
        
        target <- c(target, group)
        participant <- c(participant, pp)
        block <- c(block, names(blockdefs)[blockno])
        angdev <- c(angdev, samples)
      }
    }
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

#target effect, as we see in plot.no interaction

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
        samples <- getAngularReachDevsStats(data=samples)
        #samples <- samples[[2]]
        
        target <- c(target, group)
        participant <- c(participant, pp)
        block <- c(block, names(blockdefs)[blockno])
        angdev <- c(angdev, samples)
      }
    }
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
#driven by a difference in 60 target between baseline and washout


#Statistics (Movement Time)----
getAlignedBlockedMTMirOnlineAOV <- function(groups = c('30', '60'), blockdefs) {
  
  LCaov <- data.frame()
  for(group in groups){
    curves <- read.csv(sprintf('data/mirrorreversal-fall/data/processed/%s_step2_MovementTime.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
    curves <- curves[,-1] #remove trial rows
    participants <- colnames(curves)
    N <- length(participants)
    
    #blocked <- array(NA, dim=c(N,length(blockdefs)))
    
    target <- c()
    participant <- c()
    block <- c()
    movementtime <- c()
    
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
        movementtime <- c(movementtime, samples)
      }
    }
    LCBlocked <- data.frame(target, participant, block, movementtime)
    LCaov <- rbind(LCaov, LCBlocked)
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  return(LCaov)
  
}

#check target by block within each aligned period for each hand
alignedMTMirOnlineANOVA <- function() {
  
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)                      
  
  #looking into interaction below:
  interaction.plot(LC4aov$target, LC4aov$block, LC4aov$movementtime)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(block, target), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat(sprintf('Movement time during aligned trials across targets and blocks:\n'))
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#main effect of block, no interaction
alignedMTMirOnlineComparisonMeansBlockEffect <- function(){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)   
  
  LC4aov <- aggregate(movementtime ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","movementtime",LC4aov,within=c("block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

alignedMTMirOnlineComparisonsBlockEffect <- function(method='bonferroni'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)   
  
  LC4aov <- aggregate(movementtime ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","movementtime",LC4aov,within=c("block"))
  
  #specify contrasts
  #levels of target are: far, mid, near
  b1vsb2 <- c(-1,1,0)
  b1vsb3 <- c(-1,0,1)
  b2vsb3 <- c(0,-1,1)
  
  contrastList <- list('first vs. second block'=b1vsb2, 'first vs. last block'=b1vsb3, 'second vs. last block'=b2vsb3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
alignedMTMirOnlineComparisonsEffSizeBlockEffect <- function(method = 'bonferroni'){
  comparisons <- alignedMTMirOnlineComparisonsBlockEffect(method=method)
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

#Mirror trials MT
#3x3 anova (target x block)
mirrorMTMirOnlineANOVA <- function() {
  
  #can still use alignedMT function as it has all trials
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  
  
  LC4aov <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)               
  
  #looking into interaction below:
  interaction.plot(LC4aov$target, LC4aov$block, LC4aov$movementtime)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(target, block), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  #cat(sprintf('Quadrant %s:\n', quadrant))
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#significant interaction
mirrorMTMirOnlineComparisonMeans <- function(){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)    
  
  #LC4aov <- aggregate(percentcomp ~ target* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","movementtime",LC4aov,within=c("target", "block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('target', 'block'))
  print(cellmeans)
  
}

#we know from the plot that movement time decreases across blocks, but interesting to see target differences within each block

mirrorMTMirOnlineComparisons <- function(method='bonferroni'){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)    
  
  #LC4aov <- aggregate(percentcomp ~ target* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","movementtime",LC4aov,within=c("target", "block"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  B1_30vs60 <- c(-1,1,0,0,0,0)
  #second
  B2_30vs60 <- c(0,0,-1,1,0,0)
  #last
  B3_30vs60 <- c(0,0,0,0,-1,1)
  
  contrastList <- list('1st block: 30 vs. 60'=B1_30vs60, '2nd block: 30 vs. 60'=B2_30vs60, 'last block: 30 vs. 60'=B3_30vs60)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('target', 'block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
mirrorMTMirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- mirrorMTMirOnlineComparisons(method=method)
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

#faster MT for 30 target across all blocks

#Washout phase
#check target by block within washout period
RAEMTMirOnlineANOVA <- function() {
  
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)                   
  
  #looking into interaction below:
  interaction.plot(LC4aov$target, LC4aov$block, LC4aov$movementtime)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(block, target), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Movement time during washout trials across targets and blocks: \n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#block effect only
RAEMTMirOnlineComparisonMeansBlockEffect <- function(){
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)   
  
  LC4aov <- aggregate(movementtime ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","movementtime",LC4aov,within=c("block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

RAEMTMirOnlineComparisonsBlockEffect <- function(method='bonferroni'){
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)   
  
  LC4aov <- aggregate(movementtime ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","movementtime",LC4aov,within=c("block"))
  
  #specify contrasts
  #levels of target are: far, mid, near
  b1vsb2 <- c(-1,1,0)
  b1vsb3 <- c(-1,0,1)
  b2vsb3 <- c(0,-1,1)
  
  contrastList <- list('first vs. second block'=b1vsb2, 'first vs. last block'=b1vsb3, 'second vs. last block'=b2vsb3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
RAEMTMirOnlineComparisonsEffSizeBlockEffect <- function(method = 'bonferroni'){
  comparisons <- RAEMTMirOnlineComparisonsBlockEffect(method=method)
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

#first and second block do not differ, but last block differs from both


#compare with baseline
RAEBaselineMTMirOnlineANOVA <- function() {
  
  blockdefs <- list('first'=c(1,3), 'second'=c(4,3),'last'=c(18,3))
  LC_aligned <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)
  LC_aligned$session <- 'baseline'
  
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC_washout <- getAlignedBlockedMTMirOnlineAOV(blockdefs=blockdefs)                      
  LC_washout$session <- 'washout'
  
  LC4aov <- rbind(LC_aligned, LC_washout)
  LC4aov$block <- factor(LC4aov$block, levels = c('first', 'second', 'last'))
  LC4aov$session <- factor(LC4aov$session, levels = c('baseline', 'washout'))
  #looking into interaction below:
  #interaction.plot(LC4aov$block, LC4aov$session, LC4aov$movementtime)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(block, target, session), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Comparing movement times during washout trials with aligned trials across targets and blocks:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#main effects of block and session, no interactions
#block is explained by analysis above when considering each session
#main interest is session effect, and we see in plot that washout has lower MTs

#Statistics (Path Length)----
getAlignedBlockedPLMirOnlineAOV <- function(groups = c('30', '60'), blockdefs) {
  
  LCaov <- data.frame()
  for(group in groups){
    curves <- read.csv(sprintf('data/mirrorreversal-fall/data/processed/%s_step2_PathLength.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
    curves <- curves[,-1] #remove trial rows
    participants <- colnames(curves)
    N <- length(participants)
    
    #blocked <- array(NA, dim=c(N,length(blockdefs)))
    
    target <- c()
    participant <- c()
    block <- c()
    pathlength <- c()
    
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
        pathlength <- c(pathlength, samples)
      }
    }
    LCBlocked <- data.frame(target, participant, block, pathlength)
    LCaov <- rbind(LCaov, LCBlocked)
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  return(LCaov)
  
}

#check target by block within each aligned period for each hand
alignedPLMirOnlineANOVA <- function() {
  
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)                      
  
  #looking into interaction below:
  interaction.plot(LC4aov$target, LC4aov$block, LC4aov$pathlength)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(block, target), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat(sprintf('Path length during aligned trials across targets and blocks:\n'))
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#no effects

#mirror trials PL
mirrorPLMirOnlineANOVA <- function() {
  
  #can still use alignedMT function as it has all trials
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  
  
  LC4aov <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)               
  
  #looking into interaction below:
  interaction.plot(LC4aov$target, LC4aov$block, LC4aov$pathlength)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(target, block), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  #cat(sprintf('Quadrant %s:\n', quadrant))
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#main effects only (target we see in interaction plot that it is higher for 60 degree)
#block follow up
mirrorPLMirOnlineComparisonMeansBlockEffect <- function(){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)   
  
  LC4aov <- aggregate(pathlength ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","pathlength",LC4aov,within=c("block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

mirrorPLMirOnlineComparisonsBlockEffect <- function(method='bonferroni'){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)   
  
  LC4aov <- aggregate(pathlength ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","pathlength",LC4aov,within=c("block"))
  
  #specify contrasts
  #levels of target are: far, mid, near
  b1vsb2 <- c(-1,1,0)
  b1vsb3 <- c(-1,0,1)
  b2vsb3 <- c(0,-1,1)
  
  contrastList <- list('first vs. second block'=b1vsb2, 'first vs. last block'=b1vsb3, 'second vs. last block'=b2vsb3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
mirrorPLMirOnlineComparisonsEffSizeBlockEffect <- function(method = 'bonferroni'){
  comparisons <- mirrorPLMirOnlineComparisonsBlockEffect(method=method)
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

#each block different from each other

#Washout phase
#check target by block within washout period
RAEPLMirOnlineANOVA <- function() {
  
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)                   
  
  #looking into interaction below:
  interaction.plot(LC4aov$target, LC4aov$block, LC4aov$pathlength)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(block, target), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path length during washout trials across targets and blocks: \n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#block effect
RAEPLMirOnlineComparisonMeansBlockEffect <- function(){
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)   
  
  LC4aov <- aggregate(pathlength ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","pathlength",LC4aov,within=c("block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

RAEPLMirOnlineComparisonsBlockEffect <- function(method='bonferroni'){
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)   
  
  LC4aov <- aggregate(pathlength ~ block* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","pathlength",LC4aov,within=c("block"))
  
  #specify contrasts
  #levels of target are: far, mid, near
  b1vsb2 <- c(-1,1,0)
  b1vsb3 <- c(-1,0,1)
  b2vsb3 <- c(0,-1,1)
  
  contrastList <- list('first vs. second block'=b1vsb2, 'first vs. last block'=b1vsb3, 'second vs. last block'=b2vsb3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
RAEPLMirOnlineComparisonsEffSizeBlockEffect <- function(method = 'bonferroni'){
  comparisons <- RAEPLMirOnlineComparisonsBlockEffect(method=method)
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

#first block differs from second and last block

#compare with baseline
RAEBaselinePLMirOnlineANOVA <- function() {
  
  blockdefs <- list('first'=c(1,3), 'second'=c(4,3),'last'=c(18,3))
  LC_aligned <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)
  LC_aligned$session <- 'baseline'
  
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC_washout <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)                      
  LC_washout$session <- 'washout'
  
  LC4aov <- rbind(LC_aligned, LC_washout)
  LC4aov$block <- factor(LC4aov$block, levels = c('first', 'second', 'last'))
  LC4aov$session <- factor(LC4aov$session, levels = c('baseline', 'washout'))
  #looking into interaction below:
  #interaction.plot(LC4aov$block, LC4aov$session, LC4aov$movementtime)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(block, target, session), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Comparing path length during washout trials with aligned trials across targets and blocks:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#main effects of block and session, interaction of block and session
RAEBaselinePLMirOnlineComparisonMeans <- function(){
  blockdefs <- list('first'=c(1,3), 'second'=c(4,3),'last'=c(18,3))
  LC_aligned <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)
  LC_aligned$session <- 'baseline'
  
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC_washout <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)                      
  LC_washout$session <- 'washout'
  
  LC4aov <- rbind(LC_aligned, LC_washout)
  LC4aov$block <- factor(LC4aov$block, levels = c('first', 'second', 'last'))
  LC4aov$session <- factor(LC4aov$session, levels = c('baseline', 'washout'))  
  
  LC4aov <- aggregate(pathlength ~ block* session* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","pathlength",LC4aov,within=c("block", "session"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block', 'session'))
  print(cellmeans)
  
}

#we know from the plot that movement time decreases across blocks, but interesting to see target differences within each block

RAEBaselinePLMirOnlineComparisons <- function(method='bonferroni'){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  blockdefs <- list('first'=c(1,3), 'second'=c(4,3),'last'=c(18,3))
  LC_aligned <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)
  LC_aligned$session <- 'baseline'
  
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC_washout <- getAlignedBlockedPLMirOnlineAOV(blockdefs=blockdefs)                      
  LC_washout$session <- 'washout'
  
  LC4aov <- rbind(LC_aligned, LC_washout)
  LC4aov$block <- factor(LC4aov$block, levels = c('first', 'second', 'last'))
  LC4aov$session <- factor(LC4aov$session, levels = c('baseline', 'washout'))  
  
  LC4aov <- aggregate(pathlength ~ block* session* participant, data=LC4aov, FUN=mean)
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant","pathlength",LC4aov,within=c("block", "session"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  B1_basevswash <- c(-1,0,0,1,0,0)
  #second
  B2_basevswash <- c(0,-1,0,0,1,0)
  #last
  B3_basevswash <- c(0,0,-1,0,0,1)
  
  contrastList <- list('1st block: baseline vs. washout'=B1_basevswash, '2nd block: baseline vs. washout'=B2_basevswash, 'last block: baseline vs. washout'=B3_basevswash)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block', 'session')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
RAEBaselinePLMirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- RAEBaselinePLMirOnlineComparisons(method=method)
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

#baseline and washout differ between 1st and 2nd block

#Device (Mouse vs. Trackpad): Learning Plots ----
getDeviceMirOnlineConfInt <- function(groups = c('30','60'), set='fa2020', device){
  for(group in groups){
    data <- getDeviceLC(group=group, set=set, device=device) #this is in qualtricsdata.R
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_DeviceLC_CI.csv', group, device), row.names = F)
    }
  }
}

#use no baseline cleaning data for baseline
getDeviceAlignedMirOnline <- function(group, device){
  
  qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  #then get pplist according to device
  devqualt <- qualtdat[which(qualtdat$Q15 == device),]
  ppqualt <- devqualt$id
  dat <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_CircularAligned.csv', group), check.names = FALSE)
  
  #dat <- removeOutlierAlignedReaches(group = group, set = set)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getDeviceAlignedMirOnlineConfInt <- function(groups = c('30','60'), device){
  for(group in groups){
    data <- getDeviceAlignedMirOnline(group=group, device=device)
    #current fix for summer data being non-randomized and not counterbalanced
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_DeviceAligned_CI.csv', group, device), row.names = F)
    }
  }
}

getDeviceRAEMirOnlineConfInt <- function(groups = c('30','60'), set='fa2020', device){
  for(group in groups){
    data <- getDeviceRAE(group=group, set=set, device=device) #found in qualtricsdata.R
    
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_DeviceRAE_CI.csv', group, device), row.names = F)
    }
  }
}

plotDeviceMirOnline <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mironline-master/doc/fig/Fig2_%s_DeviceAllTasks.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,131), ylim = c(-20,140), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("%s degree target location", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(20, 110), col = 8, lty = 2)
    axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
    axis(2, at = c(-15, 0, 15, 30, 60, 90, 120)) #tick marks for y axis
    
    for(device in devices){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidenceAligned <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_DeviceAligned_CI.csv', group, device))
      groupconfidenceLC <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_DeviceLC_CI.csv', group, device))
      groupconfidenceRAE <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_DeviceRAE_CI.csv', group, device))
      
      
      
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
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(1:20), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(21:110), rev(c(21:110))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(21:110), y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(111:130), rev(c(111:130))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(111:130), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(80,0,legend=c('Mouse','Trackpad'),
           col=c(colourscheme[['Mouse']][['S']],colourscheme[['Trackpad']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
  
  
  
}

#Device (Mouse vs Trackpad): Learning Statistics ----
# we care about differences in device for each target location. We can run tests in each target, as seen in plots.
getDeviceAlignedBlockedMirOnlineAOV <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), blockdefs) {
  #when analyzing angular deviations, make sure that means used are not distorted. Angles form a circle, so regular mean
  # will be 0 for example between -175, 175. But our function that uses bootstrapping will reveal a more accurate mean
  LCaov <- data.frame()
  for(group in groups){
    for(devicetype in devices){
      curves <- getDeviceAlignedMirOnline(group=group, device=devicetype)
      #curves <- read.csv(sprintf('data/mironline-master/data/processed/%s_CircularAligned.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      angdev <- c()
      device <- c()
      
      for (ppno in c(1:N)) {
        
        pp <- participants[ppno]
        
        for (blockno in c(1:length(blockdefs))) {
          #for each participant, and every 9 trials, get the mean
          blockdef <- blockdefs[[blockno]]
          blockstart <- blockdef[1]
          blockend <- blockstart + blockdef[2] - 1
          samples <- curves[blockstart:blockend,ppno]
          samples <- getAngularReachDevsStats(data=samples)
          #samples <- samples[[2]]
          
          target <- c(target, group)
          participant <- c(participant, pp)
          block <- c(block, names(blockdefs)[blockno])
          angdev <- c(angdev, samples)
          device <- c(device, devicetype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, angdev, device)
      LCaov <- rbind(LCaov, LCBlocked)
    }

  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$device <- as.factor(LCaov$device)
  return(LCaov)
  
}

deviceAlignedMirOnlineANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
    LC4aov <- getDeviceAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$device, LC4aov$block, LC4aov$angdev)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=angdev, within= c(block), between= c(device), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Angular reach deviations during aligned trials across device and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}

#no effects for 30 degree target
# follow up: device by block interaction for 60 degree target
deviceAligned60MirOnlineComparisonMeans <- function(group = '60'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getDeviceAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="angdev",LC4aov,within=c("block"),between=c("device"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block','device'))
  print(cellmeans)
  
}

deviceAligned60MirOnlineComparisons <- function(group='60', method='bonferroni'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getDeviceAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="angdev",LC4aov,within=c("block"),between=c("device"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  B1_MvsT <- c(-1,0,0,1,0,0)
  #second
  B2_MvsT <- c(0,-1,0,0,1,0)
  #last
  B3_MvsT <- c(0,0,-1,0,0,1)
  
  contrastList <- list('1st block: Mouse vs. Trackpad'=B1_MvsT, '2nd block: Mouse vs. Trackpad'=B2_MvsT, 'last block: Mouse vs. Trackpad'=B3_MvsT)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block', 'device')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
deviceAligned60MirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- deviceAligned60MirOnlineComparisons(group='60', method=method)
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

#driven by difference between devices in block 1

#Mirror trials - we can transform to percentages again, to make things consistent with others
getDeviceMirrorMirOnline <- function(group, device){
  
  qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  #then get pplist according to device
  devqualt <- qualtdat[which(qualtdat$Q15 == device),]
  ppqualt <- devqualt$id
  dat <- read.csv(file=sprintf('data/mironline-master/data/statistics/%s_PercentCompensation.csv', group), check.names = FALSE)
  
  #dat <- removeOutlierAlignedReaches(group = group, set = set)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getDeviceMirrorBlockedMirOnlineAOV <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), blockdefs) {
  
  LCaov <- data.frame()
  for(group in groups){
    for(devicetype in devices){
      curves <- getDeviceMirrorMirOnline(group=group, device=devicetype)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      percentcomp <- c()
      device <- c()
      
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
          device <- c(device, devicetype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, percentcomp, device)
      LCaov <- rbind(LCaov, LCBlocked)
    }
  }
  
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$device <- as.factor(LCaov$device)
  return(LCaov)
  
}

deviceMirrorMirOnlineANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
    LC4aov <- getDeviceMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$device, LC4aov$block, LC4aov$percentcomp)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=percentcomp, within= c(block), between= c(device), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Angular reach deviations during mirrored trials across device and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}

#there is a block effect for 30 degrees
deviceMirror30MirOnlineComparisonMeans <- function(group = '30'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getDeviceMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="percentcomp",LC4aov,within=c("block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

deviceMirror30MirOnlineComparisons <- function(group='30', method='bonferroni'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getDeviceMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="percentcomp",LC4aov,within=c("block"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  B1vsB2<- c(-1,1,0)
  #second
  B1vsB3 <- c(-1,0,1)
  #last
  B2vsB3 <- c(0,-1,1)
  
  contrastList <- list('Block 1 vs. Block 2'=B1vsB2, 'Block 1 vs. Last block'=B1vsB3, 'Block 2 vs. Last block'=B2vsB3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
deviceMirror30MirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- deviceMirror30MirOnlineComparisons(group='30', method=method)
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
#first block differs

#there is a block effect for 60 degrees
deviceMirror60MirOnlineComparisonMeans <- function(group = '60'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getDeviceMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="percentcomp",LC4aov,within=c("block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

deviceMirror60MirOnlineComparisons <- function(group='60', method='bonferroni'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getDeviceMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="percentcomp",LC4aov,within=c("block"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  B1vsB2<- c(-1,1,0)
  #second
  B1vsB3 <- c(-1,0,1)
  #last
  B2vsB3 <- c(0,-1,1)
  
  contrastList <- list('Block 1 vs. Block 2'=B1vsB2, 'Block 1 vs. Last block'=B1vsB3, 'Block 2 vs. Last block'=B2vsB3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
deviceMirror60MirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- deviceMirror60MirOnlineComparisons(group='60', method=method)
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
#second block differs

#RAE
getDeviceRAEBlockedMirOnlineAOV <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), blockdefs) {
  #when analyzing angular deviations, make sure that means used are not distorted. Angles form a circle, so regular mean
  # will be 0 for example between -175, 175. But our function that uses bootstrapping will reveal a more accurate mean
  LCaov <- data.frame()
  for(group in groups){
    for(devicetype in devices){
      curves <- getDeviceRAE(group=group, set='fa2020', device=devicetype)
      #curves <- read.csv(sprintf('data/mironline-master/data/processed/%s_CircularAligned.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      angdev <- c()
      device <- c()
      
      for (ppno in c(1:N)) {
        
        pp <- participants[ppno]
        
        for (blockno in c(1:length(blockdefs))) {
          #for each participant, and every 9 trials, get the mean
          blockdef <- blockdefs[[blockno]]
          blockstart <- blockdef[1]
          blockend <- blockstart + blockdef[2] - 1
          samples <- curves[blockstart:blockend,ppno]
          samples <- getAngularReachDevsStats(data=samples)
          #samples <- samples[[2]]
          
          target <- c(target, group)
          participant <- c(participant, pp)
          block <- c(block, names(blockdefs)[blockno])
          angdev <- c(angdev, samples)
          device <- c(device, devicetype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, angdev, device)
      LCaov <- rbind(LCaov, LCBlocked)
    }
    
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$device <- as.factor(LCaov$device)
  return(LCaov)
  
}

deviceRAEMirOnlineANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
    LC4aov <- getDeviceRAEBlockedMirOnlineAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$device, LC4aov$block, LC4aov$angdev)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=angdev, within= c(block), between= c(device), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Angular reach deviations during aligned trials across device and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}

#no effect for 30 degrees, target effect for 60 degrees (trackpad is lower, as seen in plot), no interactions

#Device (Mouse vs. Trackpad): MT Plots ----
getDeviceMirOnlineMT <- function(groups = c('30','60'), device){
  for(group in groups){
    
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_step2_MovementTime.csv', group), check.names = FALSE)
    
    #dat <- removeOutlierAlignedReaches(group = group, set = set)
    
    #keep only data of pp from this list
    trial <- dat$trial
    ndat <- dat[,which(colnames(dat) %in% ppqualt)]
    dat <- cbind(trial, ndat)
    
    return(dat)
  }
}

getDeviceMirOnlineMTCI <- function(groups = c('30','60'), device, type='t'){
  for(group in groups){
    data <- getDeviceMirOnlineMT(group=group, device=device)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_MovementTime_CI.csv', group, device), row.names = F) 
    }
  }
}

plotDeviceMirOnlineMT <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mironline-master/doc/fig/Fig3_%s_DeviceMT.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,131), ylim = c(-1,11), 
         xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("%s degree target location", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(1), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(20, 110), col = 8, lty = 2)
    axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
    axis(2, at = c(0, 1, 2, 4, 6, 8, 10)) #tick marks for y axis
    
    for(device in devices){
      #read in files created
      groupconfidence <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_MovementTime_CI.csv', group, device))
      
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:20,]
      groupconfidenceLC <- groupconfidence[21:110,]
      groupconfidenceRAE <- groupconfidence[111:130,] 
      
      
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
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(1:20), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(21:110), rev(c(21:110))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(21:110), y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(111:130), rev(c(111:130))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(111:130), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(80,0.5,legend=c('Mouse','Trackpad'),
           col=c(colourscheme[['Mouse']][['S']],colourscheme[['Trackpad']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
  
  
  
}

#Device (Mouse vs. Trackpad): MT Stats----
getDeviceAlignedBlockedMirOnlineMTAOV <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), blockdefs) {
  #when analyzing angular deviations, make sure that means used are not distorted. Angles form a circle, so regular mean
  # will be 0 for example between -175, 175. But our function that uses bootstrapping will reveal a more accurate mean
  LCaov <- data.frame()
  for(group in groups){
    for(devicetype in devices){
      curves <- getDeviceMirOnlineMT(group=group, device=devicetype)
      #curves <- read.csv(sprintf('data/mironline-master/data/processed/%s_%s_CircularAligned.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      movementtime <- c()
      device <- c()
      
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
          movementtime <- c(movementtime, samples)
          device <- c(device, devicetype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, movementtime, device)
      LCaov <- rbind(LCaov, LCBlocked)
    }
    
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$device <- as.factor(LCaov$device)
  return(LCaov)
  
}

deviceAlignedMirOnlineMTANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
    LC4aov <- getDeviceAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$device, LC4aov$block, LC4aov$movementtime)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(block), between= c(device), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Movement time during aligned trials across device and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}

#block effect for 30 degree target, which is already evident on plot
#interaction for 60 degree target

deviceAlignedMTMirOnlineComparisonMeans <- function(group = '60'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getDeviceAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("device"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block','device'))
  print(cellmeans)
  
}

deviceAlignedMTMirOnlineComparisons <- function(group='60', method='bonferroni'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getDeviceAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("device"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  M_B1vsT_B1<- c(-1,0,0,1,0,0)
  #second
  M_B2vsT_B2 <- c(0,-1,0,0,1,0)
  #last
  M_B3vsT_B3 <- c(0,0,-1,0,0,1)
  
  contrastList <- list('Block 1: Mouse vs. Trackpad'=M_B1vsT_B1, 'Block 2: Mouse vs. Trackpad'=M_B2vsT_B2, 'Last block: Mouse vs. Trackpad'=M_B3vsT_B3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block','device')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
deviceAlignedMTMirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- deviceAlignedMTMirOnlineComparisons(group='60', method=method)
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

#mouse differs from trackpad in block 1 for 60 degree target

#Mirror trials
deviceMirrorMirOnlineMTANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
    LC4aov <- getDeviceAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$device, LC4aov$block, LC4aov$movementtime)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(block), between= c(device), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Movement time during mirror trials across device and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#main effects of device and block for 30 degree target (block is obvious, mouse is a bit faster). but no interaction
#interaction for 60 degree target
deviceMirrorMTMirOnlineComparisonMeans <- function(group = '60'){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getDeviceAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("device"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block','device'))
  print(cellmeans)
  
}

deviceMirrorMTMirOnlineComparisons <- function(group='60', method='bonferroni'){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getDeviceAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("device"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  M_B1vsT_B1<- c(-1,0,0,1,0,0)
  #second
  M_B2vsT_B2 <- c(0,-1,0,0,1,0)
  #last
  M_B3vsT_B3 <- c(0,0,-1,0,0,1)
  
  contrastList <- list('Block 1: Mouse vs. Trackpad'=M_B1vsT_B1, 'Block 2: Mouse vs. Trackpad'=M_B2vsT_B2, 'Last block: Mouse vs. Trackpad'=M_B3vsT_B3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block','device')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
deviceMirrorMTMirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- deviceMirrorMTMirOnlineComparisons(group='60', method=method)
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
#mouse is faster than trackpad for blocks 1 and 2, not sig for last block

#Washout trials
deviceRAEMirOnlineMTANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
    LC4aov <- getDeviceAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$device, LC4aov$block, LC4aov$movementtime)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(block), between= c(device), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Movement time during washout trials across device and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#no effects for 30 degree target
#interaction for 60 degree target
deviceRAEMTMirOnlineComparisonMeans <- function(group = '60'){
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getDeviceAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("device"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block','device'))
  print(cellmeans)
  
}

deviceRAEMTMirOnlineComparisons <- function(group='60', method='bonferroni'){
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getDeviceAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("device"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  M_B1vsT_B1<- c(-1,0,0,1,0,0)
  #second
  M_B2vsT_B2 <- c(0,-1,0,0,1,0)
  #last
  M_B3vsT_B3 <- c(0,0,-1,0,0,1)
  
  contrastList <- list('Block 1: Mouse vs. Trackpad'=M_B1vsT_B1, 'Block 2: Mouse vs. Trackpad'=M_B2vsT_B2, 'Last block: Mouse vs. Trackpad'=M_B3vsT_B3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block','device')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
deviceRAEMTMirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- deviceRAEMTMirOnlineComparisons(group='60', method=method)
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
#mouse is faster for all blocks


#Device (Mouse vs. Trackpad): PL Plots ----
getDeviceMirOnlinePL <- function(groups = c('30','60'), device){
  for(group in groups){
    
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_step2_PathLength.csv', group), check.names = FALSE)
    
    #dat <- removeOutlierAlignedReaches(group = group, set = set)
    
    #keep only data of pp from this list
    trial <- dat$trial
    ndat <- dat[,which(colnames(dat) %in% ppqualt)]
    dat <- cbind(trial, ndat)
    
    return(dat)
  }
}

getDeviceMirOnlinePLCI <- function(groups = c('30','60'), device, type='t'){
  for(group in groups){
    data <- getDeviceMirOnlinePL(group=group, device=device)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_PathLength_CI.csv', group, device), row.names = F) 
    }
  }
}

plotDeviceMirOnlinePL <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mironline-master/doc/fig/Fig4_%s_DevicePL.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,131), ylim = c(-3,15), 
         xlab = "Trial", ylab = "Path Length (monitor scale)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("%s degree target location", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(20, 110), col = 8, lty = 2)
    axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
    axis(2, at = c(0, 2, 4, 6, 8, 10)) #tick marks for y axis
    
    for(device in devices){
      #read in files created
      groupconfidence <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_PathLength_CI.csv', group, device))
      
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:20,]
      groupconfidenceLC <- groupconfidence[21:110,]
      groupconfidenceRAE <- groupconfidence[111:130,] 
      
      
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
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(1:20), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(21:110), rev(c(21:110))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(21:110), y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(111:130), rev(c(111:130))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[device]][['S']]
      lines(x = c(111:130), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(80,0.5,legend=c('Mouse','Trackpad'),
           col=c(colourscheme[['Mouse']][['S']],colourscheme[['Trackpad']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
  
  
  
}

#Device (Mouse vs. Trackpad): PL Stats----
getDeviceAlignedBlockedMirOnlinePLAOV <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), blockdefs) {
  #when analyzing angular deviations, make sure that means used are not distorted. Angles form a circle, so regular mean
  # will be 0 for example between -175, 175. But our function that uses bootstrapping will reveal a more accurate mean
  LCaov <- data.frame()
  for(group in groups){
    for(devicetype in devices){
      curves <- getDeviceMirOnlinePL(group=group, device=devicetype)
      #curves <- read.csv(sprintf('data/mironline-master/data/processed/%s_%s_CircularAligned.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      pathlength <- c()
      device <- c()
      
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
          pathlength <- c(pathlength, samples)
          device <- c(device, devicetype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, pathlength, device)
      LCaov <- rbind(LCaov, LCBlocked)
    }
    
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$device <- as.factor(LCaov$device)
  return(LCaov)
  
}

deviceAlignedMirOnlinePLANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
    LC4aov <- getDeviceAlignedBlockedMirOnlinePLAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$device, LC4aov$block, LC4aov$pathlength)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(block), between= c(device), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Path length during aligned trials across device and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#device effect for 30, mouse has larger path length, but no interaction
#no effects for 60

#Mirror trials
deviceMirrorMirOnlinePLANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
    LC4aov <- getDeviceAlignedBlockedMirOnlinePLAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$device, LC4aov$block, LC4aov$pathlength)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(block), between= c(device), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Path length during mirror trials across device and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#device and block effect, no interaction for 30 (mouse is larger, blocks go down as seen in plot)
#block effect, no interaction for 60 (goes down as seen in plot)

#Washout trials
deviceRAEMirOnlinePLANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
    LC4aov <- getDeviceAlignedBlockedMirOnlinePLAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$device, LC4aov$block, LC4aov$pathlength)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(block), between= c(device), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Path length during washout trials across device and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#block effect for 30, no interaction (goes down from first block)
#main effects for 60, no interaction (goes down from first block, mouse has larger PL)

#Mouse moves faster, but has larger PL (could be that trackpad users stop more along the way?)




#Sex (Male vs. Female): Learning Plots ----
getSexMirOnlineConfInt <- function(groups = c('30','60'), set='fa2020', sex){
  for(group in groups){
    data <- getSexLC(group=group, set=set, sex=sex) #this is in qualtricsdata.R
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_SexLC_CI.csv', group, sex), row.names = F)
    }
  }
}

#use no baseline cleaning data for baseline
getSexAlignedMirOnline <- function(group, sex){
  
  qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  #then get pplist according to sex
  devqualt <- qualtdat[which(qualtdat$Q5 == sex),]
  ppqualt <- devqualt$id
  dat <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_CircularAligned.csv', group), check.names = FALSE)
  
  #dat <- removeOutlierAlignedReaches(group = group, set = set)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getSexAlignedMirOnlineConfInt <- function(groups = c('30','60'), sex){
  for(group in groups){
    data <- getSexAlignedMirOnline(group=group, sex=sex)
    #current fix for summer data being non-randomized and not counterbalanced
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_SexAligned_CI.csv', group, sex), row.names = F)
    }
  }
}

getSexRAEMirOnlineConfInt <- function(groups = c('30','60'), set='fa2020', sex){
  for(group in groups){
    data <- getSexRAE(group=group, set=set, sex=sex) #found in qualtricsdata.R
    
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_SexRAE_CI.csv', group, sex), row.names = F)
    }
  }
}

plotSexMirOnline <- function(groups = c('30', '60'), sexes = c('Male','Female'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mironline-master/doc/fig/Fig5_%s_SexAllTasks.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,131), ylim = c(-20,140), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("%s degree target location", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(20, 110), col = 8, lty = 2)
    axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
    axis(2, at = c(-15, 0, 15, 30, 60, 90, 120)) #tick marks for y axis
    
    for(sex in sexes){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidenceAligned <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_SexAligned_CI.csv', group, sex))
      groupconfidenceLC <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_SexLC_CI.csv', group, sex))
      groupconfidenceRAE <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_SexRAE_CI.csv', group, sex))
      
      
      
      colourscheme <- getSexColourScheme(sexes=sex)
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(1:20), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(21:110), rev(c(21:110))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(21:110), y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(111:130), rev(c(111:130))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(111:130), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(80,0,legend=c('Male','Female'),
           col=c(colourscheme[['Male']][['S']],colourscheme[['Female']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
  
  
  
}


#Sex (Male vs Female): Learning Statistics ----
# we care about differences in device for each target location. We can run tests in each target, as seen in plots.
getSexAlignedBlockedMirOnlineAOV <- function(groups = c('30', '60'), sexes = c('Male','Female'), blockdefs) {
  #when analyzing angular deviations, make sure that means used are not distorted. Angles form a circle, so regular mean
  # will be 0 for example between -175, 175. But our function that uses bootstrapping will reveal a more accurate mean
  LCaov <- data.frame()
  for(group in groups){
    for(sextype in sexes){
      curves <- getSexAlignedMirOnline(group=group, sex=sextype)
      #curves <- read.csv(sprintf('data/mironline-master/data/processed/%s_CircularAligned.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      angdev <- c()
      sex <- c()
      
      for (ppno in c(1:N)) {
        
        pp <- participants[ppno]
        
        for (blockno in c(1:length(blockdefs))) {
          #for each participant, and every 9 trials, get the mean
          blockdef <- blockdefs[[blockno]]
          blockstart <- blockdef[1]
          blockend <- blockstart + blockdef[2] - 1
          samples <- curves[blockstart:blockend,ppno]
          samples <- getAngularReachDevsStats(data=samples)
          #samples <- samples[[2]]
          
          target <- c(target, group)
          participant <- c(participant, pp)
          block <- c(block, names(blockdefs)[blockno])
          angdev <- c(angdev, samples)
          sex <- c(sex, sextype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, angdev, sex)
      LCaov <- rbind(LCaov, LCBlocked)
    }
    
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$sex <- factor(LCaov$sex, levels = c('Male','Female'))
  return(LCaov)
  
}

sexAlignedMirOnlineANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
    LC4aov <- getSexAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$sex, LC4aov$block, LC4aov$angdev)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=angdev, within= c(block), between= c(sex), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Angular reach deviations during aligned trials across sex and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#no effects for 30 degree target
#interaction for 60 degree target
sexAligned60MirOnlineComparisonMeans <- function(group = '60'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getSexAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="angdev",LC4aov,within=c("block"),between=c("sex"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block','sex'))
  print(cellmeans)
  
}

sexAligned60MirOnlineComparisons <- function(group='60', method='bonferroni'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
  LC4aov <- getSexAlignedBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="angdev",LC4aov,within=c("block"),between=c("sex"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  B1_MvsF <- c(-1,0,0,1,0,0)
  #second
  B2_MvsF <- c(0,-1,0,0,1,0)
  #last
  B3_MvsF <- c(0,0,-1,0,0,1)
  
  contrastList <- list('1st block: Male vs. Female'=B1_MvsF, '2nd block: Male vs. Female'=B2_MvsF, 'last block: Male vs. Female'=B3_MvsF)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block', 'sex')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
sexAligned60MirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- sexAligned60MirOnlineComparisons(group='60', method=method)
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
#males and females differ during second block (males with highers angdev)

#Mirror trials - we can transform to percentages again, to make things consistent with others
getSexMirrorMirOnline <- function(group, sex){
  
  qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
  #then get pplist according to sex
  devqualt <- qualtdat[which(qualtdat$Q5 == sex),]
  ppqualt <- devqualt$id
  dat <- read.csv(file=sprintf('data/mironline-master/data/statistics/%s_PercentCompensation.csv', group), check.names = FALSE)
  
  #dat <- removeOutlierAlignedReaches(group = group, set = set)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getSexMirrorBlockedMirOnlineAOV <- function(groups = c('30', '60'), sexes = c('Male','Female'), blockdefs) {
  
  LCaov <- data.frame()
  for(group in groups){
    for(sextype in sexes){
      curves <- getSexMirrorMirOnline(group=group, sex=sextype)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      percentcomp <- c()
      sex <- c()
      
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
          sex <- c(sex, sextype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, percentcomp, sex)
      LCaov <- rbind(LCaov, LCBlocked)
    }
  }
  
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$sex <- factor(LCaov$sex, levels = c('Male','Female'))
  return(LCaov)
  
}

sexMirrorMirOnlineANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
    LC4aov <- getSexMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$sex, LC4aov$block, LC4aov$percentcomp)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=percentcomp, within= c(block), between= c(sex), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Angular reach deviations during mirrored trials across sex and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}

#there is a block effect for 30 degrees
sexMirror30MirOnlineComparisonMeans <- function(group = '30'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getSexMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="percentcomp",LC4aov,within=c("block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

sexMirror30MirOnlineComparisons <- function(group='30', method='bonferroni'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getSexMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="percentcomp",LC4aov,within=c("block"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  B1vsB2<- c(-1,1,0)
  #second
  B1vsB3 <- c(-1,0,1)
  #last
  B2vsB3 <- c(0,-1,1)
  
  contrastList <- list('Block 1 vs. Block 2'=B1vsB2, 'Block 1 vs. Last block'=B1vsB3, 'Block 2 vs. Last block'=B2vsB3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
sexMirror30MirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- sexMirror30MirOnlineComparisons(group='30', method=method)
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
#first block differs

#there is a block effect for 60 degrees
sexMirror60MirOnlineComparisonMeans <- function(group = '60'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getSexMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="percentcomp",LC4aov,within=c("block"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block'))
  print(cellmeans)
  
}

sexMirror60MirOnlineComparisons <- function(group='60', method='bonferroni'){
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  LC4aov <- getSexMirrorBlockedMirOnlineAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="percentcomp",LC4aov,within=c("block"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  B1vsB2<- c(-1,1,0)
  #second
  B1vsB3 <- c(-1,0,1)
  #last
  B2vsB3 <- c(0,-1,1)
  
  contrastList <- list('Block 1 vs. Block 2'=B1vsB2, 'Block 1 vs. Last block'=B1vsB3, 'Block 2 vs. Last block'=B2vsB3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
sexMirror60MirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- sexMirror60MirOnlineComparisons(group='60', method=method)
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
#block 2 differs

#RAE
getSexRAEBlockedMirOnlineAOV <- function(groups = c('30', '60'), sexes = c('Male','Female'), blockdefs) {
  #when analyzing angular deviations, make sure that means used are not distorted. Angles form a circle, so regular mean
  # will be 0 for example between -175, 175. But our function that uses bootstrapping will reveal a more accurate mean
  LCaov <- data.frame()
  for(group in groups){
    for(sextype in sexes){
      curves <- getSexRAE(group=group, set='fa2020', sex=sextype)
      #curves <- read.csv(sprintf('data/mironline-master/data/processed/%s_CircularAligned.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      angdev <- c()
      sex <- c()
      
      for (ppno in c(1:N)) {
        
        pp <- participants[ppno]
        
        for (blockno in c(1:length(blockdefs))) {
          #for each participant, and every 9 trials, get the mean
          blockdef <- blockdefs[[blockno]]
          blockstart <- blockdef[1]
          blockend <- blockstart + blockdef[2] - 1
          samples <- curves[blockstart:blockend,ppno]
          samples <- getAngularReachDevsStats(data=samples)
          #samples <- samples[[2]]
          
          target <- c(target, group)
          participant <- c(participant, pp)
          block <- c(block, names(blockdefs)[blockno])
          angdev <- c(angdev, samples)
          sex <- c(sex, sextype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, angdev, sex)
      LCaov <- rbind(LCaov, LCBlocked)
    }
    
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$sex <- factor(LCaov$sex, levels = c('Male','Female'))
  return(LCaov)
  
}

sexRAEMirOnlineANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
    LC4aov <- getSexRAEBlockedMirOnlineAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$sex, LC4aov$block, LC4aov$angdev)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=angdev, within= c(block), between= c(sex), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Angular reach deviations during aligned trials across sex and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}

#no effects for either target



#Sex (Male vs. Female): MT Plots ----
getSexMirOnlineMT <- function(groups = c('30','60'), sex){
  for(group in groups){
    
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to sex
    devqualt <- qualtdat[which(qualtdat$Q5 == sex),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_step2_MovementTime.csv', group), check.names = FALSE)
    
    #dat <- removeOutlierAlignedReaches(group = group, set = set)
    
    #keep only data of pp from this list
    trial <- dat$trial
    ndat <- dat[,which(colnames(dat) %in% ppqualt)]
    dat <- cbind(trial, ndat)
    
    return(dat)
  }
}

getSexMirOnlineMTCI <- function(groups = c('30','60'), sex, type='t'){
  for(group in groups){
    data <- getSexMirOnlineMT(group=group, sex=sex)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_MovementTime_CI.csv', group, sex), row.names = F) 
    }
  }
}

plotSexMirOnlineMT <- function(groups = c('30', '60'), sexes = c('Male','Female'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mironline-master/doc/fig/Fig6_%s_SexMT.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,131), ylim = c(-1,11), 
         xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("%s degree target location", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(1), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(20, 110), col = 8, lty = 2)
    axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
    axis(2, at = c(0, 1, 2, 4, 6, 8, 10)) #tick marks for y axis
    
    for(sex in sexes){
      #read in files created
      groupconfidence <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_MovementTime_CI.csv', group, sex))
      
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:20,]
      groupconfidenceLC <- groupconfidence[21:110,]
      groupconfidenceRAE <- groupconfidence[111:130,] 
      
      
      colourscheme <- getSexColourScheme(sexes=sex)
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(1:20), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(21:110), rev(c(21:110))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(21:110), y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(111:130), rev(c(111:130))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(111:130), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(80,0.5,legend=c('Male','Female'),
           col=c(colourscheme[['Male']][['S']],colourscheme[['Female']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
  
  
  
}

#Sex (Male vs. Female): MT Stats----
getSexAlignedBlockedMirOnlineMTAOV <- function(groups = c('30', '60'), sexes = c('Male','Female'), blockdefs) {
  #when analyzing angular deviations, make sure that means used are not distorted. Angles form a circle, so regular mean
  # will be 0 for example between -175, 175. But our function that uses bootstrapping will reveal a more accurate mean
  LCaov <- data.frame()
  for(group in groups){
    for(sextype in sexes){
      curves <- getSexMirOnlineMT(group=group, sex=sextype)
      #curves <- read.csv(sprintf('data/mironline-master/data/processed/%s_%s_CircularAligned.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      movementtime <- c()
      sex <- c()
      
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
          movementtime <- c(movementtime, samples)
          sex <- c(sex, sextype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, movementtime, sex)
      LCaov <- rbind(LCaov, LCBlocked)
    }
    
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$sex <- factor(LCaov$sex, levels = c('Male','Female'))
  return(LCaov)
  
}

sexAlignedMirOnlineMTANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
    LC4aov <- getSexAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$sex, LC4aov$block, LC4aov$movementtime)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(block), between= c(sex), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Movement time during aligned trials across sex and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#main effects for 30: As seen in plot, males are faster and block goes down
#same main effects for 60


#Mirror trials
sexMirrorMirOnlineMTANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
    LC4aov <- getSexAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$sex, LC4aov$block, LC4aov$movementtime)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(block), between= c(sex), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Movement time during mirror trials across sex and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}

#interaction for 30 degree target
sexMirrorMTMirOnlineComparisonMeans <- function(group = '30'){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getSexAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("sex"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block','sex'))
  print(cellmeans)
  
}

sexMirrorMTMirOnlineComparisons <- function(group='30', method='bonferroni'){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getSexAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("sex"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  M_B1vsF_B1<- c(-1,0,0,1,0,0)
  #second
  M_B2vsF_B2 <- c(0,-1,0,0,1,0)
  #last
  M_B3vsF_B3 <- c(0,0,-1,0,0,1)
  
  contrastList <- list('Block 1: Male vs. Female'=M_B1vsF_B1, 'Block 2: Male vs. Female'=M_B2vsF_B2, 'Last block: Male vs. Female'=M_B3vsF_B3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block','sex')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
sexMirrorMTMirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- sexMirrorMTMirOnlineComparisons(group='30', method=method)
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

#Males are faster during blocks 1 and last block

#interaction for 60 degree target
sexMirrorMT60MirOnlineComparisonMeans <- function(group = '60'){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getSexAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("sex"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block','sex'))
  print(cellmeans)
  
}

sexMirrorMT60MirOnlineComparisons <- function(group='60', method='bonferroni'){
  blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
  LC4aov <- getSexAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("sex"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  M_B1vsF_B1<- c(-1,0,0,1,0,0)
  #second
  M_B2vsF_B2 <- c(0,-1,0,0,1,0)
  #last
  M_B3vsF_B3 <- c(0,0,-1,0,0,1)
  
  contrastList <- list('Block 1: Male vs. Female'=M_B1vsF_B1, 'Block 2: Male vs. Female'=M_B2vsF_B2, 'Last block: Male vs. Female'=M_B3vsF_B3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block','sex')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
sexMirrorMT60MirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- sexMirrorMT60MirOnlineComparisons(group='60', method=method)
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

#Males are faster during all blocks

#Washout trials
sexRAEMirOnlineMTANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
    LC4aov <- getSexAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$sex, LC4aov$block, LC4aov$movementtime)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=movementtime, within= c(block), between= c(sex), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Movement time during washout trials across sex and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#no effects for 30
#interaction for 60
#interaction for 60 degree target
sexRAEMTMirOnlineComparisonMeans <- function(group = '60'){
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getSexAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("sex"))
  
  cellmeans <- emmeans(secondAOV,specs=c('block','sex'))
  print(cellmeans)
  
}

sexRAEMTMirOnlineComparisons <- function(group='60', method='bonferroni'){
  blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
  LC4aov <- getSexAlignedBlockedMirOnlineMTAOV(blockdefs=blockdefs)                      
  LC4aov <- LC4aov[which(LC4aov$target == group),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  secondAOV <- aov_ez("participant",dv="movementtime",LC4aov,within=c("block"),between=c("sex"))
  
  #specify contrasts
  #levels of target are: 30,60
  #first block
  M_B1vsF_B1<- c(-1,0,0,1,0,0)
  #second
  M_B2vsF_B2 <- c(0,-1,0,0,1,0)
  #last
  M_B3vsF_B3 <- c(0,0,-1,0,0,1)
  
  contrastList <- list('Block 1: Male vs. Female'=M_B1vsF_B1, 'Block 2: Male vs. Female'=M_B2vsF_B2, 'Last block: Male vs. Female'=M_B3vsF_B3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('block','sex')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
sexRAEMTMirOnlineComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- sexRAEMTMirOnlineComparisons(group='60', method=method)
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

#males are faster in all blocks


#Sex (Male vs. Female): PL Plots ----
getSexMirOnlinePL <- function(groups = c('30','60'), sex){
  for(group in groups){
    
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to sex
    devqualt <- qualtdat[which(qualtdat$Q5 == sex),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_step2_PathLength.csv', group), check.names = FALSE)
    
    #dat <- removeOutlierAlignedReaches(group = group, set = set)
    
    #keep only data of pp from this list
    trial <- dat$trial
    ndat <- dat[,which(colnames(dat) %in% ppqualt)]
    dat <- cbind(trial, ndat)
    
    return(dat)
  }
}

getSexMirOnlinePLCI <- function(groups = c('30','60'), sex, type='t'){
  for(group in groups){
    data <- getSexMirOnlinePL(group=group, sex=sex)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
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
      write.csv(confidence, file=sprintf('data/mironline-master/data/processed/%s_%s_PathLength_CI.csv', group, sex), row.names = F) 
    }
  }
}

plotSexMirOnlinePL <- function(groups = c('30', '60'), sexes = c('Male','Female'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mironline-master/doc/fig/Fig7_%s_SexPL.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,131), ylim = c(-3,15), 
         xlab = "Trial", ylab = "Path Length (monitor scale)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("%s degree target location", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(20, 110), col = 8, lty = 2)
    axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
    axis(2, at = c(0, 2, 4, 6, 8, 10)) #tick marks for y axis
    
    for(sex in sexes){
      #read in files created
      groupconfidence <- read.csv(file=sprintf('data/mironline-master/data/processed/%s_%s_PathLength_CI.csv', group, sex))
      
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:20,]
      groupconfidenceLC <- groupconfidence[21:110,]
      groupconfidenceRAE <- groupconfidence[111:130,] 
      
      
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
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(1:20), y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(21:110), rev(c(21:110))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(21:110), y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(111:130), rev(c(111:130))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      col <- colourscheme[[sex]][['S']]
      lines(x = c(111:130), y = na.omit(mid),col=col,lty=1)
    }
    
    #add legend
    legend(80,0.5,legend=c('Male','Female'),
           col=c(colourscheme[['Male']][['S']],colourscheme[['Female']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
  
  
  
}


#Sex (Male vs. Female): PL Stats----
getSexAlignedBlockedMirOnlinePLAOV <- function(groups = c('30', '60'), sexes = c('Male','Female'), blockdefs) {
  #when analyzing angular deviations, make sure that means used are not distorted. Angles form a circle, so regular mean
  # will be 0 for example between -175, 175. But our function that uses bootstrapping will reveal a more accurate mean
  LCaov <- data.frame()
  for(group in groups){
    for(sextype in sexes){
      curves <- getSexMirOnlinePL(group=group, sex=sextype)
      #curves <- read.csv(sprintf('data/mironline-master/data/processed/%s_%s_CircularAligned.csv',group), stringsAsFactors=FALSE, check.names = FALSE)  
      curves <- curves[,-1] #remove trial rows
      participants <- colnames(curves)
      N <- length(participants)
      
      #blocked <- array(NA, dim=c(N,length(blockdefs)))
      
      target <- c()
      participant <- c()
      block <- c()
      pathlength <- c()
      sex <- c()
      
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
          pathlength <- c(pathlength, samples)
          sex<- c(sex, sextype)
        }
      }
      LCBlocked <- data.frame(target, participant, block, pathlength, sex)
      LCaov <- rbind(LCaov, LCBlocked)
    }
    
  }
  #need to make some columns as factors for ANOVA
  LCaov$target <- as.factor(LCaov$target)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last'))
  LCaov$sex <- factor(LCaov$sex, levels = c('Male','Female'))
  return(LCaov)
  
}

sexAlignedMirOnlinePLANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(18,3))
    LC4aov <- getSexAlignedBlockedMirOnlinePLAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$sex, LC4aov$block, LC4aov$pathlength)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(block), between= c(sex), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Path length during aligned trials across sex and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#no effects for both 30 and 60

#Mirror trials
sexMirrorMirOnlinePLANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(21,3),'second'=c(24,3),'last'=c(96,15))
    LC4aov <- getSexAlignedBlockedMirOnlinePLAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$sex, LC4aov$block, LC4aov$pathlength)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(block), between= c(sex), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Path length during mirror trials across sex and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#block effect for both 30 and 60, as seen in plot where PL goes down across blocks

#Washout trials
sexRAEMirOnlinePLANOVA <- function(groups = c('30','60')) {
  for(group in groups){
    blockdefs <- list('first'=c(111,3),'second'=c(114,3),'last'=c(128,3))
    LC4aov <- getSexAlignedBlockedMirOnlinePLAOV(blockdefs=blockdefs)                      
    LC4aov <- LC4aov[which(LC4aov$target == group),]
    #looking into interaction below:
    interaction.plot(LC4aov$sex, LC4aov$block, LC4aov$pathlength)
    
    #learning curve ANOVA's
    # for ez, case ID should be a factor:
    LC4aov$participant <- as.factor(LC4aov$participant)
    firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=pathlength, within= c(block), between= c(sex), type=3, return_aov = TRUE) #df is k-2 or 3 levels minus 2; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
    cat(sprintf('Path length during washout trials across sex and blocks, %s degree target: \n', group))
    print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  }
}
#block effect for both 30 and 60, goes down from first block

#Males moves faster, but no difference in PL