source('ana/shared.R')
source('ana/su&fa2020online.R')

# Qualtrics Data----
#Do the experiment data have corresponding Qualtrics data?
#First two functions here will identify participants that have experimental data, but no qualtrics data (we want to remove them)
getFAWithoutQualtrics <- function(){
  
  #participant list from behavioral data
  
  datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  
  
  
  dataoutput<- c() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- handleOneFile(filename = datafilename)
    ppname <- unique(adat$participant)
    
    dataoutput <- c(dataoutput, ppname)
  }
  
  #read in Qualtrics sheet
  qualt <- read.csv('data/mirrorreversal-fall/qualtrics/Canbyork-Fall2020-Part+#1-FINAL_March+29,+2021_12.19.csv', stringsAsFactors = F)
  #find which of our dataoutput (pp with data) have qualtrics data as well
  ppqualt <- qualt$id[-c(1:2)]
  pp_no_qualt <- dataoutput[which(dataoutput %in% ppqualt == FALSE)]
  
  
  return(pp_no_qualt)
  #function returns nothing if all data we have also have corresponding Qualtrics data
  #removed pp_no_qualt from dataset: 216856, fall2020_367a19
}

getSUWithoutQualtrics <- function(){
  
  #participant list from behavioral data
  
  datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
  
  
  
  dataoutput<- c() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- handleOneFile(filename = datafilename)
    ppname <- unique(adat$participant)
    
    dataoutput <- c(dataoutput, ppname)
  }
  
  #read in Qualtrics sheet
  qualt <- read.csv('data/mReversalNewAlpha3-master/qualtrics/Canbyork-SU_September+14%2C+2020_13.14.csv', stringsAsFactors = F)
  #find which of our dataoutput (pp with data) have qualtrics data as well
  ppqualt <- qualt$Q1[-c(1)] #earlier version of qualtrics has URPP numbers in Q1 (more reliable than id column)
  pp_no_qualt <- dataoutput[which(dataoutput %in% ppqualt == FALSE)]
  
  
  return(pp_no_qualt)
  #function returns nothing if all data we have also have corresponding Qualtrics data
  
}

getPart2WithoutQualtrics <- function(){
  
  #participant list from behavioral data
  
  datafilenames <- list.files('data/mirrorgeneralization-master/data', pattern = '*.csv')
  
  
  
  dataoutput<- c() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    
    datafilename <- sprintf('data/mirrorgeneralization-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- handleOneFile(filename = datafilename)
    ppname <- unique(adat$participant)
    
    dataoutput <- c(dataoutput, ppname)
  }
  
  #read in Qualtrics sheet
  qualt <- read.csv('data/mirrorgeneralization-master/qualtrics/Canbyork-Fall2020-Part+#2-FINAL_March+29,+2021_12.20.csv', stringsAsFactors = F)
  #find which of our dataoutput (pp with data) have qualtrics data as well
  ppqualt <- qualt$id[-c(1:2)]
  pp_no_qualt <- dataoutput[which(dataoutput %in% ppqualt == FALSE)]
  
  
  return(pp_no_qualt)
  #function returns nothing if all data we have also have corresponding Qualtrics data
}

#Function below will generate a new csv file of Qualtrics data that contains only participants that also have experimental data
getQualtricsData <- function(set){
  if(set=='su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    
    
    
    dataoutput<- c() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      adat <- handleOneFile(filename = datafilename)
      ppname <- unique(adat$participant)
      
      dataoutput <- c(dataoutput, ppname)
    }
    
    qualt <- read.csv('data/mReversalNewAlpha3-master/qualtrics/Canbyork-SU_September+14%2C+2020_13.14.csv', stringsAsFactors = F)
    
    ndataoutput <- data.frame()
    for (pp in dataoutput){
      if(pp %in% qualt$Q1){
        ndat <- qualt[which(qualt$Q1 == pp),]
      }
      
      if (prod(dim(ndataoutput)) == 0){
        ndataoutput <- ndat
      } else {
        ndataoutput <- rbind(ndataoutput, ndat)
      }
    }
    
    row1qualt <- qualt[1,]
    alldat <- rbind(row1qualt, ndataoutput)
    write.csv(alldat, file='data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', row.names = F)
    
  } else if (set=='fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
    
    
    
    dataoutput<- c() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      adat <- handleOneFile(filename = datafilename)
      ppname <- unique(adat$participant)
      
      dataoutput <- c(dataoutput, ppname)
    }
    
    qualt <- read.csv('data/mirrorreversal-fall/qualtrics/Canbyork-Fall2020-Part+#1-FINAL_March+29,+2021_12.19.csv', stringsAsFactors = F)
    
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
    write.csv(alldat, file='data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', row.names = F)
    
  } else if (set=='part2'){
    datafilenames <- list.files('data/mirrorgeneralization-master/data', pattern = '*.csv')
    
    
    
    dataoutput<- c() #create place holder
    for(datafilenum in c(1:length(datafilenames))){
      
      datafilename <- sprintf('data/mirrorgeneralization-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      
      cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      adat <- handleOneFile(filename = datafilename)
      ppname <- unique(adat$participant)
      
      dataoutput <- c(dataoutput, ppname)
    }
    
    qualt <- read.csv('data/mirrorgeneralization-master/qualtrics/Canbyork-Fall2020-Part+#2-FINAL_March+29,+2021_12.20.csv', stringsAsFactors = F)
    
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
    write.csv(alldat, file='data/mirrorgeneralization-master/qualtrics/Part2_Qualtrics_ParticipantList.csv', row.names = F)
    
  }
}


# Mouse VS Trackpad: Learning ----
#device is either 'Mouse' or 'Trackpad'
getDeviceLC <- function(group, set, device){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularLC.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularLC.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
  }
  #dat <- getGroupCircularLC(group = group, set = set) #replace with saved dat 
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getDeviceLCConfInt <- function(groups = c('30','60'), set, device){
  for(group in groups){
    data <- getDeviceLC(group=group, set=set, device=device)
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
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DeviceLC_CI.csv', group, device), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DeviceLC_CI.csv', group, device), row.names = F) 
      }
    }
  }
}

getDeviceAligned <- function(group, set, device){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularAligned.csv', group), check.names = FALSE)
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularAligned.csv', group), check.names = FALSE)
  }
  #dat <- removeOutlierAlignedReaches(group = group, set = set)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getDeviceAlignedConfInt <- function(groups = c('30','60'), set, device){
  for(group in groups){
    #data <- getGroupCircularAligned(group=group, set=set)
    # use cleaned aligned trials (i.e. only trials with reaches in correct quadrant)
    data <- getDeviceAligned(group=group, set=set, device=device)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    # if(group == '30' & set == 'su2020'){
    #   n <- trialno[seq(1,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # } else if (group == '60' & set == 'su2020'){
    #   n <- trialno[seq(2,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # }
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
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DeviceAligned_CI.csv', group, device), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DeviceAligned_CI.csv', group, device), row.names = F) 
      }
    }
  }
}

getDeviceRAE <- function(group, set, device){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularRAE.csv', group), check.names = FALSE)
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularRAE.csv', group), check.names = FALSE)
  }
  #dat <- getGroupCircularRAE(group = group, set = set)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getDeviceRAEConfInt <- function(groups = c('30','60'), set, device){
  for(group in groups){
    data <- getDeviceRAE(group=group, set=set, device=device)
    
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
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DeviceRAE_CI.csv', group, device), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DeviceRAE_CI.csv', group, device), row.names = F) 
      }
    }
  }
}

plotDeviceAllTasks <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig12_%s_DeviceAllTasks.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidenceAligned <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DeviceAligned_CI.csv', group, device))
      groupconfidenceLC <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DeviceLC_CI.csv', group, device))
      groupconfidenceRAE <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DeviceRAE_CI.csv', group, device))
      
      
      
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

plotDeviceAllTasksSU <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), target='inline', set = 'su2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mReversalNewAlpha3-master/doc/fig/Fig12_%s_DeviceAllTasks.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidenceAligned <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DeviceAligned_CI.csv', group, device))
      groupconfidenceLC <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DeviceLC_CI.csv', group, device))
      groupconfidenceRAE <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DeviceRAE_CI.csv', group, device))
      
      
      
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
      if (group == '30'){
        x <- seq(1,nrow(groupconfidenceAligned),2)
      } else if (group == '60'){
        x <- seq(2,nrow(groupconfidenceAligned),2)
      }
      
      
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[device]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(21,(21 + nrow(groupconfidenceLC)) - 1,2)
      } else if (group == '60'){
        x <- seq(22,(22 + nrow(groupconfidenceLC)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[device]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(111,(111 + nrow(groupconfidenceRAE)) - 1,2)
      } else if (group == '60'){
        x <- seq(112,(112 + nrow(groupconfidenceRAE)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[device]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
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

# Mouse VS Trackpad: Movement Time----
getDeviceMT <- function(group, set, device){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_step2_MovementTime.csv',group), check.names = FALSE) #check.names allows us to keep pp id as headers
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_step2_MovementTime.csv',group), check.names = FALSE)
  }
  #dat <- getGroupAllTasksMT(group = group, set = set, step = 2)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getDeviceMTConfInt <- function(groups = c('30','60'), type = 't', set, device){
  for(group in groups){
    data <- getDeviceMT(group = group, set = set, device = device)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    # if(group == '30' & set == 'su2020'){
    #   n <- trialno[seq(1,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # } else if (group == '60' & set == 'su2020'){
    #   n <- trialno[seq(2,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # }
    
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
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DeviceMT_CI.csv', group, device), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DeviceMT_CI.csv', group, device), row.names = F) 
      }
    }
  }
}

plotDeviceMT <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig13_%s_DeviceMT.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DeviceMT_CI.csv', group, device))
      
      
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

plotDeviceMTSU <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), target='inline', set = 'su2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mReversalNewAlpha3-master/doc/fig/Fig13_%s_DeviceMT.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DeviceMT_CI.csv', group, device))
      
      
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
      if (group == '30'){
        x <- seq(1,nrow(groupconfidenceAligned),2)
      } else if (group == '60'){
        x <- seq(2,nrow(groupconfidenceAligned),2)
      }
      
      
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[device]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(21,(21 + nrow(groupconfidenceLC)) - 1,2)
      } else if (group == '60'){
        x <- seq(22,(22 + nrow(groupconfidenceLC)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[device]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(111,(111 + nrow(groupconfidenceRAE)) - 1,2)
      } else if (group == '60'){
        x <- seq(112,(112 + nrow(groupconfidenceRAE)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[device]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
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

# Mouse VS Trackpad: Path Length----
getDevicePL <- function(group, set, device){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_step2_PathLength.csv',group), check.names = FALSE) #check.names allows us to keep pp id as headers
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q15 == device),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_step2_PathLength.csv',group), check.names = FALSE)
  }
  #dat <- getGroupAllTasksPathLength(group = group, set = set, step = 2)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getDevicePLConfInt <- function(groups = c('30','60'), type = 't', set, device){
  for(group in groups){
    data <- getDevicePL(group = group, set = set, device = device)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    # if(group == '30' & set == 'su2020'){
    #   n <- trialno[seq(1,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # } else if (group == '60' & set == 'su2020'){
    #   n <- trialno[seq(2,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # }
    
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
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DevicePL_CI.csv', group, device), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DevicePL_CI.csv', group, device), row.names = F) 
      }
    }
  }
}

plotDevicePL <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig14_%s_DevicePL.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_DevicePL_CI.csv', group, device))
      
      
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

plotDevicePLSU <- function(groups = c('30', '60'), devices = c('Mouse','Trackpad'), target='inline', set = 'su2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mReversalNewAlpha3-master/doc/fig/Fig14_%s_DevicePL.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_DevicePL_CI.csv', group, device))
      
      
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
      if (group == '30'){
        x <- seq(1,nrow(groupconfidenceAligned),2)
      } else if (group == '60'){
        x <- seq(2,nrow(groupconfidenceAligned),2)
      }
      
      
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[device]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(21,(21 + nrow(groupconfidenceLC)) - 1,2)
      } else if (group == '60'){
        x <- seq(22,(22 + nrow(groupconfidenceLC)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[device]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[device]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(111,(111 + nrow(groupconfidenceRAE)) - 1,2)
      } else if (group == '60'){
        x <- seq(112,(112 + nrow(groupconfidenceRAE)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[device]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
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

# Biological Sex: Learning----
#sex is 'Male' or 'Female'
getSexLC <- function(group, set, sex){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q6 == sex),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularLC.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q5 == sex),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularLC.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
  }
  #dat <- getGroupCircularLC(group = group, set = set)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getSexLCConfInt <- function(groups = c('30','60'), set, sex){
  for(group in groups){
    data <- getSexLC(group=group, set=set, sex=sex)
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
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexLC_CI.csv', group, sex), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexLC_CI.csv', group, sex), row.names = F) 
      }
    }
  }
}

getSexAligned <- function(group, set, sex){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q6 == sex),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularAligned.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q5 == sex),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularAligned.csv', group), check.names = FALSE)
  }
  #dat <- removeOutlierAlignedReaches(group = group, set = set)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getSexAlignedConfInt <- function(groups = c('30','60'), set, sex){
  for(group in groups){
    #data <- getGroupCircularAligned(group=group, set=set)
    # use cleaned aligned trials (i.e. only trials with reaches in correct quadrant)
    data <- getSexAligned(group=group, set=set, sex=sex)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    # if(group == '30' & set == 'su2020'){
    #   n <- trialno[seq(1,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # } else if (group == '60' & set == 'su2020'){
    #   n <- trialno[seq(2,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # }
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
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexAligned_CI.csv', group, sex), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexAligned_CI.csv', group, sex), row.names = F) 
      }
    }
  }
}

getSexRAE <- function(group, set, sex){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q6 == sex),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularRAE.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q5 == sex),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularRAE.csv', group), check.names = FALSE)
  }
  #dat <- getGroupCircularRAE(group = group, set = set)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getSexRAEConfInt <- function(groups = c('30','60'), set, sex){
  for(group in groups){
    data <- getSexRAE(group=group, set=set, sex=sex)
    
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
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexRAE_CI.csv', group, sex), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexRAE_CI.csv', group, sex), row.names = F) 
      }
    }
  }
}

plotSexAllTasks <- function(groups = c('30', '60'), sexes = c('Male','Female'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig15_%s_SexAllTasks.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidenceAligned <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexAligned_CI.csv', group, sex))
      groupconfidenceLC <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexLC_CI.csv', group, sex))
      groupconfidenceRAE <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexRAE_CI.csv', group, sex))
      
      
      
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

plotSexAllTasksSU <- function(groups = c('30', '60'), sexes = c('Male','Female'), target='inline', set = 'su2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mReversalNewAlpha3-master/doc/fig/Fig15_%s_SexAllTasks.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidenceAligned <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexAligned_CI.csv', group, sex))
      groupconfidenceLC <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexLC_CI.csv', group, sex))
      groupconfidenceRAE <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexRAE_CI.csv', group, sex))
      
      
      
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
      if (group == '30'){
        x <- seq(1,nrow(groupconfidenceAligned),2)
      } else if (group == '60'){
        x <- seq(2,nrow(groupconfidenceAligned),2)
      }
      
      
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[sex]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(21,(21 + nrow(groupconfidenceLC)) - 1,2)
      } else if (group == '60'){
        x <- seq(22,(22 + nrow(groupconfidenceLC)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[sex]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(111,(111 + nrow(groupconfidenceRAE)) - 1,2)
      } else if (group == '60'){
        x <- seq(112,(112 + nrow(groupconfidenceRAE)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[sex]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
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

# Biological Sex: Movement Time----
getSexMT <- function(group, set, sex){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q6 == sex),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_step2_MovementTime.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q5 == sex),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_step2_MovementTime.csv', group), check.names = FALSE)
  }
  #dat <- getGroupAllTasksMT(group = group, set = set, step = 2)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getSexMTConfInt <- function(groups = c('30','60'), type = 't', set, sex){
  for(group in groups){
    data <- getSexMT(group = group, set = set, sex = sex)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    # if(group == '30' & set == 'su2020'){
    #   n <- trialno[seq(1,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # } else if (group == '60' & set == 'su2020'){
    #   n <- trialno[seq(2,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # }
    
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
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexMT_CI.csv', group, sex), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexMT_CI.csv', group, sex), row.names = F) 
      }
    }
  }
}

plotSexMT <- function(groups = c('30', '60'), sexes = c('Male','Female'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig16_%s_SexMT.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexMT_CI.csv', group, sex))
      
      
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

plotSexMTSU <- function(groups = c('30', '60'), sexes = c('Male','Female'), target='inline', set = 'su2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mReversalNewAlpha3-master/doc/fig/Fig16_%s_SexMT.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexMT_CI.csv', group, sex))
      
      
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
      if (group == '30'){
        x <- seq(1,nrow(groupconfidenceAligned),2)
      } else if (group == '60'){
        x <- seq(2,nrow(groupconfidenceAligned),2)
      }
      
      
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[sex]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(21,(21 + nrow(groupconfidenceLC)) - 1,2)
      } else if (group == '60'){
        x <- seq(22,(22 + nrow(groupconfidenceLC)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[sex]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(111,(111 + nrow(groupconfidenceRAE)) - 1,2)
      } else if (group == '60'){
        x <- seq(112,(112 + nrow(groupconfidenceRAE)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[sex]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
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

# Biological Sex: Path Length----
getSexPL <- function(group, set, sex){
  
  if(set == 'su2020'){
    qualtdat <- read.csv('data/mReversalNewAlpha3-master/qualtrics/SU_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q6 == sex),]
    ppqualt <- devqualt$Q1
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_step2_PathLength.csv', group), check.names = FALSE) #check.names allows us to keep pp id as headers
  } else if (set == 'fa2020'){
    qualtdat <- read.csv('data/mirrorreversal-fall/qualtrics/FA_Qualtrics_ParticipantList.csv', stringsAsFactors = F)
    #then get pplist according to device
    devqualt <- qualtdat[which(qualtdat$Q5 == sex),]
    ppqualt <- devqualt$id
    dat <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_step2_PathLength.csv', group), check.names = FALSE)
  }
  #dat <- getGroupAllTasksPathLength(group = group, set = set, step = 2)
  
  #keep only data of pp from this list
  trial <- dat$trial
  ndat <- dat[,which(colnames(dat) %in% ppqualt)]
  dat <- cbind(trial, ndat)
  
  return(dat)
}

getSexPLConfInt <- function(groups = c('30','60'), type = 't', set, sex){
  for(group in groups){
    data <- getSexPL(group = group, set = set, sex = sex)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    # if(group == '30' & set == 'su2020'){
    #   n <- trialno[seq(1,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # } else if (group == '60' & set == 'su2020'){
    #   n <- trialno[seq(2,length(trialno),2)]
    #   data <- data[n,]
    #   trialno <- c(1:nrow(data))
    # }
    
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
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexPL_CI.csv', group, sex), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexPL_CI.csv', group, sex), row.names = F) 
      }
    }
  }
}

plotSexPL <- function(groups = c('30', '60'), sexes = c('Male','Female'), target='inline', set = 'fa2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig17_%s_SexPL.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_SexPL_CI.csv', group, sex))
      
      
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

plotSexPLSU <- function(groups = c('30', '60'), sexes = c('Male','Female'), target='inline', set = 'su2020') {
  for (group in groups){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file=sprintf('data/mReversalNewAlpha3-master/doc/fig/Fig17_%s_SexPL.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_SexPL_CI.csv', group, sex))
      
      
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
      if (group == '30'){
        x <- seq(1,nrow(groupconfidenceAligned),2)
      } else if (group == '60'){
        x <- seq(2,nrow(groupconfidenceAligned),2)
      }
      
      
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[sex]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(21,(21 + nrow(groupconfidenceLC)) - 1,2)
      } else if (group == '60'){
        x <- seq(22,(22 + nrow(groupconfidenceLC)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[sex]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[sex]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      if (group == '30'){
        x <- seq(111,(111 + nrow(groupconfidenceRAE)) - 1,2)
      } else if (group == '60'){
        x <- seq(112,(112 + nrow(groupconfidenceRAE)) - 2,2)
      }
      polygon(x = c(x, rev(x)), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
      
      col <- colourscheme[[sex]][['S']]
      lines(x = x, y = na.omit(mid),col=col,lty=1)
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

# Movement Time and Path Length: Sensorimotor Battery ----
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



















