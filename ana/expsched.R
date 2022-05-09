source('ana/shared.R')

getTrialListSession1 <- function(){
  
  
  #Aligned Session
  #Create data frame containing values
  trial <- c(1:177)
  
  #1 means aligned cursor
  #2 means random
  #3 means perturb
  #4 means washout
  
  align <- matrix(rep(1,45), nrow = 45, ncol = 1)
  align_switch <- matrix(rep(2,21), nrow = 21, ncol = 1)
  perturb <- matrix(rep(3,90), nrow = 90, ncol = 1)
  washout <- matrix(rep(4,21), nrow = 21, ncol = 1)
  
  
  task <- rbind(align, align_switch, perturb, washout)
  sched_df<- data.frame(trial, task)
  return(sched_df)
  
}

plotExpSchedSession1 <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/controlmironline-master/doc/fig/Fig0_expsched.svg', width=8.5, height=4, pointsize=14, system_fonts=list(sans="Arial"))#width is 8.5, height is 4, pointsize is 14
  }
  
  #might need to specify different X and Y for each trial
  sched_df <- getTrialListSession1()
  
  X1 <- c(1,45)
  X2 <- c(46,66)
  X3 <- c(67,156)
  X4 <- c(157,177)

  
  Y <- c(0, 1)
  Y1<- c(0, .5)
  Y2<- c(.5, 1)
  
  plot(c(1:length(sched_df$trial)), seq (0,30, length.out = length(sched_df$trial)), type = 'n', axes = FALSE,
       xlab = 'Trial', ylab = '',
       xlim = c(0,177), ylim = c(-0.2,2.5))#, cex.main=.65, cex.lab=.65)
  
  #set variables for colours
  alignedcol <- '#b4b4b4'
  alignedswitchcol    <- '#dadada'
  mircol     <- '#005de4ff'
  
  #specify rects
  rect(X1[1], Y[1], X1[2], Y[2], border = alignedcol, col = alignedcol)
  
  rect(X2[1], Y[1], X2[2], Y[2], border = alignedswitchcol, col = alignedswitchcol)
  rect(X3[1], Y[1], X3[2], Y[2], border = mircol, col = mircol)
  rect(X4[1], Y[1], X4[2], Y[2], border = alignedcol, col = alignedcol)
  
  
  axis(side=1, at=c(1,46,67,157,177))#, cex.axis=.65) ## add axes back
  # axis(side=2, at=c(0.1,0.9), labels=c('0','30'))
  #Ncols <- ceiling(6/4) #6 labels, 4 rows
  
  # legend(90,3,legend=c('Aligned cursor/ Washout', 'Aligned cursor (non-dominant hand)', 'Mirror reversal'),
  #        col=c(alignedcol,alignedswitchcol,mircol),
  #        #text.col=c("#000000","#76BA1B","#4C9A2A","#A4DE02",drkbl,lgtbl),
  #        lty=1,bty='n',lwd=5, cex=.8)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}