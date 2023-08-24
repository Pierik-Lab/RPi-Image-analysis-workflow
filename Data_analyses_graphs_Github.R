

library(openxlsx)

# 1. set up sample/experiment information and read in data
#    a. read in data from an experimental info table
#    b. convert pixels to mm
#    c. quick & dirty plot to check quality
# 2. add time to data and synchronize runs
#    a. define functions
#    b. identify shift to night to add accurate time
#    c. synchronize to correct differences in start time
# 3. organize data by trait
# 4. apply calculations, by plant
#       normalize to t0
#       create time segments with a sliding window
#       calculate slopes over time segments
# 5. calculate statistics, by time point
#       load in functions form source file
# 6. write out data 
# 7. plot with short format
#    a. individual plants as quality control
#    b. means, sem and stats in single figure
# 8. Correlation ploys
#


#
setwd("//YOUR directory")


# -------------------------------------------------------------- #
# 1. set up sample/experiment information and read in data

# 1.a read in data from an experimental info table
# All backslashes should be forward slashes in the txt file
Expinf     <- read.delim("Experiment_infoExample_Github.txt")
tempname   <- gsub("\\/", "_", Expinf$Pathway)
tnc        <- strsplit(tempname, "_")
nameMat    <- matrix(unlist(tnc), ncol=length(tnc[[1]]), byrow=T)
nameMat    <- nameMat[,ncol(nameMat):1 ] # reverse the namematrix so it doesnt matter how many folders deep you are
Expinf$FileName <-  apply(nameMat[,c(2,3,1)], 1, function(X) paste0(paste(X, collapse="_"), "_output.xlsx"))

#to remove specific plants or have specific treatments/genotypes/experiments only
#Unwanted <- c("21-10-27_03_right_output.xlsx", "filename2", "etcetera")
#Expinf <- Expinf[!(Expinf$FileName %in% Unwanted),]
#Expinf <- ExpInf[Expinf$Experiment != 4, ] # e.g. exclude experiment 4 


# Make a list where every entry is a plant, and read in the data
RunL <- vector(mode= 'list', length=nrow(Expinf))
names(RunL) <- Expinf$FileName
for(i in Expinf$FileName) { RunL[[i]] <- read.xlsx(i) }
lapply(RunL, head, n=3)

# 1.b convert pixels to mm for every plant in the previously made list
for(plant in names(RunL)){
  trun   <-  RunL[[plant]]
  pixlen <-  as.numeric(trun[1,'pixlen']) / 10
  xmer   <-  trun[,'xmer'];  ymer  <-  trun[,'ymer']
  xjun   <-  trun[,'xpet'];  yjun  <-  trun[,'ypet']
  xtip   <-  trun[,'xleaf']; ytip  <-  trun[,'yleaf']
  
  pet.len      <-  sqrt( (xjun - xmer)^2 + (yjun - ymer)^2 ) / pixlen
  lam.len      <-  sqrt( (xtip - xjun)^2 + (ytip - yjun)^2 ) / pixlen
  leaf.len     <-  sqrt( (xtip - xmer)^2 + (ytip - ymer)^2 ) / pixlen
  pet.ang      <-  atan( -(yjun - ymer) / (xjun - xmer) ) / pi * 180
  lam.ang      <-  atan( -(ytip - yjun) / (xtip - xjun) ) / pi * 180
  leaf.ang     <-  atan( -(ytip - ymer) / (xtip - xmer) ) / pi * 180
  lam.pet.ang  <-  (90 - pet.ang) + 90 + lam.ang
  lam.proj.len <-  (xtip - xjun) / pixlen
  pet.proj.len <-  (xjun - xmer) / pixlen
  jun.hei      <-  -(yjun - ymer) / pixlen
  tip.hei      <-  -(ytip - ymer) / pixlen
  max.hei      <-  apply(cbind(jun.hei, tip.hei), 1, max) / pixlen
  newDF <- data.frame(image = trun$image,
                      pet.len = pet.len, lam.len = lam.len, leaf.len = leaf.len,
                      pet.ang = pet.ang, lam.ang = lam.ang, leaf.ang = leaf.ang,
                      lam.pet.ang = lam.pet.ang, 
                      lam.proj.len = lam.proj.len, pet.proj.len = pet.proj.len,
                      jun.hei = jun.hei, tip.hei = tip.hei, max.hei = max.hei,
                      m.int = trun$m.int, ld.thr = trun$ld.thr)
  RunL[[plant]] <- newDF
}
lapply(RunL, head, n=3)


# 1.c quick & dirty plot to check quality
# This will stitch all plant together in one png, be aware of size and runtime if you run a lot of plants at once. 

# Define function
quickplot_one <- function(run, trait, ntp) {
  plot(1:nrow(run), run[, trait], type='l', ann=F)
  abline(v=ntp); abline(v=ntp+15*60)}
quickplot_all <- function(picname, RunList, trait, ntp){
  no.run <- length(RunList); no.vert <- ceiling(no.run/2)
  pdf(picname , width=7, height=no.vert+1)
    par(mfrow=c(no.vert,2), mar=c(2,3,1,.1),oma=c(0,0,2,0))
    for(i in 1:no.run){ quickplot_one(RunList[[i]], trait, ntp)
                        mtext(names(RunList)[i], side=3, line=-1, col='red') }
    mtext(trait, side=3, outer=T); dev.off()
}

# Apply function to all different read-outs and write immediately to file
# ntp is Night Time Point, this is set at 7 hours (7*60 min) after the start of the experiment. Adjust if necessary. 
# Easy way to quickly check the quality of the tracking per plant
quickplot_all("pet.len_Example.pdf",  RunList=RunL, trait="pet.len",    ntp=7*60)
quickplot_all("lam.len_Example.pdf",  RunList=RunL, trait="lam.len", ntp=7*60)
quickplot_all("leaf.len_Example.pdf", RunList=RunL, trait="leaf.len",   ntp=7*60)
quickplot_all("pet.angle_Example.pdf",  RunList=RunL, trait="pet.ang",    ntp=7*60)
quickplot_all("lam.angle_Example.pdf",  RunList=RunL, trait="lam.ang", ntp=7*60)
quickplot_all("leaf.angle_Example.pdf", RunList=RunL, trait="leaf.ang",   ntp=7*60)



# -------------------------------------------------------------- #
# 2. add time to data and synchronize runs

# 2.a define functions
getnightpoint <- function(Run){
  NoNightStartFound <- TRUE
  islight   <- (Run$m.int - Run$ld.thr) > 0                  # To define night and day pictures, threshold is for legacy support of older versions
  if(sum(!islight) == 0) {
    return(NA) } else {
    while (NoNightStartFound) {                                # Continue to look for day -> night transition until found
      trans    <- min(which(!islight))                         # First night photo in the series
      check1   <- sum(islight[(trans-3):(trans-7)]) == 5       # make sure it is not an anomaly, by probing the immediately adjacent photos
      check2   <- sum(!islight[(trans+2):(trans+6)]) == 5
      if(check1 & check2) {                                    # if adjacency is good day -> night transition found
	  NoNightStartFound <- FALSE
	  } else {
	  islight[trans] <- TRUE }
    }
  }
  return(trans)                                              # return the the night time point (day -> night transition)
}

add.time.column <- function(Run, ntp){
  daymins    <-  17 * 60                                     # night starts at 17:00 hours
  cum.min    <-  seq(daymins - ntp, by=1, length=nrow(Run))  # the minute of the 24h day at which run starts eg. 9:55 -> 9*60+55 minutes
  hours      <-  floor(cum.min / 60)                         # the hour at which run started e.g. 09:55 -> 9
  cum.min    <-  cum.min %% 60                               # the remaining minutes in the hour 9:55 -> 55
  hour24     <-  hours %% 24                                 # the remaining hours of a day 9:55 -> 9, also 35:55 -> 9
  day        <-  floor(hours / 24)                           # starting day of the experiment, typically 0
  mcol       <-  paste(day, hour24, cum.min,sep=":")         # composite time value
  outp       <-  data.frame(day, hour24, cum.min, time=mcol)
  return(cbind(outp, Run))
  }

# 2.b identify shift to night to add accurate time
nightP <- sapply(RunL, getnightpoint)                        # apply the nighttimepoint seek function to all plants
sum(is.na(nightP))                                           # the number of plants where no nightshift was detected
nightP[is.na(nightP)] <- median(nightP, na.rm = T)           # insert median start of the night for those with missing night time point

pdf("NightPoints_Example.pdf", width=7, height=8, pointsize=8)
hist(nightP)                                                 # plot variance in starting time of the experiment
dev.off()

runwithT        <- vector(mode= 'list', length=nrow(Expinf)) # empty list to place all individual plant data with a time collumn
names(runwithT) <- Expinf$FileName
for(i in Expinf$FileName){ 
  runwithT[[i]] <- add.time.column( RunL[[i]], nightP[i]) }  # add time column in a loop
lapply(runwithT, head, n=3)


# 2.c synchronize to correct differences in start time

# create a data frame to attach all individual plants to.
# must encompass all time points of all experiments
#   therefore depends in part on start time and duration of experiment
starthour     <- 9                                               # with safety margin of 1 hour (experiment starts around 10:00)
timelightson  <- 8                                               # needed to set up ZeitGeber here lights go on at 8:00, 
  endhour   <- starthour + ceiling(nrow(RunL[[1]])/60)+1         # over estimate endpoint based on longest runs of all plants
  tothours  <- endhour - starthour
  mins      <- rep(0:59, times=tothours+1)
  hours.cum <- rep(starthour:endhour, each=60)
  hours.24  <- hours.cum %% 24
  day       <- floor(hours.cum/24)
  ZT        <- ((starthour  - timelightson)*60):((endhour+1-timelightson)*60-1) / 60   # Set up a ZeitGeber Time
  
  timeDF    <- data.frame(time=paste(day, hours.24, mins,sep=":"), 
                          dayF=day, hoursF=hours.24, minutesF=mins, ZT=ZT)
head(timeDF); tail(timeDF)

sync.time <- function(X, timeDF){
  tdf <- merge(timeDF, X, by.x='time', by.y='time', all.x=T)    # Merge ind. plantdata on to the fixed time data
  tdf2 <- tdf[order(tdf$minutesF), ]                            # sequentially order on time
  tdf3 <- tdf2[order(tdf2$hoursF), ]
  tdf4 <- tdf3[order(tdf3$dayF), ]
  return(tdf4)
  }
runL_ts <- lapply(runwithT, sync.time, timeDF=timeDF)           # run time syncing function on each plant
runL_ts[[1]][1461:1480,]
lapply(runL_ts, function(X) X[c(1:4, 501:504), 1:8])            # print 1st few and middle lines and some collums to check
sapply(runL_ts, dim)                                            # all dimensions should be identical


# -------------------------------------------------------------- #
# 3. organize data by trait

TraitL <- list(                                                      # organize by trait rather than plant, by sub-setting the trait from each plant
  pet.len      = sapply(runL_ts, function(X) X[, 'pet.len']),
  pet.ang      = sapply(runL_ts, function(X) X[, 'pet.ang']),
  leaf.len     = sapply(runL_ts, function(X) X[, 'leaf.len']),
  leaf.ang     = sapply(runL_ts, function(X) X[, 'leaf.ang']),
  lam.len      = sapply(runL_ts, function(X) X[, 'lam.len']),
  lam.ang      = sapply(runL_ts, function(X) X[, 'lam.ang']), 
  lam.pet.ang  = sapply(runL_ts, function(X) X[, 'lam.pet.ang']), 
  lam.proj.len = sapply(runL_ts, function(X) X[, 'lam.proj.len']), 
  pet.proj.len = sapply(runL_ts, function(X) X[, 'pet.proj.len']), 
  tip.hei      = sapply(runL_ts, function(X) X[, 'tip.hei'])
)

# diagnostic plot, proportion NAs
proportionNAs <- sapply(TraitL, function(x) apply(x, 1, function(row) sum(is.na(row)) / length(row)) )

pdf("PropNA_Example.pdf", width=7, height=8, pointsize=8)
plot(0,0, xlim=c(0, max(timeDF$ZT)), ylim=c(0,1), type='n', xlab="ZT (hours)", ylab='prop. NA')
for(i in 1:ncol(proportionNAs)){ lines(timeDF$ZT, proportionNAs[,i], lwd=2) }
dev.off()

Names         <- paste(Expinf$Experiment, Expinf$Treatment, 
                       Expinf$Ratio, nameMat[,3], nameMat[,1],sep="_") 
TraitL <- lapply(TraitL, function(x) {colnames(x) <- Names; return(x)})  # Add names to columns (ind. plants)
lapply(TraitL, function(x) x[100:103,1:5])


# -------------------------------------------------------------- #
# 4. apply calculations, by plant
#    normalize to t0
#    create time segments with a sliding window
#    calculate slopes over time segments

# define functions:

# a function to set the values of the starting time point at 0
# requires the trait and starting time point
Force_T0_at0 <- function(mat, time.df, T0.ZT){
  beforeT0  <- time.df$ZT < T0.ZT
  newmat1   <- mat[!beforeT0,]
  T0value   <- apply(newmat1, 2, function(x) {nx <- x[!is.na(x)]; return(mean(nx[1:5])) } )
  newmat2   <- t(t(mat) - T0value)
  return(newmat2) }
# a function that just retreives the "raw" values at T0, to check experimental variation
GetValues_aT0 <- function(mat, time.df, T0.ZT){
  beforeT0  <- time.df$ZT < T0.ZT
  newmat1   <- mat[!beforeT0,]
  T0value   <- apply(newmat1, 2, function(x) {nx <- x[!is.na(x)]; return(mean(nx[1:5])) } )
  return(T0value) }


# generic sliding window function to obtain lumps of data or smooth the data
#   default is mean(): other options e.g. median()
#      function can also include the time component function(y, x)
#   window size (winzise) is in hours, i.e. the same unit as the ZTvec argument
#   step size   (stepsize) is in hours, as window size
#   sta        (first time point to consider)
slindow <- function(datamat, ZTvec, sta=0, winsize=2, stepsize=2,
                    func=function(y,x) mean(y, na.rm=T) ){
  stepmid <- seq(sta, max(ZTvec), by=stepsize)
  stepsta <- stepmid - .5*winsize 
  stepend <- stepsta + .5*winsize 
  outmat  <- matrix(data=NA, ncol=ncol(datamat), nrow=length(stepsta),
                    dimnames=list(stepmid, colnames(datamat)))
  for(i in 1:length(stepsta)){
    tryCatch({xsel  <- ZTvec >= stepsta[i] & ZTvec <= stepend[i]
              tmat  <- datamat[xsel,]
              outmat[i,] <- apply(tmat, 2, func, x=ZTvec[xsel])},
             error = function(e) {return(NA)})
  }
  return(outmat)
}

# calculate slope
#getslope_ori <- function(y, x){
    #tryCatch(
     # {x <- x[!is.na(y)]
    #   y <- y[!is.na(y)]
   #    zz <- lm(y ~ x)
  #     zz$coefficients[2]},
 #   error = function(e) {return(NA)})
#}


getslope <- function(y, x){
  tryCatch(
    {sel   <- !is.na(y) & !is.na(x)
    ty    <- y[sel]; tx <- x[sel]
    nn    <- sum(sel)
    sumx  <- sum(tx, na.rm=T); sumy  <- sum(ty, na.rm=T)
    sumxy <- sum(tx*ty, na.rm=T); sumxx <- sum(tx*tx, na.rm=T)
    sumx2 <- sum(tx, na.rm=T)^2
    return( (nn*sumxy - sumx*sumy) / (nn*sumxx-sumx2) )
    },
    error = function(e) {print('err'); return(NA)})
}
#getslope(y,x)


# calculate the slope over the slindow
sloper <- function(datamat, ZTvec, sta=0, winsize=2, stepsize=2,
                   func=function(y,x) getslope(y,x) ){
 slindow(datamat, ZTvec, sta=sta, winsize=winsize, stepsize=stepsize, func=func)
}



# run functions on each trait in list:
#   1. set t0 at 0,
#   2. retrieve values at t0, to check experimental quality
#   3. obtain large segments with slindow (to compute Tukey statistics)
#   4. calculate slopes over large segments
#   5. calculate slopes over small segments with small step size (High res)
#   6. obtain larege segments of the high res slopes (to compute Tukey statistics)

# 1)
TraitL_T00      <- lapply(TraitL, Force_T0_at0, time.df=timeDF, T0.ZT=2)
# 2)
TraitL_valueT0  <- lapply(TraitL, GetValues_aT0, time.df=timeDF, T0.ZT=2)
# 3)
TraitL_slindow  <- lapply(TraitL_T00, slindow, ZTvec=timeDF$ZT, sta=1, winsize=2, stepsize=2)
# 4) obsolete
#TraitL_slopes   <- lapply(TraitL_T00, sloper,  ZTvec=timeDF$ZT, sta=1, winsize=2, stepsize=2)

# 5)
# HR = high res, each minute (1/60) and calculate the slope over 20 min (20/60)
# long calculation, so only relevant traits
TraitL_activetraits  <- TraitL_T00[c('pet.len', 'pet.ang', 'leaf.len', 'leaf.ang', 'lam.len', 'lam.ang', 'lam.pet.ang')]
TraitL_slopesHR      <- lapply(TraitL_activetraits, sloper,  ZTvec=timeDF$ZT, sta=timeDF$ZT[1], winsize=20/60, stepsize=1/60)
# 6)
TraitL_slopesHRsegm  <- lapply(TraitL_slopesHR, slindow, ZTvec=timeDF$ZT, sta=1, winsize=2, stepsize=2)


# inspect data
lapply(TraitL_T00, function(x)x[200:205,1:5])
lapply(TraitL_slindow, function(x) x[,1:5])
#lapply(TraitL_slopes, function(x) x[,1:5]) # obsolete
lapply(TraitL_slopesHR, function(x) x[120:125,1:5])
lapply(TraitL_slopesHRsegm, function(x) x[,1:5])

# quick plot initial values to check consistency
pdf("starting_values_Example.pdf", width=7, height=8, pointsize=8)
par(mfrow=c(3,2), mar=c(3,4,2,1))
for(i in 1:length(TraitL_valueT0)){
  boxplot(TraitL_valueT0[[i]] ~ Expinf$Treatment)
  mtext(names(TraitL_valueT0)[i], side=3)
  }
dev.off()



# -------------------------------------------------------------- #
# 5. calculate statistics, by timpoint

# load in functions form source file to calculate statistics by time point
source("//Your directory/GetStats_functions_Example_Github.R")

# Functions present in this source file
#   "getstats_1way"
#   "getstats_PlanComp"
#   "getstats_1wayTukLets"
#   "getstats_2way"
#   "getstats_2wayTukLets"


# ------ one-factor or two factor ??? -------- #

# two way anova,
#   define the factors of the experiment (two way anova)
treat   <- as.factor(Expinf$Treatment)
genot   <- as.factor(Expinf$Ratio) # OR Genotype in your dataJA d

# one way anova, 
#   define the factors of the experiment 
groups  <- as.factor(Expinf$Treatment)               # e.g. Treatment column of the Expinf object
groups  <- as.factor(paste(treat, genot, sep="_"))  # here collapsing two factors into one, or specify factor of interest as.factor(Expinf$variable)
levels(groups)
refctrl <- "From groups"                                      # exact name of the reference level in case all compared to ref is done; check by levels(groups)


# ------              RAW data, normalized to t0                 ------ #
# ------ do the stats calculations, for either one or two factor ------ #
# raw data that is time synchronized and normalized to t0

# ONE-WAY
# A. no particular reference (all vs all, or only two levels)
# B. compared to reference control (all vs ctrl)

#   A. no particular reference
TraitL_stats1Way   <- lapply(TraitL_T00,                                  # oneway anova, is the same as a t-test when only two levels in the factor
                             function(mat) t(apply(mat,1, getstats_1way, fac=groups)) )
TraitL_statsLumpMC <- lapply(TraitL_slindow,                              # tukey letters for mult comparison, based on groups
                             function(mat) t(apply(mat,1, getstats_1wayTukLets, fac=groups)) )
lapply(TraitL_stats1Way, function(x) x[400:405, ])
lapply(TraitL_statsLumpMC, head)

#   B. compare all to a reference control (planned comparison based on package Emmeans), 
#   multiple comparison correction is build in (default "dunnettx", a close approximation to the Dunnett adjustment)
TraitL_statsToCtrl <- lapply(TraitL_T00,                                  # all compared to control group
                             function(mat) t(apply(mat,1, getstats_PlanComp, fac=groups, ref=refctrl)) )
lapply(TraitL_statsToCtrl, function(x) x[300:305, ])


# TWO-WAY
# in case of a two factor experiment (Two-Way)
TraitL_stats2Way   <- lapply(TraitL_T00,                                                                    # twoway anova
                             function(mat) t(apply(mat,1, getstats_2way, 
                               fac1=treat, fac2=genot, nam1="Treatment", nam2="Genotype")) )                # adjust names accordingly
# tukey mult comparison over larger segments (e.g. 2 hours), obtained by slindow line ~245
TraitL_statsLumpMC <- lapply(TraitL_slindow,                                                                
                             function(mat) t(apply(mat,1, getstats_2wayTukLets, fac1=treat, fac2=genot)) )
lapply(TraitL_stats2Way, function(x) x[200:205, ])
lapply(TraitL_statsLumpMC, head)


# ------                  SLOPE data                             ------ #
# ------ do the stats calculations, for either one or two factor ------ #
# raw data that is time synchronized and normalized to t0

# ONE-WAY
# A. no particular reference (all vs all, or only two levels)
# B. compared to reference control (all vs ctrl)

#   A. no particular reference
TraitL_statsSlope1Way   <- lapply(TraitL_slopesHR,                                  # oneway anova, is the same as a t-test when only two levels in the factor
                             function(mat) t(apply(mat,1, getstats_1way, fac=groups)) )
TraitL_statsSlopeLumpMC <- lapply(TraitL_slopesHRsegm,                              # tukey letters for mult comparison, based on groups
                             function(mat) t(apply(mat,1, getstats_1wayTukLets, fac=groups)) )
lapply(TraitL_statsSlope1Way, function(x) x[400:405, ])
lapply(TraitL_statsSlopeLumpMC, head)

#   B. compare all to a reference control (planned comparison based on package EMmeans), 
#   multiple comparison correction is build in (default "dunnettx", a close approximation to the Dunnett adjustment)
TraitL_statsSlopeToCtrl <- lapply(TraitL_slopesHR,                                  # all compared to control group
                             function(mat) t(apply(mat,1, getstats_PlanComp, fac=groups, ref=refctrl)) )
lapply(TraitL_statsSlopeToCtrl, function(x) x[200:205, ])


# TWO-WAY
# in case of a two factor experiment (Two-Way)
TraitL_statsSlope2Way   <- lapply(TraitL_slopesHR,                                                                    # twoway anova
                             function(mat) t(apply(mat,1, getstats_2way, 
                               fac1=treat, fac2=genot, nam1="Treatment", nam2="Genotype")) )                # adjust names accordingly
# tukey mult comparison over larger segments (e.g. 2 hours), obtained by slindow line ~245
TraitL_statsSlopeLumpMC <- lapply(TraitL_slopesHRsegm,                                                                
                             function(mat) t(apply(mat,1, getstats_2wayTukLets, fac1=treat, fac2=genot)) )
lapply(TraitL_statsSlope2Way, function(x) x[200:205, ])
lapply(TraitL_statsSlopeLumpMC, head)




# -------------------------------------------------------------- #
# 6 write out data (possibly after plantwise calculations)

####
# all individual plant data

OutputTraitL_raw <- lapply(TraitL, function(x) cbind(timeDF, x[, order(colnames(x))]) )
lapply(OutputTraitL_raw, function(x) x[100:103,])

OutputTraitL_T00 <- lapply(TraitL_T00, function(x) cbind(timeDF, x[, order(colnames(x))]) )
lapply(OutputTraitL_T00, function(x) x[100:103,])

OutputTraitL_SlopesHR <- lapply(TraitL_slopesHR, function(x) cbind(timeDF, x[, order(colnames(x))]) )
lapply(OutputTraitL_SlopesHR, function(x) x[100:103,])

write.xlsx(OutputTraitL_raw,   file= "raw_Example.xlsx") 
write.xlsx(OutputTraitL_T00,   file= "t0to0_Example.xlsx")
write.xlsx(OutputTraitL_SlopesHR, file= "slopesHR_Example.xlsx")


####
# summary statitistics and comparions

# ------ ONE- WAY ----- #

# one way, all vs all (no reference)
OutputTraitL_stats <- lapply(TraitL_stats1Way, function(x) cbind(timeDF, x) )
lapply(OutputTraitL_stats, function(x) x[100:103,])

timeDF_slindow <- merge(timeDF, data.frame(ZT=rownames(TraitL_statsLumpMC[[1]])), by.x='ZT', by.y='ZT') 
OutputTraitL_statsLumpMC <- lapply(TraitL_statsLumpMC, 
  function(x) cbind(timeDF_slindow, x[rownames(x) %in% timeDF_slindow$ZT, order(colnames(x))]))
lapply(OutputTraitL_statsLumpMC, head)


OutputTraitL_statsSlopes <- lapply(TraitL_statsSlope1Way, function(x) cbind(timeDF, x) )
lapply(OutputTraitL_statsSlopes, function(x) x[100:103,])

timeDF_slindow <- merge(timeDF, data.frame(ZT=rownames(TraitL_statsSlopeLumpMC[[1]])), by.x='ZT', by.y='ZT') 
OutputTraitL_statsSlopeLumpMC <- lapply(TraitL_statsSlopeLumpMC, 
  function(x) cbind(timeDF_slindow, x[rownames(x) %in% timeDF_slindow$ZT, order(colnames(x))]))
lapply(OutputTraitL_statsSlopeLumpMC, head)

write.xlsx(OutputTraitL_stats, file= "stats_Example.xlsx")
write.xlsx(OutputTraitL_statsLumpMC, file= "stats_TukLetters_Example.xlsx")
write.xlsx(OutputTraitL_statsSlopes, file= "statsSlope_Example.xlsx")
write.xlsx(OutputTraitL_statsSlopeLumpMC, file= "statsSlope_TukLetters_Example.xlsx")


# one way, to control (one as reference)

OutputTraitL_stats_toCtrl <- lapply(TraitL_statsToCtrl, function(x) cbind(timeDF, x) )
lapply(OutputTraitL_stats_toCtrl, function(x) x[100:103,])
OutputTraitL_statsSlope_toCtrl <- lapply(TraitL_statsSlopeToCtrl, function(x) cbind(timeDF, x) )
lapply(OutputTraitL_statsSlope_toCtrl, function(x) x[100:103,])

#write.xlsx(OutputTraitL_stats_toCtrl, file= "stats_toCtrl.xlsx")
#write.xlsx(OutputTraitL_statsSlope_toCtrl, file= "statsSlope_toCtrl.xlsx")


# ----- TWO-WAY ------- #

OutputTraitL_stats <- lapply(TraitL_stats2Way, function(x) cbind(timeDF, x) )
lapply(OutputTraitL_stats, function(x) x[100:103,])

timeDF_slindow <- merge(timeDF, data.frame(ZT=rownames(TraitL_statsLumpMC[[1]])), by.x='ZT', by.y='ZT') 
OutputTraitL_statsLumpMC <- lapply(TraitL_statsLumpMC, 
                                   function(x) cbind(timeDF_slindow, x[rownames(x) %in% timeDF_slindow$ZT, order(colnames(x))]))
lapply(OutputTraitL_statsLumpMC, head)


OutputTraitL_statsSlopes <- lapply(TraitL_statsSlope2Way, function(x) cbind(timeDF, x) )
lapply(OutputTraitL_stats, function(x) x[100:103,])

timeDF_slindow <- merge(timeDF, data.frame(ZT=rownames(TraitL_statsLumpMC[[1]])), by.x='ZT', by.y='ZT') 
OutputTraitL_statsSlopeLumpMC <- lapply(TraitL_statsSlopeLumpMC, 
                                   function(x) cbind(timeDF_slindow, x[rownames(x) %in% timeDF_slindow$ZT, order(colnames(x))]))
lapply(OutputTraitL_statsSlopeLumpMC, head)

write.xlsx(OutputTraitL_stats, file= "stats_Example.xlsx")
write.xlsx(OutputTraitL_statsLumpMC, file= "stats_TukLetters_Example.xlsx")
write.xlsx(OutputTraitL_statsSlopes, file= "statsSlope_Example.xlsx")
write.xlsx(OutputTraitL_statsSlopeLumpMC, file= "statsSlope_TukLetters_Example.xlsx")



# -------------------------------------------------------------- #
# 7 plot data


# 7a individual graphs, for quality control
plot_single <- function(vec, xvals, plotname) {
  miny <- min(vec, na.rm=T); maxy <- max(vec, na.rm=T)
  miny <- miny - .02*abs(miny); maxy <- maxy + .02*abs(miny) # defines the yranges
  plot(0,0, xlim=range(xvals), ylim=c(miny, maxy), 
    yaxs='i', type='n', ann=F)                               # set blank canvas with x and y ranges
  rect(9, miny, 24, maxy, col='grey', border=NA)             # draw grey rectangle for the night
  lines(xvals, vec)                                          # draw the line
  mtext(plotname, side=3, line=-1.1, col='red')              # add name
}
plot_all <- function(DF, picname, traitname) {
  no.run <- ncol(DF)-5; no.vert <- ceiling(no.run/2)                                     # asses how many plant to plot
  png(picname, width=7, height=1.1*no.vert+1, unit='in', res=300, pointsize=10)          # define picture size according to no. plants
    par(mfrow=c(no.vert,2), mar=c(2,3,.1,.1), oma=c(0,0,2,0), mgp=c(1, .2, 0), tcl=-.02) # set no. figs, margins and axis layout
    for(i in (1:no.run)+5){ plot_single(DF[,i], DF$ZT, colnames(DF)[i]) }                # plot all single plants in a loop
    mtext(traitname, side=3, outer=T); dev.off()                                         # add master name to plot, and write to file: dev.off()
}
names(OutputTraitL_raw)
# writes instantly to file
#plot_all(OutputTraitL_raw[["pet.len"]],    "pet.length_timesync2.png",  "pet.length")
#plot_all(OutputTraitL_raw[["pet.ang"]],  "pet.angles_timesync2.png",  "pet.ang")
#plot_all(OutputTraitL_raw[["leaf.len"]],   "leaf.length_timesync2.png", "leaf.length")
#plot_all(OutputTraitL_raw[["leaf.angle"]], "leaf.angles_timesync2.png", "leaf.angles")
#plot_all(OutputTraitL_raw[["lam.len"]],    "lam.length_timesync2.png",  "lam.length")
#plot_all(OutputTraitL_raw[["lam.angle"]],  "lam.angles_timesync2.png",  "lam.angles")


# 7b with summary stats

# order of plotting, adjust colours to your wishes
# when compared to reference also adjust pval_cols_toref by removing the reference
# The colours given below are selected for their color blind safeness. Adjust them to suit your own project
levels(groups)
custcols    <- cbind(
                    darkred=c(146,0,0),
                      bluegreen=c(0,158,115),
                    orange=c(230,159,0),
                    blue=c(0,114,178),
                     vermil=c(213,94,0),
                     darkred=c(146,0,0),
                     orange=c(230,159,0),
                      blue=c(0,114,178),
                     bluegreen=c(0,158,115),
                     black=c(0,0,0),
                     black=c(0,0,0),
                     yellow=c(240,228,66),
                     black=c(0,0,0),
                     orange=c(230,159,0),
                     black=c(0,0,0),
                     skyblue=c(86,180,233),
                     lightblue=c(182,219,255),
                     deepdarkred=c(64,0,0),
                     blue=c(0,114,178),
                     pink=c(204,121,167))
custcols <- custcols / 255

custcols_err  <- rgb(custcols[1,], custcols[2,], custcols[3,], 0.3)
custcols_mean <- rgb(custcols[1,], custcols[2,], custcols[3,], 1)
plot(1:length(custcols_mean), pch=16, col=custcols_mean, cex=4)

pval_cols_toref <- custcols_mean[-5] # The reference is on index 5
pval_cols2W     <- rgb(custcols[1,c("black", "skyblue", "yellow")], 
                       custcols[2,c("black", "skyblue", "yellow")], 
                       custcols[3,c("black", "skyblue", "yellow")])
# define shapes
head(OutputTraitL_stats[[1]]) # to check names
#custline        <- rep(c(5,1,4), 10)
custline        <- rep(c(5,1,), 10)


maxPval       <- 12

# define plotting function
definekeyrows <- function(mat){
  coloi <- grep("^n_", colnames(mat))
  sel   <- apply(mat[, coloi], 1, min) > 3 
  sel[is.na(sel)] <- FALSE
  return(sel)
}


plot_stats <- function(Tr.list, trait, picname, Tuk.list = NULL, 
                       Pstyle = c('oneWay','toRef', 'twoWay')[1], 
                       plotTuk=FALSE, plotPVAL=FALSE, 
                       XRangeFromData = TRUE, CustXrange=c(2,25.8),
                       YRangeFromData = TRUE, CustYrange=c(-30,20),
                       HRR=rep(TRUE, nrow(Tr.list[[1]])) ) { 
# trim away NAs
  DF <- Tr.list[[trait]]
#  DF[!HRR, ] <- NA
  DF <- DF[apply(DF, 1, function(x) sum(is.na(x))) == 0, ]
  if(XRangeFromData)  { xrange <- range(DF$ZT) }
  if(!XRangeFromData) { xrange <- CustXrange}
  
  # define write out location and layout of figure
  #png(picname, width=7, height=5, unit='in', res=300, pointsize=10) 
  pdf(picname, width=7.5, height=5, pointsize=10)
  par(oma=c(.1,.1,2.5,.1), mgp=c(1.5, .2, 0), tcl=-.2, cex=1)                    # mgp -> location of axis labels, axis names and axis
                                                                                 # tcl -> size of the tickmarks
# if indicated, include Tukey Letters and segments
  if(plotTuk){
    layout(matrix(1:3, ncol=1), heights=c(0.8,3,1))
    par(mar=c(0,7,0,15))
    TukMat <- Tuk.list[[trait]]
    TukMat <- TukMat[apply(TukMat, 1, function(x) sum(is.na(x))) == 0, ]
    TukLetters <- TukMat[, 6:ncol(TukMat)]
    plot(0,0, xlim=xrange, ylim=c(ncol(TukMat)-5,0),                       # define canvas coordinate
         yaxs='i', xaxs='i', type='n', ann=F, axes=F)
    nightsta <- seq(9, max(xrange), by=24)
    rect(nightsta, 0, nightsta+15, ncol(TukMat)-5, col=rgb(0.9,0.9,0.9), border=NA)               # draw night box
    for(i in 1:ncol(TukLetters)){
      text(TukMat$ZT, i-0.5, TukLetters[,i], col=custcols_mean[i], font=2)}       # draw in letters
    segsize   <- TukMat$ZT[2] - TukMat$ZT[1]                                      # define location for segment seperators
    sta       <- TukMat$ZT - 0.5*segsize; end <- sta + segsize
    midpoints <- union(sta, end)
    segments(midpoints, 0, midpoints, ncol(TukLetters), lty=2, col='light grey')  # draw in dashed lines to seperate segments
    axis(side=2, at=1:ncol(TukLetters)-0.5, lab=NA)                               # add empty axis
    text(min(xrange), 1:ncol(TukLetters)-0.5, colnames(TukLetters),                # add names to y-axis (for Tukey)
      col=custcols_mean, font=2, pos=2, las=1, xpd=NA)
    box()
  }
  if(!plotTuk){ layout(matrix(1:2, ncol=1), heights=c(3,1)) }

# plot means and sems
  par(mar=c(0,7,0,15))
  lowvals  <- DF[, grep('mean', colnames(DF))] - DF[, grep('sem', colnames(DF))]
  highvals <- DF[, grep('mean', colnames(DF))] + DF[, grep('sem', colnames(DF))] # determine border of s.e.m.
  if(YRangeFromData) {
  miny <- min(lowvals); maxy <- max(highvals)
  miny <- miny - .1*abs(miny); maxy <- maxy + .1*abs(miny  ) 
  } else { miny <- CustYrange[1]; maxy <- CustYrange[2] };                     # setup y-axis scale
  plot(0,0, xlim=xrange, ylim=c(miny, maxy), 
       yaxs='i', xaxs='i', type='n', ann=F, axes=F)                            # set blank canvas with x and y ranges
    nightsta <- seq(9, max(xrange), by=24)
    rect(nightsta, miny, nightsta+15, maxy, col=rgb(.9,.9,.9), border=NA)                             # draw grey rectangle for the night
    abline(h=0, lty=2)                                                         # dashed line on the y=0
    if(plotTuk){
      segments(midpoints, miny, midpoints, maxy, lty=2, col='light grey') }    # draw in the dashed line if Tukey letter
    for(i in 1:ncol(lowvals)){
      polygon(c(DF$ZT, rev(DF$ZT)), c(lowvals[,i], rev(highvals[,i])), 
      col=custcols_err[i], border=NA) }                                       # draw shaded background of the sem
    for(i in 1:ncol(lowvals)){
      lines(DF$ZT, DF[,grep('mean', colnames(DF))[i] ], 
      col=custcols_mean[i], lwd=2, lty=custline[i]) }                                           # draw the mean line
  axis(side=2); box(); title(ylab=trait)                                       # add axis, graph border and label
  legend(max(xrange), maxy, grep('mean', colnames(DF), value=T), 
        col=custcols_mean, lty= custline, lwd=2, bty='n',
        xjust=0, yjust=1, xpd=NA, seg.len=6)                                              # draws the legend

# plot Pvalues
if(plotPVAL){
  par(mar=c(3,7,0,15))
  Pvals <- -(log(DF[,grep("pval", colnames(DF))], 10))                         # -10Log() transform the pvalues,
  Pvals[Pvals > maxPval]  <- maxPval                                           #   cap at a maximum pvalue
  plot(0,0, xlim=xrange, ylim=c(0, maxPval),
       yaxs='i', xaxs='i', type='n', ann=F, axes=F)                                    # set up black canvas with coordinates
    rect(nightsta, 0, nightsta+15, maxPval, col=rgb(.9,.9,.9), border=NA); box()                      # draw in night period
    abline(h=-log(0.05, 10), lty=2)                                            # Pvalue threshold line
  
  if(Pstyle == 'oneWay'){                                                      # In case of one-way, a single black line (within vs between groups variation)
    lines(DF$ZT, Pvals, col=rgb(0,0,0), lwd=2, xpd=NA) }
  if(Pstyle == 'toRef'){                                                       # In case of comparison to the reference plot aline for each comparison
    for(i in 1:ncol(Pvals)){
      lines(DF$ZT, Pvals[,i], col=pval_cols_toref[i], lwd=2, xpd=NA)}
      legend(max(xrange), maxPval, colnames(Pvals), 
             col=pval_cols_toref, lty=1, lwd=3, 
             bty='n', xjust=0, yjust=1, xpd=NA)  }
  if(Pstyle == 'twoWay') {                                                     # in case of two way ANOVA, plot the Pvalue of the 2 main and one interaction effect
    for(i in 1:3){lines(DF$ZT, Pvals[,i], col=pval_cols2W[i], lwd=2, xpd=NA)} 
    legend(max(xrange), maxPval, c(colnames(Pvals)[1:2], "pval_Interaction"), 
         col=pval_cols2W, lty=1, lwd=3, bty='n', 
         xjust=0, yjust=1, xpd=NA) }
  axis(side=2)
  title(ylab="-log10(Pval)", xpd=NA)                        # annotate axis with labels
}
  axis(side=1, seq(0, max(xrange), by=2), gap.axis = 0); title(xlab="Zeitgeber Time (hours)", xpd=NA)
  mtext(trait, side=3, outer=T, line=1, font=2)                                # add trait name above the axis
  dev.off()
}
# end of the function



HighRepRows <- definekeyrows(OutputTraitL_stats[[1]])

names(OutputTraitL_stats)
plot_stats(OutputTraitL_stats, "pet.ang",  "pet.angle.summary_Example.pdf",   Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL= F, HRR = HighRepRows)
plot_stats(OutputTraitL_stats, "lam.pet.ang",  "lamina.pet.angle.summary_Example.pdf",   Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL= F, HRR = HighRepRows)
plot_stats(OutputTraitL_stats, "lam.ang",  "lam.angle.summary_Example.pdf",   Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows)
plot_stats(OutputTraitL_stats, "leaf.ang", "leaf.angle.summary_Example.pdf",  Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows)
plot_stats(OutputTraitL_stats, "pet.len",    "pet.length.summary_Example.pdf",  Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows)
plot_stats(OutputTraitL_stats, "lam.len",    "lam.length.summary_Example.pdf",  Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows)
plot_stats(OutputTraitL_stats, "leaf.len",   "leaf.length.summary_Example.pdf", Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows)
plot_stats(OutputTraitL_stats, "lam.proj.len",   "lamina.projec.length.summary_Example.pdf", Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows)
plot_stats(OutputTraitL_stats, "pet.proj.len",   "petiole.projec.length.summary_Example.pdf", Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows)
plot_stats(OutputTraitL_stats, "tip.hei",   "tip.height.summary_Example.pdf", Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows)

## Same plots with custom X and Y ranges ##

plot_stats(OutputTraitL_stats, "pet.ang",  "pet.angle.summary_Yrange_Example.pdf",   Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL= F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-13,40))
plot_stats(OutputTraitL_stats, "lam.pet.ang",  "lamina.pet.angle.summary_Example.pdf",   Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL= F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-5,55))
plot_stats(OutputTraitL_stats, "lam.ang",  "lam.angle.summary_Yrange_Example.pdf",   Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-10,80))
plot_stats(OutputTraitL_stats, "pet.len",    "pet.length.summary_Yrange_Example.pdf",  Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-0.5,3.5))


plot_stats(OutputTraitL_stats, "lam.len",    "lam.length.summary_Example.pdf",  Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-40,40))
plot_stats(OutputTraitL_stats, "leaf.len",   "leaf.length.summary_Example.pdf", Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-40,40))
plot_stats(OutputTraitL_stats, "leaf.ang", "leaf.angle.summary_Example.pdf",  Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-40,40))
plot_stats(OutputTraitL_stats, "lam.proj.len",   "lamina.projec.length.summary_Example.pdf", Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-40,40))
plot_stats(OutputTraitL_stats, "pet.proj.len",   "petiole.projec.length.summary_Example.pdf", Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-40,40))
plot_stats(OutputTraitL_stats, "tip.hei",   "tip.height.summary_Example.pdf", Tuk.list = OutputTraitL_statsLumpMC, 
           Pstyle = 'twoWay', plotTuk=T, plotPVAL = F, HRR = HighRepRows,
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-40,40))

plot_stats(OutputTraitL_stats, "pet.angle",  "pet.angle.summaryNT.png",   Tuk.list = OutputTraitL_statsLumpMC, plotTuk=F)
plot_stats(OutputTraitL_stats, "lam.angle",  "lam.angle.summaryNT.png",   Tuk.list = OutputTraitL_statsLumpMC, plotTuk=F)
plot_stats(OutputTraitL_stats, "leaf.angle", "leaf.angle.summaryNT.png",  Tuk.list = OutputTraitL_statsLumpMC, plotTuk=F)
plot_stats(OutputTraitL_stats, "pet.len",    "pet.length.summaryNT.png",  Tuk.list = OutputTraitL_statsLumpMC, plotTuk=F)
plot_stats(OutputTraitL_stats, "lam.len",    "lam.length.summaryNT.png",  Tuk.list = OutputTraitL_statsLumpMC, plotTuk=F)
plot_stats(OutputTraitL_stats, "leaf.len",   "leaf.length.summaryNT.png", Tuk.list = OutputTraitL_statsLumpMC, plotTuk=F)

## Slopes graphs ###

names(OutputTraitL_statsSlopes)
plot_stats(OutputTraitL_statsSlopes, "pet.ang",  "pet.angle.slope.summary_Example.pdf",   Tuk.list = OutputTraitL_statsSlopeLumpMC, 
           plotTuk=T,Pstyle = 'twoWay', plotPVAL = FALSE, HRR = HighRepRows, 
           XRangeFromData = T, CustXrange = c(2,48), YRangeFromData = T, CustYrange=c(-25,20))
plot_stats(OutputTraitL_statsSlopes, "lam.ang",  "lam.angle.slope.summary_Example.pdf",   Tuk.list = OutputTraitL_statsSlopeLumpMC, 
           plotTuk=T,Pstyle = 'twoWay', plotPVAL = FALSE, HRR = HighRepRows, 
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-40,40))
plot_stats(OutputTraitL_statsSlopes, "leaf.ang",  "leaf.angle.slope.summary_Example.pdf",   Tuk.list = OutputTraitL_statsSlopeLumpMC, 
           plotTuk=T,Pstyle = 'twoWay', plotPVAL = FALSE, HRR = HighRepRows, 
           XRangeFromData = T, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-30,25))
plot_stats(OutputTraitL_statsSlopes, "pet.len",  "pet.len.slope.summary_Example.pdf",   Tuk.list = OutputTraitL_statsSlopeLumpMC, 
           plotTuk=T,Pstyle = 'twoWay', plotPVAL = FALSE, HRR = HighRepRows, 
           XRangeFromData = F, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-1,1))
plot_stats(OutputTraitL_statsSlopes, "leaf.len",  "leaf.len.slope.summary_Example.pdf",   Tuk.list = OutputTraitL_statsSlopeLumpMC, 
           plotTuk=T,Pstyle = 'twoWay', plotPVAL = FALSE, HRR = HighRepRows, 
           XRangeFromData = F, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-1,1.5))
plot_stats(OutputTraitL_statsSlopes, "lam.len",  "lam.len.slope.summary_Example.pdf",   Tuk.list = OutputTraitL_statsSlopeLumpMC, 
           plotTuk=T,Pstyle = 'twoWay', plotPVAL = FALSE, HRR = HighRepRows, 
           XRangeFromData = F, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-1,1))
plot_stats(OutputTraitL_statsSlopes, "lam.pet.ang",  "lam.pet.ang.slope.summary_Example.pdf",   Tuk.list = OutputTraitL_statsSlopeLumpMC, 
           plotTuk=T,Pstyle = 'twoWay', plotPVAL = FALSE, HRR = HighRepRows, 
           XRangeFromData = F, CustXrange = c(2,26), YRangeFromData = F, CustYrange=c(-25,20))




# ------------------------------------------------------------------------------ #

# 8. Correlation plots 


# extract data from traits of interest
trX <- 'pet.ang'; trY <- 'lam.ang'
dfX <- OutputTraitL_stats[[trX]]; dfY <- OutputTraitL_stats[[trY]]; 
dfX <- dfX[1:1420,]; dfY <- dfY[1:1420,]

# identify first and last timepoint (remove NAs and take first and last time point (tp)
tpNA  <- apply(dfX, 1, function(x) sum(is.na(x)) > 0)
tpVal <- dfX$ZT[!tpNA]
tpSta <- tpVal[1]
tpEnd <- tpVal[length(tpVal)]

# obtain mean and sem values of the groups (ctrl, treatment etc...)
meanX <- dfX [, grep('mean', colnames(dfX))]; meanY <- dfY[, grep('mean', colnames(dfY))]
errX  <- dfX [, grep('sem', colnames(dfX))];  errY  <- dfY[, grep('sem', colnames(dfY))]
lowX  <- meanX - errX; lowY  <- meanY - errY
highX <- meanX + errX; highY <- meanY + errY

# determine range to make plot
rangeX <- range(lowX, highX, na.rm=T)
rangeY <- range(lowY, highY, na.rm=T)

# make an empty canvas
plot(0,0, type='n', xlim=rangeX, ylim=rangeY,
     xlab='petiole angle', ylab='lamina angle')

# plot the lines for each groups in a loop (using the custcols defined earlier)
for(i in 1:ncol(meanX)){
  points(meanX[,i], meanY[,i], col=custcols_mean[i], pch = 16, cex = 0.5)
}

# write in start and end timepoints
idxSta <- which(dfX$ZT == tpSta)
idxEnd <- which(dfX$ZT == tpEnd)
idxMax1 <- which(meanX[,1] == max(meanX[,1], na.rm = T))
idxMax2 <- which(meanX[,2] == max(meanX[,2], na.rm = T))
text(meanX[idxSta,], meanY[idxSta,], "ZT2", font = 2)
text(meanX[idxMax1,1], meanY[idxMax1,1], paste0("ZT", round(dfX$ZT[idxMax1])), font = 2)
text(meanX[idxMax2,2], meanY[idxMax2,2], paste0("ZT", round(dfX$ZT[idxMax2])), font = 2)
text(meanX[idxEnd,], meanY[idxEnd,], paste0("ZT", round(tpEnd)), font = 2)

# plot the legend in the top left
legend(rangeX[1], rangeY[2], colnames(meanX), 
       col=custcols_mean[1:2], lty=1, lwd=3, bty='n', 
       xjust=0, yjust=1, xpd=NA)



# -------------------------------------------------------------- #
