library(car)
library(emmeans)


print("GetTukeyLetters")
GetTukeyLetters <- function(pwc, Pval, gms){ 
  dif <- Pval < 0.05
  gbr <- names(gms)[order(gms)]
  fd  <- rep(NA, length(gbr)); staend <- data.frame(low=fd, mid=fd, high=fd)
  rownames(staend) <- gbr;
  for(i in 1:length(gbr)){
    gr    <- gbr[i]
    sel   <- !dif & apply(matrix(pwc %in% gr, ncol=2),1,sum) > 0
    unigr <- unique(c(gr, as.vector(pwc[sel,])))
    ranug <- which(gbr %in% unigr)
    staend[i,] <- c(min(ranug), i, max(ranug))
  }
  letgr  <- split(x=staend, f=staend$high)           
  letvec <- rep('', length(gbr)) 
  for(i in 1:length(letgr)){
    df   <- letgr[[i]]
    tlv  <- rep("", length(gbr))
    tlv[min(df$mid):max(df)] <- letters[i]
    letvec <- paste0(letvec, tlv)
  }
  names(letvec) <- gbr
  return(letvec[names(gms)])
}


print("getstats_1way")
getstats_1way <- function(xxx, fac, NAth = 0.2) { 
  propNA <- sum(is.na(xxx)) / length(xxx)
  nams   <- paste(rep(c('mean', 'stdev', 'n', 'sem', 'conf95'), each=length(levels(fac))), levels(fac), sep='_')
  if(propNA < NAth){
    mod    <- aov(xxx ~ fac)
    summod <- summary(mod)
    pval   <- summod[[1]][1, 'Pr(>F)'] 
    ave    <- tapply(xxx, fac, mean, na.rm=T)
    stdev  <- tapply(xxx, fac, sd, na.rm=T)
    nn     <- tapply(xxx, fac, function(x) sum(!is.na(x)) )
    sem    <- stdev / nn^0.5
    conf   <- 1.96*sem
    outvec <- c(ave,stdev,nn,sem,conf,pval)
  } else { outvec <- rep(NA, 5*length(levels(fac))+1) }
  names(outvec) <- c(nams, 'pval')
  return(outvec)
}

print("getstats_PlanComp")
getstats_PlanComp <- function(xxx, fac, ref, NAth = 0.2) { 
  propNA  <- sum(is.na(xxx)) / length(xxx)
  grnams  <- levels(fac)
  refloc  <- which(grnams == ref)
  Pnams <- paste0('pval_', grnams[-refloc], ":", grnams[refloc])
  nams   <- paste(rep(c('mean', 'stdev', 'n', 'sem', 'conf95'), each=length(grnams)), levels(fac), sep='_')
  if(propNA < NAth){
    mod    <- lm(xxx ~ fac)
    lsm    <- emmeans(mod, ~ fac)
    result <- as.data.frame(contrast(lsm, "trt.vs.ctrl", ref=refloc))
    pval   <- result[,6]
    ave    <- tapply(xxx, fac, mean, na.rm=T)
    stdev  <- tapply(xxx, fac, sd, na.rm=T)
    nn     <- tapply(xxx, fac, function(x) sum(!is.na(x)) )
    sem    <- stdev / nn^0.5
    conf   <- 1.96*sem
    outvec <- c(ave,stdev,nn,sem,conf,pval)
  } else { outvec <- rep(NA, 5*length(levels(fac))+length(levels(fac))-1) }
  names(outvec) <- c(nams, Pnams)
  return(outvec)
}


print("getstats_2way")
getstats_2way <- function(xxx, fac1, fac2, nam1, nam2, NAth = 0.2) { 
  propNA <- sum(is.na(xxx)) / length(xxx)
  groups <- as.factor(paste(fac1, fac2, sep="_"))
  nams   <- paste(rep(c('mean', 'stdev', 'n', 'sem', 'conf95'), 
                  each=length(levels(groups))), levels(groups), sep='_')
  if(propNA < NAth){
    mod    <- lm(xxx ~ fac1 * fac2)
    AOV    <- Anova(mod, type="III")
    pval   <- AOV[[4]][2:4]
    ave    <- tapply(xxx, groups, mean, na.rm=T)
    stdev  <- tapply(xxx, groups, sd, na.rm=T)
    nn     <- tapply(xxx, groups, function(x) sum(!is.na(x)) )
    sem    <- stdev / nn^0.5
    conf   <- 1.96*sem
    outvec <- c(ave,stdev,nn,sem,conf,pval)
  } else { outvec <- rep(NA, 5*length(levels(groups))+3) }
  names(outvec) <- c(nams, paste0('pval_', c(nam1, nam2, paste(nam1, nam2, sep="x"))))
  return(outvec)
}


print("getstats_2wayTukLets")
getstats_2wayTukLets <- function(xxx, fac1, fac2, NAth = 0.2) { 
  propNA <- sum(is.na(xxx)) / length(xxx)
  groups <- as.factor(paste(fac1, fac2, sep="_"))
  if(propNA < NAth){
    mod    <- aov(xxx ~ groups)
    Tuk    <- TukeyHSD(mod)
    ave    <- tapply(xxx, groups, mean, na.rm=T)
    combos <- combn(levels(groups),2)
    return( GetTukeyLetters(t(combos[2:1,]), Tuk$groups[,'p adj'], ave) )
  } else { outvec <- rep(NA, length(levels(groups)))
           names(outvec) <- levels(groups)
           return(outvec) }
}

print("getstats_1wayTukLets")
getstats_1wayTukLets <- function(xxx, fac, NAth = 0.2) { 
  propNA <- sum(is.na(xxx)) / length(xxx)
  if(propNA < NAth){
    mod    <- aov(xxx ~ fac)
    Tuk    <- TukeyHSD(mod)
    ave    <- tapply(xxx, fac, mean, na.rm=T)
    combos <- combn(levels(fac),2)
    return( GetTukeyLetters(t(combos[2:1,]), Tuk$fac[,'p adj'], ave) )
  } else { outvec <- rep(NA, length(levels(fac)))
           names(outvec) <- levels(fac)
           return(outvec) }
}
