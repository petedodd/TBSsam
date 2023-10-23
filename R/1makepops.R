## this is looking at co-occurence of different symptoms
library(here)
library(data.table)
library(ggplot2)
library(glue)
library(copula)
library(here)

gh <- function(x) glue(here(x))

## =================== combined pop version ===============

## gathering a few reused things togther:
emp.plot <- function(N,D,ttl){
  ## prepare data
  nmz <- names(D)
  rnmz <- nmz[3:length(nmz)]
  ## D[,..rnmz]
  DA <- as.matrix(D[,..rnmz])
  diagn <- diag(DA)
  DA <- DA/N
  dg <- diag(DA)
  B <- outer(dg,dg)
  V <- outer(dg*(1-dg),dg*(1-dg))
  DR <- DA/B
  CV <- DA-B #covariance
  CR <- CV / sqrt(V+1e-10)
  nmz <- names(D)
  nmz <- nmz[3:length(nmz)]
  colnames(DR) <- nmz
  DR <- as.data.table(DR)
  DR[,Variables:=nmz]
  DRM <- melt(DR,id=c('Variables'))
  DRM <- DRM[!is.na(value)]
  DRM <- DRM[as.character(Variables)!=as.character(variable)]
  DRM[,summary(value)]
  DRM$Variables <- factor(DRM$Variables)
  DRM[,pairing:=paste0(Variables,' x ',variable)]
  DRM <- DRM[order(value)]
  tmp <- DRM[value>0,.(pairing,value)]
  tmp$pairing <- factor(tmp$pairing,levels=tmp$pairing)

  ## inference data
  names(D)[1:2] <- c('Label','Variables')
  DM <- melt(D,id=c('Label','Variables'))
  DM <- DM[!is.na(value)]
  key <- sort(unique(DM$variable))
  key <- data.table(var=key,id=1:length(key))
  DM <- merge(DM,key[,.(variable=var,ID1=id)],by='variable')
  DM <- merge(DM,key[,.(Variables=var,ID2=id)],by='Variables')

  ## make plot
  GP <- ggplot(tmp,aes(pairing,value))+
    geom_point()+
    coord_flip()+
    ylab('P[i,j] / (P[i] x P[j])')+
    geom_hline(yintercept = 1,col=2)
  if(ttl!='') GP <- GP + ggtitle(ttl)

  tmp2 <- DM[ID1==ID2]
  tmp2[,probability:=value/N]
  tmp2 <- tmp2[order(probability)]
  tmp2$variable <- factor(tmp2$variable,
                          ordered = TRUE,
                          levels = tmp2$variable)

  GP2 <- ggplot(tmp2,aes(variable,probability))+
    geom_point()+
    coord_flip()


  ## return
  list(GP=GP,DM=DM,key=key,N=N,GP2=GP2,pairs=list(CV=CV,CR=CR,CP=tmp,diagn=diagn))
}


## files
flz <- c('SAM_notTB.csv',
         'SAM_TB.csv')

## hash for N
NL <- list(SAM_notTB.csv = 434,
           SAM_TB.csv = 101
           )

## run loop
SDS <- KZ <- NZ <- PZ <- list()
for(fn in flz){
    D <- fread(paste(gh('data'),fn,sep='/'))
  tmp <- emp.plot(NL[[fn]],D,'')
  fn2 <- gsub('csv','pdf',fn)
  fn2 <- gh('graphs/') + fn2
  ggsave(tmp$GP,file=fn2,w=19.6,h=11.4)
  fn3 <- gsub('\\.pdf','_diag\\.pdf',fn2)
  ggsave(tmp$GP2,file=fn3,w=19.6/2,h=11.4/2)
  SDS[[fn]] <- tmp$DM
  KZ[[fn]] <- tmp$key
  NZ[[fn]] <- tmp$N
  PZ[[fn]] <- tmp$pairs
}

save(SDS,file=gh('data/SDS.Rdata'))
save(KZ,file=gh('data/KZ.Rdata'))
save(NZ,file=gh('data/NZ.Rdata'))
save(PZ,file=gh('data/PZ.Rdata'))


## --------------------
## ===  data from paper:

## night sweats
## CTB: yes=206, no=187
## TB: yes=422, no=577
## uTB: yes=568, no=1845

## y/n
ns.TB <- c(206+422,
           206+422 + 187+577)
ns.nTB <- c(568,
            568 + 1845)


## haemoptysis
## ctb: yes=2, no=88
## tb: yes=13, no=410
## utb: yes=12, no=851

## y/n
hs.TB <- c(2+13,
           2+13 + 88+410)
hs.nTB <- c(12,
            12 + 851)


## ---------------------
## === correlation modelling
set.seed(1234)
popsize <- 1e4
POPS <- POPS0 <- list()

for(fn in flz){
  ## data
  N <- NL[[fn]]
  P <- PZ[[fn]]
  COR <- P$CR
  COR[is.na(COR)] <- 0
  COR <- 0.5 * (t(COR)+COR)
  DG <- P$diagn

  pms <- P2p(COR)
  ## p2P(pms) #vice versa

  ## make MVN copula w/parms
  NormCop <- normalCopula(pms, dim = nrow(COR), dispstr = "un")

  ## specify beta distributions
  ML <- list()
  for(i in 1:nrow(COR))
    ML[[i]] <- list(shape1=DG[i]+1,shape2=N-DG[i]+1)

  ## make copula with right marginals
  multivcop <- mvdc(NormCop,rep("beta",nrow(COR)),ML)

  ## sample from copula
  v <- rMvdc(popsize,multivcop)
  FN <- gh('data/CR_') + gsub('\\.csv','\\.txt',fn)
  cat(1e2*colMeans(abs(cor(v) - COR)),file=FN)
  nmz <- colnames(v) <- rownames(COR)
  v <- as.data.table(v)
  v[,id:=1:nrow(v)]
  popn <- v[,lapply(.SD,function(x)rbinom(1,1,x)),by=id,.SDcols=nmz]

  ## samples from paper
  if(grepl('notTB',fn)){
    night.sweats <- rbeta(ncol(v),ns.TB[1],ns.TB[2])
    haemoptysis <- rbeta(ncol(v),hs.TB[1],hs.TB[2])
  } else {
    night.sweats <- rbeta(ncol(v),ns.nTB[1],ns.nTB[2])
    haemoptysis <- rbeta(ncol(v),hs.TB[1],hs.TB[2])
  }
  night.sweats <- rbinom(nrow(v),1,night.sweats)
  haemoptysis <- rbinom(nrow(v),1,haemoptysis)

  ## add into population
  popn[,night.sweats:=night.sweats]
  popn[,haemoptysis:=haemoptysis]

  ## version w/o correlations
  v <- matrix(nrow = nrow(v), ncol = length(DG))
  for(i in 1:ncol(v)) v[,i] <- rbeta(nrow(v),shape1 = ML[[i]]$shape1,shape2 = ML[[i]]$shape2)

  FN <- gh('data/CR0_') + gsub('\\.csv','\\.txt',fn)
  cat(1e2*colMeans(abs(cor(v) - COR)),file=FN)
  colnames(v) <- nmz
  v <- as.data.table(v)
  v[,id:=1:nrow(v)]
  popn0 <- v[,lapply(.SD,function(x)rbinom(1,1,x)),by=id,.SDcols=nmz]
  popn0[,night.sweats:=night.sweats]
  popn0[,haemoptysis:=haemoptysis]

  ## compare
  popn[,lapply(.SD,mean)]
  popn0[,lapply(.SD,mean)]

  ## store
  fn <- gsub('\\.csv','',fn)
  POPS[[fn]] <- popn
  POPS0[[fn]] <- popn0

  cat('--- done ---\n')
}

## save
save(POPS,file=gh('data/nPOPS.Rdata'))
save(POPS0,file=gh('data/nPOPS0.Rdata'))

## validation outputs
tmp <- POPS[['SAM_TB']]
tmp <- as.matrix(tmp)
tmp <- tmp[,2:ncol(tmp)]
tmp <- t(tmp) %*% tmp
tmp <- tmp * 1e-4 * 535
tmp[upper.tri(tmp, diag = FALSE)] <- NA
fwrite(tmp,file=here('data/TBchk.csv'))

tmp <- POPS[['SAM_notTB']]
tmp <- as.matrix(tmp)
tmp <- tmp[,2:ncol(tmp)]
tmp <- t(tmp) %*% tmp
tmp <- tmp * 1e-4 * 535
tmp[upper.tri(tmp, diag = FALSE)] <- NA
fwrite(tmp,file=here('data/notTBchk.csv'))
