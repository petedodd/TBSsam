library(ggplot2)
library(scales)

combineHE <- function(WS,
                      popsize=1e3,
                      Npops=1
                      ){

  ## LYS
  WS <- merge(WS,LYKc[,.(country,dLYS=LYS)],by='country',all.x=TRUE)
  WS <- merge(WS,LYK[,.(country,LYS)],by='country',all.x=TRUE) #undiscounted


  ALL <- list()
  sfr <- unique(WS[,.(id,TB)])
  for(n in 1:Npops){
    if(!n %% round(0.1*Npops)) cat(' [ ',n,' / ',Npops,' ] pops...\n')
    ## construct sampled population
    prev <- P$s.TBprev$r(1)
    nprev <- rbinom(1,popsize,prev)
    idz1 <- sample(sfr[TB=='TB',id],nprev)
    idz0 <- sample(sfr[TB!='TB',id],popsize-nprev)
    WH <- WS[id %in% c(idz0,idz1)]
    WH[,id:=1:nrow(WH)]
    ## ======== CEA outputs
    ## combined data
    ALL[[n]] <- WH[,.(id=n,
                      who.cost=mean(who.cost),
                      who.DALYs=mean(who.cfr*dLYS),
                      soc.cost=mean(soc.cost),
                      soc.DALYs=mean(soc.cfr*dLYS),
                      tbs1.cost=mean(tbs1.cost),
                      tbs1.DALYs=mean(tbs1.cfr*dLYS),
                      tbs2.cost=mean(tbs2.cost),
                      tbs2.DALYs=mean(tbs2.cfr*dLYS)),
                   by=country]
  }
  ## return
  ALL <- rbindlist(ALL)
  return(ALL)
}

## reshape and compute incrementals
reshapeINC <- function(A){
  M <- melt(A,id=c('country','id'))
  M[,c('algorithm','quantity'):=tstrsplit(variable,split='\\.')]
  M[,variable:=NULL]
  M <- dcast(M,country+id+algorithm ~ quantity,value.var = 'value')
  MR <- M[algorithm=='soc']
  M <- merge(M,MR[,.(country,id,soc.DALYs=DALYs,soc.cost=cost)],by=c('country','id'),all.x=TRUE)
  M[,c('DALYs averted','Incremental cost'):=.(soc.DALYs-DALYs,cost-soc.cost)]
  M <- M[algorithm!='soc']
  M
}


## ---- utilities for making CEACs
make.ceac <- function(CEA,lamz){
  crv <- lamz
  for(i in 1:length(crv)) crv[i] <- CEA[,mean(lamz[i]*Q-P>0)]
  crv
}


## lmz <- seq(from=0,to=150,by=0.5)
## CEAC <- list()
## for( cn in cnz){
##   pz <- make.ceac(M[country==cn,
##                     .(Q=`DALYs averted`,P=`Incremental cost`)],lmz)
##   pz <- unlist(pz)
##   CEAC[[cn]] <- data.table(country=cn,lambda=lmz,`Probability CE`=pz)
## }
## CEAC <- rbindlist(CEAC)

## ggplot(CEAC,aes(lambda,`Probability CE`,col=country))+
##   geom_line()




## ggplot(M[sample(nrow(M),size=1e2)],
##        aes(`DALYs averted`,`Incremental cost`,col=algorithm))+
##   geom_point()+
##   facet_wrap(~country)


## ----- CE plane

## get Hull in a country
getHull <- function(dQ,dC){
  P <- cbind(dQ,dC)
  X <- rbind(c(0,0),P)
  ch <- chull(X)
  wm <- which(ch==1)
  n <- length(ch)
  if(wm!=1) ch <- c(ch[wm:n],ch[1:(wm-1)]) #reorder
  ch <- c(ch,1)                            #back to start
  X <- X[ch,]                              #restrict
  ## reverse if needed to get direction right
  if(  X[nrow(X)-1,2]/X[nrow(X)-1,1] < X[2,2]/X[2,1] ) X <- X[nrow(X):1,]
  turn <- which(diff(X[,1])<0)[1]   #find the first reversal
  as.data.table(X[1:turn,])
}

## plots of mean CEAC

CEAplots <- function(MS){
  ## mean CEAs
  HZ <- list()
  for(cn in MS[,unique(country)]){
    tmp <- getHull(MS[country==cn,`DALYs averted`],
                   MS[country==cn,`Incremental cost`])
    tmp[,country:=cn]
    HZ[[cn]] <- tmp
  }
  HZ <- rbindlist(HZ)
  names(HZ)[1:2] <- c('DALYs averted','Incremental cost')
  HZ[,algorithm:=NA]
  ## plot
  GP <- ggplot(MS,aes(`DALYs averted`,`Incremental cost`,fill=algorithm,col=algorithm))+
    geom_point()+
    facet_wrap(~country)+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)
  ## return
  GP+geom_line(data=HZ,col=2,lty=2) + geom_point(data=HZ,shape=1,size=3,col=2)
}
