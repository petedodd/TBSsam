library(ggplot2)
library(scales)

## helpers
lo <- function(x) quantile(x,probs = 0.025)
hi <- function(x) quantile(x,probs = 1-0.025)
## rot45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))
brkt <- function(M,L,H,ndp=0) paste0(round(M,ndp),' (',
                                     round(L,ndp),' to ',
                                     round(H,ndp),')')


## === create cost data
CD <- parsecosts(gh('data/TB-Speed_SAM_Costs.csv'))
CD[,c('cost.mid','cost.sd'):=.((High+Low)/2,(High-Low)/3.92)]
## model as gamma parameters
CD[,theta:=cost.sd^2/cost.mid]
CD[,k:=cost.mid/theta]

makeCostPSA <- function(N){
  CDL <- CD[rep(1:nrow(CD),N),.(NAME,country,k,theta)]
  CDL[,id:=rep(1:N,each=nrow(CD))]
  CDL[,value := rgamma(n=nrow(CDL),shape=k,scale=theta)]
  CDL[is.na(value),value:=0.0]
  CDW <- dcast(CDL,country+id~NAME,value.var = 'value')
  CDW
}



## now resamples as loop 1:Npops, calculates mean HE outcomes for each pop and returns
## partially because with stochastic output get hard to interpret quadrimodal outputs ~ ATT+/- x death+/-
combineHE <- function(WS,
                      popsize=1e3,
                      Npops=1
                      ){

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

## across all countries
make.ceacs <- function(M,lmz){
  CEAC <- list()
  for( cn in cnz){
    cat('...',cn,'...\n')
    for(alg in M[,unique(algorithm)]){
      pz <- make.ceac(M[country==cn & algorithm==alg,
                        .(Q=`DALYs averted`,P=`Incremental cost`)],lmz)
      pz <- unlist(pz)
      CEAC[[paste(cn,alg)]] <- data.table(country=cn,algorithm=alg,lambda=lmz,`Probability CE`=pz)
    }
  }
  rbindlist(CEAC)
}





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
