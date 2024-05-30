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
    ## also sample across reassess
    ## ======== CEA outputs
    ## combined data
    ALL[[n]] <- WH[,.(id=n,
                      who.cost=mean(who.cost),
                      who.DALYs=mean(who.cfr*dLYS),
                      who.cfr=mean(who.cfr),
                      who.ATT=mean(who.ATT),
                      soc.cost=mean(soc.cost),
                      soc.DALYs=mean(soc.cfr*dLYS),
                      soc.cfr=mean(soc.cfr),
                      soc.ATT=mean(soc.ATT),
                      tbs1.cost=mean(tbs1.cost),
                      tbs1.DALYs=mean(tbs1.cfr*dLYS),
                      tbs1.cfr=mean(tbs1.cfr),
                      tbs1.ATT=mean(tbs1.ATT),
                      tbs2.cost=mean(tbs2.cost),
                      tbs2.DALYs=mean(tbs2.cfr*dLYS),
                      tbs2.cfr=mean(tbs2.cfr),
                      tbs2.ATT=mean(tbs2.ATT)),
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

CEAplots <- function(M,ring=TRUE,alph=0.1){
  MS <- M[,.(`DALYs averted`=mean(`DALYs averted`),
             `Incremental cost`=mean(`Incremental cost`)),
          by=.(country,algorithm)]
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
    geom_point(data=M,alpha=alph,shape=1)+
    geom_point(size=3,shape=3,stroke=2)+
    facet_wrap(~country)+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    ylab('Incremental cost (USD)')
  ## return
  if(ring)
    GP <- GP+geom_line(data=HZ,col=2,lty=2) + geom_point(data=HZ,shape=1,size=3,col=2)
  GP
}


makeTable <- function(MZ){
  MZ[,.(country,
        `cost per child, SOC`=brkt(soc.cost,soc.cost.lo,soc.cost.hi),
        `cost per child, WHO`=brkt(who.cost,who.cost.lo,who.cost.hi),
        `cost per child, TBS1`=brkt(tbs1.cost,tbs1.cost.lo,tbs1.cost.hi),
        `cost per child, TBS2`=brkt(tbs2.cost,tbs2.cost.lo,tbs2.cost.hi),
        `incremental cost, WHO`=brkt(DC_WHO,DC_WHO.lo,DC_WHO.hi),
        `incremental cost, TBS1`=brkt(DC_TBS1,DC_TBS1.lo,DC_TBS1.hi),
        `incremental cost, TBS2`=brkt(DC_TBS2,DC_TBS2.lo,DC_TBS2.hi),
        `100x ATT per child, SOC`=brkt(1e2*soc.ATT,1e2*soc.ATT.lo,1e2*soc.ATT.hi),
        `100x ATT per child, WHO`=brkt(1e2*who.ATT,1e2*who.ATT.lo,1e2*who.ATT.hi),
        `100x ATT per child, TBS1`=brkt(1e2*tbs1.ATT,1e2*tbs1.ATT.lo,1e2*tbs1.ATT.hi),
        `100x ATT per child, TBS2`=brkt(1e2*tbs2.ATT,1e2*tbs2.ATT.lo,1e2*tbs2.ATT.hi),
        `100x incremental ATT, WHO`=brkt(1e2*DT_WHO,1e2*DT_WHO.lo,1e2*DT_WHO.hi),
        `100x incremental ATT, TBS1`=brkt(1e2*DT_TBS1,1e2*DT_TBS1.lo,1e2*DT_TBS1.hi),
        `100x incremental ATT, TBS2`=brkt(1e2*DT_TBS2,1e2*DT_TBS2.lo,1e2*DT_TBS2.hi),
        `100x DALYs averted, WHO`=brkt(-1e2*DD_WHO,-1e2*DD_WHO.hi,-1e2*DD_WHO.lo),
        `100x DALYs averted, TBS1`=brkt(-1e2*DD_TBS1,-1e2*DD_TBS1.hi,-1e2*DD_TBS1.lo),
        `100x DALYs averted, TBS2`=brkt(-1e2*DD_TBS2,-1e2*DD_TBS2.hi,-1e2*DD_TBS2.lo),
        `100x deaths per child, SOC`=brkt(1e2*soc.cfr,1e2*soc.cfr.lo,1e2*soc.cfr.hi),
        `100x deaths per child, WHO`=brkt(1e2*who.cfr,1e2*who.cfr.lo,1e2*who.cfr.hi),
        `100x deaths per child, TBS1`=brkt(1e2*tbs1.cfr,1e2*tbs1.cfr.lo,1e2*tbs1.cfr.hi),
        `100x deaths per child, TBS2`=brkt(1e2*tbs2.cfr,1e2*tbs2.cfr.lo,1e2*tbs2.cfr.hi),
        `100x incremental deaths, WHO`=brkt(1e2*DM_WHO,1e2*DM_WHO.lo,1e2*DM_WHO.hi),
        `100x incremental deaths, TBS1`=brkt(1e2*DM_TBS1,1e2*DM_TBS1.lo,1e2*DM_TBS1.hi),
        `100x incremental deaths, TBS2`=brkt(1e2*DM_TBS2,1e2*DM_TBS2.lo,1e2*DM_TBS2.hi),
        `100x DALYs averted, WHO`=brkt(-1e2*DD_WHO,-1e2*DD_WHO.hi,-1e2*DD_WHO.lo),
        `100x DALYs averted, TBS1`=brkt(-1e2*DD_TBS1,-1e2*DD_TBS1.hi,-1e2*DD_TBS1.lo),
        `100x DALYs averted, TBS2`=brkt(-1e2*DD_TBS2,-1e2*DD_TBS2.hi,-1e2*DD_TBS2.lo),
        `ICER, WHO`=round(ICER_WHO,2),
        `ICER, TBS1`=round(ICER_TBS1,2),
        `ICER, TBS2`=round(ICER_TBS2,2),
        `ICER, TBS1 vs WHO`=round(wICER_TBS1,2),
        `ICER, TBS2 vs WHO`=round(wICER_TBS2,2)
        )]
}
