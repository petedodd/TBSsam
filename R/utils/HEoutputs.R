library(ggplot2)
library(ggpubr)
library(scales)

## helpers
lo <- function(x) quantile(x,probs = 0.025, na.rm = TRUE)
hi <- function(x) quantile(x,probs = 1-0.025, na.rm = TRUE)
## rot45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))
brkt <- function(M,L,H,ndp=0) paste0(round(M,ndp),' (',
                                     round(L,ndp),' to ',
                                     round(H,ndp),')')


## === create cost data
CD <- parsecosts(gh("data/TB-Speed_SAM_Costs.csv"))
CD[, c("cost.mid", "cost.sd") := .((High + Low) / 2, (High - Low) / 3.92)]
## model as gamma parameters
CD[, theta := cost.sd^2 / cost.mid]
CD[, k := cost.mid / theta]

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
                      Npops=1,
                      ## optional:
                      parnmz=c(), #if to compute means for parameters
                      prevdist=NULL #if not NULL, will use a different prevalence.
                      ){
  ALL <- list()
  sfr <- unique(WS[,.(id,TB)])
  for(n in 1:Npops){
    if(!n %% round(0.1*Npops)) cat(' [ ',n,' / ',Npops,' ] pops...\n')
    ## construct sampled population
    if (is.null(prevdist)) {
      prev <- P$s.TBprev$r(1)
    } else {
      prev <- prevdist(1)
    }
    nprev <- rbinom(1,popsize,prev)
    idz1 <- sample(sfr[TB=='TB',id],nprev)
    idz0 <- sample(sfr[TB!='TB',id],popsize-nprev)
    WH <- WS[id %in% c(idz0,idz1)]
    WH[,id:=1:nrow(WH)]
    ## also sample across reassess
    ## ======== CEA outputs
    ## combined data
    all.all <- WH[,
                  .(id=n,
                    who.cost=mean(who.cost),
                    who.DALYs=mean(who.cfr*dLYS),
                    who.DALYs0=mean(who.cfr*LYS),
                    who.cfr=mean(who.cfr),
                    who.ATT=mean(who.ATT),
                    soc.cost=mean(soc.cost),
                    soc.DALYs=mean(soc.cfr*dLYS),
                    soc.DALYs0=mean(soc.cfr*LYS),
                    soc.cfr=mean(soc.cfr),
                    soc.ATT=mean(soc.ATT),
                    tbs1.cost=mean(tbs1.cost),
                    tbs1.DALYs=mean(tbs1.cfr*dLYS),
                    tbs1.DALYs0=mean(tbs1.cfr*LYS),
                    tbs1.cfr=mean(tbs1.cfr),
                    tbs1.ATT=mean(tbs1.ATT),
                    tbs2.cost=mean(tbs2.cost),
                    tbs2.DALYs=mean(tbs2.cfr*dLYS),
                    tbs2.DALYs0=mean(tbs2.cfr*LYS),
                    tbs2.cfr=mean(tbs2.cfr),
                    tbs2.ATT=mean(tbs2.ATT),
                    tbprev=mean(TB == "TB"),
                    ## null comparator
                    nothing.DALYs=mean(nothing.cfr*dLYS),
                    nothing.cost=0.0,
                    ## reassessment 
                    soc.reassess=mean(soc.reassess),
                    tbs1.reassess=mean(tbs1s.reassess),
                    tbs2.reassess = mean(tbs2s.reassess),
                    who.reassess=mean(who.reassess),
                    ## ATT without reassessment, ie from 1st assessment
                    soc.ATTworeassess = mean(!soc.reassess * soc.ATT),
                    who.ATTworeassess = mean(!who.reassess * who.ATT),
                    tbs1.ATTworeassess = mean(!tbs1s.reassess * tbs1.ATT),
                    tbs2.ATTworeassess = mean(!tbs2s.reassess * tbs2.ATT),
                    ## ATT with reassessment, ie from reassessment
                    soc.ATTreassess = mean(soc.reassess * soc.ATT),
                    who.ATTreassess = mean(who.reassess * who.ATT),
                    tbs1.ATTreassess = mean(tbs1s.reassess * tbs1.ATT),
                    tbs2.ATTreassess = mean(tbs2s.reassess * tbs2.ATT),
                    ## initial assessment
                    who.assess=mean(who_scre>0),
                    soc.assess=mean(soc.ptb>0),
                    tbs1.assess=1, #NOTE everyone!
                    tbs2.assess = mean(TBS2Sa > 0)
                    ),
                  by=country]
    
    # FP and TN 
    all.nottb <- WH[TB == "not TB", .(
      id = n,
      who.FP = mean(who.ATT),
      soc.FP = mean(soc.ATT),
      tbs1.FP = mean(tbs1.ATT),
      tbs2.FP = mean(tbs2.ATT),
      who.TN = mean(!who.ATT),
      soc.TN = mean(!soc.ATT),
      tbs1.TN = mean(!tbs1.ATT),
      tbs2.TN = mean(!tbs2.ATT)
    ),
    by = country
    ]
    result <- merge(all.all, all.nottb, by = c("country", "id"))
    
    # FN and TP
    all.tb <- WH[TB == "TB", .(
      id = n,
      who.TP = mean(who.ATT),
      soc.TP = mean(soc.ATT),
      tbs1.TP = mean(tbs1.ATT),
      tbs2.TP = mean(tbs2.ATT),
      who.FN = mean(!who.ATT),
      soc.FN = mean(!soc.ATT),
      tbs1.FN = mean(!tbs1.ATT),
      tbs2.FN = mean(!tbs2.ATT)
    ),
    by = country
    ]
    result <- merge(result, all.tb, by = c("country", "id"))
    if(length(parnmz)>0){ #include parameters as outputs
      pobj <- WH[, lapply(.SD, mean), by = country, .SDcols = parnmz]
      pobj[, id := n]
      result <- merge(result, pobj, by = c("country", "id"))
    }
    ALL[[n]] <- result
  }
  ALL <- rbindlist(ALL)
  ## --- include increments:
  ## incremental wrt SOC
  ALL[,c('DC_TBS1','DC_TBS2','DC_WHO'):=.(tbs1.cost-soc.cost,tbs2.cost-soc.cost,who.cost-soc.cost)]
  ALL[,c('DD_TBS1','DD_TBS2','DD_WHO'):=.(tbs1.DALYs-soc.DALYs,tbs2.DALYs-soc.DALYs,who.DALYs-soc.DALYs)]
  ALL[,c('DD0_TBS1','DD0_TBS2','DD0_WHO'):=.(tbs1.DALYs0-soc.DALYs0,tbs2.DALYs0-soc.DALYs0,who.DALYs0-soc.DALYs0)]
  ALL[,c('DT_TBS1','DT_TBS2','DT_WHO'):=.(tbs1.ATT-soc.ATT,tbs2.ATT-soc.ATT,who.ATT-soc.ATT)]
  ALL[,c('DM_TBS1','DM_TBS2','DM_WHO'):=.(tbs1.cfr-soc.cfr,tbs2.cfr-soc.cfr,who.cfr-soc.cfr)]
  ## wrt WHO
  ALL[,c('wDC_TBS1','wDC_TBS2'):=.(tbs1.cost-who.cost,tbs2.cost-who.cost)]
  ALL[,c('wDD_TBS1','wDD_TBS2'):=.(tbs1.DALYs-who.DALYs,tbs2.DALYs-who.DALYs)]
  ALL[,c('wDD0_TBS1','wDD0_TBS2'):=.(tbs1.DALYs0-who.DALYs0,tbs2.DALYs0-who.DALYs0)]
  ALL[,c('wDT_TBS1','wDT_TBS2'):=.(tbs1.ATT-who.ATT,tbs2.ATT-who.ATT)]
  ALL[,c('wDM_TBS1','wDM_TBS2'):=.(tbs1.cfr-who.cfr,tbs2.cfr-who.cfr)]
  ## wrt TBS1 for TBS2
  ALL[,c('tDC_TBS2'):=.(tbs2.cost-tbs1.cost)]
  ALL[,c('tDD_TBS2'):=.(tbs2.DALYs-tbs1.DALYs)]
  ALL[,c('tDD0_TBS2'):=.(tbs2.DALYs0-tbs1.DALYs0)]
  ALL[,c('tDT_TBS2'):=.(tbs2.ATT-tbs1.ATT)]
  ALL[,c('tDM_TBS2'):=.(tbs2.cfr-tbs1.cfr)]
  ## wrt TBS1 for WHO
  ALL[,c('t1DT_WHO'):=.(who.ATT-tbs1.ATT)]
  ALL[,c('t1DM_WHO'):=.(who.cfr-tbs1.cfr)]
  ALL[,c('t1DD0_WHO'):=.(who.DALYs0-tbs1.DALYs0)]
  ALL[,c('t1DD_WHO'):=.(who.DALYs-tbs1.DALYs)]
  ALL[,c('t1DC_WHO'):=.(who.cost-tbs1.cost)]
    ## wrt TBS2 for WHO
  ALL[,c('t2DT_WHO'):=.(who.ATT-tbs2.ATT)]
  ALL[,c('t2DM_WHO'):=.(who.cfr-tbs2.cfr)]
  ALL[,c('t2DD0_WHO'):=.(who.DALYs0-tbs2.DALYs0)]
  ALL[,c('t2DD_WHO'):=.(who.DALYs-tbs2.DALYs)]
  ALL[,c('t2DC_WHO'):=.(who.cost-tbs2.cost)]
  ## return
  return(ALL)
}

## reshape and compute incrementals
reshapeINC <- function(A,exclude.soc=TRUE){
  M <- melt(A,id=c('country','id'))
  M[,c('algorithm','quantity'):=tstrsplit(variable,split='\\.')]
  M[,variable:=NULL]
  M <- dcast(M,country+id+algorithm ~ quantity,value.var = 'value')
  MR <- M[algorithm=='soc']
  M <- merge(M,MR[,.(country,id,soc.DALYs=DALYs,soc.cost=cost)],by=c('country','id'),all.x=TRUE)
  M[,c('DALYs averted','Incremental cost'):=.(soc.DALYs-DALYs,cost-soc.cost)]
  M <- M[algorithm!='nothing']
  if(exclude.soc){
    M <- M[algorithm!='soc']
  }
  M
}


## ---- utilities for making CEACs
make.ceac <- function(CEA,lamz){
  crv <- lamz
  for(i in 1:length(crv)) crv[i] <- CEA[,mean(lamz[i]*Q-P>0)]
  crv
}

make.enb <- function(CEA,lamz){
  crv <- lamz
  for(i in 1:length(crv)) crv[i] <- CEA[,mean(lamz[i]*Q-P)]
  crv
}


## across all countries
## NOTE could be faster (see below for pattern)
make.ceacs <- function(M,lmz){
  CEAC <- list()
  for( cn in M[,unique(country)]){
    cat('...',cn,'...\n')
    for(alg in M[,unique(algorithm)]){
      pz <- make.ceac(M[country==cn & algorithm==alg,
                        .(Q=`DALYs averted`,P=`Incremental cost`)],lmz)
      pz <- unlist(pz)
      enb <- make.enb(M[country==cn & algorithm==alg,
                        .(Q=`DALYs averted`,P=`Incremental cost`)],lmz)
      enb <- unlist(enb)
      CEAC[[paste(cn,alg)]] <- data.table(country=cn,
                                          algorithm=alg,
                                          lambda=lmz,
                                          `Probability CE`=pz,
                                          ENB = enb
                                          )
    }
  }
  rbindlist(CEAC)
}


## CEAF data
make.ceafs <- function(D,lamz){
  addon <- data.table(L=lamz,id=rep(1:max(D$id),each=length(lamz)))
  B <- merge(D,addon,by='id',allow.cartesian=TRUE)
  B[,enb:=L*(-DALYs)-cost]
  B[,max.enb:=max(enb),by=.(country,L,id)]
  B[,.(P=mean(enb==max.enb),ENB=mean(enb)),by=.(country,L,algorithm)]
}


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


## more hull utilities
getHZ <- function(MS){
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
  ## text and locations
  HZ[,txt:=c(as.character(round(`Incremental cost`))[-1],NA_character_),by=country]
  HZ[,icer:=c(diff(`Incremental cost`) / diff(`DALYs averted`),NA_real_),by=country]
  HZ[,txt:=as.character(round(icer))]
  HZ[,X:=c(`DALYs averted`[-1],NA_real_),by=country]
  HZ[,Y:=c(`Incremental cost`[-1],NA_real_),by=country]
  HZ[,X:=(X+`DALYs averted`)/2]
  HZ[,Y:=(Y+`Incremental cost`)/2]
  HZ
}

## plots of mean CEAC

CEAplots <- function(M,ring=TRUE,alph=0.1){
  MS <- M[,.(`DALYs averted`=mean(`DALYs averted`),
             `Incremental cost`=mean(`Incremental cost`),
             DALYsd=sd(`DALYs averted`),
             COSTsd=sd(`Incremental cost`)),
          by=.(country,algorithm)]
  ## mean CEAs
  HZ <- getHZ(MS)
  shft <- HZ[`DALYs averted`>0,mean(`DALYs averted`,na.rm=TRUE)]/10
  ## plot
  GP <- ggplot(MS,aes(`DALYs averted`,`Incremental cost`,col=algorithm))+
    ## geom_point(data=M,alpha=alph,shape=1,show.legend = FALSE)+
    geom_errorbar(aes(ymin=`Incremental cost`-COSTsd,ymax=`Incremental cost`+COSTsd),width=0)+
    geom_errorbarh(aes(xmin=`DALYs averted`-DALYsd,xmax=`DALYs averted`+DALYsd),height=0)+
    geom_point(size=3,shape=3,stroke=2)+
    facet_wrap(~country)+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    ylab('Incremental cost (USD)')+
    theme_bw() +
    theme(
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 12),    
      axis.title.x = element_text(size = 14),                     
      axis.title.y = element_text(size = 14),                     
      strip.background = element_blank(),
      strip.text = element_text(colour = "black")   
          ) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(colour = "black", size = 14)
      ) +
    scale_color_manual(name = "Diagnostic approach",
                       values = c("who" = "royalblue3", "tbs1" = "orangered2", "tbs2" = "seagreen", "soc" = "black"),
                       labels = c("who" = "WHO TDA", "tbs1" = "One-step TDA", "tbs2" = "Two-step TDA"))
    
  ## return
  if(ring)
    GP <- GP +
      geom_line(data=HZ,col=1,lty=2,show.legend=FALSE) +
      geom_point(data=HZ,shape=1,size=3,col=1,show.legend=FALSE)+
      geom_text(data=HZ,aes(label=txt,x=X,y=Y),
                nudge_x = -shft, nudge_y = 0,
                size=6,col=1,
                show.legend = FALSE)
  GP
}

## function to generate a rankogram plot
makeRankogram <- function(A) {
  ## create ranks
  RK <- A[,
  {
    icer <- c(-DC_TBS1 / DD_TBS1, -DC_TBS2 / DD_TBS2, -DC_WHO / DD_WHO)
    der <- rank(icer) # NOTE need more thought? OK if all +ve
    names(der) <- c("TBS1", "TBS2", "WHO")
    names(icer) <- paste0("I_", names(der))
    as.list(c(der, icer))
  },
  by = .(country, id)
  ]

  ## calculate probabilities (inelegant)
  RKS <- list(
    RK[, .(arm = "TBS1", p1 = mean(TBS1 == 1), p2 = mean(TBS1 == 2), p3 = mean(TBS1 == 3))],
    RK[, .(arm = "TBS2", p1 = mean(TBS2 == 1), p2 = mean(TBS2 == 2), p3 = mean(TBS2 == 3))],
    RK[, .(arm = "WHO", p1 = mean(WHO == 1), p2 = mean(WHO == 2), p3 = mean(WHO == 3))]
  )
  RKS <- rbindlist(RKS)
  names(RKS)[2:4] <- 1:3
  RKS <- melt(RKS, id = "arm")
  names(RKS)[2:3] <- c("Rank", "Probability")

  ## plot
  ggplot(RKS, aes(Rank, Probability, col = arm, group = arm)) +
    geom_point(size = 2) +
    geom_line() +
    scale_y_continuous(label = percent, limits = c(0, 1)) +
    theme_bw()
}



makeTable <- function(MZ){
  MZ[,.(country,
        ## --- ATT
        `100x ATT per child, SOC`=brkt(1e2*soc.ATT,1e2*soc.ATT.lo,1e2*soc.ATT.hi),
        `100x ATT per child, TBS1`=brkt(1e2*tbs1.ATT,1e2*tbs1.ATT.lo,1e2*tbs1.ATT.hi),
        `100x ATT per child, TBS2`=brkt(1e2*tbs2.ATT,1e2*tbs2.ATT.lo,1e2*tbs2.ATT.hi),
        `100x ATT per child, WHO`=brkt(1e2*who.ATT,1e2*who.ATT.lo,1e2*who.ATT.hi),
        ## --- ATT from 1st assessments
        `100x ATT per child from 1st assessments, SOC`=
          brkt(1e2*soc.ATTworeassess,1e2*soc.ATTworeassess.lo,1e2*soc.ATTworeassess.hi),
        `100x ATT per child from 1st assessments, TBS1`=
          brkt(1e2*tbs1.ATTworeassess,1e2*tbs1.ATTworeassess.lo,1e2*tbs1.ATTworeassess.hi),
        `100x ATT per child from 1st assessments, TBS2`=
          brkt(1e2*tbs2.ATTworeassess,1e2*tbs2.ATTworeassess.lo,1e2*tbs2.ATTworeassess.hi),
        `100x ATT per child from 1st assessments, WHO`=
          brkt(1e2*who.ATTworeassess,1e2*who.ATTworeassess.lo,1e2*who.ATTworeassess.hi),
        ## --- ATT from reassessments
        `100x ATT per child from reassessments, SOC`=
          brkt(1e2*soc.ATTreassess,1e2*soc.ATTreassess.lo,1e2*soc.ATTreassess.hi),
        `100x ATT per child from reassessments, TBS1`=
          brkt(1e2*tbs1.ATTreassess,1e2*tbs1.ATTreassess.lo,1e2*tbs1.ATTreassess.hi),
        `100x ATT per child from reassessments, TBS2`=
          brkt(1e2*tbs2.ATTreassess,1e2*tbs2.ATTreassess.lo,1e2*tbs2.ATTreassess.hi),
        `100x ATT per child from reassessments, WHO`=
          brkt(1e2*who.ATTreassess,1e2*who.ATTreassess.lo,1e2*who.ATTreassess.hi),
        ## --- FPs
        `% FP, SOC`=brkt(1e2*soc.FP,1e2*soc.FP.lo,1e2*soc.FP.hi),
        `% FP, TBS1`=brkt(1e2*tbs1.FP,1e2*tbs1.FP.lo,1e2*tbs1.FP.hi),
        `% FP, TBS2`=brkt(1e2*tbs2.FP,1e2*tbs2.FP.lo,1e2*tbs2.FP.hi),
        `% FP, WHO`=brkt(1e2*who.FP,1e2*who.FP.lo,1e2*who.FP.hi),
        ## --- TPs
        `% TP, SOC`=brkt(1e2*soc.TP,1e2*soc.TP.lo,1e2*soc.TP.hi),
        `% TP, TBS1`=brkt(1e2*tbs1.TP,1e2*tbs1.TP.lo,1e2*tbs1.TP.hi),
        `% TP, TBS2`=brkt(1e2*tbs2.TP,1e2*tbs2.TP.lo,1e2*tbs2.TP.hi),
        `% TP, WHO`=brkt(1e2*who.TP,1e2*who.TP.lo,1e2*who.TP.hi),
        ## --- TNs
        `% TN, SOC`=brkt(1e2*soc.TN,1e2*soc.TN.lo,1e2*soc.TN.hi),
        `% TN, TBS1`=brkt(1e2*tbs1.TN,1e2*tbs1.TN.lo,1e2*tbs1.TN.hi),
        `% TN, TBS2`=brkt(1e2*tbs2.TN,1e2*tbs2.TN.lo,1e2*tbs2.TN.hi),
        `% TN, WHO`=brkt(1e2*who.TN,1e2*who.TN.lo,1e2*who.TN.hi),
        ## --- FNs
        `% FN, SOC`=brkt(1e2*soc.FN,1e2*soc.FN.lo,1e2*soc.FN.hi),
        `% FN, TBS1`=brkt(1e2*tbs1.FN,1e2*tbs1.FN.lo,1e2*tbs1.FN.hi),
        `% FN, TBS2`=brkt(1e2*tbs2.FN,1e2*tbs2.FN.lo,1e2*tbs2.FN.hi),
        `% FN, WHO`=brkt(1e2*who.FN,1e2*who.FN.lo,1e2*who.FN.hi),
        ## --- 1st assessments
        `% assessed, SOC`=brkt(1e2*soc.assess,1e2*soc.assess.lo,1e2*soc.assess.hi),
        `% assessed, TBS1`=brkt(1e2*tbs1.assess,1e2*tbs1.assess.lo,1e2*tbs1.assess.hi),
        `% assessed, TBS2`=brkt(1e2*tbs2.assess,1e2*tbs2.assess.lo,1e2*tbs2.assess.hi),
        `% assessed, WHO`=brkt(1e2*who.assess,1e2*who.assess.lo,1e2*who.assess.hi),
        ## --- reassessments
        `% reassessed, SOC`=brkt(1e2*soc.reassess,1e2*soc.reassess.lo,1e2*soc.reassess.hi),
        `% reassessed, TBS1`=brkt(1e2*tbs1.reassess,1e2*tbs1.reassess.lo,1e2*tbs1.reassess.hi),
        `% reassessed, TBS2`=brkt(1e2*tbs2.reassess,1e2*tbs2.reassess.lo,1e2*tbs2.reassess.hi),
        `% reassessed, WHO`=brkt(1e2*who.reassess,1e2*who.reassess.lo,1e2*who.reassess.hi),

        ## --- D ATT
        `100x incremental ATT, TBS1 v SOC`=brkt(1e2*DT_TBS1,1e2*DT_TBS1.lo,1e2*DT_TBS1.hi),
        `100x incremental ATT, TBS2 v SOC`=brkt(1e2*DT_TBS2,1e2*DT_TBS2.lo,1e2*DT_TBS2.hi),
        `100x incremental ATT, WHO v SOC`=brkt(1e2*DT_WHO,1e2*DT_WHO.lo,1e2*DT_WHO.hi),
        `100x incremental ATT, TBS2 v TBS1`=brkt(1e2*tDT_TBS2,1e2*tDT_TBS2.lo,1e2*tDT_TBS2.hi),
        `100x incremental ATT, WHO v TBS1`=brkt(1e2*t1DT_WHO,1e2*t1DT_WHO.lo,1e2*t1DT_WHO.hi),
        `100x incremental ATT, WHO v TBS2`=brkt(1e2*t2DT_WHO,1e2*t2DT_WHO.lo,1e2*t2DT_WHO.hi),

        ## --- deaths
        `100x deaths per child, SOC`=brkt(1e2*soc.cfr,1e2*soc.cfr.lo,1e2*soc.cfr.hi),
        `100x deaths per child, TBS1`=brkt(1e2*tbs1.cfr,1e2*tbs1.cfr.lo,1e2*tbs1.cfr.hi),
        `100x deaths per child, TBS2`=brkt(1e2*tbs2.cfr,1e2*tbs2.cfr.lo,1e2*tbs2.cfr.hi),
        `100x deaths per child, WHO`=brkt(1e2*who.cfr,1e2*who.cfr.lo,1e2*who.cfr.hi),
        ## --- D deaths
        `100x incremental deaths, TBS1 v SOC`=brkt(1e2*DM_TBS1,1e2*DM_TBS1.lo,1e2*DM_TBS1.hi),
        `100x incremental deaths, TBS2 v SOC`=brkt(1e2*DM_TBS2,1e2*DM_TBS2.lo,1e2*DM_TBS2.hi),
        `100x incremental deaths, WHO v SOC`=brkt(1e2*DM_WHO,1e2*DM_WHO.lo,1e2*DM_WHO.hi),
        `100x incremental deaths, TBS2 v TBS1`=brkt(1e2*tDM_TBS2,1e2*tDM_TBS2.lo,1e2*tDM_TBS2.hi),
        `100x incremental deaths, WHO v TBS1`=brkt(1e2*t1DM_WHO,1e2*t1DM_WHO.lo,1e2*t1DM_WHO.hi),
        `100x incremental deaths, WHO v TBS2`=brkt(1e2*t2DM_WHO,1e2*t2DM_WHO.lo,1e2*t2DM_WHO.hi),

        ## --- undiscounted dalys
        `100x undiscounted LYS, SOC`=brkt(-1e2*soc.DALYs0,-1e2*soc.DALYs0.hi,-1e2*soc.DALYs0.lo),
        `100x undiscounted LYS, TBS1`=brkt(-1e2*tbs1.DALYs0,-1e2*tbs1.DALYs0.hi,-1e2*tbs1.DALYs0.lo),
        `100x undiscounted LYS, TBS2`=brkt(-1e2*tbs2.DALYs0,-1e2*tbs2.DALYs0.hi,-1e2*tbs2.DALYs0.lo),
        `100x undiscounted LYS, WHO`=brkt(-1e2*who.DALYs0,-1e2*who.DALYs0.hi,-1e2*who.DALYs0.lo),
        ## --- D undiscounted dalys
        `100x undiscounted LYS, TBS1 v SOC`=brkt(-1e2*DD0_TBS1,-1e2*DD0_TBS1.hi,-1e2*DD0_TBS1.lo),
        `100x undiscounted LYS, TBS2 v SOC`=brkt(-1e2*DD0_TBS2,-1e2*DD0_TBS2.hi,-1e2*DD0_TBS2.lo),
        `100x undiscounted LYS, WHO v SOC`=brkt(-1e2*DD0_WHO,-1e2*DD0_WHO.hi,-1e2*DD0_WHO.lo),
        `100x undiscounted LYS, TBS2 v TBS1`=brkt(-1e2*tDD0_TBS2,-1e2*tDD0_TBS2.hi,-1e2*tDD0_TBS2.lo),
        `100x undiscounted LYS, WHO v TBS1`=brkt(-1e2*t1DD0_WHO,-1e2*t1DD0_WHO.hi,-1e2*t1DD0_WHO.lo),
        `100x undiscounted LYS, WHO v TBS2`=brkt(-1e2*t2DD0_WHO,-1e2*t2DD0_WHO.hi,-1e2*t2DD0_WHO.lo),

        ## --- dalys
        `100x DALYs averted, SOC`=brkt(-1e2*soc.DALYs,-1e2*soc.DALYs.hi,-1e2*soc.DALYs.lo),
        `100x DALYs averted, TBS1`=brkt(-1e2*tbs1.DALYs,-1e2*tbs1.DALYs.hi,-1e2*tbs1.DALYs.lo),
        `100x DALYs averted, TBS2`=brkt(-1e2*tbs2.DALYs,-1e2*tbs2.DALYs.hi,-1e2*tbs2.DALYs.lo),
        `100x DALYs averted, WHO`=brkt(-1e2*who.DALYs,-1e2*who.DALYs.hi,-1e2*who.DALYs.lo),
        ## --- D dalys
        `100x DALYs averted, TBS1 v SOC`=brkt(-1e2*DD_TBS1,-1e2*DD_TBS1.hi,-1e2*DD_TBS1.lo),
        `100x DALYs averted, TBS2 v SOC`=brkt(-1e2*DD_TBS2,-1e2*DD_TBS2.hi,-1e2*DD_TBS2.lo),
        `100x DALYs averted, WHO v SOC`=brkt(-1e2*DD_WHO,-1e2*DD_WHO.hi,-1e2*DD_WHO.lo),
        `100x DALYs averted, TBS2 v TBS1`=brkt(-1e2*tDD_TBS2,-1e2*tDD_TBS2.hi,-1e2*tDD_TBS2.lo),
        `100x DALYs averted, WHO v TBS1`=brkt(-1e2*t1DD_WHO,-1e2*t1DD_WHO.hi,-1e2*t1DD_WHO.lo),
        `100x DALYs averted, WHO v TBS2`=brkt(-1e2*t2DD_WHO,-1e2*t2DD_WHO.hi,-1e2*t2DD_WHO.lo),

        ## --- C
        `cost per child, SOC`=brkt(soc.cost,soc.cost.lo,soc.cost.hi),
        `cost per child, TBS1`=brkt(tbs1.cost,tbs1.cost.lo,tbs1.cost.hi),
        `cost per child, TBS2`=brkt(tbs2.cost,tbs2.cost.lo,tbs2.cost.hi),
        `cost per child, WHO`=brkt(who.cost,who.cost.lo,who.cost.hi),
        ## --- DC
        `incremental cost, TBS1 v SOC`=brkt(DC_TBS1,DC_TBS1.lo,DC_TBS1.hi),
        `incremental cost, TBS2 v SOC`=brkt(DC_TBS2,DC_TBS2.lo,DC_TBS2.hi),
        `incremental cost, WHO v SOC`=brkt(DC_WHO,DC_WHO.lo,DC_WHO.hi),
        `incremental cost, TBS2 v TBS1`=brkt(tDC_TBS2,tDC_TBS2.lo,tDC_TBS2.hi),
        `incremental cost, WHO v TBS1`=brkt(t1DC_WHO,t1DC_WHO.lo,t1DC_WHO.hi),
        `incremental cost, WHO v TBS2`=brkt(t2DC_WHO,t2DC_WHO.lo,t2DC_WHO.hi),

        ## --- undiscounted ICERS
        `ICER (no discounting), TBS1 v SOC`=round(ICER0_TBS1,2),
        `ICER (no discounting), TBS2 v SOC`=round(ICER0_TBS2,2),
        `ICER (no discounting), WHO v SOC`=round(ICER0_WHO,2),
        `ICER (no discounting), TBS2 v TBS1`=round(tICER0_TBS2,2),
        `ICER (no discounting), TBS1 v WHO`=round(wICER0_TBS1,2),
        `ICER (no discounting), TBS2 v WHO`=round(wICER0_TBS2,2),
        ## --- ICERS
        `ICER, TBS1 v SOC`=round(ICER_TBS1,2),
        `ICER, TBS2 v SOC`=round(ICER_TBS2,2),
        `ICER, WHO v SOC`=round(ICER_WHO,2),
        `ICER, TBS2 v TBS1`=round(tICER_TBS2,2),
        `ICER, TBS1 v WHO`=round(wICER_TBS1,2),
        `ICER, TBS2 v WHO`=round(wICER_TBS2,2)
        )]
}
