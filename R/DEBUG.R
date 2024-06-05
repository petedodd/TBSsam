names(CF)

## ----- parameter input names
## -- reassessment parms
## s.reassess.choice.sp
## s.reassess.choice.se
## s.reassess.sp
## s.reassess.se
## 
## -- clinical parms
## sens.clin
## spec.clin
## sens.clinCXR.soc
## spec.clinCXR.soc

## ----- parameter names in data get added 
## 

## initialize CF
CF <- rbindlist(list(pop,popt,pop0,popt0)) #all
CF <- CF[method=='copulas']
Nfold <- 10
CF <- CF[rep(1:nrow(CF),Nfold)]
Nreps <- nrow(CF)
CF[,id:=1:Nreps]
CF[, CXR.avail := 1] # code as available
## generate parameter samples
AP <- getAlgoParms(Nreps) #mainly/all for SOC NOTE all stochastic elts here
CF <- merge(CF,AP,by='id')
CF[,rrp:=0.03]
## ===== selectively change inputs here
## NOTE reassessment off here:
CF[,s.reassess.choice.sp:=1]
CF[,s.reassess.choice.se:=0]
## TODO add other changes here
## NOTE supressing countries
CF <- merge(CF,CDW[country=='Zambia'],by=c('id')) #NOTE CDW from main script
CF[,c.s.ATT:= rrp * c.s.rrATT + (1-rrp) * c.s.rsATT] #use a mean cost (same outcomes)
CF[,c('who.cost','soc.cost','tbs1.cost','tbs2.cost'):=0.0] #initialize costs

## === apply algorithms
## === WHO algorithm
ans <- WHO.algorithm(CF)
CF[,c('who.ATT','who.cost'):=ans]
## === SOC algorithm
ans <- SOC.algorithm(CF)
CF[,c('soc.ATT','soc.cost'):=ans]
## === TBS algorithms
## NOTE these act by side effect rather than return
TBS1s.algorithm(CF)
TBS2s.algorithm(CF)
## ======== outcomes
AddCFRs(CF)


## ===== look at outputs

## CFRs
CF[,.(who=mean(who.cfr),soc=mean(soc.cfr),
      tbs1=mean(tbs1.cfr),tbs2=mean(tbs2.cfr)),by=TB]
## default:
## TB       who       soc      tbs1      tbs2
## <char>     <num>     <num>     <num>     <num>
## 1: not TB 0.1385290 0.1385290 0.1385290 0.1385290
## 2:     TB 0.1852365 0.2358361 0.1900569 0.2002889
## no reassess: TB 0.2209214 0.3479336 0.2327258 0.2589225

## se/sp of algs as a whole
CF[,.(who=mean(who.ATT),soc=mean(soc.ATT),
      tbs1=mean(tbs1.ATT),tbs2=mean(tbs2.ATT)),by=TB]
## <char>   <num>   <num>   <num>   <num>
## 1: not TB 0.57173 0.14165 0.27195 0.22675
## 2:     TB 0.94344 0.82107 0.93183 0.90714
## no reassess: 2:     TB 0.8572 0.55062 0.8286 0.7653

## costs of algs as a whole
CF[,.(who=mean(who.cost),soc=mean(soc.cost),
      tbs1=mean(tbs1.cost),tbs2=mean(tbs2.cost))]
## 1: 168.7456 95.24549 145.6186 130.4126
## no reassess: 1: 162.4572 77.86253 137.032 119.7223
