## modelling the scores on the synthetic populations
library(here)
library(glue)
library(data.table)
library(ggplot2)
gh <- function(x) glue(here(x))

## load dependencies
source(gh('R/utils/scores.R')) #scores are coded in here

## load synthetic populations
load(file=gh('data/POPS.Rdata'))
load(file=gh('data/POPS0.Rdata'))

## add in scores:

## --- WHO
pop <- POPS[["WHO_notTB"]]
pop <- appendWHOscores(pop)
names(pop)
## TODO need contact here too

pop0 <- POPS0[["WHO_notTB"]]
pop0 <- appendWHOscores(pop0)
popt <- POPS[["WHO_TB"]]
popt <- appendWHOscores(popt)
popt0 <- POPS0[["WHO_TB"]]
popt0 <- appendWHOscores(popt0)

## --- TB Speed
pop <- POPS[["TBS_notTB"]]
pop <- appendTBSscores(pop)
names(pop)
## BUG   object 'Contact_with_adult_TB_case' not found


## compare
pop0[,method:='no correlation']
pop[,method:='copulas']
popt0[,method:='no correlation']
popt[,method:='copulas']
pop0[,TB:='not TB']
pop[,TB:='not TB']
popt0[,TB:='TB']
popt[,TB:='TB']
CF <- rbindlist(list(pop,popt,pop0,popt0))

## ggplot(CF,aes(score_noX,y=..density..,col=method))+
##   geom_density()+
##   facet_wrap(~TB)

CF[,mean(score_X),by=.(method,TB)] #very similar
CF[,sd(score_X),by=.(method,TB)] #very similar


## TODO
## // code TBS scores
## // comparison of score histograms w/ & w/o correlations accounted for
## cost data template
## cost & treatment outcomes
## CFRs & DALYs
## mixture of TB vs not TB
## HE outcomes
