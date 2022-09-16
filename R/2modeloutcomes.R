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

## ## add in column with WHO scores
## appendWHOscores(popWT)
## appendWHOscores(popWN)

pop <- POPS[["WHO_notTB"]]
appendWHOscores(pop)

## TODO
## code TBS scores
## comparison of score histograms w/ & w/o correlations accounted for
## cost data template
## cost & treatment outcomes
## CFRs & DALYs
## mixture of TB vs not TB
## HE outcomes
