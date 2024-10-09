#!/bin/bash

# testing basecase run
R --slave --vanilla --args <R/2modeloutcomes.R none


# Define an array of parameters for the R script
params=(
  "s.soc.testingcov"
  "s.soc.CXRonly"
  "s.soc.Xpertonly"
  "s.TBprev"
  "s.reassess.choice.sp"
  "s.reassess.choice.se"
  "s.soc.scrcov"
  "s.soc.screen.se"
  "s.soc.screen.sp"
  "s.CFR.sam.noTB"
  "s.CFR.sam.TBATT"
  "s.CFR.sam.TBnoATT"
  "s.soc.clin.se"
  "s.soc.clin.sp"
  "s.soc.clinCXR.se"
  "s.soc.clinCXR.sp"
  "s.soc.clinGA.se"
  "s.soc.clinGA.sp"
  "s.soc.clinCXRGA.se"
  "s.soc.clinCXRGA.sp"
  "s.soc.reassessafterclin.sebar"
  "s.soc.reassessafterclin.sp"
  "s.soc.reassessafterclinCXR.sebar"
  "s.soc.reassessafterclinCXR.sp"
  "s.soc.reassessafterclinGA.sebar"
  "s.soc.reassessafterclinGA.sp"
  "c.s.soc.scre"
  "c.s.soc.exam"
  "c.s.soc.CXR"
  "c.s.soc.CXRxga"
  "c.s.soc.xga"
  "c.s.soc.reassessCXRxga"
  "c.s.tbs1step.diag.clin"
  "c.s.tbs1step.diag.test"
  "c.s.tbs2step.scre"
  "c.s.tbs2step.diag"
  "c.s.who.scre"
  "c.s.who.hiv.diag"
  "c.s.who.diag"
  "c.s.rsATT"
  "c.s.rrATT"
  "c.s.reassessCXRxgastall"
)

# Loop through each parameter and execute the R script for both UQ and LQ
for param in "${params[@]}"; do
  R --slave --vanilla --args "$param" UQ < R/2modeloutcomes.R &
  R --slave --vanilla --args "$param" LQ < R/2modeloutcomes.R &
  wait
done




# testing a cost parameter
#R --slave --vanilla --args <R/2modeloutcomes.R c.s.who.diag UQ & R --slave --vanilla --args <R/2modeloutcomes.R c.s.who.diag LQ
#R --slave --vanilla --args <R/2modeloutcomes.R c.s.tbs1step.diag.test UQ & R --slave --vanilla --args <R/2modeloutcomes.R c.s.tbs1step.diag.test LQ
#R --slave --vanilla --args <R/2modeloutcomes.R c.s.rrATT UQ & R --slave --vanilla --args <R/2modeloutcomes.R c.s.rrATT LQ

# testing a non-cost parameter


# NOTE depending on your set up, you may be able to parallelize by using & like:
# R --slave --vanilla --args <2modeloutcomes.R s.TBprev UQ & R --slave --vanilla --args <2modeloutcomes.R s.TBprev LQ
# to run these two on separate cores at once (more with using more &s)
# otherwise these commands will need separating like:
# R --slave --vanilla --args <2modeloutcomes.R s.TBprev UQ
# R --slave --vanilla --args <2modeloutcomes.R s.TBprev LQ

# NOTE will probably need to write a little script to gather up results from outputs
# NOTE may wish to write a script to upload results to a google sheet linked to paper
