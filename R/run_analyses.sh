#!/bin/bash

# testing basecase run
R --slave --vanilla --args <2modeloutcomes.R none

# testing a cost parameter
R --slave --vanilla --args <2modeloutcomes.R c.s.who.diag UQ & R --slave --vanilla --args <2modeloutcomes.R c.s.who.diag LQ

# testing a non-cost parameter
R --slave --vanilla --args <2modeloutcomes.R s.TBprev UQ & R --slave --vanilla --args <2modeloutcomes.R s.TBprev LQ


# NOTE depending on your set up, you may be able to parallelize by using & like:
# R --slave --vanilla --args <2modeloutcomes.R s.TBprev UQ & R --slave --vanilla --args <2modeloutcomes.R s.TBprev LQ
# to run these two on separate cores at once (more with using more &s)
# otherwise these commands will need separating like:
# R --slave --vanilla --args <2modeloutcomes.R s.TBprev UQ
# R --slave --vanilla --args <2modeloutcomes.R s.TBprev LQ

# NOTE will probably need to write a little script to gather up results from outputs
# NOTE may wish to write a script to upload results to a google sheet linked to paper
