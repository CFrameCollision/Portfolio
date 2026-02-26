# Certain calculations are reperformed, across scripts, so this is not the most efficient
# way of doing them. Each script is designed to work alone.
source("factorAnalysisStuff.R")
source("impAnalysis.R")
source("linearModelAnalysis.R")
source("lmControlling.r")
source("causalityAndLMTest.r")
source("pathModelAnalysis.R")
source("timeOrder.r")

stop("Done")
# Put any WIP files here

# DOA files. These are analyses I'm no longer using/needing.
source("CFAscripts/cfa.r")
