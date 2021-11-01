clear
set mem 300m
set more off
infile caseid probcrime cogskill ncogskill momcog momncog het inv1 inv2 using simulation.out
sum probcrime

gen ratio = inv1/inv2

egen x1 = std(cogskill)
egen x2 = std(ncogskill)
egen x3 = std(momcog)
egen x4 = std(momncog)
egen x5 = std(het)

gen x1sq = x1*x1
gen x2sq = x2*x2
gen x3sq = x3*x3
gen x4sq = x4*x4
gen x5sq = x5*x5
gen x1x2 = x1*x2
gen x1x3 = x1*x3
gen x1x4 = x1*x4
gen x1x5 = x1*x5
gen x2x3 = x2*x3
gen x2x4 = x2*x4
gen x2x5 = x2*x5
gen x3x4 = x3*x4
gen x3x5 = x3*x5
gen x4x5 = x4*x5

gen x1cb = x1*x1sq
gen x2cb = x2*x2sq
gen x3cb = x3*x3sq
gen x4cb = x4*x4sq
gen x5cb = x5*x5sq

reg ratio x1 x2 x3 x4 x5 x1sq x2sq x3sq x4sq x5sq x1x2 x1x3 x1x4 x1x5 x2x3 x2x4 x2x5 x3x4 x3x5 x4x5 x1cb x2cb x3cb x4cb x5cb 
reg inv1 x1 x2 x3 x4 x5 x1sq x2sq x3sq x4sq x5sq x1x2 x1x3 x1x4 x1x5 x2x3 x2x4 x2x5 x3x4 x3x5 x4x5 x1cb x2cb x3cb x4cb x5cb 
reg inv2 x1 x2 x3 x4 x5 x1sq x2sq x3sq x4sq x5sq x1x2 x1x3 x1x4 x1x5 x2x3 x2x4 x2x5 x3x4 x3x5 x4x5 x1cb x2cb x3cb x4cb x5cb 

save simulation, replace

keep ratio
outfile using ratiocrime.out, replace

