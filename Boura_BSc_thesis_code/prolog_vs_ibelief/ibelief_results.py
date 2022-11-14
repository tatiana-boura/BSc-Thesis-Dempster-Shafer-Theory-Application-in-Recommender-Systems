# for Python
import numpy as np
import time
import sys
# for R - ibelief
import rpy2.robjects.packages as rpackages
import rpy2.robjects as robj


# script is run as: python <script_name> <theta_size> <num_mf> <num_fp_per_mf>
theta_size = int(sys.argv[1])       # Theta size 
num_mf = int(sys.argv[2])           # number of mass functions
num_fp_per_mf = int(sys.argv[3])    # number of focal points per mass functions

print("Theta size: ", theta_size)
print("num_mf: ", num_mf)
print("num_fp_per_mf: ", num_fp_per_mf)


# import R packages needed
base = rpackages.importr('base')
ibelief = rpackages.importr('ibelief')

# create masses from 
Mass=ibelief.RandomMass(nbFocalElement=num_fp_per_mf, ThetaSize=theta_size, nbMass=num_mf, Type=2)

#______combinations rule____________________________________________________________________________

seconds_before_ibelief = time.perf_counter_ns()

MassCombined=ibelief.DST(Mass,2)    # same as base.cbind(Mass)

ibelief_time_c = time.perf_counter_ns()-seconds_before_ibelief

print("\nDempster's rule | ibelief run in : %s ns"% ibelief_time_c)

# create unique name for .csv
file_name = "log_file_Dempster"+"_"+str(theta_size)+"_"+str(num_mf)+"_"+str(num_fp_per_mf)+".csv"
print(file_name)
path = "./plots/Dempster_rule/"+file_name
print(path)

# open that file
file_object = open(path, 'w+')
# write time in ns
file_object.write("ibelief time in ns,"+ str(ibelief_time_c)+"\n")
file_object.close()
  

MassCombined_l=np.asarray(MassCombined)
MassCombined_l=MassCombined_l.tolist()
# flatten a list: [[],[],[]]
MassCombined_l = [m for sublist in MassCombined_l for m in sublist]
#print(MassCombined_l[:100])
val = np.sum(MassCombined_l)
print(val)

# ________bel_________________________________________________________________________________ 

seconds_before_ibelief = time.perf_counter_ns()

Bel = ibelief.mtobel(MassCombined)

ibelief_time_b = time.perf_counter_ns()-seconds_before_ibelief

print("\nBel | ibelief run in : %s ns\n"% ibelief_time_b)

# create unique name for .csv
file_name = "log_file_bel"+"_"+str(theta_size)+"_"+str(num_mf)+"_"+str(num_fp_per_mf)+".csv"
print(file_name)
path = "./plots/bel/"+file_name
print("ibelief ",path)

# open that file
file_object = open(path, 'w+')
# write time in ns
file_object.write("ibelief time in ns bel,"+ str(ibelief_time_b)+"\n")
file_object.write("ibelief time in ns bel and comb,"+ str(ibelief_time_b+ibelief_time_c)+"\n")
file_object.close()
