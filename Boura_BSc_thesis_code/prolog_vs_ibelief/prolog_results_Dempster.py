# for Python
import math
import time
import sys

# for Eclipse
from pyclp import *

init()  # init ECLiPSe engine

Compound("use_module",Atom("../dst_master.pl")).post_goal()

result, dummy = resume() 
if result != SUCCEED:
  print(result,dummy)

V=Var()
CombinedMass=Var()
TarsAndVals=Var()

seconds_before_CSP = time.perf_counter_ns()
Compound("bpa",CombinedMass,V,TarsAndVals).post_goal() 

result, dummy = resume() 
if result != SUCCEED:
  print(result,dummy)

CSP_time = time.perf_counter_ns()-seconds_before_CSP

print("\nDempster's rule | ECLiPSe run in : %s ns"% CSP_time)

# focal_points_per_mass_function^num_of_mass_functions
total_combinations = (int(sys.argv[1]))**(int(sys.argv[2]))

print("Combinations length:",total_combinations)

print("Sum is:",math.fsum(V.value()))


# create unique name for .csv
file_name = "log_file_Dempster"+"_"+sys.argv[3]+"_"+sys.argv[2]+"_"+sys.argv[1]+".csv"
print(file_name)
path = "./plots/Dempster_rule/"+file_name
print(path)

# open that file
file_object = open(path, 'a')
file_object.write("Total number of combinations,"+str(total_combinations)+",") 
file_object.write("Focal points per mass,"+str(sys.argv[1])+",")
file_object.write("Number of mass,"+str(sys.argv[2])+",")
# write theta size
file_object.write("Theta size,"+str(sys.argv[3])+",")
# write time in ns
file_object.write("CSP time in ns,"+ str(CSP_time)+"\n")
file_object.close()

cleanup()  # Shutdown ECLiPSe engine