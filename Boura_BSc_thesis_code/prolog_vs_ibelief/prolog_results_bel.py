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

A=Var()
B=Var()

A_arg = [int(a) for a in list(sys.argv[4:])]
print(A_arg)

Compound("=", A, PList(A_arg)).post_goal()  # Post goal
resume()  # Resume execution of ECLiPSe engine

print("A.value() ", A.value())

seconds_before_CSP = time.perf_counter_ns()
Compound("belief",A,B).post_goal() 

result, dummy = resume() 
if result != SUCCEED:
  print(result,dummy)

print(B.value())

CSP_time = time.perf_counter_ns()-seconds_before_CSP

print("\nBel | ECLiPSe run in : %s ns"% CSP_time)


# create unique name for .csv
file_name = "log_file_bel"+"_"+sys.argv[1]+"_"+sys.argv[2]+"_"+sys.argv[3]+".csv"
print(file_name)
path = "./plots/bel/"+file_name
print("prolog ",path)


# open that file
file_object = open(path, 'a')

file_object.write("Set,[")  
for i in A_arg:
  file_object.write(" "+str(i))
file_object.write(" ],")

file_object.write("Focal points per mass,"+str(sys.argv[3])+",")
file_object.write("Number of mass,"+str(sys.argv[2])+",")
# write theta size
file_object.write("Theta size,"+str(sys.argv[1])+",")
# write time in ns
file_object.write("CSP time in ns,"+ str(CSP_time)+"\n")
file_object.close()


cleanup()  # Shutdown ECLiPSe engine