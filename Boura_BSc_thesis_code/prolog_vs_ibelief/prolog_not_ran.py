import sys


CSP_time = 0

# focal_points_per_mass_function^num_of_mass_functions
total_combinations = (int(sys.argv[3]))**(int(sys.argv[2]))

# create unique name for .csv
file_name = "log_file_Dempster"+"_"+sys.argv[1]+"_"+sys.argv[2]+"_"+sys.argv[3]+".csv"
path = "./plots/Dempster_rule/"+file_name

# open that file
file_object = open(path, 'a')
file_object.write("Total number of combinations,"+str(total_combinations)+",") 
file_object.write("Focal points per mass,"+str(sys.argv[3])+",")
file_object.write("Number of mass,"+str(sys.argv[2])+",")
# write theta size
file_object.write("Theta size,"+str(sys.argv[1])+",")
# write time in ns
file_object.write("CSP time in ns,"+ str(CSP_time)+"\n")
file_object.close()


file_name = "log_file_bel"+"_"+sys.argv[1]+"_"+sys.argv[2]+"_"+sys.argv[3]+".csv"
print(file_name)
path = "./plots/bel/"+file_name
print("prolog ",path)

# open that file
file_object = open(path, 'a')

file_object.write("Set,[0],")

file_object.write("Focal points per mass,"+str(sys.argv[3])+",")
file_object.write("Number of mass,"+str(sys.argv[2])+",")
# write theta size
file_object.write("Theta size,"+str(sys.argv[1])+",")
# write time in ns
file_object.write("CSP time in ns,"+ str(CSP_time)+"\n")
file_object.close()
