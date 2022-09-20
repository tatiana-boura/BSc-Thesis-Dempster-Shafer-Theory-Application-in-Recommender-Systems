#! /bin/sh

echo -e "\n\nThis is a script for running multiple times the application of DST in Prolog (CLP) implementation in the chosen dataset"

for i in {1..20}
do
	echo -e "\nRunning script with fraction = 0.002."
	python dst_application.py 0.002
done

for i in {1..20}
do
	echo -e "\nRunning script with fraction = 0.004."
	python dst_application.py 0.004
done

for i in {1..20}
do
	echo -e "\nRunning script with fraction = 0.006."
   	python dst_application.py 0.006
done

for i in {1..20}
do
	echo -e "\nRunning script with fraction = 0.008."
   	python dst_application.py 0.008
done

for i in {1..20}
do
	echo -e "\nRunning script with fraction = 0.01."
   	python dst_application.py 0.01
done

## For 0.02 it crushes with error 'global_trail_overflow' ##

echo -e "\n\nStart making the plots"

python plot_results_DR.py
python plot_results_Bel.py


echo -e "\n\nEnd of script"