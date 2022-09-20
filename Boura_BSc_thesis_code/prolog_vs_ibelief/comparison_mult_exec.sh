#! /bin/sh

# general notes to kepp in mind

# ibelief mtob computes only one time the bel for all the sets of the
# powerset 2^|Theta| but needs combination rule before

# csp bel() does not need combination rule before

# for large ammount of focal points: numberOfMass*numberOfFp >= 80 Prolog cannot perform

# -- KEEP THETA SIZE = 10 ---------------------------------------------------------------------------- 

# make some results for the plots

g++ example_gen_ibelief_comparison.cpp


thetaSize=10

for numberOfMass in {9..10..1}   # numberOfMass = 8, 9, 10
do
	for numberOfFp in {10..14..2}   # numberOfFp = 10, 12, 14
	do
		# run ibelief
		python ibelief_results.py $thetaSize $numberOfMass $numberOfFp
		# Dempsters rule and bel cannot run
		python prolog_not_ran.py $thetaSize $numberOfMass $numberOfFp

	done
done

# -- KEEP THETA SIZE = 10 ---------------------------------------------------------------------------- 

for numberOfMass in {2..6..1}   # numberOfMass = 2, 4, 6
do
	for numberOfFp in {4..10..2}   # numberOfFp = 5, 10
	do
		# run ibelief
		python ibelief_results.py $thetaSize $numberOfMass $numberOfFp
		# create data for ECLiPSe bel and then run
		./a.out $thetaSize $numberOfMass $numberOfFp
		# run Dempsters rule
		python prolog_results_Dempster.py $numberOfFp $numberOfMass $thetaSize
		# run belief
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 2 
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 2 3 
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 7 9 
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 4 5 9 10
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 2 3 6 9 10
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 4 5 6 7 8
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 4 5 6 7 8 9
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 3 4 5 6 7 8 9 10

	done
done

# -- KEEP THETA SIZE = 15 -----------------------------------------------------------------------------------------------------------------


thetaSize=15

for numberOfMass in {8..10..1}   # numberOfMass = 8, 9, 10
do
	for numberOfFp in {10..14..2}   # numberOfFp = 10, 12, 14
	do
		# run ibelief
		python ibelief_results.py $thetaSize $numberOfMass $numberOfFp
		# Dempsters rule and bel cannot run
		python prolog_not_ran.py $thetaSize $numberOfMass $numberOfFp

	done
done


for numberOfMass in {2..6..1}   # numberOfMass = 2, 4, 6
do
	for numberOfFp in {4..10..2}   # numberOfFp = 5, 10
	do
		# run ibelief
		python ibelief_results.py $thetaSize $numberOfMass $numberOfFp

		# create data for ECLiPSe bel and then run
		./a.out $thetaSize $numberOfMass $numberOfFp
		# run Dempsters rule
		python prolog_results_Dempster.py $numberOfFp $numberOfMass $thetaSize
		# run belief
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 2 3
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 7 9 11 
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 4 5 9 10 13 15
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 2 3 6 9 10 11 15
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 4 5 6 7 8 9 13 14 
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 4 5 6 7 8 9 11 12 13 14 15
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15

	done
done

# -- KEEP THETA SIZE = 20 ---------------------------------------------------------------------------------------------------------------

thetaSize=20

for numberOfMass in {8..10..1}   # numberOfMass = 8, 9, 10
do
	for numberOfFp in {10..14..2}   # numberOfFp = 10, 12, 14
	do
		# run ibelief
		python ibelief_results.py $thetaSize $numberOfMass $numberOfFp
		# Dempsters rule and bel cannot run
		python prolog_not_ran.py $thetaSize $numberOfMass $numberOfFp

	done
done

for numberOfMass in {2..6..1}   # numberOfMass = 2, 4, 6
do
	for numberOfFp in {4..10..2}   # numberOfFp = 5, 10
	do
		# run ibelief
		python ibelief_results.py $thetaSize $numberOfMass $numberOfFp
		# create data for ECLiPSe bel and then run
		./a.out $thetaSize $numberOfMass $numberOfFp
		# run Dempsters rule
		python prolog_results_Dempster.py $numberOfFp $numberOfMass $thetaSize
		# run belief
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 11 
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 2 3 5
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 7 9 12 14
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 4 5 9 10 11 13 15
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 2 3 6 9 10 12 16 17 18
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 4 5 6 7 8 9 10 12 16 17 19
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 4 5 6 7 8 9 10 11 12 13 14 15 16 18
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20

	done
done

# -- KEEP THETA SIZE = 25 ---------------------------------------------------------------------------------------------------------------

# for THETA SIZE >= 25 ibelief cannot allocate the memory needed , so both Dempsters rule and bel
# cannot work!
# again for numberOfMass>6 and numberOfFp>10 prolog does not work as well


thetaSize=25

for numberOfMass in {2..6..1}   # numberOfMass = 4, 6
do
	for numberOfFp in {4..10..2}   # numberOfFp = 5, 10
	do
		# create data for ECLiPSe bel and then run
		./a.out $thetaSize $numberOfMass $numberOfFp
		# run Dempsters rule
		python prolog_results_Dempster.py $numberOfFp $numberOfMass $thetaSize
		# run belief
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 11 
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 2 3 5
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 7 9 12 14 16 17 18
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 4 5 9 10 11 13 15 16 17 18
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 2 3 6 9 10 12 16 17 18 19 20 21
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 4 5 6 7 8 9 10 12 16 17 19 22 23 24
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 23 24
		python prolog_results_bel.py $thetaSize $numberOfMass $numberOfFp 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

	done
done

for numberOfMass in {8..10..1}   # numberOfMass = 8, 9, 10
do
	for numberOfFp in {10..14..2}   # numberOfFp = 10, 12, 14
	do
		# i belief cannot run
		# Dempsters rule and bel cannot run
		python prolog_not_ran.py $thetaSize $numberOfMass $numberOfFp

	done
done


# now make the plots and tables
python plot_ibelief_vs_prolog_Dempster.py
python plot_ibelief_vs_prolog_bel.py