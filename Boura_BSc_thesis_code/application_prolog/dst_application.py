# -*- coding: utf-8 -*-

# Import libraries needed

# for dataset
from ctypes import *
import pandas as pd
import numpy as np
import itertools
import random
import time
import math
import csv
import sys

# for Eclipse
from pyclp import *

# Open dataset (.csv) and load it to a Pandas Dataframe.

file = open('vodclickstream_uk_movies_03.csv')
data= pd.read_csv(file)


# From the dataset's page in kaggle we've noticed that in 'genres' and 'release date', 
# if there is no value in those columns regarding a row, then that cell includes the value 
# 'NOT AVAILABLE'. So, those rows are dropped, because as quoted in the article's "Discovering 
# user preferences using Dempster–Shafer theory, Luigi Troiano, Luis J. Rodríguez-Muñiz, Irene Díaz",
# 7th page: "In this model it is assumed that only items sharing all the characteristics are provided"

data = data.drop(data[data.genres == 'NOT AVAILABLE'].index, axis=0)
data = data.drop(data[data.release_date == 'NOT AVAILABLE'].index, axis=0)


# Datasets percentage of use from command line
data = data.sample(frac = float(sys.argv[1]))

# Τhe features-columns 'duration' and 'row ID' do not give us any indication  
# about the user's preferences, so we will not be considering them. 

data = data.drop(['duration'], axis=1)  # dropping duration cause many zeros
data = data.drop(data.columns[0], axis=1)  # dropping first column cause strange


# datetime is date and time of the click and release_date is release date.

# The feature release_date seems quite specific, so at first we thought about using release
# year as a preference. The problem with that is that it is quite strange for one to watch
# movies released only in one specific year, but one can prefer watching movies that were 
# released a certain decade. 
# For that reason we will be using as feature the release decade (release_decade).


release_decade = []

for row in data['release_date']:
  date_split_list = row.split("-")          # split release date
                                            # and use only release year
  list_of_char = list(date_split_list[0])   # change last digit (ex. 1992 -> 1990)
  list_of_char[3] = '0'
  date_split_list[0] = ''.join(list_of_char)

  release_decade.append(date_split_list[0])  # store release decade

data['release_decade'] = release_decade
data = data.drop(['release_date'], axis=1)  # dropping release date, because now we have release decade!



# The feature datetime seems quite specific as well, so we thought of using click hour (click hour) as a preference. 
# But, again, because one is more likely to watch a movie within a range of hours, we will be making 5 categories for 
# the click hour: Morning (06-11), Midday (12-16), Afternoon(17-20), Evening(21-00), Night(01-05).

click_hour = []

for row in data['datetime']:
  date_split_list = row.split(" ")                     # split date and time
  date_split_list = date_split_list[1].split(":")      # split time
  
  # use only hour
  h = date_split_list[0]
  if (h=='06') or (h=='07') or (h=='08') or (h=='09') or (h=='10')  or (h=='11') :                                                  
    click_hour.append('Morning')
  elif (h=='12') or (h=='13') or (h=='14') or (h=='15') or (h=='16'):
    click_hour.append('Midday') 
  elif (h=='17') or (h=='18') or (h=='19') or (h=='20'):
    click_hour.append('Afternoon')
  elif (h=='21') or (h=='22') or (h=='23') or (h=='00'):
    click_hour.append('Evening') 
  else:
    click_hour.append('Night')         

data['click hour'] = click_hour
data = data.drop(['datetime'], axis=1)  # dropping date_time, because now we have click hour!!


# Find Φ_G regarding c_G s.t. c_G is genre-characteristic
genre_values = []

for row in data['genres']:
  split_list = row.split(", ")
  genre_values += split_list

genre_values = list(set(genre_values))

# We are going to remove films that have at least one of the following genres: Documentary, History, 
# War, Talk-Show, Biography, Musical, Music, Sport

movies_to_be_dropped = []

for index, row in data.iterrows():
  split_list = row['genres'].split(", ")
  if ('Documentary' in split_list) or ('History' in split_list) or ('War' in split_list) or ('Talk-Show' in split_list) or ('Biography' in split_list) or ('Musical' in split_list) or ('Music' in split_list) or ('Sport' in split_list) :
    movies_to_be_dropped.append(row['movie_id'])

for movie in movies_to_be_dropped:
  data = data.drop(data[data.movie_id == movie].index) 


# We observed that there are some duplicate rows in the dataset,  
# so we will be dropping them, as they are no use to our modeling.

data = data.drop_duplicates()
data = data.reset_index()      # make sure indexes pair with number of rows
data = data.drop(['index'], axis=1)


# Now make a new DataFrame regarding the user preferences. This will help us compute the bpa as presented in 
# "Discovering user preferences using Dempster–Shafer theory, Luigi Troiano, Luis J. Rodríguez-Muñiz, Irene Díaz".

# create new dataframe
d = {'user_id': [] }
data_per_user_ = pd.DataFrame(data=d)                            

# first column will be the users (one user per row and no duplicates)
data_per_user_['user_id'] = (data['user_id'].unique()).tolist()   
empty_sets = []

# create different lists of empty sets
for i in range(5):
  empty_sets.append([set() for x in range(len(data_per_user_))])  

# make those sets part of the dataframe column-wise
data_per_user_['title'] = empty_sets[0]                           
data_per_user_['genres'] = empty_sets[1]
data_per_user_['movie_id'] = empty_sets[2]
data_per_user_['release_decade'] = empty_sets[3]
data_per_user_['click hour'] = empty_sets[4]

# for each user
for index, row in data_per_user_.iterrows():

  # make a temporary dataframe with the preferences of this particular user - only
  df_temp = data.loc[data['user_id'] == row['user_id']]           
  
  # now for every preference in the temporary dataframe - for every movie and its other characteristics -
  # collect them together to make only one row of the user preferences in the new user-oriented dataset
  # note that sets are used, to avoid duplicates

  for movie in df_temp['movie_id'].unique().tolist():
    row['movie_id'].add(movie)
   
  for title in df_temp['title'].unique().tolist():
    row['title'].add(title)

  for genres in df_temp['genres'].unique().tolist():
    split_genres = genres.split(", ")
    for genre in split_genres:
      row['genres'].add(genre)
  
  for release_decade in (df_temp['release_decade'].unique()).tolist():
    row['release_decade'].add(release_decade)
   
  for hour in df_temp['click hour'].unique().tolist():
    row['click hour'].add(hour)
  

# Now, what we want to do is drop the rows that have only one preferable movie, because   
# we would like to see how DS theory behaves with multiple user preferences.

# count the length of the 'movie_id' cell for each row (user) and store in new column
data_per_user_['length'] = data_per_user_.movie_id.str.len()  
# drop rows that have cell value equal to 1                          
data_per_user_ = data_per_user_.drop(data_per_user_[data_per_user_.length == 1].index) 

data_per_user_ = data_per_user_.reset_index()  # make sure indexes pair with number of rows
data_per_user_ = data_per_user_.drop(['index'], axis=1)


# Drop length column, as we no longer need it.
data_per_user_ = data_per_user_.drop(['length'], axis=1)


# Time to start computing the bpa's in regards to a feature.
# That will be done with the following type: m_i(K) = #U_K/#U .

# There will not be any computation of m_title, m_movie_id as these are just
# two different ways of refering to movie

###### Compute #U ###################################################################################################

number_of_users = len(data_per_user_['user_id'])

# function to round float to the nearest allowable value
def roundf(x,bitsToRound):

    i = cast(pointer(c_float(x)), POINTER(c_int32)).contents.value
    bits = bin(i)
    bits = bits[:-bitsToRound] + "0"*bitsToRound
    i = int(bits,2)
    y = cast(pointer(c_int32(i)), POINTER(c_float)).contents.value

    return y

#___ For characteristic: Release decade_______________________________________________________________________________

# Count unique number of 'release_decade'
release_decades_count = set()

for index, row in data_per_user_.iterrows():      # for every user
  for x in row['release_decade']:                   # find ther preferences regarding the release_decade
    release_decades_count.add(x)                    # add it to set

number_of_years = len(release_decades_count)


# Compute #U_k without computing the whole powerset as discused in the article

# create new dataframe to store bpa-related values
d = {'release_decades': [], 'count' : [], 'bpa' : [], 'movies_proj' : [], 'sets_indexed' : [] }
df_bpa_release_decades_ = pd.DataFrame(data=d)    

# for each user
for index, row in data_per_user_.iterrows():
  # only focal points - from users only!
  if (df_bpa_release_decades_['release_decades'] == set(row['release_decade'])).any() == False:
    df_bpa_release_decades_.loc[df_bpa_release_decades_.shape[0]] = [row['release_decade'], 1, 0, set(), set()]
  else:
    df_bpa_release_decades_.loc[df_bpa_release_decades_['release_decades'] == set(row['release_decade']), 'count'] +=1

# compute bpa: count/number_of_users
for index, row in df_bpa_release_decades_.iterrows():
  # round bt by 10 digits binary
  df_bpa_release_decades_.loc[index, 'bpa'] = roundf((row['count'])/number_of_users,10)

# sum bpa's - must be ~1 (may not be exactly due to python arithmetic)
sum_rd=np.sum(df_bpa_release_decades_['bpa'].tolist())


#___ For characteristic: Click hour______________________________________________________________________________________

# Count unique number of 'click hour'.
click_hours_count = set()

for index, row in data_per_user_.iterrows():      # for every user
  for x in row['click hour']:                     # find ther preferences regarding the click hour
    click_hours_count.add(x)                    # add it to set

number_of_hours = len(click_hours_count)


# Compute #U_k without computing the whole powerset as discused in the article

# create new dataframe to store bpa-related values
d = {'click_hours': [], 'count' : [], 'bpa' : [], 'movies_proj' : [], 'sets_indexed' : [] }
df_bpa_click_hours_ = pd.DataFrame(data=d)    

# for each user
for index, row in data_per_user_.iterrows():
  # only focal points - from users only!
  if (df_bpa_click_hours_['click_hours'] == set(row['click hour'])).any() == False:
    df_bpa_click_hours_.loc[df_bpa_click_hours_.shape[0]] = [row['click hour'], 1, 0, set(), set()]
  else:
    df_bpa_click_hours_.loc[df_bpa_click_hours_['click_hours'] == set(row['click hour']), 'count'] +=1

# compute bpa: count/number_of_users
for index, row in df_bpa_click_hours_.iterrows():
  # round bt by 10 digits binary
  df_bpa_click_hours_.loc[index, 'bpa'] = roundf((row['count'])/number_of_users,10)

# sum bpa's - must be ~1 (may not be exactly due to python arithmetic)
sum_ch = np.sum(df_bpa_release_decades_['bpa'].tolist())


#___ For characteristic: Genre______________________________________________________________________________________

# Count unique number of 'genres'.
genres_count = set()

for index, row in data_per_user_.iterrows():      # for every user
  for x in row['genres']:                         # find ther preferences regarding the genres
    genres_count.add(x)                           # add it to set

number_of_genres = len(genres_count)

# Compute #U_k without computing the whole powerset as discused in the article

# create new dataframe to store bpa-related values
d = {'genres': [], 'count' : [], 'bpa' : [], 'movies_proj' : [], 'sets_indexed' : [] }
df_bpa_genres_ = pd.DataFrame(data=d)    

# for each user
for index, row in data_per_user_.iterrows():
  # only focal points - from users only!
  if (df_bpa_genres_['genres'] == set(row['genres'])).any() == False:
    df_bpa_genres_.loc[df_bpa_genres_.shape[0]] = [row['genres'], 1, 0,set(),set()]
  else:
    df_bpa_genres_.loc[df_bpa_genres_['genres'] == set(row['genres']), 'count'] +=1

# compute bpa: count/number_of_users
for index, row in df_bpa_genres_.iterrows():
  # round bt by 10 digits binary
  df_bpa_genres_.loc[index, 'bpa'] = roundf((row['count'])/number_of_users,10)

# sum bpa's - must be ~1 (may not be exactly due to python arithmetic)
sum_g = np.sum(df_bpa_release_decades_['bpa'].tolist())


######  Project bpa's to movies, in order to combine them later on c

# At first, get a hold of the movies that we consider after deleting rows etc.
'''
print("print(sum_rd) ",sum_rd)
print("print(sum_ch) ",sum_ch)
print("print(sum_g) ",sum_g)
'''
movies_considered = set()
# A |= B  -> A U B
for index, row in data_per_user_.iterrows():
  movies_considered |= row['movie_id']

movies_considered = list(movies_considered)
movies_considered = sorted(movies_considered)
# store movies as int's where every unique id is an unique int 
movies_considered_int = [i for i in range(len(movies_considered))] 

#___ For characteristic: Release decade_______________________________________________________________________________

# loop through all the movies we are considering
for i in range(len(movies_considered)):
  # make a temp df of the entries that share the particular movie_id
  df_temp = data.loc[movies_considered[i] == data['movie_id']]
  # for every release decade found in the temporary df (rd)
  for rd in df_temp['release_decade'].unique().tolist():
     # for every focal point of the characteristic
    for _, row_rd in df_bpa_release_decades_.iterrows():
      # if the rd intersects with the focal point...
      if rd in row_rd['release_decades']:
        # ... then it belongs to the projected itemset
        row_rd['movies_proj'].add(movies_considered[i])

#___ For characteristic: Click hour__________________________________________________________________________________

# loop through all the movies we are considering
for i in range(len(movies_considered)):
  # make a temp df of the entries that share the particular movie_id
  df_temp = data.loc[movies_considered[i] == data['movie_id']]
  # for every click hour found in the temporary df (rd)
  for rd in df_temp['click hour'].unique().tolist():
    # for every focal point of the characteristic
    for _, row_rd in df_bpa_click_hours_.iterrows():
      # if the rd intersects with the focal point...
      if rd in row_rd['click_hours']:
          # ... then it belongs to the projected itemset
          row_rd['movies_proj'].add(movies_considered[i])

#___ For characteristic: Genre______________________________________________________________________________________

# loop through all the movies we are considering
for i in range(len(movies_considered)):
  # make a temp df of the entries that share the particular movie_id
  df_temp = data.loc[movies_considered[i] == data['movie_id']]
  # for every genre(s) found in the temporary df (rd)
  for rd_ in df_temp['genres'].unique().tolist():
    # genres are strings like this : 'Drama, Romance'
    split_list = rd_.split(", ")
    for rd in split_list:
      # for every focal point of the characteristic
      for _, row_rd in df_bpa_genres_.iterrows():
        # if the rd intersects with the focal point...
        if rd in row_rd['genres']:
            # ... then it belongs to the projected itemset
            row_rd['movies_proj'].add(movies_considered[i])


######  Prepare the bpa in order to use Dempster's Rule in ECLiPSe Prolog CSP ####################################  

# Make a dictionary where every movie_id [key] corresponds to a (unique) int value

movies_dict = {}

# give as value of each key an accending number
count = 0
for m in movies_considered:
    movies_dict[m] = count
    count = count + 1

# __Prepare bpa for the characteristic release_decade__________

# convert each movie_proj set to a set of ints
# that is going to help for indexing later
for index_rd, row_rd in df_bpa_release_decades_.iterrows():
  for m in row_rd['movies_proj']:
    # here we will be using the movies_dict dictionary
    row_rd['sets_indexed'].add(movies_dict[m])

# __Prepare bpa for the characteristic click_hour______________

# convert each movie_proj set to a set of ints
# that is going to help for indexing later
for index_rd, row_rd in df_bpa_click_hours_.iterrows():
  for m in row_rd['movies_proj']:
    # here we will be using the movies_dict dictionary
    row_rd['sets_indexed'].add(movies_dict[m])

# __Prepare bpa for the characteristic genres__________________

# convert each movie_proj set to a set of ints
# that is going to help for indexing later
for index_rd, row_rd in df_bpa_genres_.iterrows():
  for m in row_rd['movies_proj']:
    # here we will be using the movies_dict dictionary
    row_rd['sets_indexed'].add(movies_dict[m])


######## Before computing we should take care of the following problem :

'''
As the characteristics 'click hour' and 'genres' are multivalued (a move can have more than one
click hour and genre) after the projection the following phenomenon may occur:
                          m(1,[1,2,3],0.2)
                          m(1,[1,2,3],0.45)
If that happens, we must combine these two focal points, as they are actually the same one:
                          m(1,[1,2,3],0.65)
'''

#___ For characteristic: Click hour__________________________________________________________________________________

# drop columns that are not needed
df_bpa_click_hours_ = df_bpa_click_hours_.drop(['click_hours', 'count', 'movies_proj'], axis=1)
# get the sets and store them into a list
list_of_sets_l = [sorted(list(s)) for s in df_bpa_click_hours_["sets_indexed"].tolist()]
# sort the list
list_of_sets_l.sort()
# get only one istance of each set
list_of_sets_l = list(list_of_sets_l for list_of_sets_l,_ in itertools.groupby(list_of_sets_l))
# create new dict and then new df
dictionary = {'sets_indexed': list_of_sets_l, 'bpa' : [0.0 for i in range(len(list_of_sets_l))] }
df_bpa_click_hours_n = pd.DataFrame(dictionary)

# from old df sum bpa of same sets
for index_rdn, row_rdn in df_bpa_click_hours_n.iterrows():
  for index_rd, row_rd in df_bpa_click_hours_.iterrows():
    if sorted(list(row_rd['sets_indexed'])) == sorted(list(row_rdn['sets_indexed'])):
      df_bpa_click_hours_n.at[index_rdn,'bpa']+= row_rd['bpa']


#___ For characteristic: Genre______________________________________________________________________________________

# drop columns that are not needed
df_bpa_genres_ = df_bpa_genres_.drop(['genres', 'count', 'movies_proj'], axis=1)
# get the sets and store them into a list
list_of_sets_l = [sorted(list(s)) for s in df_bpa_genres_["sets_indexed"].tolist()]
# sort the list
list_of_sets_l.sort()
# get only one istance of each set
list_of_sets_l = list(list_of_sets_l for list_of_sets_l,_ in itertools.groupby(list_of_sets_l))
# create new dict and then new df
dictionary = {'sets_indexed': list_of_sets_l, 'bpa' : [0.0 for i in range(len(list_of_sets_l))] }
df_bpa_genres_n = pd.DataFrame(dictionary)

# from old df sum bpa of same sets
for index_gn, row_gn in df_bpa_genres_n.iterrows():
  for index_g, row_g in df_bpa_genres_.iterrows():
    if sorted(list(row_g['sets_indexed'])) == sorted(list(row_gn['sets_indexed'])):
      df_bpa_genres_n.at[index_gn,'bpa']+= row_g['bpa']


######## Now we have computed the bpa's correctly

# Create m_1 ,m_2, m_3 in the needed form for DST Prolog implementation
Theta = [i+1 for i in movies_considered_int]

m1_rd = []
m2_ch = []
m3_g = []

# the form will be: 
# m(accending_number_of_mass_function:{1,..,n}, set:[i_0,i_1,...,i_m] | m:{1,...,|Theta|}, bpa_val:{0.0,...,1.0})

for _, row_rd in df_bpa_release_decades_.iterrows():
  # because : set:[i_0,i_1,...,i_m] | m:{1,...,|Theta|}
  s_ind = [i+1 for i in sorted(list(row_rd['sets_indexed']))]
  m1_rd.append((1, s_ind, row_rd['bpa'] ))
    
for _, row_ch in df_bpa_click_hours_n.iterrows():
  # because : set:[i_0,i_1,...,i_m] | m:{1,...,|Theta|}
  s_ind = [i+1 for i in sorted(list(row_ch['sets_indexed']))]
  m2_ch.append((2, s_ind, row_ch['bpa']))

for _, row_g in df_bpa_genres_n.iterrows():
  # because : set:[i_0,i_1,...,i_m] | m:{1,...,|Theta|}
  s_ind = [i+1 for i in sorted(list(row_g['sets_indexed']))]
  m3_g.append((3, s_ind,row_g['bpa'] ))





####### PYTHON + Prolog #######################################################################


# Start DST ###################################################################################
  
init()  # init ECLiPSe engine

#----------------------------------------------------------------------------
# create module that will contain the information about
# the Universe: theta, num_of_m, mass functions 
file_object = open('../mass_func.pl', 'w+')

# write the information for module exporting
file_object.write(":-module(mass_func).\n")
file_object.write(":-export(m/3).\n")
file_object.write(":-export(theta/1).\n")
file_object.write(":-export(num_of_m/1).\n")

# write about theta and num_of_func
file_object.write("\ntheta("+str(Theta)+").\n")
file_object.write("num_of_m(3).\n")

file_object.write("\n")
for m_num, focal_set, bpa in m1_rd:
  file_object.write("m("+str(m_num)+","+str(focal_set)+","+str(bpa)+").\n")

file_object.write("\n")
for m_num, focal_set, bpa in m2_ch:
  file_object.write("m("+str(m_num)+","+str(focal_set)+","+str(bpa)+").\n")

file_object.write("\n")
for m_num, focal_set, bpa in m3_g:
  file_object.write("m("+str(m_num)+","+str(focal_set)+","+str(bpa)+").\n")

file_object.close()

#-----------------------------------------------------------------------------------------------------------------------

# :- use_module(dst_master.pl)
Compound("use_module",Atom("../dst_master.pl")).post_goal() 

#----- Dempster's Rule--------------------------------------------------------------------------------------------------

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


CSP_time_DR = time.perf_counter_ns()-seconds_before_CSP

# focal_points_per_mass_function^num_of_mass_functions
total_combinations = len(m1_rd)*len(m2_ch)*len(m3_g)

print("\nCombinations length:",total_combinations)
print("Num of non-intersecting combinations :",total_combinations-len(TarsAndVals.value()))
print("Num of intersecting combinations :",len(TarsAndVals.value()))

print("\nSum is:",math.fsum(V.value()))

file_object = open('log_file.csv', 'a')
file_object.write("Total number of combinations,"+str(total_combinations)+",") 
file_object.write("Number of non-intersecting combinations,"+str(total_combinations-len(TarsAndVals.value()))+",") 
file_object.write("Number of intersecting combinations,"+str(len(TarsAndVals.value()))+",") 
file_object.write("Number of focal points in combined mass function,"+str(len(CombinedMass.value()))+",") 
file_object.close()

#----- Dempster's Rule --------------------------------------------------------------------------------------------------

#----- Belief -----------------------------------------------------------------------------------------------------------

########## Compute belief for the whole Universe ##############

A_Theta = Var()
B_Theta = Var()
C_Theta = Var()

Compound("=", A_Theta, PList(Theta)).post_goal()  # Post goal
resume()  # Resume execution of ECLiPSe engine

seconds_before_CSP = time.perf_counter_ns()
Compound("belief_comb",A_Theta,B_Theta,C_Theta).post_goal() 

result, dummy = resume() 
if result != SUCCEED:
  print(result,dummy)

CSP_time_Bel_Theta = time.perf_counter_ns()-seconds_before_CSP

# file for plots
file_object = open('log_file_bel_Theta.csv', 'a')
# write the information for module exporting
file_object.write("Theta size,"+str(len(Theta))+",")
file_object.write("Combinations,"+str(len(C_Theta.value()))+",")
file_object.write("Time in ns,"+ str(CSP_time_Bel_Theta)+"\n")
file_object.close()

########## Compute belief for the set S={1} ###############

A_1 = Var()
B_1 = Var()
C_1 = Var()

Compound("=", A_1, PList([1])).post_goal()  # Post goal
resume()  # Resume execution of ECLiPSe engine

seconds_before_CSP = time.perf_counter_ns()
Compound("belief_comb",A_1,B_1,C_1).post_goal() 

result, dummy = resume() 
if result != SUCCEED:
  print(result,dummy)

CSP_time_Bel_1 = time.perf_counter_ns()-seconds_before_CSP

# file for plots
file_object = open('log_file_bel_1.csv', 'a')
# write the information for module exporting
file_object.write("Theta size,"+str(len(Theta))+",")
if C_1.value().isNil():
  file_object.write("Combinations,"+str(0)+",")
else:
  file_object.write("Combinations,"+str(len(C_1.value()))+",")
file_object.write("Time in ns,"+ str(CSP_time_Bel_1)+"\n")
file_object.close()

########## Compute belief for the set S, where |S|=|Theta|/2 #########

A_half = Var()
B_half = Var()
C_half = Var()

# create randomly a set S E 2^Theta with |S|= |Theta|/2
Theta_c = Theta.copy()
for _ in range(int(len(Theta)/2)):
  random_int = random.randint(0, len(Theta_c)-1)
  del Theta_c[random_int]

Compound("=", A_half, PList(Theta_c)).post_goal()  # Post goal
resume()  # Resume execution of ECLiPSe engine

seconds_before_CSP = time.perf_counter_ns()
Compound("belief_comb",A_half,B_half,C_half).post_goal()

result, dummy = resume() 
if result != SUCCEED:
  print(result,dummy)

CSP_time_Bel_half = time.perf_counter_ns()-seconds_before_CSP

# file for plots
file_object = open('log_file_bel_half.csv', 'a')
# write the information for module exporting
file_object.write("Theta size,"+str(len(Theta))+",")
if C_half.value().isNil():
  file_object.write("Combinations,"+str(0)+",")
else:
  file_object.write("Combinations,"+str(len(C_half.value()))+",")
file_object.write("Time in ns,"+ str(CSP_time_Bel_half)+"\n")
file_object.close()

################################################################
# file for combination-time plots
file_object = open('log_file_bel_combinations.csv', 'a')

# Theta has bel ~ 1 always - so no need to check for nil
file_object.write("Combinations,"+str(len(C_Theta.value()))+",")
file_object.write("Time in ns,"+ str(CSP_time_Bel_Theta)+"\n")

# this set may have belief 0 - set is Nil
if C_1.value().isNil():
  file_object.write("Combinations,"+str(0)+",")
else:
  file_object.write("Combinations,"+str(len(C_1.value()))+",")
file_object.write("Time in ns,"+ str(CSP_time_Bel_1)+"\n")

# this set may have belief 0 - set is Nil
if C_half.value().isNil():
  file_object.write("Combinations,"+str(0)+",")
else:
  file_object.write("Combinations,"+str(len(C_half.value()))+",")
file_object.write("Time in ns,"+ str(CSP_time_Bel_half)+"\n")

file_object.close()
################################################################



#############################################################

cleanup()  # Shutdown ECLiPSe engine

#----- Belief -----------------------------------------------------------------------------------------------------------

#----- Dempster's Rule--------------------------------------------------------------------------------------------------

print("\nDempster's rule | CSP run in : %s ns"% CSP_time_DR)
print("\n______________________________________\n")

# some more information about Dempster's rule
file_object = open('log_file.csv', 'a')
# write the information for module exporting
file_object.write("Number of focal points in m1,"+str(len(m1_rd))+",")
file_object.write("Number of focal points in m2,"+str(len(m2_ch))+",")
file_object.write("Number of focal points in m3,"+str(len(m3_g))+",") 
# write theta size
file_object.write("Theta size,"+str(len(Theta))+",")
# write time in ns
file_object.write("Time in ns,"+ str(CSP_time_DR)+"\n")

file_object.close()

#----- Dempster's Rule--------------------------------------------------------------------------------------------------