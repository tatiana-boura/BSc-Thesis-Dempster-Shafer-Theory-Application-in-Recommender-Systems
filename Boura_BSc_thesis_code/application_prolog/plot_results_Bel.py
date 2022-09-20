# Import libraries needed
import matplotlib.pyplot as plt
import pandas as pd
import sys


def sortByFirstElem(elem):
	return elem[0]

def sortBySecondElem(elem):
	return elem[1]


def plot_combinations(f):

  # plot #combinations vs time
  file = open(f)
  data= pd.read_csv(file)

  data.columns = ["0","# combinations","2","time(ns)"  ]
  data = data.drop(["0", "2"], axis=1)

  # we want to sort the focal points regarding the time

  # make a list of tuples where [(# combinations, time)]
  zipped_list_m1 = list(zip(data["# combinations"].tolist(), data["time(ns)"].tolist()))
  # sort list with key=time
  zipped_list_m1.sort(key=sortBySecondElem)
  # now make a list of two lists where [[sorted_combinations_regarding_time],[sorted_time]]
  unzipped_list_m1 = list(map(list, zip(*zipped_list_m1)))

  # x axis values
  x1 = unzipped_list_m1[0]
  # corresponding y axis values
  y1 = unzipped_list_m1[1]
  # plotting the line 1 points 
  plt.plot(x1, y1, marker='o', linestyle="None", color="lime", alpha=.4)

  # naming the x axis
  plt.xlabel('number of combinations')
  # naming the y axis
  plt.ylabel('time(ns)')
    
  # giving a title to my graph
  plt.title('combinations vs time')

  # function to save plot as .png
  plt.savefig('combinations_time_plot_bel.png')

  # function to show the plot
  plt.show()

  return

def plot_set(f, set_name, save_name, plot_color):

  # plot |Theta| vs time
  file = open(f)
  data= pd.read_csv(file)
  
  data.columns = ["0","Theta size","2","# combinations","4","time(ns)"  ]
  data = data.drop(["0", "2","4"], axis=1)

  # we want to sort the Theta size regarding the time for computing the belief of Universe

  # make a list of tuples where [(Theta size, time)]
  zipped_list_m1 = list(zip(data["Theta size"].tolist(), data["time(ns)"].tolist()))
  # sort list with key=time
  zipped_list_m1.sort(key=sortBySecondElem)
  # now make a list of two lists where [[sorted_Theta size_regarding_time],[sorted_time]]
  unzipped_list_m1 = list(map(list, zip(*zipped_list_m1)))

  # x axis values
  x1 = unzipped_list_m1[0]
  # corresponding y axis values
  y1 = unzipped_list_m1[1]
  # plotting the line 1 points 
  plt.plot(x1, y1, marker='o', linestyle="None", color=plot_color, alpha=.4)

  # naming the x axis
  plt.xlabel('Theta size')
  # naming the y axis
  plt.ylabel('time(ns)')
    
  # giving a title to my graph
  t = "Belief of "+set_name+" - Theta size vs time"
  plt.title(t)

  # function to save plot as .png
  s = "belief_of_"+save_name+"_plot.png"
  plt.savefig(s)

  # function to show the plot
  plt.show()

  return

plot_combinations('log_file_bel_combinations.csv')
plot_set('log_file_bel_Theta.csv', '|U|', 'theta',"violet")
plot_set('log_file_bel_1.csv', '{1}', '1', 'teal')
plot_set('log_file_bel_half.csv', 'sets with length |U|/2', 'half', 'purple')