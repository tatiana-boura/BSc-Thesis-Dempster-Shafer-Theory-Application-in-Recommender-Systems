# Import libraries needed
import matplotlib.pyplot as plt
import pandas as pd
import sys


file = open('log_file.csv')
data= pd.read_csv(file)


data.columns = ["0","# combinations","2","# non-intersecting combinations", "4","# intersecting combinations", "6","# focal points in combined bpa","8","# focal points in m1","10","# focal points in m2","12","# focal points in m3","14", "Theta size","16", "time(ns)"  ]
data = data.drop(["0", "2", "4", "6", "8", "10", "12", "14", "16"], axis=1)

def sortByFirstElem(elem):
	return elem[0]

def sortBySecondElem(elem):
	return elem[1]




def plot_combinations(data):

  # plot #combinations and #intersecting combinations vs time

  # we want to sort the combinations regarding the time 

  # make a list of tuples where [(# combination, time)]
  zipped_list_tc = list(zip(data["# combinations"].tolist(), data["time(ns)"].tolist()))
  # sort list with key=time
  zipped_list_tc.sort(key=sortBySecondElem)
  # now make a list of two lists where [[sorted_combinations_regarding_time],[sorted_time]]
  unzipped_list_tc = list(map(list, zip(*zipped_list_tc)))

  # line 1 points
  # x axis values
  x1 = unzipped_list_tc[0]
  # corresponding y axis values
  y1 = unzipped_list_tc[1]
  # plotting the line 1 points 
  plt.plot(x1, y1, color= "red", alpha=.6, label = "# combs", marker='o', linestyle="None")
  
  # line 2 points
  # make a list of tuples where [(# intersecting combinations, time)]
  zipped_list_ic = list(zip(data["# intersecting combinations"].tolist(), data["time(ns)"].tolist()))
  # sort list with key=time
  zipped_list_ic.sort(key=sortBySecondElem)
  # now make a list of two lists where [[sorted_intersecting_combinations_regarding_time],[sorted_time]]
  unzipped_list_ic = list(map(list, zip(*zipped_list_ic)))

  # line 1 points
  # x axis values
  x2 = unzipped_list_ic[0]
  # corresponding y axis values
  y2 = unzipped_list_ic[1]
  # plotting the line 2 points 
  
  plt.plot(x2, y2, color= "black", label = "# inter coms", marker='x', linestyle="None")
  
 
  plt.legend(loc='best')
  # naming the x axis
  plt.xlabel('number of combinations')
  # naming the y axis
  plt.ylabel('time(ns)')
    
  # giving a title to my graph
  plt.title('combinations vs time')

  # function to save plot as .png
  plt.savefig('combinations_time_plot_DR.png')

  # function to show the plot
  plt.show()

  return


def plot_Theta_size(data):

  # plot Theta size vs time

  # we want to sort the time regarding the Theta size

  # make a list of tuples where [(Theta size, time)]
  zipped_list = list(zip(data["Theta size"].tolist(), data["time(ns)"].tolist()))

  # sort list with key=Theta_size
  zipped_list.sort(key=sortByFirstElem)

  # now make a list of two lists where [[list_of_sorted_Thetas],[sorted_time_regarding_Theta]]
  unzipped_list = list(map(list, zip(*zipped_list)))

  # x axis values
  x = unzipped_list[0]
  # corresponding y axis values
  y = unzipped_list[1]
    
  # plot the points 
  plt.plot(x, y, color= "brown", marker='o', linestyle="None",alpha=.5)
    
  # naming the x axis
  plt.xlabel('Theta size')
  # naming the y axis
  plt.ylabel('time(ns)')
    
  # giving a title to my graph
  plt.title('Theta size vs time')

  # function to save plot as .png
  plt.savefig('theta_size_time_plot.png')

  # function to show the plot
  plt.show()

  return


def plot_focal_points(data):

  # plot #focal points vs time

  # we want to sort the focal points regarding the time

  #_____m1_______________________________________________________________________________________________
  # make a list of tuples where [(# focal points in m1, time)]
  zipped_list_m1 = list(zip(data["# focal points in m1"].tolist(), data["time(ns)"].tolist()))
  # sort list with key=time
  zipped_list_m1.sort(key=sortBySecondElem)
  # now make a list of two lists where [[sorted_m1_focal_points_regarding_time],[sorted_time]]
  unzipped_list_m1 = list(map(list, zip(*zipped_list_m1)))

  # line 1 points
  # x axis values
  x1 = unzipped_list_m1[0]
  # corresponding y axis values
  y1 = unzipped_list_m1[1]
  # plotting the line 1 points 
  plt.plot(x1, y1, label = "# f.p. m1", marker='o', linestyle="None",alpha=.5)

  #_____m2_______________________________________________________________________________________________
  # make a list of tuples where [(# focal points in m2, time)]
  zipped_list_m2 = list(zip(data["# focal points in m2"].tolist(), data["time(ns)"].tolist()))
  # sort list with key=time
  zipped_list_m2.sort(key=sortBySecondElem)
  # now make a list of two lists where [[sorted_m2_focal_points_regarding_time],[sorted_time]]
  unzipped_list_m2 = list(map(list, zip(*zipped_list_m2)))

  # line 2 points
  # x axis values
  x2 = unzipped_list_m2[0]
  # corresponding y axis values
  y2 = unzipped_list_m2[1]
  # plotting the line 2 points 
  plt.plot(x2, y2, label = "# f.p. m2", marker='o', linestyle="None",alpha=.5)

  #_____m3_______________________________________________________________________________________________
  # make a list of tuples where [(# focal points in m3, time)]
  zipped_list_m3 = list(zip(data["# focal points in m3"].tolist(), data["time(ns)"].tolist()))
  # sort list with key=time
  zipped_list_m3.sort(key=sortBySecondElem)
  # now make a list of two lists where [[sorted_m3_focal_points_regarding_time],[sorted_time]]
  unzipped_list_m3 = list(map(list, zip(*zipped_list_m3)))

  # line 3 points
  # x axis values
  x3 = unzipped_list_m3[0]
  # corresponding y axis values
  y3 = unzipped_list_m3[1]
  # plotting the line 3 points 
  plt.plot(x3, y3, label = "# f.p. m3", marker='o', linestyle="None",alpha=.5)
 
  plt.legend(loc='best')  
  # naming the x axis
  plt.xlabel('number of focal points')
  # naming the y axis
  plt.ylabel('time(ns)')
    
  # giving a title to my graph
  plt.title('focal points vs time')

  # function to save plot as .png
  plt.savefig('focal_points_time_plot.png')

  # function to show the plot
  plt.show()

  return


plot_combinations(data)
plot_Theta_size(data)
plot_focal_points(data)