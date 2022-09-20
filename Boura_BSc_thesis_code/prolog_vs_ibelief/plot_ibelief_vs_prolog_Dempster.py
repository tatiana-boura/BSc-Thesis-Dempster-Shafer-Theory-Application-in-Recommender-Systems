import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import csv
import os

df_theta_10 = pd.DataFrame()
df_theta_15 = pd.DataFrame()
df_theta_20 = pd.DataFrame()
df_theta_25 = pd.DataFrame()

path_to_dir = './plots/Dempster_rule'

list_only_prolog = []

for filename in os.listdir(path_to_dir):
    f = os.path.join(path_to_dir,filename)
    if os.path.isfile(f):
        # the first line of the .csv has different length and we want to connect it to the rest of the csv
        with open(f, newline='') as f_:
          # open .csv and get first line: that of ibelief
          reader = csv.reader(f_)
          ibelief_info = next(reader)  
          # get number of remaining rows
          rows_remaining = len(list(reader))

          if rows_remaining == 0:
            # these are the cases where ibelief did not run
            list_only_prolog.append(f)
          else:
            # read dataframe without first row (of ibelief)
            data= pd.read_csv(f, skiprows=[0], header=None)
            # name the columns
            data.columns = ["0","# combinations","2","# focal points per mass", "4","# mass functions", "6","# Theta size","8", "prolog time(ns)"  ]
            # add new columns : ibelief data
            data['10'] = [ibelief_info[0]]
            data['ibelief time(ns)'] = [int(ibelief_info[1])]
            # drop the not needed columns
            data = data.drop(["0", "2", "4", "6", "8", "10"], axis=1)

            #data['total # of focal points'] = data['# focal points per mass']*data['# mass functions']

            if data.iloc[0, 3]==10:
              df_theta_10 = pd.concat([df_theta_10, data], axis=0)
            elif data.iloc[0, 3]==15 :
              df_theta_15 = pd.concat([df_theta_15, data], axis=0)
            elif data.iloc[0, 3]==20 :
              df_theta_20 = pd.concat([df_theta_20, data], axis=0)
            elif data.iloc[0, 3]==25 :
              df_theta_25 = pd.concat([df_theta_25, data], axis=0)

for f in list_only_prolog:
  data= pd.read_csv(f, header=None)
  # name the columns
  data.columns = ["0","# combinations","2","# focal points per mass", "4","# mass functions", "6","# Theta size","8", "prolog time(ns)"  ]
  # add new columns : ibelief data
  data['10'] = [ibelief_info[0]]
  data['ibelief time(ns)'] = [0]
  # drop the not needed columns
  data = data.drop(["0", "2", "4", "6", "8", "10"], axis=1)

  df_theta_25 = pd.concat([df_theta_25, data], axis=0)

def make_table_theta_size(argv):

  df = argv[0]
  filename = argv[1]

  df_temp = df.drop(["# Theta size"], axis=1)
  df_temp = df_temp.sort_values(by=['# combinations','# focal points per mass'])

  df_temp['prolog time(ns)'] = df_temp['prolog time(ns)'].replace([0],np.inf)
  df_temp['ibelief time(ns)'] = df_temp['ibelief time(ns)'].replace([0],np.inf)

  c = "aliceblue"
  c_col = "#dff0ff"

  col_colors = [c_col,c_col,c_col,c_col,c_col]

  colors=[]
  for _ in range(len(df_temp.values)):
    colors.append([c,c,c,c,c])

  fig, ax = plt.subplots()
  ax.axis('off')
  ax.axis('tight')
  t = ax.table(cellText=df_temp.values, colWidths = [0.3]*len(df_temp.columns),  colLabels=df_temp.columns,  loc='center',cellLoc='center',cellColours=colors, colColours=col_colors)

  to_save = path_to_dir+"/plot_results/"+filename+".png"
  plt.savefig(to_save, bbox_inches="tight",dpi=250)

  plt.show()

  return

make_table_theta_size([df_theta_10, "theta_10"])
make_table_theta_size([df_theta_15, "theta_15"])
make_table_theta_size([df_theta_20, "theta_20"])
make_table_theta_size([df_theta_25, "theta_25"])

def sortByFirstElem(elem):
	  return elem[0]

def plot_per_theta_size(data, theta, no_ibelief):

  data = data.sort_values(by=['# combinations','# focal points per mass'])
  data = data.iloc[:-15 , :]
  #display(data)

  # plot #combinations and for the theta size

  # we want to sort the combinations regarding the time 
  # make a list of tuples where [(# combination, time)]
  zipped_list_tp = list(zip(data["# combinations"].tolist(), data["prolog time(ns)"].tolist()))
  # sort list with key=time
  zipped_list_tp.sort(key=sortByFirstElem)
  # now make a list of two lists where [[sorted_combinations],[sorted_time_regarding_combinations]]
  unzipped_list_tp = list(map(list, zip(*zipped_list_tp)))

  plt.figure(figsize=(20, 10))
  
  # line 1 points
  # x axis values
  x1 = unzipped_list_tp[0]
  # corresponding y axis values
  y1 = unzipped_list_tp[1]
  # plotting the line 1 points 
  plt.plot(x1, y1, color= "cyan", label = "CSP", marker='o')#, linestyle="None")
  
  if no_ibelief==False:
    # line 2 points
    # make a list of tuples where [(# intersecting combinations, time)]
    zipped_list_te = list(zip(data["# combinations"].tolist(), data["ibelief time(ns)"].tolist()))
    # sort list with key=time
    zipped_list_te.sort(key=sortByFirstElem)
    # now make a list of two lists where [[sorted_combinations],[sorted_time_regarding_combinations]]
    unzipped_list_te = list(map(list, zip(*zipped_list_te)))


    # line 21 points
    # x axis values
    x2 = unzipped_list_te[0]
    # corresponding y axis values
    y2 = unzipped_list_te[1]
    # plotting the line 2 points 

    plt.plot(x2, y2, color= "lime", label = "ibelief", marker='o')#, linestyle="None")
  

  plt.legend(loc='best')
  # naming the x axis
  plt.xlabel('number of combinations')
  # naming the y axis
  plt.ylabel('time(ns)')
    
  # giving a title to my graph
  title_str = "|U|="+str(theta)
  plt.title(title_str)

  for xc in x1:
    plt.axvline(x=xc, color='grey', alpha=.5, linestyle='--')

  # function to save plot as .png
  to_save = path_to_dir+"/plot_results/dempster_theta"+str(theta)+".png"
  plt.savefig(to_save, bbox_inches="tight",dpi=250)

  # function to show the plot
  plt.show()

  return

plot_per_theta_size(df_theta_10,10,False)
plot_per_theta_size(df_theta_15,15,False)
plot_per_theta_size(df_theta_20,20,False)
plot_per_theta_size(df_theta_25,25,True)