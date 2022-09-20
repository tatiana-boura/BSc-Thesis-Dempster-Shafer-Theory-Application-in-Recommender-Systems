import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import csv
import os

df_theta_10 = pd.DataFrame()
df_theta_15 = pd.DataFrame()
df_theta_20 = pd.DataFrame()
df_theta_25 = pd.DataFrame()

def sortByFirstElem(elem):
	  return elem[0]

def plot_per_data_sep(data, theta, number_of_mf, number_of_fp):

  # plot set_len and time

  # determin size from the start
  plt.figure(figsize=(10, 10))

  x1=[] # to be evaluated

  # make a list of tuples where [(sets length, time)]
  zipped_list_tp = list(zip(data["sets length"].tolist(), data["prolog time(ns)"].tolist()))
  # sort list with key=time
  zipped_list_tp.sort(key=sortByFirstElem)
  # now make a list of two lists where [[sorted_sets length],[sorted_time_regarding_sets_length]]
  unzipped_list_tp = list(map(list, zip(*zipped_list_tp)))

  # line 1 points
  # x axis values
  x1 = unzipped_list_tp[0]
  # corresponding y axis values
  y1 = unzipped_list_tp[1]
  # plotting the line 1 points 
  plt.plot(x1, y1, color= "cyan", label = "CSP", marker='o')

  plt.legend(loc='best')
  # naming the x axis
  plt.xlabel('length set')
  # naming the y axis
  plt.ylabel('time(ns)')
    
  # plot vertical lines
  # plot the line that correspond to every x (here: length of set)
  for xc in x1:
    plt.axvline(x=xc, color='grey', alpha=.5, linestyle='--')

  # giving a title to my graph
  title_str = "|U|="+str(theta)+", number of mass functions="+str(number_of_mf)+", number of focal points per mass function="+str(number_of_fp)
  plt.title(title_str)

  # function to save plot as .png
  to_save = path_to_dir+"/plot_results/belief_"+str(theta)+"_"+str(number_of_mf)+"_"+str(number_of_fp)+".png"
  print(to_save)
  plt.savefig(to_save, bbox_inches="tight",dpi=250)


  plt.clf()


  # if we should plot ibelief and it has not been plotted previously
 
  # line 2 points
  # make a list of tuples where [(sets length, time)]
  zipped_list_te = list(zip(data["sets length"].tolist(), data["ibelief time(ns) bel"].tolist()))
  # sort list with key=time
  zipped_list_te.sort(key=sortByFirstElem)
  # now make a list of two lists where [[sorted_sets length],[sorted_time_regarding_sets_length]]
  unzipped_list_te = list(map(list, zip(*zipped_list_te)))

  # line 2 points
  # x axis values
  x2 = unzipped_list_te[0]
  # corresponding y axis values
  y2 = unzipped_list_te[1]
  # plotting the line 2 points 

  plt.plot(x2, y2, color= "lime", label = "ibelief bel", marker='o')

  # line 2 points
  # make a list of tuples where [(sets length, time)]
  zipped_list_tb = list(zip(data["sets length"].tolist(), data["ibelief time(ns) bel and rule"].tolist()))
  # sort list with key=time
  zipped_list_tb.sort(key=sortByFirstElem)
  # now make a list of two lists where [[sorted_sets length],[sorted_time_regarding_sets_length]]
  unzipped_list_tb = list(map(list, zip(*zipped_list_tb)))

  # line 3 points
  # x axis values
  x3 = unzipped_list_tb[0]
  # corresponding y axis values
  y3 = unzipped_list_tb[1]
  # plotting the line 2 points 

  plt.plot(x3, y3, color= "magenta", label = "ibelief bel and rule", marker='o')

  plt.legend(loc='best')
  # naming the x axis
  plt.xlabel('length set')
  # naming the y axis
  plt.ylabel('time(ns)')
    
  # plot vertical lines
  # plot the line that correspond to every x (here: length of set)
  for xc in x1:
    plt.axvline(x=xc, color='grey', alpha=.5, linestyle='--')

  # giving a title to my graph
  title_str = "|U|="+str(theta)+", number of mass functions="+str(number_of_mf)+", number of focal points per mass function="+str(number_of_fp)
  plt.title(title_str)

  # function to save plot as .png
  to_save = path_to_dir+"/plot_results/belief_"+str(theta)+"_"+str(number_of_mf)+"_"+str(number_of_fp)+"_i.png"
  print(to_save)
  plt.savefig(to_save, bbox_inches="tight",dpi=250)

  # function to show the plot
  # plt.show()

  return


def plot_per_data(data, theta, number_of_mf, number_of_fp, no_ibelief, no_CSP):

  # plot set_len and time

  # determin size from the start
  plt.figure(figsize=(10, 10))

  x1=[] # to be evaluated
  belief_already_plotted=False

  if no_CSP==False:
    # plot Prolog results

    # make a list of tuples where [(sets length, time)]
    zipped_list_tp = list(zip(data["sets length"].tolist(), data["prolog time(ns)"].tolist()))
    # sort list with key=time
    zipped_list_tp.sort(key=sortByFirstElem)
    # now make a list of two lists where [[sorted_sets length],[sorted_time_regarding_sets_length]]
    unzipped_list_tp = list(map(list, zip(*zipped_list_tp)))

    # line 1 points
    # x axis values
    x1 = unzipped_list_tp[0]
    # corresponding y axis values
    y1 = unzipped_list_tp[1]
    # plotting the line 1 points 
    plt.plot(x1, y1, color= "cyan", label = "CSP", marker='o')

  else:
    # do not plot Prolog results
    # make fake set's length
    x1 = [1,3,5,7,9,13,16,20]
    # ibelief is the same for every subset of the powerset 2^|U|
    y1 = [data.iloc[0,4]]*len(x1)
    y2 = [data.iloc[0,5]]*len(x1)

    # plot the 'fake' lines
    plt.plot(x1, y1, color= "lime", label = "ibelief bel", marker='o')
    plt.plot(x1, y2, color= "magenta", label = "ibelief bel and rule", marker='o')

    belief_already_plotted=True

  # if we should plot ibelief and it has not been plotted previously
  if no_ibelief==False and belief_already_plotted==False:
    # line 2 points
    # make a list of tuples where [(sets length, time)]
    zipped_list_te = list(zip(data["sets length"].tolist(), data["ibelief time(ns) bel"].tolist()))
    # sort list with key=time
    zipped_list_te.sort(key=sortByFirstElem)
    # now make a list of two lists where [[sorted_sets length],[sorted_time_regarding_sets_length]]
    unzipped_list_te = list(map(list, zip(*zipped_list_te)))

    # line 2 points
    # x axis values
    x2 = unzipped_list_te[0]
    # corresponding y axis values
    y2 = unzipped_list_te[1]
    # plotting the line 2 points 

    plt.plot(x2, y2, color= "lime", label = "ibelief bel", marker='o')

    # line 2 points
    # make a list of tuples where [(sets length, time)]
    zipped_list_tb = list(zip(data["sets length"].tolist(), data["ibelief time(ns) bel and rule"].tolist()))
    # sort list with key=time
    zipped_list_tb.sort(key=sortByFirstElem)
    # now make a list of two lists where [[sorted_sets length],[sorted_time_regarding_sets_length]]
    unzipped_list_tb = list(map(list, zip(*zipped_list_tb)))

    # line 3 points
    # x axis values
    x3 = unzipped_list_tb[0]
    # corresponding y axis values
    y3 = unzipped_list_tb[1]
    # plotting the line 2 points 

    plt.plot(x3, y3, color= "magenta", label = "ibelief bel and rule", marker='o')

  plt.legend(loc='best')
  # naming the x axis
  plt.xlabel('length set')
  # naming the y axis
  plt.ylabel('time(ns)')
    
  # plot vertical lines
  # plot the line that correspond to every x (here: length of set)
  for xc in x1:
    plt.axvline(x=xc, color='grey', alpha=.5, linestyle='--')

  # giving a title to my graph
  title_str = "|U|="+str(theta)+", number of mass functions="+str(number_of_mf)+", number of focal points per mass function="+str(number_of_fp)
  plt.title(title_str)

  # function to save plot as .png
  to_save = path_to_dir+"/plot_results/belief_"+str(theta)+"_"+str(number_of_mf)+"_"+str(number_of_fp)+".png"
  print(to_save)
  plt.savefig(to_save, bbox_inches="tight",dpi=250)

  # function to show the plot
  # plt.show()

  return

path_to_dir = './plots/bel'

list_only_prolog = []

list_only_prolog = []

for filename in sorted(os.listdir(path_to_dir)):
  f = os.path.join(path_to_dir,filename)
  if os.path.isfile(f):
    # the first two lines of the .csv has different length and we want to connect it to the rest of the csv
    with open(f, newline='') as f_:
      # open .csv and get first line: that of ibelief
      reader = csv.reader(f_)
      ibelief_info_1 = next(reader)
      ibelief_info_2 = ibelief_info_1

      if len(ibelief_info_1) == 2:
        # ibelief ran with those data
        # get next line
        ibelief_info_2 = next(reader)

      data=pd.DataFrame()

      # ibelief did run
      if len(ibelief_info_1) == 2:        
        # read dataframe without first and second row  (of ibelief)
        data= pd.read_csv(f, skiprows=[0,1], header=None)
        # name the columns
        data.columns = ["0","set","2","# focal points per mass", "4","# mass functions", "6","# Theta size","8", "prolog time(ns)"  ]
        # add new columns : ibelief data
        data['10'] = [ibelief_info_1[0]]*len(data.index)
        data['ibelief time(ns) bel'] = [int(ibelief_info_1[1])]*len(data.index)
        data['12'] = [ibelief_info_2[0]]*len(data.index)
        data['ibelief time(ns) bel and rule'] = [int(ibelief_info_2[1])]*len(data.index)
        # drop the not needed columns
        data = data.drop(["0", "2", "4", "6", "8", "10", "12"], axis=1)

      # ibelief did not run
      else: 
        # read dataframe without skipping
        data= pd.read_csv(f, header=None)
        # name the columns
        data.columns = ["0","set","2","# focal points per mass", "4","# mass functions", "6","# Theta size","8", "prolog time(ns)"  ]
        # add new columns : ibelief data
        data['10'] = [0]*len(data.index)
        data['ibelief time(ns) bel'] = [0]*len(data.index)
        data['12'] = [0]*len(data.index)
        data['ibelief time(ns) bel and rule'] = [0]*len(data.index)
        # drop the not needed columns
        data = data.drop(["0", "2", "4", "6", "8", "10", "12"], axis=1)

      # make all columns except for 'set' numeric
      l = ["# focal points per mass", "# mass functions", "# Theta size","prolog time(ns)", "ibelief time(ns) bel", "ibelief time(ns) bel and rule"  ]
      data[l] = data[l].apply(pd.to_numeric)

      # the plots will be per data set and x : axis length of set we want to find the 'belief'
      list_of_sets = list(data['set'])
      more_than_one=False
      # compute length of set
      sets_len_list = []
      for s in list_of_sets:
        if s == '[0]':
          sets_len_list.append(0)
        else:
          sets_len_list.append(len(s.split(" "))-2)
          more_than_one=True
      # set length as new column
      data['sets length'] = sets_len_list

      data.reset_index()
      
      if more_than_one:
        # plots of Prolog when ibelief was unable to compute
        if data.iloc[0, 3]==25: 
          i=0
          plot_per_data(data, data.iloc[0, 3], data.iloc[0, 2], data.iloc[0, 1], True, False)
        else: # plots of Prolog and ibelief
          plot_per_data_sep(data, data.iloc[0, 3], data.iloc[0, 2], data.iloc[0, 1])
      #else:
        #  plots of ibelief when Prolog was unable to compute
        plot_per_data(data, data.iloc[0, 3], data.iloc[0, 2], data.iloc[0, 1], False, True)
      
      # make datasets regarding theta size
      if data.iloc[0, 3]==10:
        df_theta_10 = pd.concat([df_theta_10, data], axis=0)
      elif data.iloc[0, 3]==15 :
        df_theta_15 = pd.concat([df_theta_15, data], axis=0)
      elif data.iloc[0, 3]==20 :
        df_theta_20 = pd.concat([df_theta_20, data], axis=0)
      elif data.iloc[0, 3]==25 :
        df_theta_25 = pd.concat([df_theta_25, data], axis=0)

def make_table_theta_size_csp(argv, colWidths):

  df_temp = argv[0]
  filename = argv[1]

  df_temp = df_temp.drop(["# Theta size"], axis=1)
  df_temp = df_temp.drop(["sets length"], axis=1)

  df_temp = df_temp[(df_temp["# mass functions"] == 2)  | (df_temp["# mass functions"] == 6)  | (df_temp["# mass functions"] == 10) ]
  df_temp = df_temp[(df_temp["# focal points per mass"] == 6)  | (df_temp["# focal points per mass"] == 10) | (df_temp["# focal points per mass"] == 14)   ]

  df_temp['prolog time(ns)'] = df_temp['prolog time(ns)'].replace([0],np.inf)
  df_temp['set'] = df_temp['set'].replace(["[0]"],"-")

  df_temp = df_temp.drop(["ibelief time(ns) bel"], axis=1)
  df_temp = df_temp.drop(["ibelief time(ns) bel and rule"], axis=1)

  l = ["# focal points per mass", "# mass functions","prolog time(ns)"]
  df_temp[l] = df_temp[l].astype(float)


  c = "aliceblue"
  c_col = "#dff0ff"

  col_colors = [c_col] * len(df_temp.columns)

  colors=[]
  for _ in range(len(df_temp.values)):
    colors.append([c] * len(df_temp.columns))

  fig, ax = plt.subplots()
  ax.axis('off')
  ax.axis('tight')
  t = ax.table(cellText=df_temp.values, colWidths = [colWidths,0.25,0.2,0.25],  colLabels=df_temp.columns,  loc='center',cellLoc='center',cellColours=colors, colColours=col_colors)
  t.auto_set_font_size(False)
  t.set_fontsize(11)
  t.scale(2, 2)
  
  to_save = path_to_dir+"/plot_results/Copy of "+filename+".png"
  plt.savefig(to_save, bbox_inches="tight",dpi=300)
  

  plt.show()

  return

def make_table_theta_size_ibel(argv, colWidths):

  df_temp = argv[0]
  filename = argv[1]

  df_temp = df_temp.drop(["# Theta size"], axis=1)
  df_temp = df_temp.drop(["sets length"], axis=1)

  df_temp = df_temp[(df_temp["# mass functions"] == 2)  | (df_temp["# mass functions"] == 6)  | (df_temp["# mass functions"] == 10) ]
  df_temp = df_temp[(df_temp["# focal points per mass"] == 6)  | (df_temp["# focal points per mass"] == 10) | (df_temp["# focal points per mass"] == 14)   ]

  df_temp['set'] = df_temp['set'].replace(["[0]"],"-")
  df_temp['ibelief time(ns) bel'] = df_temp['ibelief time(ns) bel'].replace([0],np.inf)
  df_temp['ibelief time(ns) bel and rule'] = df_temp['ibelief time(ns) bel and rule'].replace([0],np.inf)

  l = ["# focal points per mass", "# mass functions","ibelief time(ns) bel", "ibelief time(ns) bel and rule"  ]
  df_temp[l] = df_temp[l].astype(float)

  df_temp = df_temp.drop(["prolog time(ns)"], axis=1)


  c = "aliceblue"
  c_col = "#dff0ff"

  col_colors = [c_col] * len(df_temp.columns)

  colors=[]
  for _ in range(len(df_temp.values)):
    colors.append([c] * len(df_temp.columns))

  fig, ax = plt.subplots()
  ax.axis('off')
  ax.axis('tight')
  t = ax.table(cellText=df_temp.values, colWidths = [colWidths,0.25,0.2,0.25,0.3],  colLabels=df_temp.columns,  loc='center',cellLoc='center',cellColours=colors, colColours=col_colors)
  t.auto_set_font_size(False)
  t.set_fontsize(11)
  t.scale(2, 2)
  
  to_save = path_to_dir+"/plot_results/"+filename+"_i.png"
  plt.savefig(to_save, bbox_inches="tight",dpi=300)
  

  plt.show()

  return

make_table_theta_size_csp([df_theta_10, "theta_10"],0.3)
make_table_theta_size_csp([df_theta_15, "theta_15"],0.4)
make_table_theta_size_csp([df_theta_20, "theta_20"],0.5)
make_table_theta_size_csp([df_theta_25, "theta_25"],0.6)


make_table_theta_size_ibel([df_theta_10, "theta_10"],0.3)
make_table_theta_size_ibel([df_theta_15, "theta_15"],0.4)
make_table_theta_size_ibel([df_theta_20, "theta_20"],0.5)
make_table_theta_size_ibel([df_theta_25, "theta_25"],0.6)