import json
import pathlib
import re
from matplotlib.patches import Patch, Rectangle
from matplotlib.collections import PatchCollection
import matplotlib

matplotlib.use("pgf")

import matplotlib.pyplot as plt

plt.rcParams.update({
    "pgf.texsystem": "pdflatex",
    'font.family': 'serif',
    'font.serif' : 'Libertine',
    'text.usetex': True,
    'pgf.preamble': "\n".join([
        r"\RequirePackage[T1]{fontenc} \RequirePackage[tt=false, type1=true]{libertine} \RequirePackage[varqu]{zi4} \RequirePackage[libertine]{newtxmath}",
                               r"\usepackage{amsfonts}"
                                r"\usepackage{amsmath}",
                                r"\usepackage{cleveref}",
                                r"\usepackage{halloweenmath}",
                               r"\usepackage[dvipsnames]{xcolor}",
                                r"\newcommand{\bcol}{CarnationPink}"
                                r"\newcommand{\scol}{Emerald}"
                                r"\newcommand{\rcol}{RedOrange}"
                            r"\newcommand{\Bv}{\mathbf{\textcolor{\bcol}{B}}}",
                            r"\newcommand{\Sv}{\texttt{\textcolor{\scol}{S}}}",
                            r"\newcommand{\Rv}{\mathsf{\textcolor{\rcol}{R}}}",
                               r"\newcommand{\sem}[1]{ \ensuremath{\mathrightghost_{#1}}}",
                              r"\newcommand{\semb}{\sem{\Bv}}",
                            r"\newcommand{\sems}{\sem{\Sv}}",
                            r"\newcommand{\semr}{\sem{\Rv}}",
                            r"\newcommand{\sembs}{\sem{\Bv + \Sv}}",
                            r"\newcommand{\sembr}{\sem{\Bv + \Rv}}",
                            r"\newcommand{\semsr}{\sem{\Sv + \Rv}}",
                            r"\newcommand{\sembsr}{\sem{\Bv + \Sv + \Rv}}"
                               ]),
    'pgf.rcfonts': False,
})
import pandas as pd
import subprocess
from time import sleep
import json
import numpy as np
import os
# Always map the same colors to these versions so easier to compare similar for all tohers
carnation_pink = (1.0, 0.65, 0.79)
emerald = (0, 0.66, 0.62)
redorange= (0.94, 0.29, 0.18)
color_map = {"1" : carnation_pink, "41": emerald, "4" : emerald, "5" : redorange, "14" : "aqua", "15" : "peru", "45" : "darkorange" ,"145" : "gray", "base": "white"}
legend_map = {"1" : r"\semb", "4" : r"\sems", "5" : r"\semr",
              "14" : r"\sembs", "45" : r"\semsr", "15" : r"\sembr",
              "145" : r"\sembsr",
              }

Combined_path = pathlib.Path("combinations/stats/")
V4_path = pathlib.Path("specv4_tests/litmus_stl/stats")
V4_path_barr = pathlib.Path("specv4_tests/litmus_stl_barrier/stats")
V5_path = pathlib.Path("specv5_tests/stats/")

# See https://stackoverflow.com/questions/31908982/python-matplotlib-multi-color-legend-entry/31909401#31909401
# define an object that will be used by the legend
class MulticolorPatch(object):
    def __init__(self, colors):
        self.colors = colors


# define a handler for the MulticolorPatch object
class MulticolorPatchHandler(object):
    def legend_artist(self, legend, orig_handle, fontsize, handlebox):
        width, height = handlebox.width, handlebox.height
        patches = []
        for i, c in enumerate(orig_handle.colors):
            patches.append(Rectangle([width / len(orig_handle.colors) * i - handlebox.xdescent,
                                          -handlebox.ydescent],
                                         width / len(orig_handle.colors),
                                         height,
                                         facecolor=c,
                                         #fill=False,
                                         color=None,
                                         linewidth=1,
                                         edgecolor='black', transform=handlebox.get_transform()))

        patch = PatchCollection(patches, match_original=True) # the problem is patchcllection that does not
        handlebox.add_artist(patch)

        return patch

def save_times(time_dict, barrier=False):
    # One could precompute this or at least dont compute it every time
    file_dict = {"14" : [], "15" : [], "45" : [], "145" : []}
    if barrier:
        for t in Combined_path.glob("*.txt"):

            if "combined" not in t.name:
                continue
            if "barrier" in t.name:

                #print(t.name)
                file_name = (t.name.split("_"))[0]
                version = re.sub("[^0-9]", "", file_name)
                file_dict[version].append(t)
    else:
        for t in Combined_path.glob("*.txt"):
            if "combined" not in t.name:
                continue
            if "barrier" in t.name:
                continue
            # print(t.name)
            file_name = (t.name.split("_"))[0]
            version = re.sub("[^0-9]", "", file_name)
            file_dict[version].append(t)
    #print(file_dict)

    for key, ver_file in file_dict.items():
        for file in ver_file:
            #print(file.name)
            ver = re.sub("[^0-9]", "", file.name.split("_")[0]) # one of the comnbinations eg. 14
            ver_exe = file.name.split("_")[1] # the version for that combination e.g. 14 1 4
            #print(ver)
            #print(ver_exe)
            with open(file) as f:
                time= json.load(f)["total_time"]
                #print(time)
                if ver == ver_exe: # i.e 145 = 145 then we do not append version modifier
                    dic_key = ver
                else:
                    dic_key = ver + "_" + ver_exe
                #print(dic_key)
                time_dict[dic_key].append(time)


def exec_loop(barrier=False):

    time_dict = {"14" : [], "14_1" : [], "14_41" : [], "14_base" : [],
                 "15" : [], "15_1" : [], "15_5" : [], "15_base" : [],
                 "45" : [], "45_41" : [], "45_5" : [], "45_base" : [],
                 "145" : [], "145_1" : [], "145_4" : [], "145_5" : [], "145_14" : [], "145_15" : [], "145_45" : [], "145_base" : [],
                 }
    if barrier:
        script_source = "./combinations/execute_combined_examples_barrier.sh"
    else:
        script_source = "./combinations/execute_combined_examples_no_barrier.sh"


    print("Currently in Warm up phase")
# Somehow cwd is not overriden in wondpws so I need to give the full path
    for i in range(0,50):
        subprocess.run([script_source], stdout=subprocess.DEVNULL)
        sleep(0.2)


    print("Tracking the time results now.")
    for i in range(0,1000):
        subprocess.run([script_source], stdout=subprocess.DEVNULL)
        save_times(time_dict, barrier)
        if i % 100 == 0:
            print("Finished {}/1000 runs", i)

    print("Now saving the results..")
    safe_eval(time_dict, barrier)
    print("Finshed!")
    return


def safe_eval(time_dict, barrier=False):
    # new directory called times if it does not exist
    if barrier:
        f_name = "all_times_barrier.txt"
    else:
        f_name = "all_times.txt"
    # safe a list to a file
    with open(f_name, "w") as file:
        json.dump(time_dict, file)
    print("Executed")
    return

def set_box_color(bp, color):
    plt.setp(bp['boxes'], color=color)
    plt.setp(bp['whiskers'], color=color)
    plt.setp(bp['caps'], color=color)
    plt.setp(bp['medians'], color=color)




def evaluate_Comb():

    group_dicts = []
    for path in ["all_times.txt", "all_times_barrier.txt"]:
        f = open(path)
        time_dict = json.load(f)
        group_dict = {"14G": {}, "15G" : {}, "45G" : {}, "145G" : {}}
        for key, arr in time_dict.items():
            #print(key)
            group_version = key.split("_")[0] + "G"
            #print(group_version)
            group_dict[group_version][key] = arr
        group_dicts.append(group_dict)
    #print(group_dict)


    plt.figure(figsize=(7, 2.5))
    x = 0
    width = 2
    norm_val = width / 4+ 0.5
    ticks = [x + 0.5] # used for xticks
    legend = []
    for j, grp_dict in enumerate(group_dicts):
        if j == 1:
            x += 5
            ticks[-1] += 5 # update last ticks
        for key, dic in grp_dict.items():

            comb_ver = key[0:-1]
            arr = []
            for ver, val in dic.items():
                new_ver = ver.split("_")
                if len(new_ver) == 1:
                    c_ver = new_ver[0] # split on "14" = ["14"] 14_1 = [14, 1]
                else:
                    c_ver = new_ver[1]
                arr.append((c_ver, np.mean(val), np.std(val)))
            if key == "145G": # switch order that the combined versions (of 2 parts) are before the single versions
                print(arr)
                for i in range(1,4):
                    arr[i], arr[i+3] = arr[i+3], arr[i] # simple swap
                print(arr)

            spaces2 = [-2,-1,0,1,2,3,4,5,6,7,8,9,10,11, 12]
            matplotlib.rcParams['hatch.linewidth'] = 4.2
            for i, (ver, mean, std) in enumerate(arr):
                if len(ver) > 1 and 'base' not in ver and '41' not in ver:
                    first = ver[0]
                    second = ver[1]


                    if len(ver) == 2: # since I align on edge here I need to account for offset in the width
                        plt.bar(x + (spaces2[i] * norm_val) , mean, width=norm_val , color="none",
                                 hatch="////", edgecolor=color_map[first])
                        plt.bar(x + (spaces2[i] * norm_val), mean ,width=norm_val, hatch="//", color="none",
                                 edgecolor=color_map[second])

                    if len(ver) == 3: #edge case 145
                        third = ver[2]
                        plt.bar(x + (spaces2[i] * norm_val), mean, width=norm_val, color="none",
                                hatch = "////", edgecolor=color_map[first])
                        plt.bar(x + (spaces2[i] * norm_val), mean, width=norm_val, color="none",
                                hatch="//", edgecolor=color_map[second], linewidth=1)
                        plt.bar(x + (spaces2[i] * norm_val), mean, width=norm_val, color="none",
                                hatch="/", edgecolor=color_map[third], linewidth=1)
                    plt.bar(x + (spaces2[i] * norm_val), mean, yerr=std, width=norm_val, color='none',
                            edgecolor='black', linewidth=1,
                            error_kw=dict(lw=1, capsize=2, capthick=0.5))  # white empty to center std and outline
                else:
                    plt.bar(x + (spaces2[i] * norm_val), mean, yerr=std, width=norm_val,
                            color=color_map[ver], edgecolor='black', error_kw= dict(lw=1, capsize = 2, capthick=0.5))

                if ver not in legend:
                    legend.append(ver)
                else:
                    legend.append("_None")  # ignored labels
                if i == 0: # since at 0 we plot another bar we need to offset the legend again by adding a none entry
                    legend.append("_None")
            x += width + 4 * norm_val + 1.5
            ticks.append(x+0.5)
    # Legend
    ticks = ticks[:-1] # last entry is superflous
    ticks[-1] += 2

    legend2 = []
    handle= []

    for key, color in color_map.items():
        if key == "41":
            continue
        if len(key) == 1 or 'base' in key:
            p = Patch(facecolor=color, edgecolor='black', linewidth=1)

        else:
            c1 = color_map[key[0]]
            c2 = color_map[key[1]]
            if len(key) == 2:

                p = MulticolorPatch([c1, c2])
            else:
                c3 = color_map[key[2]]
                p = MulticolorPatch([c1,c2,c3])
        if 'base' in key:
            legend2.append("non-spec") # change name
        else:
            legend2.append(legend_map[key])
        handle.append(p)

    ticks_label = [r"\cref{lst:v1-v4-combined}", r"\cref{lst:v1-v5-comb}", r"\cref{lst:v4-v5-comb}",
                       r"\cref{lst:v1-v4-v5-comb}"]
    ticks_label += [label + "\n Fence"  for label in ticks_label]

    plt.xticks(ticks, ticks_label, multialignment="left",)  #fontsize="small",)

    plt.legend(handle, legend2, loc='upper right', bbox_to_anchor=(1.2, 1), ncol=1,
               handler_map={MulticolorPatch: MulticolorPatchHandler()},
               fancybox=False, shadow=False,fontsize='small')
    #legend_ = plt.legend(handle, legend2, loc='upper center', bbox_to_anchor=(1.2, 1), ncol=1,
    #                    handler_map={MulticolorPatch: MulticolorPatchHandler()},
    #                    fancybox=False, shadow=False, fontsize='small')
    plt.ylabel("Execution Time (milliseconds)",)# fontsize='small')


    plt.savefig("exec_times_comb.pgf", bbox_inches="tight")

from collections import defaultdict
import itertools
def evaluateV4V5(V4=True):

    file_dict = {"14": [], "15": [], "45": [], "145": []}
    time_dict = {}
    if V4:
        for t in itertools.chain(V4_path.glob("*.txt"), V4_path_barr.glob("*.txt")):
            # create key
            print(t.name)
            file_name = (t.name.split("."))[0]
            print(file_name)
            if "barrier" in file_name:

                barr, name = file_name.split("_")[1:3]
                key = name + "_" + barr
            else:
                key = (file_name.split("_"))[1]

            # get time
            with open(t) as f:
                print(t)
                time = json.load(f)["total_time"]
            time_dict[key] = time
        print(time_dict)
    else:
        for t in itertools.chain(V5_path.glob("*.txt")):
            # create key
            print(t.name)
            file_name = (t.name.split("."))[0]
            print(file_name)
            key = file_name

            # get time
            with open(t) as f:
                time = json.load(f)["total_time"]
            time_dict[key] = time
        print(time_dict)

    #print(group_dict)
    df = pd.DataFrame.from_dict(time_dict, orient='index')
    print(df)


# Explanation on how to use the script:
# use exec_loop to run the script executing the combination test cases a 1000 times. This saves the data.
# Do this once with barrier=True and once for false to collect both
# Then use evaluate_Comb to generate the pgf plot.
# Because of this bug https://github.com/matplotlib/matplotlib/issues/15491 you need to patch the pgf backend yourself
# by applying the solution proposed in the thread.
# evaluateV4V5 can be used as well to get the times of V4 and V5. However, only for a single run right now.
if __name__ == '__main__':
    #evaluateV4V5(V4=False)

    exec_loop(barrier=True)
    #evaluate_Comb()

