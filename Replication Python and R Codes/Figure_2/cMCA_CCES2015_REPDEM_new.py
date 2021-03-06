import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import prince
from sklearn.cluster import DBSCAN
import itertools
from cmca import CMCA
from ccmca import CCMCA
plt.style.use('ggplot')

alpha = r'$ \alpha $'

tableau10 = {
    'blue': '#507AA6',
    'orange': '#F08E39',
    'red': '#DF585C',
    'teal': '#78B7B2',
    'green': '#5BA053',
    'yellow': '#ECC854',
    'purple': '#AF7BA1',
    'pink': '#FD9EA9',
    'brown': '#9A7460',
    'gray': '#BAB0AC',
    1: '#507AA6',
    0: '#F08E39',
    2: '#DF585C',
    3: '#78B7B2',
    4: '#5BA053',
    5: '#ECC854',
    6: '#AF7BA1',
    7: '#FD9EA9',
    8: '#9A7460',
    9: '#BAB0AC',
    -1: '#BAB0AC',
    'LDP': '#507AA6',
    'DPJ': '#F08E39',
    "JRP": '#DF585C',
    'DEM': '#507AA6',
    'REP': '#F08E39',
}

def fillna_based_on_dtype(df):
    for key in dict(df.dtypes).keys():
        if df.dtypes[key] == np.object:
            df[key] = df[key].fillna('na')
        else:
            df[key] = df[key].fillna(99)

def csv_to_mats_2(csv):
    df = pd.read_csv(csv)

    X = df.iloc[:,np.r_[0:df.shape[1]]]

    X_d = X[X["partyid"] <= 3]
    X_r = X[X["partyid"] >= 5]

    print("missing value ratio (DEM)", X_d.isna().sum().sum() / (X_d.shape[0] * X_d.shape[1]))
    print("missing value ratio (REP)", X_r.isna().sum().sum() / (X_r.shape[0] * X_r.shape[1]))

    fillna_based_on_dtype(X_d)
    fillna_based_on_dtype(X_r)

    return (X_d, X_r)

X_d, X_r = csv_to_mats_2('./issuevalue_short.csv')
X_d['partyid'] = X_d['partyid'].replace([3, 2], 1)
X_r['partyid'] = X_r['partyid'].replace([5, 6, 7], 2)
X = pd.concat([X_d, X_r])

##Disctionay for Level and Party
party = {1:"Dem", 2:"Rep"}

cmca = CMCA(n_components=2, copy=True, check_input=True)
cmca = cmca.fit(fg=X_r.iloc[:,1:(X.shape[1]-1)], bg=X_d.iloc[:,1:(X.shape[1]-1)], alpha=1.5)
rep_con = list()
for u,v,w in zip(X_r['socialideology'],X_r['economicideology'],X_r['natsecurityideology']):
    if u==3 or v==3 or w==3:
        rep_con.append(2)
    else:
        rep_con.append(3)

Y_fg = np.array(cmca.transform(X_r.iloc[:,1:(X.shape[1]-1)]))
Y_bg = np.array(cmca.transform(X_d.iloc[:,1:(X.shape[1]-1)]))
Y_fg_col = np.array(cmca.transform(X_r.iloc[:,1:(X.shape[1]-1)], axis='col'))
prefix_to_info = cmca.gen_prefix_to_info()

X_r["rep_con"] = rep_con

f = plt.figure()
plt.xlim([-1.5, 1])
plt.ylim([-2, 1.5])
for label in X_r['rep_con'].unique():
    plt.scatter(Y_fg[X_r['rep_con'] == label, 0], Y_fg[X_r['rep_con'] == label, 1], c=tableau10[label], label=label, alpha=0.3, linewidths=0)
plt.scatter(Y_bg[:, 0], Y_bg[:, 1], c=tableau10[X_d["partyid"].iloc[0]], label=party[X_d["partyid"].iloc[0]], alpha=0.8, linewidths=0)
handles, labels = plt.gca().get_legend_handles_labels()
labels = ["Rep_Con", "Rep_Oth", "Dem"]
plt.legend(handles, labels, loc="lower right", shadow=False, scatterpoints=1, fontsize=8)
plt.xlabel('cPC1')
plt.ylabel('cPC2')
plt.title("cMCA (tg: Rep, bg: Dem, " + str(alpha) + ": 1.5)")
plt.show()
f.savefig("cMCA_CCES2015_repdem_new.pdf", bbox_inches='tight')
