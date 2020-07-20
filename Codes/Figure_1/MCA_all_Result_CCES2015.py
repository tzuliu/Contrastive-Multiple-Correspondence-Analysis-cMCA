import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import prince
from sklearn.cluster import DBSCAN
import itertools
from cmca import CMCA
from ccmca import CCMCA
plt.style.use('ggplot')

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

X_d, X_r = csv_to_mats_2('./issuevalue.csv')
X_d['partyid'] = X_d['partyid'].replace([3, 2], 1)
X_r['partyid'] = X_r['partyid'].replace([5, 6, 7], 2)
X = pd.concat([X_d, X_r])

##Disctionay for Level and Party
party = {1:"Dem", 2:"Rep"}

##Fitting MCA and export plots
mca = prince.MCA(n_components=2, n_iter=3, copy=True, check_input=True, engine='auto')
mca = mca.fit(X.iloc[:,1:(X.shape[1]-1)])
Y = np.array(mca.transform(X.iloc[:,1:(X.shape[1]-1)]))
f = plt.figure()
plt.xlim([-1.3, 1.3])
plt.ylim([-1, 4])
for label in np.unique(X["partyid"]):
    plt.scatter(Y[X["partyid"]==label, 0], Y[X["partyid"]==label, 1], c=tableau10[label], label=party[label], alpha=0.6, linewidths=0)
plt.legend(loc="upper left")
plt.xlabel('PC1')
plt.ylabel('PC2')
plt.title("MCA Result of CCES 2015")
plt.show()
f.savefig("MCA(all)_Result_CCES2015.pdf", bbox_inches='tight')
