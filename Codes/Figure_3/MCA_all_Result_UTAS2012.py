import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import prince
from sklearn.cluster import DBSCAN
import itertools
from cmca import CMCA
from ccmca import CCMCA
plt.style.use('ggplot')

## Set up color
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
    0: '#507AA6',
    1: '#F08E39',
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
}

## Recode NA by data type
def fillna_based_on_dtype(df):
    for key in dict(df.dtypes).keys():
        if df.dtypes[key] == np.object:
            df[key] = df[key].fillna('na')
        else:
            df[key] = df[key].fillna(99)


## Extract data by parties
def csv_to_mats(csv, rtype="v", jrp=False):
    df = pd.read_csv(csv)
    if rtype == "v":
        df = df[df.cv != "candidate"]
    else:
        df = df[df.cv != "voter"]

    X = df.iloc[:,np.r_[3,7:12,14:df.shape[1]]]

    if jrp:
        X_ldp = X[X["psup_short"] == "LDP"]
        X_dpj = X[X["psup_short"] == "DPJ"]
        X_jrp = X[X["psup_short"] == "JRP"]

        print("missing value ratio (LDP)", X_ldp.isna().sum().sum() / (X_ldp.shape[0] * X_ldp.shape[1]))
        print("missing value ratio (DPJ)", X_dpj.isna().sum().sum() / (X_dpj.shape[0] * X_dpj.shape[1]))
        print("missing value ratio (JRP)", X_jrp.isna().sum().sum() / (X_dpj.shape[0] * X_dpj.shape[1]))

        fillna_based_on_dtype(X_ldp)
        fillna_based_on_dtype(X_dpj)
        fillna_based_on_dtype(X_jrp)
    else:
        X_ldp = X[X["psup_short"] == "LDP"]
        X_dpj = X[X["psup_short"] == "DPJ"]

        print("missing value ratio (LDP)", X_ldp.isna().sum().sum() / (X_ldp.shape[0] * X_ldp.shape[1]))
        print("missing value ratio (DPJ)", X_dpj.isna().sum().sum() / (X_dpj.shape[0] * X_dpj.shape[1]))

        fillna_based_on_dtype(X_ldp)
        fillna_based_on_dtype(X_dpj)


    if jrp:
        return (X_ldp, X_dpj, X_jrp)
    else:
        return (X_ldp, X_dpj)

## Load data
X_ldp, X_dpj, X_jrp = csv_to_mats('./utas12_ooc.csv', rtype="v", jrp=True)
X = pd.concat([X_ldp, X_dpj, X_jrp])
print(X_ldp.shape, X_dpj.shape, X_jrp.shape, X.shape)

##Fitting MCA and export plots
mca = prince.MCA(n_components=2, n_iter=3, copy=True, check_input=True, engine='auto')
mca = mca.fit(X.iloc[:,6:X.shape[1]])
Y = np.array(mca.transform(X.iloc[:,6:X.shape[1]]))
f = plt.figure()
plt.xlim([-1, 6])
plt.ylim([-1, 2])
for label in np.unique(X["psup_short"]):
    plt.scatter(Y[X["psup_short"]==label, 0], Y[X["psup_short"]==label, 1], label=label, c=tableau10[label], alpha=0.6, linewidths=0)
plt.legend(loc="upper right")
plt.xlabel('PC1')
plt.ylabel('PC2')
plt.title("MCA Result of UTAS 2012 (All Parties)")
plt.show()
f.savefig("MCA(all)_Result_UTAS2012.pdf", bbox_inches='tight')
