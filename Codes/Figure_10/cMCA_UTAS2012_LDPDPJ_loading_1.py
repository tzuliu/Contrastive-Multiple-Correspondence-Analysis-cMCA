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

## Set up color
tableau10 = {
    'orange': '#F08E39',
    'blue': '#507AA6',
    'red': '#DF585C',
    'teal': '#78B7B2',
    'green': '#5BA053',
    'yellow': '#ECC854',
    'purple': '#AF7BA1',
    'pink': '#FD9EA9',
    'brown': '#9A7460',
    'gray': '#BAB0AC',
    0: '#F08E39',
    1: '#507AA6',
    2: '#DF585C',
    3: '#78B7B2',
    4: '#5BA053',
    5: '#ECC854',
    6: '#AF7BA1',
    7: '#FD9EA9',
    8: '#9A7460',
    9: '#800000',
    -1: '#BAB0AC',
    'LDP': '#507AA6',
    'DPJ': '#F08E39',
    "JRP": '#DF585C',
    'DEM': '#507AA6',
    'REP': '#F08E39',
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
X_ldp, X_dpj, X_jrp = csv_to_mats('.utas12_ooc.csv', rtype="v", jrp=True)
X_ldp['policy00'] = X_ldp['policy00'].replace([0,3,4,5,6,7,8,9,10], [1,2,3,3,3,4,4,5,5])
X_dpj['policy00'] = X_dpj['policy00'].replace([0,3,4,5,6,7,8,9,10], [1,2,3,3,3,4,4,5,5])
X_jrp['policy00'] = X_jrp['policy00'].replace([0,3,4,5,6,7,8,9,10], [1,2,3,3,3,4,4,5,5])
X = pd.concat([X_ldp, X_dpj, X_jrp])
print(X_ldp.shape, X_dpj.shape, X_jrp.shape, X.shape)

##Disctionay for Level and Party
party = {"LDP":"LDP", "DPJ":"DPJ", "JRP":"JRP"}

##Fitting cMCA and export plots
cmca = CMCA(n_components=2, copy=True, check_input=True)
cmca = cmca.fit(fg=X_ldp.iloc[:,6:X.shape[1]], bg=X_dpj.iloc[:,6:X.shape[1]], alpha=1.5)
Y_fg = np.array(cmca.transform(X_ldp.iloc[:,6:X.shape[1]]))
Y_bg = np.array(cmca.transform(X_dpj.iloc[:,6:X.shape[1]]))
Y_fg_col = np.array(cmca.transform(X_ldp.iloc[:,6:(X.shape[1])], axis='col'))
prefix_to_info = cmca.gen_prefix_to_info()

used_others_label = False
f = plt.figure()
for key in prefix_to_info.keys():
    indices = prefix_to_info[key]['indices']
    rank_1 = prefix_to_info[key]['loading_ranks_norm_0']
    rank_1 = rank_1 if rank_1 < 9 else -1
    texts = [int(float(postfix)) for postfix in prefix_to_info[key]['postfixes']]
    label = key if rank_1 >= 0 else 'others'
    if label == 'others':
        if used_others_label:
            label = None
        else:
            used_others_label = True

    plt.scatter(cmca.loadings[indices, 0], cmca.loadings[indices, 1], c=tableau10[rank_1], label=label)
    for i, txt in enumerate(texts):
        plt.annotate(txt, (cmca.loadings[indices[i], 0], cmca.loadings[indices[i], 1]), fontsize=8)
plt.title('Loadings (cPC1)')
xpad = (cmca.loadings[:, 0].max() - cmca.loadings[:, 0].min()) * 0.1
ypad = (cmca.loadings[:, 1].max() - cmca.loadings[:, 1].min()) * 0.1
plt.xlim([cmca.loadings[:, 0].min() - xpad, cmca.loadings[:, 0].max() + xpad])
plt.ylim([cmca.loadings[:, 1].min() - ypad, cmca.loadings[:, 1].max() + ypad])
plt.tight_layout()
handles, labels = plt.gca().get_legend_handles_labels()
handles = [handles[9], handles[8], handles[7], handles[4], handles[3], handles[6], handles[1], handles[2], handles[5], handles[0]]
labels = [labels[9], labels[8], labels[7], labels[4], labels[3], labels[6], labels[1], labels[2], labels[5], labels[0]]
plt.xlabel('cPC1')
plt.ylabel('cPC2')
plt.legend(handles, labels, loc='best', shadow=False, scatterpoints=1, fontsize=8)
plt.show()
f.savefig("cMCA_UTAS2012_ldpdpj_loading_1.pdf", bbox_inches='tight')
