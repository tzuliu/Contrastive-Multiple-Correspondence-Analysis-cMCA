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
Y_fg = np.array(cmca.transform(X_r.iloc[:,1:(X.shape[1]-1)]))
Y_bg = np.array(cmca.transform(X_d.iloc[:,1:(X.shape[1]-1)]))
Y_fg_col = np.array(cmca.transform(X_r.iloc[:,1:(X.shape[1]-1)], axis='col'))
prefix_to_info = cmca.gen_prefix_to_info()

used_others_label = False
f = plt.figure()
for key in prefix_to_info.keys():
    indices = prefix_to_info[key]['indices']
    rank_1 = prefix_to_info[key]['loading_ranks_norm_1']
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
plt.title('Loadings (cPC2)')
xpad = (cmca.loadings[:, 0].max() - cmca.loadings[:, 0].min()) * 0.1
ypad = (cmca.loadings[:, 1].max() - cmca.loadings[:, 1].min()) * 0.1
plt.xlim([cmca.loadings[:, 0].min() - xpad, cmca.loadings[:, 0].max() + xpad])
plt.ylim([cmca.loadings[:, 1].min() - ypad, cmca.loadings[:, 1].max() + ypad])
plt.tight_layout()
handles, labels = plt.gca().get_legend_handles_labels()
handles = [handles[9], handles[0], handles[5], handles[3], handles[2], handles[8], handles[6], handles[7], handles[4], handles[1]]
labels = [labels[9], labels[0],  labels[5], labels[3], labels[2], labels[8], labels[6], labels[7], labels[4], labels[1]]
plt.xlabel('cPC1')
plt.ylabel('cPC2')
plt.legend(handles, labels, loc='lower right', shadow=False, scatterpoints=1, fontsize=7)
plt.show()
f.savefig("cMCA_CCES2015_repdem_loading_2.pdf", bbox_inches='tight')
