import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import prince
from sklearn import utils
from sklearn.cluster import DBSCAN
import itertools
from cmca import CMCA
from ccmca import CCMCA
from matplotlib import rc
plt.style.use('ggplot')

df = pd.read_csv("./uk2018.csv")
df["prtclcgb"].replace({5: 8, 9: 8, 10:8, 11:8, 12:8, 13:8, 15:8, 19:8}, inplace=True)
df["prtclcgb"].replace({6: 5}, inplace=True)
df["prtclcgb"].replace({7: 6}, inplace=True)
df["prtclcgb"].replace({8: 7}, inplace=True)

alpha = r'$ \alpha $'

tableau10 = {
    'teal': '#78B7B2',
    'blue': '#507AA6',
    'orange': '#F08E39',
    'red': '#DF585C',
    'green': '#5BA053',
    'purple': '#AF7BA1',
    'yellow': '#ECC854',
    'brown': '#9A7460',
    'pink': '#FD9EA9',
    'gray': '#BAB0AC',
    7: '#9A7460',
    1: '#507AA6',
    2: '#F08E39',
    3: '#DF585C',
    4: '#5BA053',
    0: '#78B7B2',
    6: '#ECC854',
    5: '#AF7BA1',
    8: '#FD9EA9',
    9: '#BAB0AC',
    -1: '#BAB0AC',
    99: '#BAB0AC',
    'LDP': '#507AA6',
    'DPJ': '#F08E39'
}

def fillna_based_on_dtype(df):
    for key in dict(df.dtypes).keys():
        if df.dtypes[key] == np.object:
            df[key] = df[key].fillna('na')
        else:
            df[key] = df[key].fillna(99)

def df_to_mat(df):

    X = df.iloc[:,np.r_[1:(df.shape[1])]]

    X_con = X[X["prtclcgb"] == 1]
    X_lab = X[X["prtclcgb"] == 2]
    X_ldp = X[X["prtclcgb"] == 3]
    X_snp = X[X["prtclcgb"] == 4]
    X_gre = X[X["prtclcgb"] == 5]
    X_uip = X[X["prtclcgb"] == 6]
    X_oth = X[X["prtclcgb"] == 7]

    print("missing value ratio (CON)", X_con.isna().sum().sum() / (X_con.shape[0] * X_con.shape[1]))
    print("missing value ratio (LAB)", X_lab.isna().sum().sum() / (X_lab.shape[0] * X_lab.shape[1]))
    print("missing value ratio (LDP)", X_ldp.isna().sum().sum() / (X_ldp.shape[0] * X_ldp.shape[1]))
    print("missing value ratio (SNP)", X_snp.isna().sum().sum() / (X_snp.shape[0] * X_snp.shape[1]))
    print("missing value ratio (GRE)", X_gre.isna().sum().sum() / (X_gre.shape[0] * X_gre.shape[1]))
    print("missing value ratio (UIP)", X_uip.isna().sum().sum() / (X_uip.shape[0] * X_uip.shape[1]))
    print("missing value ratio (OTH)", X_oth.isna().sum().sum() / (X_oth.shape[0] * X_oth.shape[1]))

    fillna_based_on_dtype(X_con)
    fillna_based_on_dtype(X_lab)
    fillna_based_on_dtype(X_ldp)
    fillna_based_on_dtype(X_snp)
    fillna_based_on_dtype(X_gre)
    fillna_based_on_dtype(X_uip)
    fillna_based_on_dtype(X_oth)

    return(X_con, X_lab, X_ldp, X_snp, X_gre, X_uip, X_oth)

X_con, X_lab, X_ldp, X_snp, X_gre, X_uip, X_oth = df_to_mat(df)

X = pd.concat([X_con, X_lab, X_ldp, X_snp, X_gre, X_uip, X_oth])
print(X_con.shape, X_lab.shape, X_ldp.shape, X_snp.shape, X_gre.shape, X_uip.shape, X_oth.shape, X.shape)

##Disctionay for Level and Party
party = {1:"Con", 2:"Lab", 3:"LD", 4:"SNP", 5:"Green", 6:"UKIP", 7:"Other"}

##Fitting cMCA and export plots
cmca = CMCA(n_components=2, copy=True, check_input=True)
cmca = cmca.fit(fg=X_lab.iloc[:,0:(X_lab.shape[1]-3)], bg=X_con.iloc[:,0:(X_con.shape[1]-3)], alpha=1.5)

Y_fg = np.array(cmca.transform(X_lab.iloc[:,0:(X.shape[1]-3)]))
Y_bg = np.array(cmca.transform(X_con.iloc[:,0:(X.shape[1]-3)]))
Y_fg_col = np.array(cmca.transform(X_lab.iloc[:,0:(X.shape[1]-3)], axis='col'))
prefix_to_info = cmca.gen_prefix_to_info()

f_6 = plt.figure()
plt.xlim([-2.5, 2.5])
plt.ylim([-2.5, 2.5])
plt.scatter(Y_fg[:, 0], Y_fg[:, 1], c=tableau10[X_lab["prtclcgb"].iloc[0]], label=party[X_lab["prtclcgb"].iloc[0]], alpha=0.3, linewidths=0)
plt.scatter(Y_bg[:, 0], Y_bg[:, 1], c=tableau10[X_con["prtclcgb"].iloc[0]], label=party[X_con["prtclcgb"].iloc[0]], alpha=0.3, linewidths=0)
handles, labels = plt.gca().get_legend_handles_labels()
handles = [handles[1],handles[0]]
labels = ["Con","Lab"]
plt.legend(handles, labels, loc="lower right", shadow=False, scatterpoints=1, fontsize=8)
plt.xlabel('cPC1')
plt.ylabel('cPC2')
plt.title("cMCA (tg: LAB, bg: CON, " + str(alpha) + ": 1.5)")
plt.show()
f_6.savefig("cMCA_ESS2018_labcon_org.pdf", bbox_inches='tight')
