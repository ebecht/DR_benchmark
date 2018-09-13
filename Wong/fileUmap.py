#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  4 22:14:26 2018

@author: evan
"""

#def fileUmap(datafileIn, datafileOut,n,mdist,metric):
    
import sys
import umap
import numpy as np
    # Get the arguments passed in
string = sys.argv[1]
pattern = sys.argv[2]

    # Perform the splitting
ans = string.split(pattern)

datafileIn = ans[0]
datafileOut = ans[1]
n=int(ans[2])
ncomp = int(ans[3])
mdist=float(ans[4])
metric=ans[5]



data = np.loadtxt(open(datafileIn, "rb"), delimiter=",", skiprows=1)

umapOut = umap.UMAP(n_neighbors=n,n_components=ncomp,min_dist=mdist,metric=metric,verbose=True).fit_transform(data)
np.savetxt(datafileOut, umapOut, delimiter=",")

    
    
#string = "testfile.csv---testout.csv---23---23---5"
#pattern = "---"
#ans = string.split(pattern)
#
#ans[1]

#fileUmap("dataForUmap.csv","umapDataOut.csv",15,0.2,"euclidean")

#ebedding = fileUmap("testdata.csv","testdataOut.csv",15,0.2,"euclidean")

#import numpy as np
#import matplotlib as plt
#data = readcsv("testdata.csv")
#umapData = np.asarray(runUmap(data,15,0.2,"euclidean"))
#
#
#
#import matplotlib.pyplot as plt 
#plt.scatter(umapData[:,0],umapData[:,1],s=.1)
#plt.show()
