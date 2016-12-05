# -*- coding: utf-8 -*-
"""
Created on Tue Dec 01 15:14:32 2015

@author: sujat
"""


batchSize = 10000
total = len(train)

cols = 'Upc'
index = 0

iterations = total/batchSize
iterations = 3
iter = 2

for iter in range(1,iterations):
    dataFlat = pd.get_dummies(train[cols][index+1:iter*batchSize])
    index = index + batchSize
       
    if iter ==1:
        data = dataFlat
    else:
        pd.merge(data,dataFlat)

#    return data
    
    

    