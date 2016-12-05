# -*- coding: utf-8 -*-
"""
Created on Mon Nov 16 11:15:15 2015

@author: sujat
"""
%matplotlib inline  
import numpy as np
import pandas as pd
import os

import matplotlib.pyplot as plt


import helper

PROJECT_PATH = 'C:/Users/sujat/OneDrive/Coursera/Projects/Kaggle/Walmart Recruiting - Trip type classification'
DATA_TRAIN_PATH = os.path.join(PROJECT_PATH, 'train.csv')
DATA_TEST_PATH = os.path.join(PROJECT_PATH, 'test.csv')


"""
Utility Functions for this project
"""

"""
Data loading from the CSV file given a specific path
"""
def load_data(path_train=DATA_TRAIN_PATH,path_test=DATA_TEST_PATH):
    train = pd.read_csv(DATA_TRAIN_PATH)
    test = pd.read_csv(DATA_TEST_PATH)
    #return np.array(train),np.array(test)
    return train,test


"""
Given the day of the week returns the index
"""

def DayofWeek(day):
    if day == 'Saturday':
        return 1
    elif day == 'Sunday':
        return 2    
    elif day == 'Monday':
        return 3     
    elif day == 'Tuesday':
        return 4     
    elif day == 'Wednesday':
        return 5     
    elif day == 'Thursday':
        return 6     
    elif day == 'Friday':
        return 7     
    else:   
        return -1


"""
Add a ID field for each department description
"""

def AddDeptID():
    global train
    columns = ['DeptID','DepartmentDescription']
    Dept = pd.DataFrame(columns=columns)
    DeptDesc = train['DepartmentDescription'].unique() 
    Dept['DepartmentDescription'] = DeptDesc
    Dept['DeptID'] = range(len(DeptDesc))    
    train = pd.merge(train,Dept,left_on = 'DepartmentDescription',right_on='DepartmentDescription',how='left')

def AddWeekDayID():
    train['WeekDayID'] = train['Weekday'].apply(DayofWeek)
    
def CheckIfNan(DepartmentDescription):
    if DepartmentDescription == np.NaN:
        return 1
    else:
        return 0


"""
def AddSeqID(DepartmentDescription):
    global Dept
    global Count
    DeptID = Dept[Dept['DepartmentDescription'] == DepartmentDescription]['DeptID']
 
    if len(DeptID) < 1:
        Count = len(Dept)+1        
        Dept = Dept.append({'DeptID': Count,'DepartmentDescription': DepartmentDescription},ignore_index=True)
"""
       

"""
Loading and formatting the data
"""
###ToDo -  How to handle Nan,NA and other null data

train,test = load_data()
#AddDeptID()
#AddWeekDayID()
train.head()
train.size
"""
Visualizing the data
"""    

"""
Plot 1: Number of items purchased  per weekday
"""  

"""
Group the data per weekday and get the #of items purchased per weekday
"""


byWeekday = train.groupby('Weekday')
count = byWeekday['VisitNumber'].count()
dfItemCount = pd.DataFrame(count)
dfItemCount.reset_index(inplace=True)
dfItemCount["DayIndex"] = dfItemCount['Weekday'].apply(DayofWeek)
dfItemCount.sort(['DayIndex'],inplace=True)
dfItemCount = dfItemCount[['Weekday','VisitNumber']]
dfItemCount.set_index('Weekday',inplace=True)
dfItemCount.plot(kind='bar')


"""
Triptype Vs Department

"""
train.head()
keys = 'TripType','DepartmentDescription'
byDepartment = train.groupby(keys)
count = byDepartment['DepartmentDescription'].size()
dfDeptCount = pd.DataFrame(count)
dfDeptCount.head(100)
dfDeptCount.reset_index(inplace=True)
TripType = 30
TripType3 = dfDeptCount[(dfDeptCount.TripType) == TripType]
TripType3.set_index('DepartmentDescription',inplace=True)
TripType3[0]
TripType3.plot(kind='bar')



"""
Scatter plot of DeptID and TripType
"""
train_without999 = train[train['TripType'] != 999]
labels = range(7)
fig = plt.figure()
ax = fig.add_subplot(111)
scatter1 = ax.scatter(x=train_without999['DeptID'],y=train_without999['TripType'],
            c=train_without999['WeekDayID'],alpha=.5)

ax.set_xlabel('X', fontsize = 10)
ax.set_ylabel('Y', fontsize = 10)
ax.legend([scatter1], ['Saturday','Sunday', 'Monday','Tuesday','Wednesday','Thursday','Friday'])
plt.show()
train_without999['Weekday'].unique()



"""
Creating features for each categorized column
"""

import pandas as pd 
from sklearn.feature_extraction import DictVectorizer 

 
def one_hot_dataframe(data, cols, replace=False): 
     """ Takes a dataframe and a list of columns that need to be encoded. 
         Returns a 3-tuple comprising the data, the vectorized data, 
         and the fitted vectorizor.""" 
     vec = DictVectorizer(sparse=False) 
     vecData = pd.DataFrame(vec.fit_transform(data[cols].T.to_dict().values())) 
     vecData.columns = vec.get_feature_names() 
     vecData.index = data.index 
     if replace is True: 
         data = data.drop(cols, axis=1) 
         data = data.join(vecData) 
     return (data, vecData, vec) 





train_flat = train
cols = ['VisitNumber','Upc']

trainnew,vecData,vec = one_hot_dataframe(train_flat[1:50000],cols,replace=True)


trainnew.info()
train.info()

just_dummies = pd.get_dummies(train['Weekday'])
train_flat = pd.concat([train_flat, just_dummies], axis=1) 
just_dummies = pd.get_dummies(train['DepartmentDescription'])
train_flat = pd.concat([train_flat, just_dummies], axis=1)      



#just_dummies = pd.get_dummies(train[train['VisitNumber'] ==8]['Upc'],prefix = 'UPC_').astype(np.int8)

train.info()
trainnew = train
trainnew['Upc'] = trainnew['Upc'].astype(str)
trainnew.info()

#pd.get_dummies(train['Upc'][:50000],prefix = 'Upc_').info()
#pd.get_dummies(train['Upc'][:50000],prefix = 'Upc_',sparse=True).astype(np.int8).info()

from sklearn.feature_extraction import DictVectorizer
vec = DictVectorizer(sparse = True )

vecItems = trainnew[cols].T.to_dict().values()

vecData = vec.fit_transform(vecItems)

vecData = pd.DataFrame(vecData.toarray())
vecData.columns = vec.get_feature_names()

trainnew[cols].info()
vecData.info()
vecData[[1]]



                           
                              
                              
train_flat = pd.concat([train_flat, just_dummies], axis=1)      

        



"""
Sample code from forum
"""
train.groupby(['Weekday', 'TripType'], as_index=False)['VisitNumber'].agg(uniqueCount)

def uniqueCount(x):
    return np.size(np.unique(x))
    

train['VisitNumber'].head()
train_result = train['VisitNumber'] 
train_result = train_result.groupby(by=train_result.index, sort=False).mean()
train_result
df.groupby(['series_id', 'year']).mean()
 
 
 
 
 
 
    
    