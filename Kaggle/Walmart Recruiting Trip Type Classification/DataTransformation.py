# -*- coding: utf-8 -*-
"""
Created on Wed Dec 02 21:11:23 2015

@author: sujat
"""


"""
Flatten a data by converting the following categorical columns into binary
Column 1 : Weekday
           Department Description
           
"""

import gc
import numpy as np
import os
import pandas as pd

import matplotlib.pyplot as plt
from sklearn.feature_extraction import DictVectorizer 
from sklearn.feature_extraction import DictVectorizer as DV 

os.getcwd()
PROJECT_PATH = os.getcwd()+ '/OneDrive/Coursera/Projects/Kaggle/Walmart Recruiting - Trip type classification'


DATA_TRAIN_PATH = os.path.join(PROJECT_PATH, 'train.csv')
DATA_TEST_PATH = os.path.join(PROJECT_PATH, 'test.csv')


"""
Data loading from the CSV file given a specific path
"""
def load_data(path_train=DATA_TRAIN_PATH,path_test=DATA_TEST_PATH):
    train = pd.read_csv(DATA_TRAIN_PATH)
    test = pd.read_csv(DATA_TEST_PATH)
    #return np.array(train),np.array(test)
    return train,test
    

    
train,test = load_data()
#AddDeptID()
#AddWeekDayID()
train.head()
train.size



def flatten_data(data,cols):
    train_flat = data    
    just_dummies = pd.get_dummies(data[cols]).astype(np.int8)
    train_flat = pd.concat([train_flat, just_dummies], axis=1)
    colIndex = train_flat.columns.get_loc(cols)
    train_flat.drop(train_flat.columns[colIndex],axis = 1,inplace = True)
    return train_flat    
    
###Flatten the data for the Weekday column
data = train[['TripType','VisitNumber','Weekday']]
cols = 'Weekday'
train_flat_wd = flatten_data(data,cols)
train_flat_wd = train_flat_wd.groupby(['TripType','VisitNumber']).max()
train_flat_wd.head()

data = test[['VisitNumber','Weekday']]
cols = 'Weekday'
test_flat_wd = flatten_data(data,cols)
test_flat_wd = test_flat_wd.groupby(['VisitNumber']).max()
test_flat_wd.head()


###Flatten the data for the Department Description
data = train[['TripType','VisitNumber','DepartmentDescription']]
cols = 'DepartmentDescription'

len(train['DepartmentDescription'].unique())

train_flat_dd = flatten_data(data,cols)
train_flat_dd = train_flat_dd.groupby(['TripType','VisitNumber']).sum()
train_flat_dd.head()


data = test[['VisitNumber','DepartmentDescription']]
cols = 'DepartmentDescription'
test_flat_dd = flatten_data(data,cols)
test_flat_dd = test_flat_dd.groupby(['VisitNumber']).sum()
test_flat_dd.head()



###Combine the ScanCount for each VisitNumber
train_flat_sc = train.groupby(['TripType','VisitNumber'])['ScanCount'].sum()
train_flat_sc.head()
test_flat_sc = test.groupby(['VisitNumber'])['ScanCount'].sum()
test_flat_sc.head()



### Merge the columns to obtain a flattened training set and testing set
train_flat = pd.concat([train_flat_wd, train_flat_dd,train_flat_sc],axis =1) 
test_flat = pd.concat([test_flat_wd, test_flat_dd,test_flat_sc],axis =1) 
train_flat.reset_index(inplace = True)
test_flat.reset_index(inplace = True)

train_flat.shape
test_flat.shape


###Unit Testing scripts
'''
Find set of records from train to verify
train[train['ScanCount'] == max(train['ScanCount'])]##142397
train[train['VisitNumber'] == 142397]
dept = train_flat[train_flat['VisitNumber'] == 142397]
dept['PERSONAL CARE']
'''

traincol = np.array(train_flat.columns)
testcol = np.array(test_flat.columns)
c=np.in1d (traincol,testcol)
traincol[c == False]


test_flat['HEALTH AND BEAUTY AIDS']=0



'''
###Vectorize sample
train.fillna( 'NA', inplace = True ) 
newtrain = train[['VisitNumber','DepartmentDescription']].T.to_dict().values()

newtrain = train[['VisitNumber','DepartmentDescription']].T.to_dict()


newtrain[["VisitNumber"]]

vectorizer = DV( sparse = False ) 
vec_x_cat_train = vectorizer.fit_transform( newtrain ) 
train[train['VisitNumber'] == 7]
vec_x_cat_train[1]
vectorizer.get_feature_names()
np.hstack(( x_num_train, vec_x_cat_train )) 


train.dtypes
newtrain = train[['VisitNumber','FinelineNumber']].T.to_dict().values()
vectorizer = DV( sparse = False  ) 
vec_x_cat_train = vectorizer.fit_transform( newtrain ) 
train[train['VisitNumber'] == 7]
vec_x_cat_train[1]
vectorizer.get_feature_names()
np.hstack(( x_num_train, vec_x_cat_train )) 

'''



