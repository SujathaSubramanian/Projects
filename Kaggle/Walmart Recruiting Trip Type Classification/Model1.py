# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 14:55:23 2015

@author: sujat
"""
from sklearn import ensemble
from sklearn.calibration import CalibratedClassifierCV
from sklearn.cross_validation import StratifiedKFold, StratifiedShuffleSplit
from sklearn.metrics import log_loss
from sklearn.ensemble import GradientBoostingClassifier
import scipy as sp
import gc
import numpy as np
import os
import pandas as pd



def xgBoost_cv(x, y, calibrate=False):
    skf = StratifiedKFold(y, n_folds=5, random_state=23)
    original_params = {'n_estimators': 1000, 'max_leaf_nodes': 4, 'max_depth': None, 'random_state': 2,
                   'min_samples_split': 5}

    for label, color, setting in [('No shrinkage', 'orange',
                               {'learning_rate': 1.0, 'subsample': 1.0}),
                              ('learning_rate=0.1', 'turquoise',
                               {'learning_rate': 0.1, 'subsample': 1.0}),
                              ('subsample=0.5', 'blue',
                               {'learning_rate': 1.0, 'subsample': 0.5}),
                              ('learning_rate=0.1, subsample=0.5', 'gray',
                               {'learning_rate': 0.1, 'subsample': 0.5}),
                              ('learning_rate=0.1, max_features=2', 'magenta',
                               {'learning_rate': 0.1, 'max_features': 2})]:
                                   
        params = dict(original_params)
        params.update(setting)

    scores, predictions = [], None
    clf = ensemble.GradientBoostingClassifier(**params)
    
    for train_index, test_index in skf:
        '''if calibrate:
            # Make training and calibration
            calibrated_classifier = CalibratedClassifierCV(classifier, method='isotonic', cv=get_cv(y[train_index]))
            fitted_classifier = calibrated_classifier.fit(x[train_index, :], y[train_index])
        else:
        '''
        #print( train_index)
        #print train[train_index]
        fitted_classifier = clf.fit(x[train_index, :], y[train_index])
        preds = fitted_classifier.predict_proba(x[test_index, :])
        # Free memory
        '''calibrated_classifier = None
        '''
        fitted_classifier = None
        gc.collect()

        scores.append(log_loss(y[test_index], preds))
        predictions = np.append(predictions, preds, axis=0) if predictions is not None else preds
    return scores, predictions

def xgBoost_fit(train, labels,test):
    params = {'n_estimators': 1000, 'max_leaf_nodes': 4, 'max_depth': None, 'random_state': 2,
                       'min_samples_split': 5,'learning_rate':0.1,'max_features':2}

    scores, predictions = [], None
    clf = ensemble.GradientBoostingClassifier(**params)
    
    clf.fit(train, labels)
    predictions = clf.predict_proba(test)
    # Free memory
    '''calibrated_classifier = None
    '''
    fitted_classifier = None
    gc.collect()
    scores = log_loss(labels, predictions)
    return scores, predictions
    
def cross_entropy(pred, label):
    """
    :param pred: prediction matrix with shape (n_pred, n_class),
                 each row is rescaled to sum to 1
    :param label: a list/array with label[i] = index of correct class,
                  each element shoudl be on the interval [0, n_class - 1]
    :return: cross entropy / multiclass logarithmic loss
    """
    tiny = np.ones(pred.shape) * 1e-15
    pred = np.max((np.min((pred, 1-tiny), 0), tiny), 0)
    pred = (pred.T / pred.sum(1)).T
    return -np.mean([np.log(p[label[i]]) for i, p in enumerate(pred)])


def llfun(act, pred):
    epsilon = 1e-15
    pred = sp.maximum(epsilon, pred)
    pred = sp.minimum(1-epsilon, pred)
    ll = sum(act*sp.log(pred) + sp.subtract(1,act)*sp.log(sp.subtract(1,pred)))
    ll = sum(ll)
    ll = ll * -1.0/len(act)
    return ll

def write_blender_data(path, file_name, predictions):
    file_path = os.path.join(path, file_name)
    np.savetxt(file_path, predictions, delimiter=',', fmt='%.5f')
    
def save_submission(path_sample_submission, output_file_path, predictions):
    sample = pd.read_csv(path_sample_submission)
    #submission = pd.DataFrame(predictions, index=sample[0:30000].VisitNumber.values, columns=[3,4,5,6,7,8,9])
    submission = pd.DataFrame(predictions, index=sample.VisitNumber.values, columns=sample.columns[1:])
    submission.to_csv(output_file_path, index_label='VisitNumber')
    
#sample.columns[1:]

MODE = 'cv'
MODEL_NAME = 'XGBoost'
BLEND_PATH = os.getcwd()+'\OneDrive\Coursera\Projects\Kaggle\Walmart Recruiting - Trip type classification\CV'
DATA_SAMPLE_SUBMISSION_PATH = os.getcwd() +'\OneDrive\Coursera\Projects\Kaggle\Walmart Recruiting - Trip type classification\sample_submission.csv'
SUBMISSION_PATH = os.getcwd() +'\OneDrive\Coursera\Projects\Kaggle\Walmart Recruiting - Trip type classification\Submission'


#train = train_flat[0:30000]
train = train_flat
labels = train.TripType.values
train = train.drop('TripType',axis = 1)
train = np.array(train, dtype=float)
train.shape
labels = np.array(labels,dtype=float)
np.unique(labels)
calibrate = False

#test = np.array(test_flat[0:30000], dtype=float)
test = np.array(test_flat, dtype=float)
test.shape

if MODE == 'cv':
        scores, predictions = xgBoost_cv(train, labels, calibrate=False)
        print 'CV:', scores, 'Mean log loss:', np.mean(scores)
        write_blender_data(BLEND_PATH, MODEL_NAME + '.csv', predictions)

elif MODE == 'submission':
        scores, predictions = xgBoost_fit(train, labels, test)
        print 'CV:', scores, 'Mean log loss:', np.mean(scores)        
        save_submission(DATA_SAMPLE_SUBMISSION_PATH,
                              os.path.join(SUBMISSION_PATH, MODEL_NAME + '.csv'),
                              predictions)
'''
elif MODE == 'holdout':
        train, labels, _, _ = stratified_split(train, labels, test_size=.7)
        score = hold_out_evaluation(clf, train, labels, calibrate=False)
        print 'Log loss:', score
'''
else:
        print 'Unknown mode'


##
#Ran xgboost_fit with not tuned parameters. Got the log loss function as 
#7.99330
##
         
        
