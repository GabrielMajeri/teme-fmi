"Helper functions for training and evaluating models"

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

from sklearn.metrics import classification_report, roc_auc_score, plot_confusion_matrix
from sklearn.model_selection import KFold, train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

from tqdm.auto import tqdm

def create_pipeline(clf):
    "Create a pipeline which normalizes the input data before feeding it to the model"
    return Pipeline([
        ('scaler', StandardScaler()),
        ('classifier', clf),
    ])

def cross_validate_model(clf, data, labels, n_folds=5, **kwargs):
    pipe = create_pipeline(clf)

    print(f"Testing model using {n_folds}-fold cross validation")

    scores = []
    kfold = KFold(n_folds)
    for train_index, validation_index in tqdm(kfold.split(data), total=kfold.get_n_splits(data)):
        X_train = data[train_index]
        y_train = labels[train_index]

        pipe.fit(X_train, y_train, **kwargs)

        X_validation = data[validation_index]
        y_validation = labels[validation_index]

        score = pipe.score(X_validation, y_validation)

        scores.append(score)

    print("Validation accuracies:", scores)
    print("Mean:", np.mean(scores))

def create_logistic_regression_model():
    from sklearn.linear_model import LogisticRegression

    return LogisticRegression(C=1e-4)

def create_linear_svc_model():
    from sklearn.linear_model import SGDClassifier

    # With the default hinge loss, SGDClassifier
    # trains a Support Vector Classifier
    return SGDClassifier()

def create_xgb_classifier_model():
    from xgboost import XGBClassifier

    return XGBClassifier(n_jobs=2, verbosity=1)

def train_model_and_generate_submission(clf, data, labels, test_data):
    "Train a model on the whole labeled dataset, then generate predictions"

    pipe = create_pipeline(clf)

    X_test = process_map(process_data, test_data, max_workers=4)
    X_test = scaler.transform(X_test)

    # Run the pipeline on the unlabeled data to generate predictions
    test_preds = pipe.predict(X_test)

    # Store them in the DataFrame
    predictions_df = test_df.assign(label=test_preds)
    predictions_df.head()

    # Save the predictions to a text file
    predictions_df.to_csv('submission.txt')

def compute_model_statistics(clf, data, labels):
    "Compute and display model statistics on a labeled dataset"
    y_pred = clf.predict(data)

    print("AUC:", roc_auc_score(y_pred, labels))
    print(classification_report(y_pred, labels))

    plot_confusion_matrix(clf, data, labels, labels=[0, 1], values_format='d')
    plt.show()
