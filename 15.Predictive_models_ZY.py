import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt




file="1to1_rf_imputed_OHE_170422.csv"
data = pd.read_csv(file)
vars_to_exlcude = ['IDs', 'Unnamed: 0']
data0 = data.drop(vars_to_exlcude, axis=1)

#recode sex variable with binary digits
data0['sex'] = data0['sex'].replace(['Female','Male'],[1,0])
data0['sex'] = data0['sex'].astype('category')

#recode skin_color variable with binary digits
data0['skin_colour'] = data0['skin_colour'].replace(['Dark','Light'],[1,0])
data0['skin_colour'] = data0['skin_colour'].astype('int')

#rearrange outcome variable (encoded as the final variable)
y = data.loc[:,'final']
y = list(y)
data0['response'] = y
data2 = data0.drop(['final'], axis=1)


##Variables with selection proportion over 80%

var_80 = ['current_tobacco_smoking', 'ethnic_background', 'monocyte_count',
         'standing_height','total_protein', 'vitamin_d', 'weight','average_total_household_income_before_tax_2',
         'hair_colour_natural_before_greying_Black', 'use_of_sun_uv_protection_3',
         'PRSs', 'phosphate', 'hair_colour_natural_before_greying_Red', 'country_of_birth_uk_elsewhere',
         'skin_colour', 'types_of_transport_used_excluding_work', 'igf',
         'glucose2','alcohol_drinker_status_0', 'own_or_rent_accommodation_lived_in',
         'eosinophill_count', 'smoking_status_0', 'childhood_sunburn_occasions_2',
         'morning_evening_person_chronotype', 'testosterone', 'getting_up_in_morning',
         'childhood_sunburn_occasions_0', 'current_employment_status', 'close_to_major_road',
         'use_of_sun_uv_protection_2', 'year_of_birth', 'frequency_of_solarium_sunlamp_use_2',
         'lamb_mutton_intake', 'neutrophill_count', 'traffic_intensity_on_the_nearest_major_road',
         'systolic_blood_pressure_automated_reading', 'reticulocyte_count',
         'greenspace_percentage_buffer_1000m', 'non_oily_fish_intake', 'response']
#print(len(var_80))
data_80 = data2.filter(var_80)
data1 = data_80
data1



from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
X = data1.loc[:, data1.columns != 'response']
y = data1['response']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=0)

scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)


###SVM
# Imports
from sklearn.datasets import make_blobs
from sklearn.model_selection import train_test_split
import numpy as np
import matplotlib.pyplot as plt
from sklearn import svm
from sklearn.metrics import plot_confusion_matrix
from sklearn.svm import SVC  
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, confusion_matrix  
%matplotlib inline
from sklearn.model_selection import GridSearchCV

param_grid = {'C': [0.1, 1, 10], 'gamma': [1,0.1,0.01],'kernel': ['linear','rbf']}
grid = GridSearchCV(SVC(probability=True),param_grid,refit=True,verbose=2, cv=10)
grid.fit(X_train,y_train)
print(grid.best_estimator_)
grid_predictions = grid.predict(X_test)
print(grid.best_params_)


best_kernel = grid.best_params_['kernel']
best_C = grid.best_params_['C']
best_gamma = grid.best_params_['gamma']
print(best_kernel,best_C,best_gamma)


# Initialize SVM classifier
clf = svm.SVC(kernel=best_kernel, probability=True, C=best_C, gamma=best_gamma)

# Fit data
clf = clf.fit(X_train, y_train)

# Predict the test set
svm_predictions = clf.predict(X_test)
svm_probs = clf.predict_proba(X_test)[:, 1]



##RF
from sklearn.ensemble import RandomForestClassifier
# Create the model with 10000 trees
#n_estimatorsint, default=100 (The number of trees in the forest)

model = RandomForestClassifier(n_estimators=5000, 
                               bootstrap = True,
                               max_features = 'sqrt')
model.fit(X_train, y_train)

# Actual class predictions
rf_predictions = model.predict(X_test)
# Probabilities for each class
rf_probs = model.predict_proba(X_test)[:, 1]


##Logistic regression
from sklearn.linear_model import LogisticRegressionCV
param_grid = [0.0001]#, 0.01, 0.1, 1, 10] 
model = LogisticRegressionCV(cv=10, random_state=0, Cs = param_grid).fit(X_train, y_train)

lr_predictions = model.predict(X_test)
lr_probs = model.predict_proba(X_test)[:, 1]


##DT

from sklearn.tree import DecisionTreeClassifier
params = {'max_leaf_nodes': list(range(2, 50)), 'min_samples_split': [2, 3, 4, 5]}
grid_search_cv = GridSearchCV(DecisionTreeClassifier(random_state=42), params, verbose=1, cv=10)
grid_search_cv.fit(X_train, y_train)
grid_search_cv.best_estimator_


# Actual class predictions
dt_predictions = grid_search_cv.predict(X_test)
# Probabilities for each class
dt_probs = grid_search_cv.predict_proba(X_test)[:, 1]




from sklearn.metrics import roc_curve
from sklearn.metrics import roc_auc_score
from matplotlib import pyplot


# Calculate roc auc
ns_probs = [0 for _ in range(len(y_test))]
ns_fpr, ns_tpr, _ = roc_curve(y_test, ns_probs)
rf_fpr, rf_tpr, _ = roc_curve(y_test, rf_probs)
lr_fpr, lr_tpr, _ = roc_curve(y_test, lr_probs)
dt_fpr, dt_tpr, _ = roc_curve(y_test, dt_probs)
grid_fpr, grid_tpr, _ = roc_curve(y_test, svm_probs)

lr_roc_value = roc_auc_score(y_test, lr_probs)
rf_roc_value = roc_auc_score(y_test, rf_probs)
svm_roc_value = roc_auc_score(y_test, dt_probs)
dt_roc_value = roc_auc_score(y_test, svm_probs)


plt.figure()
plt.plot(
    lr_fpr, lr_tpr,
    label="Logistic Regression (AUC = {0:0.2f})".format(lr_roc_value),
    color="deeppink",
    linestyle=":",
    linewidth=4,
)


plt.plot(
    rf_fpr, rf_tpr,
    label="Random Forest (AUC = {0:0.2f})".format(rf_roc_value),
    color="navy",
    linestyle=":",
    linewidth=4,
)


plt.plot(
    grid_fpr, grid_tpr,
    label="SVM (AUC = {0:0.2f})".format(svm_roc_value),
    color="cornflowerblue",
    linestyle=":",
    linewidth=4,
)


plt.plot(
    dt_fpr, dt_tpr,
    label="Decision Tree (AUC = {0:0.2f})".format(dt_roc_value),
    color="darkorange",
    linestyle=":",
    linewidth=4,
)

plt.plot([0, 1], [0, 1], "k--", lw=2)
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.0])
plt.xlabel("False Positive Rate")
plt.ylabel("True Positive Rate")
#plt.title("ROC Curve of Different Binary Classifiers with lasso-selected variables")
plt.legend(loc="lower right")
plt.savefig("ROC_Curve_of_Different_Binary_Classifiers.pdf")




#Import scikit-learn metrics module for accuracy calculation
from sklearn import metrics

# Model Accuracy: how often is the classifier correct?
print("Accuracy_svm:",metrics.accuracy_score(y_test, svm_predictions))
print("Accuracy_rf:",metrics.accuracy_score(y_test, rf_predictions))
print("Accuracy_lr:",metrics.accuracy_score(y_test, lr_predictions))
print("Accuracy_dt:",metrics.accuracy_score(y_test, dt_predictions))

# Model Precision: what percentage of positive tuples are labeled as such?
print("Precision_svm:",metrics.precision_score(y_test, svm_predictions))
print("Precision_rf:",metrics.precision_score(y_test, rf_predictions))
print("Precision_lr:",metrics.precision_score(y_test, lr_predictions))
print("Precision_dt:",metrics.precision_score(y_test, dt_predictions))

# Model Recall: what percentage of positive tuples are labelled as such?
print("Recall_svm:",metrics.recall_score(y_test, svm_predictions))
print("Recall_rf:",metrics.recall_score(y_test, rf_predictions))
print("Recall_lr:",metrics.recall_score(y_test, lr_predictions))
print("Recall_dt:",metrics.recall_score(y_test, dt_predictions))






























































































