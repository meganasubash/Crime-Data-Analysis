# =========================================
# 1. IMPORT LIBRARIES
# =========================================
import pandas as pd
import numpy as np

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import accuracy_score, classification_report

from sklearn.ensemble import RandomForestClassifier
from sklearn.utils.class_weight import compute_class_weight


# =========================================
# 2. LOAD DATASET
# =========================================
df = pd.read_csv("crime_sample.csv")

# Clean column names
df.columns = df.columns.str.strip().str.lower().str.replace(" ", "_")

print("Columns:", df.columns)


# =========================================
# 3. HANDLE MISSING VALUES
# =========================================
df = df.dropna()


# =========================================
# 4. GROUP CRIMES (FOR BINARY TARGET)
# =========================================
def categorize_crime(crime):
    crime = crime.lower()
    
    if any(word in crime for word in ['assault', 'robbery', 'battery']):
        return 'Violent'
    else:
        return 'Non-Violent'

df['crime_binary'] = df['crm_cd_desc'].apply(categorize_crime)

print("\nBinary Class Distribution:\n")
print(df['crime_binary'].value_counts())


# =========================================
# 5. DEFINE FEATURES & TARGET
# =========================================
y = df['crime_binary']
X = df.drop(['crm_cd_desc', 'crime_binary'], axis=1)


# =========================================
# 6. ENCODE TARGET
# =========================================
le = LabelEncoder()
y = le.fit_transform(y)


# =========================================
# 7. ENCODE FEATURES
# =========================================
X = pd.get_dummies(X, drop_first=True)


# =========================================
# 8. TRAIN-TEST SPLIT
# =========================================
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y   
)


# =========================================
# 9. HANDLE CLASS IMBALANCE
# =========================================
classes = np.unique(y_train)
weights = compute_class_weight(class_weight='balanced', classes=classes, y=y_train)

class_weights = dict(zip(classes, weights))

print("\nClass Weights:", class_weights)


# =========================================
# 10. TRAIN RANDOM FOREST MODEL
# =========================================
model = RandomForestClassifier(
    n_estimators=400,
    max_depth=20,
    min_samples_split=5,
    class_weight=class_weights,  
    random_state=42,
    n_jobs=-1
)

model.fit(X_train, y_train)


# =========================================
# 11. PREDICTIONS
# =========================================
y_pred = model.predict(X_test)


# =========================================
# 12. EVALUATION
# =========================================
print("\nAccuracy:", accuracy_score(y_test, y_pred))

print("\nClassification Report:\n")
print(classification_report(y_test, y_pred))


# =========================================
# 13. CONVERT BACK TO LABELS (OPTIONAL)
# =========================================
y_pred_labels = le.inverse_transform(y_pred)

print("\nSample Predictions:\n")
print(y_pred_labels[:10])