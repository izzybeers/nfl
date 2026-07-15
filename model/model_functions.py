import numpy as np
import pandas as pd 
import os
from pathlib import Path
from datetime import datetime
from xgboost import XGBClassifier
from sklearn.ensemble import RandomForestClassifier, HistGradientBoostingClassifier
from sklearn.inspection import permutation_importance
from sklearn.model_selection import RandomizedSearchCV, StratifiedKFold
from sklearn.calibration import CalibratedClassifierCV
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.linear_model import LogisticRegression
from scipy.stats import randint, uniform
import os
os.environ["KERAS_BACKEND"] = "torch"
import keras
from keras.models import Sequential
from keras.layers import Input, Dense, Dropout
from keras.callbacks import EarlyStopping
import joblib
from dotenv import load_dotenv
from supabase import create_client, Client

load_dotenv()
supabase: Client = create_client(os.getenv("supabase_endpoint"), os.getenv("supabase_api_key"))

def write_to_supabase_predictions_table(table_name, df):
    data_to_insert = df.to_dict(orient='records')
    response = supabase.schema('predictions').table(table_name).insert(data_to_insert).execute()
    return response

def read_from_supabase(schema, table_name, select = '*', eq_col_name = None, eq_value = None, chunk_size = None):
    if chunk_size is not None:
        df_results = pd.DataFrame()
        start_indx = 0
        while True:
            if eq_col_name is None:
                res = pd.DataFrame(supabase.schema(schema).table(table_name).select(select).range(start_indx, start_indx + chunk_size-1).execute().data)
            else:
                res = pd.DataFrame(supabase.schema(schema).table(table_name).select(select).eq(eq_col_name, eq_value).range(start_indx, start_indx + chunk_size-1).execute().data)
            df_results = pd.concat([df_results, res])
            if len(res) < chunk_size:
                break
            else:
                start_indx = start_indx + chunk_size
        return df_results
    else:
        if eq_col_name is None:
            return(pd.DataFrame(supabase.schema(schema).table(table_name).select(select).execute().data))
        else:
            return(pd.DataFrame(supabase.schema(schema).table(table_name).select(select).eq(eq_col_name, eq_value).execute().data))


def create_model_directories(response, base_folder = 'models'):
    response_folder_path = Path(base_folder) / response
    response_folder_path.mkdir(parents = True, exist_ok = True)
    model_paths = []
    for model_name in ['xgb', 'rf', 'gbm', 'neural_net']:
        model_path = Path(response_folder_path) / model_name
        model_path.mkdir(parents = True, exist_ok = True)
        model_paths.append(model_path)
    return [response_folder_path] + model_paths

def run_xgboost(X_train, Y_train, combos = 100):
    
    base_xgb = XGBClassifier(
        enable_categorical = True,
        eval_metric = 'logloss',
        random_state = 123,
        n_jobs = -1,
        tree_method = 'hist'
    )
    search = RandomizedSearchCV(
        estimator = base_xgb,
        param_distributions = {
            'n_estimators': randint(50,600),
            'max_depth': randint(3,10),
            'learning_rate': uniform(0.01, 0.2),
            'min_child_weight': randint(1,20),
            'subsample': uniform(0.4,0.6)
        },
        n_iter = combos,
        scoring = 'neg_log_loss',
        cv = 5,
        n_jobs = -1,
        random_state = 123
    )
    search.fit(X_train, Y_train)
    uncalibrated_model = search.best_estimator_
    calibrated_model = CalibratedClassifierCV(
        estimator=uncalibrated_model, 
        method='sigmoid'
    )
    calibrated_model.fit(X_train, Y_train)
    return {
        'uncalibrated_model': uncalibrated_model,
        'calibrated_model': calibrated_model,
        'importance': search.best_estimator_.feature_importances_,
        'feature_columns': X_train.columns.tolist(),
        'categorical_columns': X_train.select_dtypes(
            include=['category', 'object']
        ).columns.tolist(),
        'category_levels': {
            col: X_train[col].cat.categories.tolist()
            for col in X_train.select_dtypes(include='category').columns
    }
}

def run_rf(X_train, X_test, Y_train, combos = 100):
   X_train = X_train.copy()
   X_train[X_train.select_dtypes(include='number').columns] = X_train[X_train.select_dtypes(include='number').columns].fillna(-999)
   base_rf = RandomForestClassifier(n_jobs = -1, random_state = 123)
   search = RandomizedSearchCV(
      estimator = base_rf,
      param_distributions={
            'n_estimators': randint(100, 600),
            'max_depth': [None, 5, 10, 15],
            'min_samples_split': randint(2, 10),
            'min_samples_leaf': randint(1, 5)
        },
        n_iter = combos,
        cv = 5,
        scoring = 'neg_log_loss',
        n_jobs = -1,
        random_state = 123
   )

   X_train_rf = pd.get_dummies(X_train, dummy_na=True, dtype=float)
   X_test_rf = pd.get_dummies(X_test, dummy_na=True, dtype=float)
   X_test_rf = X_test_rf.reindex(columns=X_train_rf.columns, fill_value=0)
   X_train_rf = X_train_rf.fillna(-999)
   X_test_rf = X_test_rf.fillna(-999)

   search.fit(X_train_rf, Y_train)
   uncalibrated_model = search.best_estimator_
   calibrated_model = CalibratedClassifierCV(
        estimator=uncalibrated_model, 
        method='sigmoid'
    )
   calibrated_model.fit(X_train_rf, Y_train)
   
   return [uncalibrated_model, calibrated_model, search.best_estimator_.feature_importances_, X_train_rf.columns.tolist(), X_test_rf]

def run_gbm(X_train, Y_train, combos = 100):
    X_train = X_train.copy()

    # Normalize every non-numeric column to pandas 'category' and coerce
    # numeric-looking values to floats before sklearn sees them.
    for col in X_train.columns:
        if pd.api.types.is_numeric_dtype(X_train[col]):
            X_train[col] = pd.to_numeric(X_train[col], errors='coerce')
        else:
            X_train[col] = X_train[col].astype('category')

    # Replace rare values with NA instead of dropping the whole feature.
    # This keeps useful data while avoiding folds where a feature becomes
    # constant and triggers sliding_window_view errors inside sklearn's
    # histogram binning.
    cv = 5
    for col in X_train.columns:
        counts = X_train[col].value_counts(dropna=False)
        rare_levels = counts[counts < cv].index
        if len(rare_levels) == 0:
            continue

        # Use isinstance check for categorical dtype to avoid pandas deprecation
        if isinstance(X_train[col].dtype, pd.CategoricalDtype) or pd.api.types.is_object_dtype(X_train[col]):
            col_data = X_train[col].astype('category').copy()
            col_data = col_data.where(~col_data.isin(rare_levels), other=pd.NA)
            X_train[col] = col_data.astype('category')
        elif pd.api.types.is_numeric_dtype(X_train[col]):
            X_train.loc[X_train[col].isin(rare_levels), col] = np.nan
        else:
            col_data = X_train[col].astype('category').copy()
            col_data = col_data.where(~col_data.isin(rare_levels), other=pd.NA)
            X_train[col] = col_data.astype('category')

    constant_cols = [
        col for col in X_train.columns
        if X_train[col].nunique(dropna=True) <= 1
    ]
    if constant_cols:
        X_train = X_train.drop(columns=constant_cols)

    # Drop any column that becomes effectively constant inside any CV training fold.
    unstable_cols = set()
    try:
        skf = StratifiedKFold(n_splits=cv, shuffle=True, random_state=123)
        for train_idx, _ in skf.split(X_train, Y_train):
            fold = X_train.iloc[train_idx]
            for col in fold.columns:
                if fold[col].nunique(dropna=True) <= 1:
                    unstable_cols.add(col)
    except Exception:
        # If stratification fails (e.g., Y_train constant), skip this safety check.
        unstable_cols = set()

    if unstable_cols:
        X_train = X_train.drop(columns=list(unstable_cols))

    # Drop low-variability columns (<= 2 distinct values) as extra safety for
    # histogram binning; this is aggressive but acceptable for smoke tests.
    low_variability = [col for col in X_train.columns if X_train[col].nunique(dropna=True) <= 2]
    if low_variability:
        X_train = X_train.drop(columns=low_variability)

    base_gbm = HistGradientBoostingClassifier(
        early_stopping = True,
        categorical_features = 'from_dtype',
        random_state = 123
    )
    search = RandomizedSearchCV(
        estimator = base_gbm,
        param_distributions={
            'learning_rate': uniform(0.01, 0.2),
            'max_iter': randint(100, 500),
            'max_depth': randint(3, 10),
            'min_samples_leaf': randint(10, 50),
            'l2_regularization': uniform(0, 1)
        },
        n_iter = combos,
        scoring = 'neg_log_loss',
        cv = 5,
        random_state = 123
    )
    search.fit(X_train, Y_train)
    uncalibrated_model = search.best_estimator_
    calibrated_model = CalibratedClassifierCV(
        estimator=uncalibrated_model, 
        method='sigmoid'
    )
    calibrated_model.fit(X_train, Y_train)
    importances = permutation_importance(uncalibrated_model, X_train, Y_train).importances_mean
    return [uncalibrated_model, calibrated_model, importances, X_train.columns.tolist()]

def standardize_and_encode(X_train, X_test):
   X_train = X_train.copy()
   X_test = X_test.copy()
   raw_feature_columns = X_train.columns.tolist()
   scaler = StandardScaler()
   X_test = X_test.reindex(columns=raw_feature_columns)
   numeric_cols = X_train.select_dtypes('number').columns
   categorical_cols = X_train.select_dtypes(['category', 'object','bool']).columns
   for col in categorical_cols:
    X_train[col] = X_train[col].astype("object").where(X_train[col].notna(), "__missing__")
    X_test[col] = X_test[col].astype("object").where(X_test[col].notna(), "__missing__")
   X_train[numeric_cols] = X_train[numeric_cols].astype('float64')
   X_test[numeric_cols] = X_test[numeric_cols].astype('float64')
   X_train[numeric_cols] = scaler.fit_transform(X_train[numeric_cols])
   X_test[numeric_cols] = scaler.transform(X_test[numeric_cols])

   encoder = OneHotEncoder(handle_unknown = 'ignore', sparse_output = False)
   train_encoded = encoder.fit_transform(X_train[categorical_cols])
   test_encoded = encoder.transform(X_test[categorical_cols])

   train_encoded = pd.DataFrame(train_encoded, columns = encoder.get_feature_names_out(categorical_cols), index = X_train.index)
   test_encoded = pd.DataFrame(test_encoded, columns = encoder.get_feature_names_out(categorical_cols), index = X_test.index)

   X_train = pd.concat([X_train[numeric_cols], train_encoded], axis = 1)
   X_test = pd.concat([X_test[numeric_cols], test_encoded], axis = 1)

   X_train[numeric_cols] = X_train[numeric_cols].fillna(0)
   X_test[numeric_cols] = X_test[numeric_cols].fillna(0)

   constant_cols = X_train.columns[X_train.nunique(dropna = False) <= 1]
   X_train = X_train.drop(columns = constant_cols)
   X_test = X_test.drop(columns = constant_cols, errors = 'ignore')
   
   return (
        X_train,
        X_test,
        {
            'scaler': scaler,
            'encoder': encoder,
            'raw_feature_columns': raw_feature_columns,
            'numeric_columns': numeric_cols.tolist(),
            'categorical_columns': categorical_cols.tolist(),
            'constant_columns': constant_cols.tolist(),
            'final_feature_columns': X_train.columns.tolist()
        }
)


def run_neural_net(X_train, X_test, Y_train, Y_test):
    X_train_standardized, X_test_standardized, preprocessing_artifacts = standardize_and_encode(X_train, X_test)

    best_cal = 1000
    best_model = None
    best_calibrator = None

    split_row = int(len(X_train_standardized) * 0.8)
    X_model_train = X_train_standardized.iloc[:split_row]
    Y_model_train = Y_train.iloc[:split_row]
    X_calibration = X_train_standardized.iloc[split_row:]
    Y_calibration = Y_train.iloc[split_row:]
    
    t1 = datetime.now()
    activation = 'relu'
    for dense_layer_1 in [64, 128, 256]:
        print(f"Tuning dense layer 1 {dense_layer_1}")
        for dense_layer_2 in [None, 64]:
            print(f"Tuning dense layer 2 {dense_layer_2}")
            #for activation in ['relu', 'tanh', 'elu']:
                    #print(f"Tuning activation {activation}")
            for dropout_rate in [0, 0.2]:
                print(f"Tuning dropout {dropout_rate}")
                for optimizer_name, learning_rate in [
                    ('adam', 0.0001),
                    ('rmsprop', 0.0001),
                    ('rmsprop', 0.0003)
                    ]:
                        print(f"Tuning optimizer {optimizer_name} with learning rate {learning_rate}")
                        for batch_size in [64, 128]:
                            print(f"Tuning batch size {batch_size}")
                            running_total = 0
                            running_total_cal = 0
                            for seed in [123, 456, 789]:
                                keras.utils.set_random_seed(seed)
                                model = Sequential()
                                model.add(Input(shape = (X_train_standardized.shape[1],))) 
                                model.add(Dense(dense_layer_1, activation = activation))
                                if dense_layer_2 is not None:
                                    model.add(Dense(dense_layer_2, activation = activation))
                                model.add(Dropout(dropout_rate))
                                model.add(Dense(1, activation = 'sigmoid'))

                                if optimizer_name == 'adam':
                                    optimizer = keras.optimizers.Adam(learning_rate=learning_rate)
                                else:
                                    optimizer = keras.optimizers.RMSprop(learning_rate=learning_rate)
                                model.compile(loss = 'binary_crossentropy', optimizer = optimizer, metrics = [keras.metrics.BinaryCrossentropy(name = 'log_loss')])
                                
                                early_stop = EarlyStopping(monitor = 'val_loss', patience = 5, restore_best_weights = True)
                                history = model.fit(X_model_train, Y_model_train, batch_size = batch_size, epochs = 100, callbacks = [early_stop], validation_split = 0.2, verbose = 0)
                                
                                raw_probs = model.predict(X_calibration, verbose = 0).ravel()
                                calibrator = LogisticRegression()
                                calibrator.fit(raw_probs.reshape(-1,1), Y_calibration)

                                raw_probs_test = model.predict(X_test_standardized, verbose = 0).ravel()
                                new_probs = calibrator.predict_proba(raw_probs_test.reshape(-1,1))[:,1]
                                cal = calibration_score(new_probs, Y_test)
                                running_total += calibration_score(raw_probs_test, Y_test)
                                running_total_cal += cal
                            mean_cal = running_total_cal/3
                            print(f"Calibration score on uncalibrated model: {running_total/3}")
                            print(f"Calibration score on calibrated model: {mean_cal}")
                            if mean_cal < best_cal:
                                print('new best model')
                                best_cal = mean_cal
                                best_model = model
                                best_calibrator = calibrator
                                # best_params = {
                                #     'dense_layer_1': dense_layer_1,
                                #     'dense_layer_2': dense_layer_2,
                                #     'activation': activation,
                                #     'dropout_rate': dropout_rate,
                                #     'optimizer_name': optimizer_name,
                                #     'learning_rate': learning_rate,
                                #     'batch_size': batch_size
                                # }
                            print(' ')
                            print(' ')
    print(f"{round((datetime.now() - t1).total_seconds()/60,2)} minutes")
    return [best_model, best_calibrator, preprocessing_artifacts, X_train_standardized, X_test_standardized]

def all_calibration_metrics_from_predictions(uncalibrated_preds, calibrated_preds, actuals):
    uncalibrated_score = calibration_score(uncalibrated_preds, actuals)
    calibrated_score = calibration_score(calibrated_preds, actuals)

    calibration_score_under_30_before_calibrating = calibration_score(model_probability = uncalibrated_preds[uncalibrated_preds < 0.3], bet_hit = actuals[uncalibrated_preds < 0.3])
    calibration_score_30_to_60_before_calibrating = calibration_score(model_probability = uncalibrated_preds[(uncalibrated_preds >= 0.3) & (uncalibrated_preds < 0.6)], bet_hit = actuals[(uncalibrated_preds >= 0.3) & (uncalibrated_preds < 0.6)]) if len(uncalibrated_preds[(uncalibrated_preds >= 0.3) & (uncalibrated_preds < 0.6)]) > 0 else None
    calibration_score_above_60_before_calibrating = calibration_score(model_probability = uncalibrated_preds[uncalibrated_preds >= 0.6], bet_hit = actuals[uncalibrated_preds >= 0.6]) if len(uncalibrated_preds[(uncalibrated_preds > 0.6)]) > 0 else None
    
    calibration_score_under_30_after_calibrating = calibration_score(model_probability = calibrated_preds[calibrated_preds < 0.3], bet_hit = actuals[calibrated_preds < 0.3])
    calibration_score_30_to_60_after_calibrating = calibration_score(model_probability = calibrated_preds[(calibrated_preds >= 0.3) & (calibrated_preds < 0.6)], bet_hit = actuals[(calibrated_preds >= 0.3) & (calibrated_preds < 0.6)]) if len(calibrated_preds[(calibrated_preds >= 0.3) & (calibrated_preds < 0.6)]) > 0 else None
    calibration_score_above_60_after_calibrating = calibration_score(model_probability = calibrated_preds[calibrated_preds >= 0.6], bet_hit = actuals[calibrated_preds >= 0.6]) if len(calibrated_preds[(calibrated_preds > 0.6)]) > 0 else None
    
    uncalibrated_categories_table = assess_probability_ranges(calculate_probability_ranges(pd.DataFrame({'Model_Probability': uncalibrated_preds, 'BetHit': actuals})), return_categories = True)
    calibrated_categories_table = assess_probability_ranges(calculate_probability_ranges(pd.DataFrame({'Model_Probability': calibrated_preds, 'BetHit': actuals})), return_categories = True)
    uncalibrated_num_bad = sum(uncalibrated_categories_table.query("Assessment == 'Bad'")['n'])
    uncalibrated_num_inside_range = sum(uncalibrated_categories_table.query("Assessment == 'Inside Target Range'")['n'])
    uncalibrated_num_near_range = sum(uncalibrated_categories_table.query("Assessment == 'Near Target Range'")['n'])
    calibrated_num_bad = sum(calibrated_categories_table.query("Assessment == 'Bad'")['n'])
    calibrated_num_inside_range = sum(calibrated_categories_table.query("Assessment == 'Inside Target Range'")['n'])
    calibrated_num_near_range = sum(calibrated_categories_table.query("Assessment == 'Near Target Range'")['n'])

    return([uncalibrated_score, calibrated_score,
            calibration_score_under_30_before_calibrating, calibration_score_30_to_60_before_calibrating, calibration_score_above_60_before_calibrating,
            calibration_score_under_30_after_calibrating, calibration_score_30_to_60_after_calibrating, calibration_score_above_60_after_calibrating,
            uncalibrated_num_bad, uncalibrated_num_inside_range, uncalibrated_num_near_range,
            calibrated_num_bad, calibrated_num_inside_range, calibrated_num_near_range])
    

def calculate_probability_ranges(df):
  return df.assign(ProbabilityFloor = lambda x: np.where(x['Model_Probability'] < 0.10, np.floor(x['Model_Probability'] / 0.02) * 0.02,
                                               np.where(x['Model_Probability'] < 0.30, np.floor((x['Model_Probability'] - 0.10) / 0.05) * 0.05 + 0.10,
                                                        np.floor((x['Model_Probability'] - 0.30) / 0.10) * 0.10 + 0.30)),
                   ProbabilityCeiling = lambda x: np.where(x['Model_Probability'] < 0.10, x['ProbabilityFloor'] + 0.02,
                                                 np.where(x['Model_Probability'] < 0.30, x['ProbabilityFloor'] + 0.05,
                                                          x['ProbabilityFloor'] + 0.10)))\
                   .assign(ProbabilityRange = lambda x: (round(x['ProbabilityFloor'] * 100)).astype(str) + '% to ' + (round(x['ProbabilityCeiling'] * 100)).astype(str) + '%')\
                        .assign(ProbabilityRange = lambda x: pd.Categorical(x['ProbabilityRange']))\
                            .sort_values('ProbabilityFloor')

#df must have columns: ProbabilityRange, ProbabilityFloor, ProbabilityCeiling (as derived in the calculate_probability_ranges function), along with BetHit (true/false) indicating whether bet won
def assess_probability_ranges(df, return_categories = True):
  df = df.groupby(['ProbabilityRange', 'ProbabilityFloor', 'ProbabilityCeiling'], as_index = False)\
    .agg(PctWin = ('BetHit', 'mean'),
         n = ('BetHit','size'),
         MeanPrediction=('Model_Probability', 'mean'))
  
  if return_categories:
    assessments = df.assign(Assessment = lambda x: np.where(x['n'] < 10, 'Insufficient Data',
                                                            np.where((x['PctWin'] >= x['ProbabilityFloor']) & (x['PctWin'] <= x['ProbabilityCeiling']), 'Inside Target Range',
                                                                     np.where((abs(x['PctWin'] - x['ProbabilityCeiling']) < x['ProbabilityFloor']*.1)| (abs(x['ProbabilityFloor'] - x['PctWin'])  < x['ProbabilityFloor']*.1), 'Near Target Range',
                                                                              'Bad'))))\
                                                                                .sort_values('ProbabilityFloor')
  else:
      assessments_df = df.assign(DistanceFromMean = lambda x: abs(x['PctWin'] - x['MeanPrediction']),
                                 Weight = lambda x: np.sqrt(x['n']))\
                                    .assign(WeightedAssessment = lambda x: x['DistanceFromMean']*x['Weight']).query('n>=10')
      if assessments_df.empty:
         return np.nan
      assessments = sum(assessments_df['WeightedAssessment'].dropna())/sum(assessments_df['Weight'].dropna())
  return assessments 


def calibration_score(model_probability, bet_hit):
  
  df = pd.DataFrame({
    'Model_Probability': model_probability,
    'BetHit': bet_hit
  })
  
  return assess_probability_ranges(calculate_probability_ranges(df), return_categories = False)


def apply_saved_standardize_and_encode(X_new, preprocessing_artifacts):
    X_new = X_new.copy()

    raw_feature_columns = preprocessing_artifacts["raw_feature_columns"]
    numeric_cols = preprocessing_artifacts["numeric_columns"]
    categorical_cols = preprocessing_artifacts["categorical_columns"]
    constant_cols = preprocessing_artifacts["constant_columns"]
    final_feature_columns = preprocessing_artifacts["final_feature_columns"]

    scaler = preprocessing_artifacts["scaler"]
    encoder = preprocessing_artifacts["encoder"]

    # Match raw training schema
    X_new = X_new.reindex(columns=raw_feature_columns)

    # Numeric handling
    X_new[numeric_cols] = X_new[numeric_cols].astype("float64")
    X_num = pd.DataFrame(
        scaler.transform(X_new[numeric_cols]),
        columns=numeric_cols,
        index=X_new.index
    )

    # Categorical handling
    for col in categorical_cols:
        X_new[col] = X_new[col].astype("object").where(
            X_new[col].notna(),
            "__missing__"
        )

    X_cat_array = encoder.transform(X_new[categorical_cols])

    X_cat = pd.DataFrame(
        X_cat_array,
        columns=encoder.get_feature_names_out(categorical_cols),
        index=X_new.index
    )

    X_out = pd.concat([X_num, X_cat], axis=1)

    # Match training cleanup
    X_out[numeric_cols] = X_out[numeric_cols].fillna(0)
    X_out = X_out.drop(columns=constant_cols, errors="ignore")

    # Final hard alignment to exact model input columns
    X_out = X_out.reindex(columns=final_feature_columns, fill_value=0)

    return X_out

def align_categories_to_train(X_train, X_test_model):
    X_test_model = X_test_model.copy()
    cat_cols = X_train.select_dtypes(
        include=["object", "str", "category"]
    ).columns
    cat_cols = [col for col in cat_cols if col in X_test_model.columns]
    for col in cat_cols:
        train_categories = X_train[col].astype("category").cat.categories
        X_test_model[col] = pd.Categorical(
            X_test_model[col],
            categories=train_categories
        )
    return X_test_model

def prepare_new_data(response, new_data):
    new_data = new_data.copy()
    #pull train as reference to make sure new data has in same form as train data was:
    train = pd.read_parquet(Path(f"./ml_ready_data/train/{response}.parquet"))
    X_train = train.drop(columns = [response])
    new_data = new_data.reindex(columns=X_train.columns)
    train_bool_cols = X_train.select_dtypes(include=['bool']).columns
    new_data[train_bool_cols] = new_data[train_bool_cols].astype(float)
    new_data = align_categories_to_train(X_train, new_data)
    return [X_train, new_data]

def get_prediction_by_model_type(response, model_type, extra_calibration, root_folder, new_data):
    X_train, new_data = prepare_new_data(response, new_data)
    if extra_calibration:
        model_name = 'calibrated_model'
    else:
        model_name = 'uncalibrated_model'
    if model_type == 'neural_net':
        preprocessing_artifacts = joblib.load(f"{root_folder}neural_net/preprocessing.joblib")
        new_data_standardized = apply_saved_standardize_and_encode(new_data, preprocessing_artifacts) 
        uncalibrated_model = keras.models.load_model(f"{root_folder}neural_net/uncalibrated_model.keras")
        uncalibrated_preds = uncalibrated_model.predict(new_data_standardized, verbose = 0).ravel()
        if extra_calibration:
            calibrated_model = joblib.load(f"{root_folder}neural_net/calibrated_model.joblib")
            preds = calibrated_model.predict_proba(uncalibrated_preds.reshape(-1,1))[:,1]
        else:
            preds = uncalibrated_preds     
    elif model_type in ['gbm', 'rf', 'xgb']:
        feature_columns = joblib.load(f"{root_folder}/{model_type}/feature_columns.joblib")
        model = joblib.load(f"{root_folder}/{model_type}/{model_name}.joblib")
        if model_type == 'rf':
            new_data_rf = pd.get_dummies(new_data, dummy_na=True, dtype=float)
            new_data_rf = new_data_rf.reindex(columns=feature_columns, fill_value=0)
            preds = model.predict_proba(new_data_rf)[:,1]
        else:
            new_data_model = new_data.reindex(columns=feature_columns)
            new_data_model = align_categories_to_train(X_train, new_data_model)
            preds = model.predict_proba(new_data_model)[:, 1]
    else: #ensembles
        each_model_pred = []
        for component_type in ['gbm', 'rf', 'xgb']:
            model = joblib.load(f"{root_folder}{component_type}/{model_name}.joblib")
            feature_columns = joblib.load(f"{root_folder}/{component_type}/feature_columns.joblib")
            if component_type == 'rf':
                new_data_rf = pd.get_dummies(new_data, dummy_na=True, dtype=float)
                new_data_rf = new_data_rf.reindex(columns=feature_columns, fill_value=0)
                each_model_pred.append(model.predict_proba(new_data_rf)[:,1])
            else:
                new_data_model = new_data.reindex(columns=feature_columns)
                new_data_model = align_categories_to_train(X_train, new_data_model)
                each_model_pred.append(model.predict_proba(new_data_model)[:, 1])
        if model_type == 'full ensemble':
            preprocessing_artifacts = joblib.load(f"{root_folder}neural_net/preprocessing.joblib")
            new_data_standardized = apply_saved_standardize_and_encode(new_data, preprocessing_artifacts) 
            uncalibrated_neural_net_model = keras.models.load_model(f"{root_folder}neural_net/uncalibrated_model.keras")
            uncalibrated_neural_net_preds = uncalibrated_neural_net_model.predict(new_data_standardized, verbose = 0).ravel()
            if extra_calibration:
                calibrated_neural_net_model = joblib.load(f"{root_folder}neural_net/calibrated_model.joblib")
                each_model_pred.append(calibrated_neural_net_model.predict_proba(uncalibrated_neural_net_preds.reshape(-1,1))[:,1])
            else:
                each_model_pred.append(uncalibrated_neural_net_preds)   
        preds = pd.DataFrame(each_model_pred).apply(lambda col: np.mean(col), axis = 0)
    return preds
