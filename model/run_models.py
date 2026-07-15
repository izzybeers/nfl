#%%
import numpy as np
import pandas as pd
from pathlib import Path
import json
import joblib
import warnings
from sklearn.metrics import log_loss
%load_ext autoreload
%autoreload 2
from model_functions import create_model_directories, run_xgboost, apply_saved_standardize_and_encode, run_rf, run_gbm, run_neural_net, assess_probability_ranges, calculate_probability_ranges, calibration_score, all_calibration_metrics_from_predictions, align_categories_to_train, get_prediction_by_model_type, write_to_supabase_predictions_table
import os
os.environ["KERAS_BACKEND"] = "torch"
import keras
warnings.filterwarnings(
    "ignore",
    category=UserWarning,
    message=r".*`sklearn\.utils\.parallel\.delayed` should be used with `sklearn\.utils\.parallel\.Parallel`.*",
)
#%%

#%%

comparisons_table = pd.DataFrame({
  'response_var': [],
  'num_columns': [],
  'model_type': [],
  'extra_calibration': [],
  'calibration_score': [],
  'validation_log_loss': [],
  'calibration_score_under_30': [],
  'calibration_score_30_to_60': [],
  'calibration_score_above_60': [],
  'num_inside_range': [],
  'num_near_range': [],
  'num_bad': [],
  'top_20_vars': []
})


for file in Path('./ml_ready_data/train/').glob('*.parquet'):
    response = file.stem
    if response not in set(comparisons_table['response_var']):
      print(f"Beginning training for {response}")
      train = pd.read_parquet(file)
      test = pd.read_parquet(Path(f"./ml_ready_data/test/{response}.parquet"))
      train = train.drop(columns = ['gsis_id', 'team', 'display_name', 'game_id'], errors = 'ignore')
      train[response]= pd.Categorical(train[response])
      train[train.select_dtypes(include=['bool']).columns] = train[train.select_dtypes(include=['bool']).columns].astype(int)
      test = test.drop(columns = ['gsis_id', 'team', 'display_name', 'game_id'], errors = 'ignore')
      test[response] = pd.Categorical(test[response])
      test[test.select_dtypes(include=['bool']).columns] = test[test.select_dtypes(include=['bool']).columns].astype(int)

      constant_cols = train.columns[train.nunique(dropna = False) <= 1]  
      train = train.drop(columns = constant_cols)
      test = test.drop(columns = constant_cols)

      X_train = train.drop(columns = [response])
      Y_train = train[response].astype(int)
      X_test = test.drop(columns = [response])
      Y_test = test[response].astype(int)
      
      train_cat_cols = X_train.select_dtypes(
      include=['object', 'str', 'category']
  ).columns
      test_cat_cols = X_test.select_dtypes(
          include=['object', 'str', 'category']
      ).columns
      X_train[train_cat_cols] = X_train[train_cat_cols].astype('category')
      X_test[test_cat_cols] = X_test[test_cat_cols].astype('category')
      cat_cols = X_train.select_dtypes(
      include=['object', 'str', 'category']
  ).columns

      for col in cat_cols:
          # Establish the training vocabulary.
          X_train[col] = X_train[col].astype('category')

          # Use that same vocabulary for test.
          # Values absent from training become NaN / "unknown".
          valid_mask = X_test[col].isin(X_train[col].cat.categories)
          X_test[col] = pd.Categorical(
              X_test[col].astype(object).where(valid_mask),
              categories=X_train[col].cat.categories,
              ordered=False
          )
          X_test[col] = X_test[col].astype('category')


      response_folder, xgb_folder, rf_folder, gbm_folder, neural_net_folder = create_model_directories(response)

      #XGBOOST:
      print('Beginning xgboost')
      xgb_dict  = run_xgboost(X_train = X_train, Y_train = Y_train, combos = 100)
      
      xgb_preds_uncalibrated = xgb_dict['uncalibrated_model'].predict_proba(X_test)[:,1]
      xgb_preds_calibrated = xgb_dict['calibrated_model'].predict_proba(X_test)[:,1]
      
      (xgb_calibration_score_before_calibrating, xgb_calibration_score_after_calibrating,
      xgb_calibration_score_under_30_before_calibrating, xgb_calibration_score_30_to_60_before_calibrating, xgb_calibration_score_above_60_before_calibrating,
      xgb_calibration_score_under_30_after_calibrating, xgb_calibration_score_30_to_60_after_calibrating, xgb_calibration_score_above_60_after_calibrating,
      xgb_uncalibrated_num_bad, xgb_uncalibrated_num_inside_range, xgb_uncalibrated_num_near_range,
      xgb_calibrated_num_bad, xgb_calibrated_num_inside_range, xgb_calibrated_num_near_range) = all_calibration_metrics_from_predictions(xgb_preds_uncalibrated, xgb_preds_calibrated, Y_test)
      
      importance_df = pd.DataFrame({'columns': X_train.columns, 'importance': xgb_dict['importance']}).sort_values('importance', ascending = False)
      
      comparisons_table = pd.concat([comparisons_table, pd.DataFrame([
      {
          'response_var': response,
          'num_columns': X_train.shape[1],
          'model_type': 'xgb',
          'extra_calibration': False,
          'calibration_score': xgb_calibration_score_before_calibrating,
          'validation_log_loss': log_loss(Y_test, xgb_preds_uncalibrated),
          'calibration_score_under_30': xgb_calibration_score_under_30_before_calibrating,
          'calibration_score_30_to_60': xgb_calibration_score_30_to_60_before_calibrating,
          'calibration_score_above_60': xgb_calibration_score_above_60_before_calibrating,
          'num_inside_range': xgb_uncalibrated_num_inside_range,
          'num_near_range': xgb_uncalibrated_num_near_range,
          'num_bad': xgb_uncalibrated_num_bad,
          'top_20_vars': ', '.join(importance_df['columns'].iloc[:20])
      },
      {
          'response_var': response,
          'num_columns': X_train.shape[1],
          'model_type': 'xgb',
          'extra_calibration': True,
          'calibration_score': xgb_calibration_score_after_calibrating,
          'validation_log_loss': log_loss(Y_test, xgb_preds_calibrated),
          'calibration_score_under_30': xgb_calibration_score_under_30_after_calibrating,
          'calibration_score_30_to_60': xgb_calibration_score_30_to_60_after_calibrating,
          'calibration_score_above_60': xgb_calibration_score_above_60_after_calibrating,
          'num_inside_range': xgb_calibrated_num_inside_range,
          'num_near_range': xgb_calibrated_num_near_range,
          'num_bad': xgb_calibrated_num_bad,
          'top_20_vars': ', '.join(importance_df['columns'].iloc[:20])
      }
  ])], ignore_index=True)

      xgb_dict['uncalibrated_model'].save_model(
          xgb_folder / 'uncalibrated_model.json'
      )
      for key in ['calibrated_model', 'importance', 'feature_columns', 'categorical_columns', 'category_levels']:
        joblib.dump(
            xgb_dict[key],
            xgb_folder / f'{key}.joblib'
        )


      print('Beginning Random Forest')
      rf_uncalibrated_model, rf_calibrated_model, importance, rf_feature_columns, X_test_adjusted  = run_rf(X_train, X_test, Y_train, combos = 100)
      
      rf_preds_uncalibrated = rf_uncalibrated_model.predict_proba(X_test_adjusted)[:,1]
      rf_preds_calibrated = rf_calibrated_model.predict_proba(X_test_adjusted)[:,1]
      
      (rf_calibration_score_before_calibrating, rf_calibration_score_after_calibrating,
      rf_calibration_score_under_30_before_calibrating, rf_calibration_score_30_to_60_before_calibrating, rf_calibration_score_above_60_before_calibrating,
      rf_calibration_score_under_30_after_calibrating, rf_calibration_score_30_to_60_after_calibrating, rf_calibration_score_above_60_after_calibrating,
      rf_uncalibrated_num_bad, rf_uncalibrated_num_inside_range, rf_uncalibrated_num_near_range,
      rf_calibrated_num_bad, rf_calibrated_num_inside_range, rf_calibrated_num_near_range) = all_calibration_metrics_from_predictions(rf_preds_uncalibrated, rf_preds_calibrated, Y_test)
      
      importance_df = pd.DataFrame({'columns': X_test_adjusted.columns, 'importance': importance}).sort_values('importance', ascending = False)
      
      comparisons_table = pd.concat([comparisons_table, pd.DataFrame([
      {
          'response_var': response,
          'num_columns': len(rf_feature_columns),
          'model_type': 'rf',
          'extra_calibration': False,
          'calibration_score': rf_calibration_score_before_calibrating,
          'validation_log_loss': log_loss(Y_test, rf_preds_uncalibrated),
          'calibration_score_under_30': rf_calibration_score_under_30_before_calibrating,
          'calibration_score_30_to_60': rf_calibration_score_30_to_60_before_calibrating,
          'calibration_score_above_60': rf_calibration_score_above_60_before_calibrating,
          'num_inside_range': rf_uncalibrated_num_inside_range,
          'num_near_range': rf_uncalibrated_num_near_range,
          'num_bad': rf_uncalibrated_num_bad,
          'top_20_vars': ', '.join(importance_df['columns'].iloc[:20])
      },
      {
          'response_var': response,
          'num_columns': len(rf_feature_columns),
          'model_type': 'rf',
          'extra_calibration': True,
          'calibration_score': rf_calibration_score_after_calibrating,
          'validation_log_loss': log_loss(Y_test, rf_preds_calibrated),
          'calibration_score_under_30': rf_calibration_score_under_30_after_calibrating,
          'calibration_score_30_to_60': rf_calibration_score_30_to_60_after_calibrating,
          'calibration_score_above_60': rf_calibration_score_above_60_after_calibrating,
          'num_inside_range': rf_calibrated_num_inside_range,
          'num_near_range': rf_calibrated_num_near_range,
          'num_bad': rf_calibrated_num_bad,
          'top_20_vars': ', '.join(importance_df['columns'].iloc[:20])
      }
  ])], ignore_index=True)


      joblib.dump(rf_uncalibrated_model, rf_folder / 'uncalibrated_model.joblib')
      joblib.dump(rf_calibrated_model, rf_folder / 'calibrated_model.joblib')
      joblib.dump(rf_feature_columns, rf_folder / 'feature_columns.joblib')
      
      print('Beginning GBM')
      gbm_uncalibrated_model, gbm_calibrated_model, importance, gbm_feature_columns  = run_gbm(X_train, Y_train, combos = 100)
      
    # Align test set to the features used to train the GBMwere yo (some sparse
    # categorical columns may have been dropped inside run_gbm).
      X_test_gbm = X_test.reindex(columns=gbm_feature_columns)

      gbm_preds_uncalibrated = gbm_uncalibrated_model.predict_proba(X_test_gbm)[:,1]
      gbm_preds_calibrated = gbm_calibrated_model.predict_proba(X_test_gbm)[:,1]
      
      (gbm_calibration_score_before_calibrating, gbm_calibration_score_after_calibrating,
      gbm_calibration_score_under_30_before_calibrating, gbm_calibration_score_30_to_60_before_calibrating, gbm_calibration_score_above_60_before_calibrating,
      gbm_calibration_score_under_30_after_calibrating, gbm_calibration_score_30_to_60_after_calibrating, gbm_calibration_score_above_60_after_calibrating,
      gbm_uncalibrated_num_bad, gbm_uncalibrated_num_inside_range, gbm_uncalibrated_num_near_range,
      gbm_calibrated_num_bad, gbm_calibrated_num_inside_range, gbm_calibrated_num_near_range) = all_calibration_metrics_from_predictions(gbm_preds_uncalibrated, gbm_preds_calibrated, Y_test)
      
      importance_df = pd.DataFrame({'columns': gbm_feature_columns, 'importance': importance}).sort_values('importance', ascending = False)
      
      comparisons_table = pd.concat([comparisons_table, pd.DataFrame([
      {
          'response_var': response,
          'num_columns': X_train.shape[1],
          'model_type': 'gbm',
          'extra_calibration': False,
          'calibration_score': gbm_calibration_score_before_calibrating,
          'validation_log_loss': log_loss(Y_test, gbm_preds_uncalibrated),
          'calibration_score_under_30': gbm_calibration_score_under_30_before_calibrating,
          'calibration_score_30_to_60': gbm_calibration_score_30_to_60_before_calibrating,
          'calibration_score_above_60': gbm_calibration_score_above_60_before_calibrating,
          'num_inside_range': gbm_uncalibrated_num_inside_range,
          'num_near_range': gbm_uncalibrated_num_near_range,
          'num_bad': gbm_uncalibrated_num_bad,
          'top_20_vars': ', '.join(importance_df['columns'].iloc[:20])
      },
      {
          'response_var': response,
          'num_columns': X_train.shape[1],
          'model_type': 'gbm',
          'extra_calibration': True,
          'calibration_score': gbm_calibration_score_after_calibrating,
          'validation_log_loss': log_loss(Y_test, gbm_preds_calibrated),
          'calibration_score_under_30': gbm_calibration_score_under_30_after_calibrating,
          'calibration_score_30_to_60': gbm_calibration_score_30_to_60_after_calibrating,
          'calibration_score_above_60': gbm_calibration_score_above_60_after_calibrating,
          'num_inside_range': gbm_calibrated_num_inside_range,
          'num_near_range': gbm_calibrated_num_near_range,
          'num_bad': gbm_calibrated_num_bad,
          'top_20_vars': ', '.join(importance_df['columns'].iloc[:20])
      }
  ])], ignore_index=True)

      joblib.dump(
          gbm_uncalibrated_model,
          gbm_folder / "uncalibrated_model.joblib"
      )

      joblib.dump(
          gbm_calibrated_model,
          gbm_folder / "calibrated_model.joblib"
      )

      joblib.dump(
          gbm_feature_columns,
          gbm_folder / "feature_columns.joblib"
      )
      gbm_categorical_columns = X_train[gbm_feature_columns].select_dtypes(
      include=['category']
  ).columns.tolist()

      gbm_category_levels = {
          col: X_train[col].cat.categories.tolist()
          for col in gbm_categorical_columns
      }
      joblib.dump(
          gbm_categorical_columns,
          gbm_folder / 'categorical_columns.joblib'
      )

      joblib.dump(
          gbm_category_levels,
          gbm_folder / 'category_levels.joblib'
      )

      print('Beginning neural net')
      
      neural_net_uncalibrated_model, neural_net_calibrated_model, preprocessing_artifacts, X_train_standardized, X_test_standardized = run_neural_net(X_train, X_test, Y_train, Y_test)
    
      neural_net_preds_uncalibrated = neural_net_uncalibrated_model.predict(X_test_standardized, verbose = 0).ravel()
      neural_net_preds_calibrated = neural_net_calibrated_model.predict_proba(neural_net_preds_uncalibrated.reshape(-1,1))[:,1]

      (neural_net_calibration_score_before_calibrating, neural_net_calibration_score_after_calibrating,
      neural_net_calibration_score_under_30_before_calibrating, neural_net_calibration_score_30_to_60_before_calibrating, neural_net_calibration_score_above_60_before_calibrating,
      neural_net_calibration_score_under_30_after_calibrating, neural_net_calibration_score_30_to_60_after_calibrating, neural_net_calibration_score_above_60_after_calibrating,
      neural_net_uncalibrated_num_bad, neural_net_uncalibrated_num_inside_range, neural_net_uncalibrated_num_near_range,
      neural_net_calibrated_num_bad, neural_net_calibrated_num_inside_range, neural_net_calibrated_num_near_range) = all_calibration_metrics_from_predictions(neural_net_preds_uncalibrated, neural_net_preds_calibrated, Y_test)
      
      comparisons_table = pd.concat([comparisons_table, pd.DataFrame([
      {
          'response_var': response,
          'num_columns': X_train_standardized.shape[1],
          'model_type': 'neural_net',
          'extra_calibration': False,
          'calibration_score': neural_net_calibration_score_before_calibrating,
          'validation_log_loss': log_loss(Y_test, neural_net_preds_uncalibrated),
          'calibration_score_under_30': neural_net_calibration_score_under_30_before_calibrating,
          'calibration_score_30_to_60': neural_net_calibration_score_30_to_60_before_calibrating,
          'calibration_score_above_60': neural_net_calibration_score_above_60_before_calibrating,
          'num_inside_range': neural_net_uncalibrated_num_inside_range,
          'num_near_range': neural_net_uncalibrated_num_near_range,
          'num_bad': neural_net_uncalibrated_num_bad,
          'top_20_vars': None
      },
      {
          'response_var': response,
          'num_columns': X_train_standardized.shape[1],
          'model_type': 'neural_net',
          'extra_calibration': True,
          'calibration_score': neural_net_calibration_score_after_calibrating,
          'validation_log_loss': log_loss(Y_test, neural_net_preds_calibrated),
          'calibration_score_under_30': neural_net_calibration_score_under_30_after_calibrating,
          'calibration_score_30_to_60': neural_net_calibration_score_30_to_60_after_calibrating,
          'calibration_score_above_60': neural_net_calibration_score_above_60_after_calibrating,
          'num_inside_range': neural_net_calibrated_num_inside_range,
          'num_near_range': neural_net_calibrated_num_near_range,
          'num_bad': neural_net_calibrated_num_bad,
          'top_20_vars': None
      }
  ])], ignore_index=True)

      neural_net_uncalibrated_model.save(neural_net_folder / 'uncalibrated_model.keras')
      joblib.dump(neural_net_calibrated_model, neural_net_folder / 'calibrated_model.joblib')
      joblib.dump(preprocessing_artifacts, neural_net_folder / "preprocessing.joblib")
      
      print('Calculating ensemble models')
      #ensemble between all tree models:
      avg_uncalibrated_prediction_trees = np.mean([gbm_preds_uncalibrated, rf_preds_uncalibrated, xgb_preds_uncalibrated], axis = 0)
      ensemble_uncalibrated_score_trees = calibration_score(avg_uncalibrated_prediction_trees, Y_test)
      avg_calibrated_prediction_trees = np.mean([gbm_preds_calibrated, rf_preds_calibrated, xgb_preds_calibrated], axis = 0)
      ensemble_calibrated_score_trees = calibration_score(avg_calibrated_prediction_trees, Y_test)
      
      (tree_ensemble_calibration_score_before_calibrating, tree_ensemble_calibration_score_after_calibrating,
      tree_ensemble_calibration_score_under_30_before_calibrating, tree_ensemble_calibration_score_30_to_60_before_calibrating, tree_ensemble_calibration_score_above_60_before_calibrating,
      tree_ensemble_calibration_score_under_30_after_calibrating, tree_ensemble_calibration_score_30_to_60_after_calibrating, tree_ensemble_calibration_score_above_60_after_calibrating,
      tree_ensemble_uncalibrated_num_bad, tree_ensemble_uncalibrated_num_inside_range, tree_ensemble_uncalibrated_num_near_range,
      tree_ensemble_calibrated_num_bad, tree_ensemble_calibrated_num_inside_range, tree_ensemble_calibrated_num_near_range) = all_calibration_metrics_from_predictions(avg_uncalibrated_prediction_trees, avg_calibrated_prediction_trees, Y_test)
      
      comparisons_table = pd.concat([comparisons_table, pd.DataFrame([
        {
            'response_var': response,
            'num_columns': X_train.shape[1],
            'model_type': 'tree ensemble',
            'extra_calibration': False,
            'calibration_score': tree_ensemble_calibration_score_before_calibrating,
            'validation_log_loss': log_loss(Y_test, avg_uncalibrated_prediction_trees),
            'calibration_score_under_30': tree_ensemble_calibration_score_under_30_before_calibrating,
            'calibration_score_30_to_60': tree_ensemble_calibration_score_30_to_60_before_calibrating,
            'calibration_score_above_60': tree_ensemble_calibration_score_above_60_before_calibrating,
            'num_inside_range': tree_ensemble_uncalibrated_num_inside_range,
            'num_near_range': tree_ensemble_uncalibrated_num_near_range,
            'num_bad': tree_ensemble_uncalibrated_num_bad,
            'top_20_vars': None
        },
        {
            'response_var': response,
            'num_columns': X_train.shape[1],
            'model_type': 'tree ensemble',
            'extra_calibration': True,
            'calibration_score': tree_ensemble_calibration_score_after_calibrating,
            'validation_log_loss': log_loss(Y_test, avg_calibrated_prediction_trees),
            'calibration_score_under_30': tree_ensemble_calibration_score_under_30_after_calibrating,
            'calibration_score_30_to_60': tree_ensemble_calibration_score_30_to_60_after_calibrating,
            'calibration_score_above_60': tree_ensemble_calibration_score_above_60_after_calibrating,
            'num_inside_range': tree_ensemble_calibrated_num_inside_range,
            'num_near_range': tree_ensemble_calibrated_num_near_range,
            'num_bad': tree_ensemble_calibrated_num_bad,
            'top_20_vars': None
        }
    ])], ignore_index=True)

      #ensemble between all models:
      avg_uncalibrated_prediction_all_models = np.mean([gbm_preds_uncalibrated, rf_preds_uncalibrated, xgb_preds_uncalibrated, neural_net_preds_uncalibrated], axis = 0)
      ensemble_uncalibrated_score_all_models = calibration_score(avg_uncalibrated_prediction_all_models, Y_test)
      avg_calibrated_prediction_all_models = np.mean([gbm_preds_calibrated, rf_preds_calibrated, xgb_preds_calibrated, neural_net_preds_calibrated], axis = 0)
      ensemble_calibrated_score_all_models = calibration_score(avg_calibrated_prediction_all_models, Y_test)
      
      (full_ensemble_calibration_score_before_calibrating, full_ensemble_calibration_score_after_calibrating,
      full_ensemble_calibration_score_under_30_before_calibrating, full_ensemble_calibration_score_30_to_60_before_calibrating, full_ensemble_calibration_score_above_60_before_calibrating,
      full_ensemble_calibration_score_under_30_after_calibrating, full_ensemble_calibration_score_30_to_60_after_calibrating, full_ensemble_calibration_score_above_60_after_calibrating,
      full_ensemble_uncalibrated_num_bad, full_ensemble_uncalibrated_num_inside_range, full_ensemble_uncalibrated_num_near_range,
      full_ensemble_calibrated_num_bad, full_ensemble_calibrated_num_inside_range, full_ensemble_calibrated_num_near_range) = all_calibration_metrics_from_predictions(avg_uncalibrated_prediction_all_models, avg_calibrated_prediction_all_models, Y_test)
      
      comparisons_table = pd.concat([comparisons_table, pd.DataFrame([
        {
            'response_var': response,
            'num_columns': None,
            'model_type': 'full ensemble',
            'extra_calibration': False,
            'calibration_score': full_ensemble_calibration_score_before_calibrating,
            'validation_log_loss': log_loss(Y_test, avg_uncalibrated_prediction_all_models),
            'calibration_score_under_30': full_ensemble_calibration_score_under_30_before_calibrating,
            'calibration_score_30_to_60': full_ensemble_calibration_score_30_to_60_before_calibrating,
            'calibration_score_above_60': full_ensemble_calibration_score_above_60_before_calibrating,
            'num_inside_range': full_ensemble_uncalibrated_num_inside_range,
            'num_near_range': full_ensemble_uncalibrated_num_near_range,
            'num_bad': full_ensemble_uncalibrated_num_bad,
            'top_20_vars': None
        },
        {
            'response_var': response,
            'num_columns': None,
            'model_type': 'full ensemble',
            'extra_calibration': True,
            'calibration_score': full_ensemble_calibration_score_after_calibrating,
            'validation_log_loss': log_loss(Y_test, avg_calibrated_prediction_all_models),
            'calibration_score_under_30': full_ensemble_calibration_score_under_30_after_calibrating,
            'calibration_score_30_to_60': full_ensemble_calibration_score_30_to_60_after_calibrating,
            'calibration_score_above_60': full_ensemble_calibration_score_above_60_after_calibrating,
            'num_inside_range': full_ensemble_calibrated_num_inside_range,
            'num_near_range': full_ensemble_calibrated_num_near_range,
            'num_bad': full_ensemble_calibrated_num_bad,
            'top_20_vars': None
        }
    ])], ignore_index=True)
      

      comparisons_table.to_parquet(
        Path('models') / 'model_comparisons.parquet',
        index=False
      )
      comparisons_table.to_csv(
        Path('models') / 'model_comparisons.csv',
        index=False
      )
    
#%%

#%%

comparisons_table = pd.read_parquet('./models/model_comparisons.parquet')

prevalence_df = pd.DataFrame()
for file in Path('./ml_ready_data/train/').glob('*.parquet'):
    response = file.stem
    train = pd.read_parquet(file)
    prevalence = np.mean(train[response].astype(int))
    prevalence_df = pd.concat([prevalence_df, pd.DataFrame({'response_var': [response], 'prevalence': [prevalence]})], axis = 0)

prevalence_df['baseline_log_loss'] = prevalence_df["prevalence"].apply(lambda p: -(p * np.log(p) + (1 - p) * np.log(1 - p)))

comparisons_table_with_baseline_log_loss = comparisons_table.merge(prevalence_df.drop(columns = 'prevalence'), on = 'response_var', how = 'left')

comparisons_table_with_baseline_log_loss = comparisons_table_with_baseline_log_loss\
    .assign(pct_improvement_over_baseline = lambda row: (row['baseline_log_loss'] - row['validation_log_loss'])/row['baseline_log_loss'],
            pct_bad = lambda row: row['num_bad']/(row['num_bad'] + row['num_inside_range'] + row['num_near_range']))\

survivors = comparisons_table_with_baseline_log_loss[(comparisons_table_with_baseline_log_loss['calibration_score'] <= 0.05) & (comparisons_table_with_baseline_log_loss['pct_bad'] <= 0.25) & (comparisons_table_with_baseline_log_loss['pct_improvement_over_baseline']>0)]


survivors = survivors.assign(high_validation_log_loss_improvement = lambda x: x['pct_improvement_over_baseline'] >= 0.1,
                             rounded_log_loss_improvement = lambda x: 5*(round(100*x['pct_improvement_over_baseline']/5))/100,
                             rounded_calibration = lambda x: round(x['calibration_score']/2,2)*2,
                             range_calibrations = lambda x: (x[['calibration_score_under_30','calibration_score_30_to_60','calibration_score_above_60']]).max(axis=1) - (x[['calibration_score_under_30','calibration_score_30_to_60','calibration_score_above_60']]).min(axis=1),
                             rounded_range_calibrations = lambda x: round(x['range_calibrations']/2,2)*2)

#make sure all response vars made it to survivors table, otherwise reevaluate why any got missed:
set(comparisons_table['response_var']) - set(survivors['response_var'])

best_models = survivors.sort_values(['response_var', 'high_validation_log_loss_improvement', 'rounded_calibration', 'rounded_range_calibrations', 'rounded_log_loss_improvement', 'calibration_score'],
                      ascending = [True, False, True, True, False, True]).drop_duplicates(subset = 'response_var', keep = 'first')

best_models['extra_calibration'] = best_models['extra_calibration'].astype(bool)

assessments = pd.DataFrame()
for i in range(len(best_models)):
    response = best_models['response_var'].iloc[i]
    model_type = best_models['model_type'].iloc[i]
    extra_calibration = best_models['extra_calibration'].iloc[i]
    root_folder = f"./models/{best_models['response_var'].iloc[i]}/"
    test = pd.read_parquet(Path(f"./ml_ready_data/test/{response}.parquet"))
    X_test = test.drop(columns = [response], errors = 'ignore')
    Y_test = test[response].astype(int)
    preds = get_prediction_by_model_type(response, model_type, extra_calibration, root_folder, X_test)
    preds_df = pd.DataFrame({'Model_Probability': preds, 'BetHit': Y_test.values})
    assessments = pd.concat([assessments, assess_probability_ranges(calculate_probability_ranges(preds_df))[['ProbabilityFloor', 'ProbabilityCeiling', 'Assessment', 'ProbabilityRange', 'PctWin', 'n', 'MeanPrediction']].assign(
            response_var=response,
            model_type=model_type,
            extra_calibration=extra_calibration,
            root_folder=root_folder
        )], axis = 0, ignore_index = True)
assessments = assessments.rename(columns = {'Assessment': 'AssessmentCategory'}).drop(columns = ['ProbabilityRange'])

try:
    write_to_supabase_predictions_table('ModelSelections', assessments)
    print('Successfully wrote')
except Exception as e:
    print(f"Error writing: {e}")


#fix file path to include actual model and maybe standardize this process so it can easily be done in the predict script.
#add additional columns to supabase





#%%