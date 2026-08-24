#%%


import os
os.environ["KERAS_BACKEND"] = "torch"
import xgboost
from sklearn.base import clone
from pathlib import Path
import joblib
import pandas as pd
import keras
from sklearn.linear_model import LogisticRegression
from sklearn.calibration import CalibratedClassifierCV
from keras.callbacks import EarlyStopping
from model_functions import standardize_and_encode, create_model_directories, read_from_supabase, supabase

model_lookup_table = read_from_supabase('predictions', 'ModelSelections')

full_data_responses = {f.stem for f in Path('./ml_ready_data/fulldata/').glob('*.parquet')}
selection_defs = model_lookup_table[["response_var", "model_type", "extra_calibration", "training_root_folder"]].drop_duplicates()
conflicts = selection_defs.groupby("response_var").size().loc[lambda x: x > 1]
if not conflicts.empty:
    raise ValueError(f"Conflicting ModelSelections: {conflicts.index.tolist()}")
selected_models = selection_defs.reset_index(drop=True)
missing_full_data = set(selected_models['response_var']) - full_data_responses
if missing_full_data:
    raise ValueError(
        f"Selected models missing full-data files: {sorted(missing_full_data)}"
    )
for file in Path('./ml_ready_data/fulldata/').glob('*.parquet'):
    response = file.stem
    if ('rushing_receiving' in response) or (response == 'anytime_td_scorer'):
        print(f"Beginning fitting model for {response}")
        train = pd.read_parquet(file)
        train = train.drop(columns = ['gsis_id', 'team', 'season', 'display_name', 'game_id', 'jersey_number'], errors = 'ignore')
        train[response]= pd.Categorical(train[response])
        train[train.select_dtypes(include=['bool']).columns] = train[train.select_dtypes(include=['bool']).columns].astype(int)

        X_train = train.drop(columns = [response])
        Y_train = train[response].astype(int)
        
        this_model_rows = selected_models[selected_models["response_var"] == response]
        if this_model_rows.empty:
            print(f"Skipping {response}: no selected model")
            continue
        this_selection = this_model_rows.iloc[0]

        model_selection = this_selection['model_type']
        calibrated = this_selection["extra_calibration"]
        if isinstance(calibrated, str):
            calibrated = calibrated.lower() == "true"
        else:
            calibrated = bool(calibrated)
        training_model_path = Path(this_selection["training_root_folder"])
        
        def refit_tree_model(X_train, Y_train, new_folder_to_save, model_selection, calibrated, response, id_cols):
            old_folder = training_model_path / model_selection
            uncalibrated_model = joblib.load(old_folder / "uncalibrated_model.joblib")
            feature_columns = joblib.load(old_folder / "feature_columns.joblib")
            if model_selection == "xgb":
                old_categorical_columns = joblib.load(
                    old_folder / "categorical_columns.joblib"
                )
            bad_feature_cols = [c for c in feature_columns if c in id_cols]
            if bad_feature_cols:
                print(f"{response} / {model_selection} was originally trained with identifier columns: {bad_feature_cols}.")
            if model_selection == 'rf':
                X_model = pd.get_dummies(X_train, dummy_na=True, dtype=float)
                X_model = X_model.reindex(columns=feature_columns, fill_value=0)
                X_model = X_model.fillna(-999)
            elif model_selection == 'xgb':
                X_model = X_train.reindex(columns=feature_columns).copy()
                old_numeric_columns = [col for col in feature_columns if col not in old_categorical_columns]
                for col in old_numeric_columns:
                    X_model[col] = pd.to_numeric(X_model[col], errors="coerce")
                for col in old_categorical_columns:
                    X_model[col] = X_model[col].astype("category")
            else:  # gbm
                X_model = X_train.reindex(columns=feature_columns)
                for col in X_model.columns:
                    if pd.api.types.is_numeric_dtype(X_model[col]):
                        X_model[col] = pd.to_numeric(X_model[col], errors="coerce")
                    else:
                        X_model[col] = X_model[col].astype("category")
            joblib.dump(feature_columns, new_folder_to_save / "feature_columns.joblib")
            if not calibrated:
                new_uncalibrated = clone(uncalibrated_model)
                new_uncalibrated.fit(X_model, Y_train)
                joblib.dump(new_uncalibrated, new_folder_to_save / "model.joblib")
            else:
                new_calibrated = CalibratedClassifierCV(
                    estimator=clone(uncalibrated_model),
                    method="sigmoid",
                    cv=5
                )
                new_calibrated.fit(X_model, Y_train)
                joblib.dump(new_calibrated, new_folder_to_save / "model.joblib")

        def refit_neural_net_model(X_train, Y_train, training_model_path, new_folder_to_save, calibrated):
            old_folder = training_model_path / "neural_net"
            old_preprocessing = joblib.load(old_folder / "preprocessing.joblib")
            old_raw_feature_columns = old_preprocessing["raw_feature_columns"]
            old_numeric_columns = old_preprocessing["numeric_columns"]
            old_categorical_columns = old_preprocessing["categorical_columns"]
            missing_old_nn_cols = [c for c in old_raw_feature_columns if c not in X_train.columns]
            if missing_old_nn_cols:
                raise ValueError(
                    f"Neural net full fit is missing old raw NN features: {missing_old_nn_cols}"
                )
            extra_full_cols_ignored = [c for c in X_train.columns if c not in old_raw_feature_columns]
            if extra_full_cols_ignored:
                print(f"Ignoring extra full-fit NN columns: {extra_full_cols_ignored}")
            X_train = X_train[old_raw_feature_columns].copy()
            for col in old_numeric_columns:
                X_train[col] = pd.to_numeric(X_train[col], errors="coerce")
            for col in old_categorical_columns:
                X_train[col] = X_train[col].astype("object")
            X_train_standardized, _, preprocessing_artifacts = standardize_and_encode(X_train,X_train)
            params = joblib.load(old_folder / "selected_params.joblib")
            if calibrated:
                calibration_split = int(len(X_train_standardized) * 0.85)
                X_model_train = X_train_standardized.iloc[:calibration_split]
                Y_model_train = Y_train.iloc[:calibration_split]
                X_calibration = X_train_standardized.iloc[calibration_split:]
                Y_calibration = Y_train.iloc[calibration_split:]
            else:
                X_model_train = X_train_standardized
                Y_model_train = Y_train
            keras.utils.set_random_seed(123)
            model = keras.Sequential()
            model.add(keras.layers.Input(shape=(X_train_standardized.shape[1],)))
            model.add(keras.layers.Dense(params["dense_layer_1"], activation=params["activation"]))
            if params["dense_layer_2"] is not None:
                model.add(keras.layers.Dense(params["dense_layer_2"], activation=params["activation"]))
            model.add(keras.layers.Dropout(params["dropout_rate"]))
            model.add(keras.layers.Dense(1, activation="sigmoid"))
            if params["optimizer_name"] == "adam":
                optimizer = keras.optimizers.Adam(learning_rate=params["learning_rate"])
            elif params["optimizer_name"] == "rmsprop":
                optimizer = keras.optimizers.RMSprop(learning_rate=params["learning_rate"])
            else:
                optimizer = keras.optimizers.Adam(learning_rate=params["learning_rate"])
            model.compile(
                optimizer=optimizer,
                loss="binary_crossentropy"
            )
            early_stop = EarlyStopping(
                monitor="val_loss",
                patience=params.get("early_stopping_patience", 5),
                restore_best_weights=True
            )
            model.fit(
                X_model_train,
                Y_model_train,
                epochs=params.get("epochs", 100),
                batch_size=params["batch_size"],
                validation_split=0.2,
                callbacks=[early_stop],
                verbose=0
            )
            model.save(new_folder_to_save / "uncalibrated_model.keras")
            joblib.dump(preprocessing_artifacts, new_folder_to_save / "preprocessing.joblib")
            joblib.dump(params, new_folder_to_save / "selected_params.joblib")
            #calibrated_model:
            if calibrated:
                uncalibrated_calibration_preds = model.predict(
                    X_calibration,
                    verbose=0
                ).ravel()

                calibrator = LogisticRegression()
                calibrator.fit(
                    uncalibrated_calibration_preds.reshape(-1, 1),
                    Y_calibration
                )

                joblib.dump(
                    calibrator,
                    new_folder_to_save / "calibrated_model.joblib"
                )

        if model_selection in ['gbm','rf','xgb']:
            print('skipping tree')
            response_folder, new_folder_to_save  = create_model_directories(response, model_names = [model_selection], training_mode = False)
            refit_tree_model(X_train, Y_train, new_folder_to_save, model_selection, calibrated, response, ['gsis_id','week','season','jersey_number','game_id','team','display_name'])
        elif model_selection == 'neural_net':
            response_folder, new_folder_to_save  = create_model_directories(response, model_names = [model_selection], training_mode = False)
            refit_neural_net_model(X_train, Y_train, training_model_path, new_folder_to_save, calibrated)
            keras.backend.clear_session()
            import gc
            gc.collect()
        else: #either type of ensemble
            if model_selection == 'full ensemble':
                response_folder, xgb_folder, rf_folder, gbm_folder, neural_net_folder   = create_model_directories(response, training_mode = False)
                refit_neural_net_model(X_train, Y_train, training_model_path, neural_net_folder, calibrated)
                keras.backend.clear_session()
                import gc
                gc.collect()
            else:
                response_folder, xgb_folder, rf_folder, gbm_folder = create_model_directories(response, model_names = ['xgb','rf','gbm'], training_mode = False)
            refit_tree_model(X_train, Y_train, xgb_folder, "xgb", calibrated, response, ['gsis_id','week','season','jersey_number','game_id','team','display_name'])
            refit_tree_model(X_train, Y_train, gbm_folder, "gbm", calibrated, response, ['gsis_id','week','season','jersey_number','game_id','team','display_name'])
            refit_tree_model(X_train, Y_train, rf_folder, "rf", calibrated, response, ['gsis_id','week','season','jersey_number','game_id','team','display_name'])
        fullfit_root_folder = f"./models/full_fit/{response}/"

        supabase.schema('predictions').table('ModelSelections').update({'fullfit_root_folder': fullfit_root_folder})\
            .eq('response_var', response).execute()


    #cleanup older, stale models that no longer apply but are sitting in the directory:
    import shutil

    expected_model_folders = {
        "xgb": {"xgb"},
        "rf": {"rf"},
        "gbm": {"gbm"},
        "neural_net": {"neural_net"},
        "tree ensemble": {"xgb", "gbm", "rf"},
        "full ensemble": {"xgb", "gbm", "rf", "neural_net"},
    }

    for _, row in selected_models.iterrows():
        response = row["response_var"]
        model_selection = row["model_type"]
        response_folder = Path("./models/full_fit") / response
        if not response_folder.exists():
            continue
        keep_folders = expected_model_folders[model_selection]
        for model_folder in ["xgb", "rf", "gbm", "neural_net"]:
            model_path = response_folder / model_folder
            if model_path.exists() and model_folder not in keep_folders:
                print(f"Removing stale model folder: {model_path}")
                shutil.rmtree(model_path)
#%%

