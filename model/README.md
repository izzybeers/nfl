# NFL Betting Models

This folder contains the modeling, probability calibration, model-selection, inference, expected-value, and portfolio-optimization components of the NFL Bet Recommender.

The goal of the modeling system is not simply to classify whether an outcome will occur. The downstream betting system relies directly on the predicted probabilities, so **probability quality and calibration are primary modeling objectives**.

Model outputs are ultimately exposed through a FastAPI service deployed on **Google Cloud Platform**, where they are used by the data-collection pipeline and Shiny front end.

## Modeled Outcomes

The project fits separate binary probability models for a range of player and team outcomes.

### Player Models

- passing yards
- rushing yards
- receiving yards
- rushing + receiving yards
- receptions
- anytime touchdown scorer

For yardage and reception props, separate response variables are created for the modeled thresholds.

Examples:

```text
passing_yards_240
rushing_yards_60
receiving_yards_80
rushing_receiving_yards_100
receptions_6
```

Each model estimates the probability that its corresponding outcome occurs, which matches with a bet threshold on DraftKings.

### Team Models

- `team_win` — probability that a team wins the game
- `team_differential_*` — probability associated with modeled point-differential thresholds

These probabilities can then be matched to sportsbook moneylines and spread markets by the front end.

## Modeling Pipeline

At a high level:

```text
Raw / engineered NFL data
          ↓
Data preparation + feature selection
          ↓
Chronological train / validation / holdout split
          ↓
Train candidate model families
          ↓
Evaluate calibration + log loss
          ↓
Select model for each response variable
          ↓
Refit selected model on full fitting data
          ↓
Save production model artifacts
          ↓
Serve predictions through GCP API
```

Each response variable is treated independently. This allows different outcomes and thresholds to use different model families when their validation performance differs.

## Model Families

Candidate models include:

### XGBoost

`XGBClassifier` models are tuned with randomized hyperparameter search using 5-fold cross-validation.

Parameters considered include:

- number of estimators
- tree depth
- learning rate
- minimum child weight
- subsampling rate

Hyperparameter search is scored using **log loss**.

### Random Forest

Random Forest classifiers are also tuned with randomized search and log-loss-based cross-validation.

Categorical features are one-hot encoded and the final training feature set is saved so prediction-time data can be aligned to the original model schema.

### GBM

Scikit-learn's `HistGradientBoostingClassifier` provides another tree-based candidate.

The preprocessing pipeline handles:

- categorical variables
- rare categorical levels
- constant features
- features that become unstable within cross-validation folds

Permutation importance is used to estimate feature importance for these models.

### Neural Network

Neural-network candidates are built with Keras.

The network uses:

- standardized numeric features
- one-hot encoded categorical features
- one or two dense layers
- ReLU activation
- optional dropout
- Adam or RMSprop optimization
- early stopping

Candidate architectures are evaluated across multiple random seeds.

Preprocessing objects are saved with the model so production data can be transformed using the exact same schema as the training data.

## Ensembles

In addition to individual models, the system evaluates two ensemble types:

### Tree Ensemble

Combines predictions from:

- XGBoost
- Random Forest
- Gradient Boosting

### Full Ensemble

Combines:

- XGBoost
- Random Forest
- Gradient Boosting
- Neural Network

The selected model can therefore differ substantially across response variables. One prop threshold may use an individual Random Forest while another uses a neural network or ensemble.

## Probability Calibration

Because the betting system calculates expected value directly from model probabilities, calibration is treated as a first-class modeling problem.

Tree models are evaluated both:

- without additional calibration
- with sigmoid calibration using `CalibratedClassifierCV`

Neural networks can receive an additional logistic-regression calibration layer fitted to their raw predicted probabilities.

The model-selection process therefore considers both the underlying model and whether additional calibration improves its probability estimates.

## Calibration Assessment

Calibration is evaluated by dividing predictions into probability ranges and comparing:

```text
mean predicted probability
          vs.
actual observed hit rate
```

The bins become progressively wider as probabilities increase:

- below 10%: 2-percentage-point bins
- 10%–30%: 5-percentage-point bins
- above 30%: 10-percentage-point bins

Bins with enough observations are classified as:

- `Inside Target Range`
- `Near Target Range`
- `Bad`

A custom calibration score measures the weighted distance between the average model probability and actual hit rate across probability bins.

Calibration is also evaluated separately for:

```text
< 30%
30%–60%
> 60%
```

This helps identify models that appear well calibrated overall but perform poorly in the probability ranges most relevant to particular types of bets.

## Model Evaluation and Selection

The current modeling cycle uses a chronological split:

```text
2022–2023 → training
2024      → validation / model selection
2025      → final untouched holdout
```

The chronological design prevents future seasons from influencing model selection for earlier seasons.

Candidate models are evaluated using metrics including:

- validation log loss
- baseline log loss
- improvement over baseline log loss
- calibration score
- proportion of predictions falling into poorly calibrated probability bins

Traditional classification accuracy is not the primary objective because the betting system needs reliable probabilities rather than only binary predictions.

A model that predicts a 20% event at 20% is useful to an EV system even though that individual outcome will usually lose.

## Feature Selection

The initial modeling datasets contain a very large number of engineered NFL features.

`data_prep_functions.R` reduces this feature space using an **Information Value tournament**.

Features are organized by:

- scope
  - player
  - team
  - opponent
- timeframe
  - current season
  - recent games
  - historical seasons
- statistical type
  - opportunity (snaps, targets, carries)
  - production (yards, receptions, completions)
  - efficiency (yards per carry, yards per reception, completion rate)

Within each group, the most informative variables are retained based on Information Value.

Highly correlated candidates are then reduced so that the final datasets contain a smaller set of informative, less-redundant features.

The feature-selection process is fit only using the training data and then applied consistently to validation, holdout, full-fit, and prediction datasets.

## Full-Fit Models

After model selection is complete, `fit_models_on_full_data.py` rebuilds the selected production models using the full fitting dataset.

The selected model configuration for each response variable is stored in Supabase in:

```text
predictions.ModelSelections
```

The refitting process:

1. reads the selected model type and calibration decision
2. loads the selected training configuration
3. fits the model using the full available fitting dataset
4. saves required preprocessing and model artifacts
5. removes stale model artifacts that are no longer part of the selected model
6. updates the production model location in Supabase

Production artifacts are organized approximately as:

```text
models/
├── training/
│   └── <response variable>/
└── full_fit/
    └── <response variable>/
        ├── xgb/
        ├── rf/
        ├── gbm/
        └── neural_net/
```

Only the model directories required by the selected individual model or ensemble are retained.

Depending on model type, saved artifacts can include:

```text
uncalibrated_model.joblib
uncalibrated_model.keras
calibrated_model.joblib
preprocessing.joblib
selected_params.joblib
feature_columns.joblib
```

## Production Prediction API

Production model inference is exposed through a **FastAPI service deployed to Google Cloud Run**.

### `/predict`

Accepts a response variable and prepared model data.

The API:

1. reads the selected production model from `ModelSelections`
2. loads the appropriate full-fit model artifacts
3. applies the saved preprocessing pipeline
4. applies calibration if selected
5. returns `Model_Probability`

The weekly data-collection pipeline uses this endpoint to generate current player and team predictions.

### `/get_bets_with_ev`

Calculates betting expected value and risk from:

- model probability
- sportsbook odds
- model-quality information
- bet characteristics

The endpoint can screen the available bets and return positive-EV opportunities for use by the front end and optimizer.

### `/get_portfolio`

Constructs optimized portfolios from the available bets.

The optimizer accounts for:

- expected returns
- individual bet risk
- relationships/correlations between bets
- maximum number of bets
- portfolio standard-deviation constraint
- total wager amount
- minimum stake size

It returns recommended portfolio weights and stake amounts along with portfolio-level metrics such as expected return, variance, and standard deviation.

## Relationship to the Rest of the Project

```text
data_collection/
      ↓
model/ml_ready_data/
      ↓
model training + selection
      ↓
models/full_fit/
      ↓
GCP FastAPI service
   ↙           ↘
weekly         EV + portfolio
predictions    optimization
   ↓               ↓
Supabase      Shiny front end
```

The `data_collection` pipeline creates the modeling inputs and requests weekly predictions.

Supabase stores the resulting probabilities and model-selection metadata.

The Shiny front end combines those probabilities with current DraftKings lines and uses the GCP API for EV calculations and portfolio optimization.

## Key Files

### `model_functions.py`

Shared Python functionality for:

- model fitting
- calibration
- preprocessing
- prediction
- probability evaluation
- Supabase access
- betting and portfolio helper functions

### `data_prep_functions.R`

Transforms the large engineered datasets produced by `data_collection` into model-ready response-specific datasets.

Responsibilities include:

- constructing binary response variables
- chronological splitting
- missing-data indicators
- Information Value calculations
- correlation-based feature reduction
- applying the selected feature set consistently across datasets

### `fit_models_on_full_data.py`

Refits the selected production model for each response variable and saves the resulting full-fit model artifacts.

### `main.py`

FastAPI application deployed to GCP.

Provides production endpoints for:

```text
/predict
/get_bets_with_ev
/get_portfolio
```

### `ml_ready_data/`

Contains the response-specific datasets generated by the R data-preparation pipeline.

Typical structure:

```text
ml_ready_data/
├── train/
├── test/
├── final_test/
└── fulldata/
```

## Python Dependencies

The production model/API environment is defined by the project's deployment requirements file.

Major dependencies include:

- NumPy
- pandas
- scikit-learn
- XGBoost
- Keras
- PyTorch backend
- SciPy
- statsmodels
- joblib
- Supabase
- FastAPI
- Uvicorn

## Security

Model and API source code can be committed to Git.

Credentials should not be committed, including:

- Supabase credentials
- `.env`
- service-account credentials
- API keys or tokens

Secrets are supplied through environment variables in local and production environments.