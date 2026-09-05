# NFL Data Collection & Feature Engineering

This folder contains the R-based data collection and feature-engineering pipeline for the NFL Bet Recommender.

The pipeline builds historical training datasets and current-week prediction datasets from player, team, opponent, roster, injury, schedule, weather, and other contextual information.

Its output feeds the machine-learning models in the `model` folder.

## Pipeline Overview

At a high level:

```text
NFL schedules
Player data
Team data
Opponent data
Depth charts / rosters
Injuries
Playoff context
Weather
        ↓
Data cleaning and joins
        ↓
Historical / rolling features
        ↓
Player usage and workload features
        ↓
Blue-chip player analysis
        ↓
Player-specific and team-specific datasets
        ↓
Model preparation + feature selection
        ↓
Training data OR current-week predictions
```

The same core feature-engineering pipeline is used for both historical model development and live weekly predictions so that training-time and prediction-time data remain consistent.

## Main Entry Point

### `data_collection.R`

`data_collection.R` orchestrates the complete pipeline.

The primary function is:

```r
data_collection(
  mode,
  min_year,
  max_year,
  wk,
  num_iv_winners = 10,
  test_mode
)
```

The pipeline supports three primary modes.

## Pipeline Modes

### `train`

Builds historical data for model development.

After feature engineering and model preparation, response-specific Parquet files are written to:

```text
model/ml_ready_data/train/
model/ml_ready_data/test/
model/ml_ready_data/final_test/
```

The current chronological split is:

```text
2022–2023 → training
2024      → validation / model selection
2025      → final holdout
```

### `full_fit`

Creates the datasets used to refit the selected production models after model selection.

Outputs are written to:

```text
model/ml_ready_data/fulldata/
```

These files are consumed by `fit_models_on_full_data.py`.

### `predict`

Builds data for a specific upcoming NFL week.

In this mode the pipeline:

1. retrieves current schedule, player, team, roster, injury, and other available information
2. rebuilds the engineered features for the requested week
3. applies the same response-specific data preparation used during training
4. sends each prepared dataset to the GCP `/predict` endpoint
5. receives model probabilities
6. restores player/team and game identifiers
7. writes the current predictions to Supabase

Predictions are stored in:

```text
predictions.PlayerPredictions
predictions.TeamPredictions
```

with an `updated_at` timestamp.

To refresh injuries, weather, depth-chart information, or other information later in the week, the normal `predict` pipeline can simply be rerun.

## Response Families

The pipeline prepares data for eight primary model families.

### Player

```text
passing_yards
rushing_yards
receiving_yards
anytime_td_scorer
rushing_receiving_yards
receptions
```

### Team

```text
team_win
team_differential
```

For yardage, receptions, and point differential, the model-preparation stage creates separate binary response variables for the thresholds used by the betting system.

## Major Data-Collection Components

### Team Data

Team-level data is assembled in `scripts/team_game_data.R`.

The resulting features describe areas such as:

- offensive production
- defensive production
- passing and rushing volume
- efficiency
- scoring
- red-zone performance
- penalties
- game results
- historical and rolling team performance
- opponent performance

For the team models, the pipeline constructs four conceptual groups:

```text
team offense
team defense
opponent offense
opponent defense
```

These are combined to describe each matchup from the perspective of the team being modeled.

### Player Data

Player-level data is assembled in `scripts/player_data.R`.

The pipeline produces model-specific datasets based on position:

| Model | Included Positions |
| --- | --- |
| Passing | QB |
| Rushing | QB, RB |
| Receiving | RB, WR, TE |
| Receptions | RB, WR, TE |
| Rushing + Receiving | QB, RB, WR, TE |
| Touchdowns | QB, RB, WR, TE |

Player data is joined to relevant team, opponent, game, injury, and roster information before model preparation.

## Historical and Rolling Features

The pipeline creates features representing both recent performance and longer-term context.

These include measures based on:

- current-season performance
- recent games
- previous season
- two seasons ago
- lagged outcomes
- team performance
- opponent performance
- matchup history

The calculations are designed so that a row represents information that would have been available **before that game**, preventing current-game results from leaking into model inputs.

Missing-data masks are also created for cases such as:

- Week 1
- rookies
- players without prior-season history
- unavailable lagged statistics

## Quarterback Context

Receiver-related models incorporate information about the quarterback expected to play with the receiver.

Examples include quarterback-level measures for:

- passing yards
- aggressiveness
- air yards per attempt
- air yards relative to the sticks
- bad-throw rate
- sacks
- passing attempts

This allows receiving, reception, touchdown, and combined rushing/receiving models to account for the offensive environment surrounding the player.

## Usage, Promotion, and Demotion Features

A substantial part of the player feature engineering attempts to estimate a player's expected role rather than relying only on raw recent usage.

### True Talent Baseline

The pipeline estimates a player's normal expected share of team volume.

For receivers, this is based on target share.

For rushers, it is based on carry share.

Early in a season, the baseline blends current-year usage with prior-season information. For players without established NFL history, rookie estimates provide a fallback based on factors including draft round and expected team offensive volume.

As more current-season games are played, the player's current role receives increasing weight.

### Adjusted Share

The baseline is recalculated using the currently active players on the roster.

This produces features such as:

```text
adjusted_target_share
adjusted_rush_share
```

which estimate how the available volume may be redistributed when teammates are unavailable.

### Promotion / Demotion Indicators

The pipeline also calculates features including:

```text
delta_adjusted_share_receiving
delta_adjusted_share_rushing
recent_form_delta_receiving
recent_form_delta_rushing
```

These help distinguish between:

- a player's normal role
- a temporary opportunity caused by an absent teammate
- a recent increase or decrease in usage
- a more persistent role change

## Blue-Chip Player Analysis

`scripts/blue_chip_analysis.R` identifies particularly important offensive and defensive players whose availability may materially affect team performance.

Separate analyses are performed for areas including:

- quarterback passing
- rushing
- receiving
- defensive pass rush
- run defense
- secondary/pass defense

The analysis considers factors such as:

- player production across the current and previous seasons
- share of team production
- how the team performs when the player plays versus when the player is absent
- whether performance remained stable after a team change

The resulting features indicate both:

- whether a player qualifies as a blue-chip contributor
- whether the team or opponent currently has a blue-chip player unavailable

These features are incorporated into both player and team models.

## Injuries and Availability

Current and historical injury information is incorporated into model data.

The pipeline includes flags for conditions such as:

```text
less_practice
illness
out_not_injury_related
out
```

In prediction mode, injury retrieval is designed to fail gracefully if the external injury dataset is temporarily unavailable rather than preventing the entire weekly pipeline from running.

Depth-chart and roster information is also used to determine player roles and expected starting quarterbacks.

## Weather

Prediction-mode data includes available weather information for upcoming games.

Weather-derived context includes:

- temperature
- wind
- whether the team is accustomed to unusually hot or cold conditions
- the corresponding weather familiarity of the opponent (ex: Miami Dolphins playing a winter game in Buffalo)

Because forecasts change as kickoff approaches, rerunning the prediction pipeline later in the week refreshes these inputs.

## Playoff and Game Context

The pipeline also incorporates contextual information including:

- playoff-clinching status
- season stage
- home / away / neutral location
- divisional games
- travel
- short and long weeks
- schedule timing
- player age
- other game-level context

## Building the Model-Specific Datasets

After the shared feature-engineering stages are complete, the full dataset is split into response-specific datasets.

Irrelevant target variables and feature families are removed from each model.

For example:

- passing models remove unrelated rushing and receiving outcomes
- receiving models emphasize receiving-relevant player and QB information
- team moneyline models exclude the point-differential target
- team differential models exclude the win target

This prevents unrelated outcome fields from being used as predictors.

## Model Preparation and Feature Selection

The pipeline calls:

```text
model/data_prep_functions.R
```

to convert each engineered dataset into the final machine-learning input.

This stage handles:

- binary response construction
- missing-data masks
- chronological splitting
- Information Value analysis
- feature categorization
- correlation-based feature reduction
- final model-column selection

The Information Value process groups statistical features by:

```text
Scope:
- Player
- Team
- Opponent

Timeframe:
- Current Season
- Recent Games
- Historical Seasons

Type:
- Opportunity (snaps, targets, carries)
- Production (yards, completions, receptions)
- Efficiency (yards per carry, yards per reception, completion rate)
```

The strongest candidates from each category are retained, with highly correlated variables subsequently reduced.

This takes the very large feature-engineering output and creates a substantially smaller model-ready feature set.

## Current-Week Prediction Flow

```text
data_collection(mode = "predict")
             ↓
current player/team/game data
             ↓
feature engineering
             ↓
model_prep()
             ↓
response-specific prediction datasets
             ↓
GCP /predict API
             ↓
Model_Probability
             ↓
restore player/team identifiers
             ↓
Supabase
├── PlayerPredictions
└── TeamPredictions
             ↓
Shiny front end
```

The data-collection layer prepares the inputs; the Python model service performs the actual production inference.

## Scripts

### `data_collection.R`

Main pipeline orchestrator.

### `scripts/global.R`

Shared configuration, lookup, and utility functionality used by the data-collection scripts.

### `scripts/player_data.R`

Collects and engineers player-level statistics and historical features.

### `scripts/team_game_data.R`

Collects and engineers team, opponent, and game-level features.

### `scripts/get_injuries.R`

Retrieves and standardizes injury information.

### `scripts/get_playoff_clinching_data.R`

Collects playoff-clinching and late-season context.

### `scripts/blue_chip_analysis.R`

Contains the analysis used to identify high-impact offensive and defensive players and incorporate their availability into the model data.

### `../model/data_prep_functions.R`

Shared model-preparation and feature-selection functions used after the raw feature-engineering pipeline is complete.

## Running the Pipeline

Example current-week prediction run:

```r
source("data_collection.R")

season = 2026
week = 1

data_collection(
  mode = "predict",
  min_year = 2020,
  max_year = season,
  wk = week,
  num_iv_winners = 10,
  test_mode = FALSE
)
```

For historical model preparation:

```r
data_collection(
  mode = "train",
  min_year = 2020,
  max_year = 2025,
  wk = NULL,
  num_iv_winners = 10,
  test_mode = FALSE
)
```

For creating the full-fit datasets after model selection:

```r
data_collection(
  mode = "full_fit",
  min_year = 2020,
  max_year = 2025,
  wk = NULL,
  num_iv_winners = 10,
  test_mode = FALSE
)
```

The exact season bounds should be updated when a new model-training cycle is performed.

## Test Mode

`test_mode = TRUE` provides additional validation behavior without using the normal production workflow.

Among other checks, the current pipeline can write intermediate model datasets to `validation_files/` so that the final player and team inputs can be inspected before production predictions are written.

## External Services

### Supabase

Supabase stores persistent project data used by the pipeline, including:

- lookup tables
- selected model metadata
- blue-chip results
- playoff information
- player predictions
- team predictions

### GCP Model API

Production predictions are generated by the GCP-hosted model API.

The data-collection pipeline calls:

```text
/predict
```

for each prepared response-variable dataset.

EV and portfolio endpoints are part of the same GCP API, but are primarily consumed by the front-end application rather than by the normal data-collection workflow.

## Security

Credentials and configuration secrets should not be committed to Git.

This includes:

- `.Renviron`
- `.env`
- Supabase keys
- API credentials
- cloud service credentials

Secrets should instead be supplied through environment variables.