# NFL Betting Model

An end-to-end machine learning system for generating NFL betting probabilities and constructing risk-aware betting portfolios.

The project combines NFL data collection, feature engineering, probability modeling, calibration, betting-market analysis, and portfolio optimization into a weekly prediction pipeline. Models generate probabilities for team and player outcomes, which are compared against sportsbook odds to identify positive expected-value opportunities and construct diversified portfolios subject to risk constraints.

> **Status:** Core model and backend development is complete. Deployment and frontend integration are in progress.

---

## Overview

Most sports prediction projects focus on predicting winners or maximizing classification accuracy. This project is built around a different objective:

**produce well-calibrated probabilities that can be converted into betting decisions.**

A probability of 70% is useful only if events assigned roughly 70% probability actually occur about 70% of the time. Because expected value depends directly on predicted probability, model evaluation emphasizes calibration and log loss rather than accuracy alone.

The full pipeline includes:

- automated NFL data collection and feature engineering
- separate models for team and player betting markets
- multiple machine learning model families and ensembles
- probability calibration and out-of-sample validation
- sportsbook odds integration and expected-value calculation
- correlation-aware portfolio optimization
- API endpoints for prediction, bet evaluation, and portfolio construction
- database storage for historical and live prediction data

---

## Markets Modeled

The system produces probabilities for a range of NFL outcomes.

### Team Markets

- Moneyline / team win probability
- Team spread or point-differential thresholds

### Player Markets

- Passing yards
- Rushing yards
- Receiving yards
- Receptions
- Combined rushing + receiving yards
- Anytime touchdown scorer

Each continuous player outcome is modeled as a collection of probability thresholds. For example, rather than predicting a single expected receiving-yard value, the system estimates probabilities for exceeding several receiving-yard thresholds, corresponding with player prop bets.

This allows the same modeling framework to be compared directly against sportsbook prop lines, such as Passing Yards 150+ Yards or Receiving Yards 60+ Yards.

---

## System Architecture

At a high level, the production workflow is:

```text
R Data Collection: from NFLReadR and playoff clinching scenarios articles
      |
      v
R Feature Engineering: rolling and historical performance aggregates, blue chip impact metrics, usage-adjusted target shares, opponent/matchup features, roster and depth-chart context, game-environment variables, early season / rookie fallback priors, and playoff clinching scenarios.
      |
      v
Model-Ready Prediction Data: to trim the dataset from 3000 columns to ~100-300, an information value analysis is run to choose the most informative set of uncorrelated columns. 
      |
      v
Machine Learning Fitting: for each bet type (passing 150, passing 180, ... , rushing 25 yards, rushing 40 yards, ... anytime td scorer ... team win ... etc), fit the following models: random forest, GBM, XGBoost, neural net, tree ensembles, and full ensembles. Select the best model type for each bet. First run with a train/test/holdout split, and once model selection and holdout evaluation are complete, rerun the models on the full dataset.
      |
      v
Python Prediction API: once the models are complete, the API takes in a set of bets, runs the corresponding models on them and generates a probability.
      |
      v
Supabase: stores the model predictions into a database.
      |
      v
Sportsbook Lines + Predictions: when using the app live, the betting lines are scraped in real time.
      |
      v
Expected Value API: An API takes in a set of bets and their corresponding model probabilities and sportsbook odds to calculate expected value and risk.
      |
      v
Portfolio Optimization API: The API takes in a group of bets and their expected values and risks, and returns a set of betting portfolios that take into account risk thresholds and correlations between bets (bets for 2 players on the same team would be correlated, for example).
      |
      v
Frontend: A shiny app displays individual bet recommendations along with portfolio recommendations using the user's preferred risk tolerance.