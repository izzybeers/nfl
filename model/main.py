#%%

from fastapi import FastAPI
from pydantic import BaseModel
import pandas as pd
from datetime import datetime
import numpy as np
from time import perf_counter
from model_functions import read_from_supabase, get_prediction_by_model_type, calculate_probability_ranges
from portfolio_optimization_functions import remove_large_monotonicity_violations, EV_CAPS, get_mu_sigma, calculate_weights, measure_portfolio_metrics, get_portfolios_from_weights, get_amounts_from_portfolio, find_best_portfolio_by_mu_with_sd_cap


app = FastAPI()

class PredictionRequest(BaseModel):
    response_var: str
    data: dict[str, list]

@app.post('/predict')
def predict(request: PredictionRequest):
    df = pd.DataFrame(request.data)
    if df.empty:
        raise ValueError ('Provided dataframe is empty.')
    r = request.response_var
    model_lookup_table = read_from_supabase('predictions', 'ModelSelections')
    model_mapping = model_lookup_table[['response_var', 'model_type', 'fullfit_root_folder', 'extra_calibration']].drop_duplicates()
    selected_model =  model_mapping[model_mapping['response_var'] == r]
    if selected_model.empty:
        raise ValueError(f'No model found for response var: {r}')
    model_type = selected_model['model_type'].iloc[0]
    extra_calibration = selected_model['extra_calibration'].iloc[0]
    root_folder = selected_model['fullfit_root_folder'].iloc[0]
    preds = get_prediction_by_model_type(r, model_type, root_folder, df, extra_calibration)

    if 'team' in r:
        results = pd.DataFrame({
        'response_var': r,
        'Week': df['week'].astype(int),
        'team': df['team'],
        'Model_Probability': preds
    })
    else:
        results = pd.DataFrame({
            'response_var': r,
            'Week': df['week'].astype(int),
            'gsis_id': df['gsis_id'],
            'Model_Probability': preds
        })
    return results.to_dict(orient='records')

class Portfolio:
    def __init__(self, season):
        max_year = season - 1 #correlations calculated up through the prior season
        corr = read_from_supabase('MainData', 'Correlations').query('max_year == @max_year')
        corr['created_at'] = pd.to_datetime(corr['created_at'])
        latest_timestamp = corr['created_at'].max()
        if corr.empty:
            raise ValueError(f'No correlations found through {max_year}.')
        self.correlations = corr[corr['created_at'] == latest_timestamp]
        self.model_selections = read_from_supabase('predictions', 'ModelSelections')

    def calculate_ev_and_risk(self, df, penalize_by_model = False, positive_only = True, apply_ev_caps = False):
        model_lookup_table = self.model_selections[['response_var', 'model_type', 'fullfit_root_folder', 'extra_calibration', 'model_assessment']].drop_duplicates()
        ev_caps = pd.DataFrame({'Type': list(EV_CAPS.keys()), 'cap': list(EV_CAPS.values())})
        df = df.assign(Odds = lambda x: pd.to_numeric(x['Odds'], errors='coerce'),
                    ProfitPer100 = lambda x: np.where(x['Odds'] > 0, x['Odds'], 100**2/(-1*x['Odds'])),
                    EVProfitPer100 = lambda x: x['Model_Probability']*x['ProfitPer100'] - 100*(1-x['Model_Probability']),
                    Type = lambda x: np.where(
                        x['response_var'].isin(['anytime_td_scorer', 'team_win']),
                        x['response_var'],
                        x['response_var'].str.rsplit('_',n=1).str[0]
                    ),
                    Risk_Raw = lambda x: x['Model_Probability']*(1 - x['Model_Probability'])*(x['ProfitPer100']/100 + 1)**2)
        if apply_ev_caps:
            df = df.merge(ev_caps, on = 'Type', how = 'left').assign(cap=lambda x: x["cap"].fillna(np.inf)).query('EVProfitPer100 <= cap').drop(columns = 'cap')
        if positive_only:
            df = df.query('EVProfitPer100 > 0')
        if penalize_by_model:
            df = df.merge(model_lookup_table[['response_var', 'model_assessment']].drop_duplicates(), on='response_var', how='left')\
                .assign(Risk_Score = lambda x: x['Risk_Raw']*np.where(x['model_assessment']=='Extra Strong', 1,
                                                            np.where(x['model_assessment'] == 'Strong', 1.5,
                                                            np.where(x['model_assessment'] == 'Medium', 2, 3))))\
                                                                .drop(columns = 'model_assessment')
        else:
            df = df.assign(Risk_Score = lambda x: x['Risk_Raw'])
        # if penalize_by_bin:
        #     df = calculate_probability_ranges(df).merge(model_lookup_table[['response_var','ProbabilityFloor','ProbabilityCeiling', 'bin_assessment']].drop_duplicates(), on = ['response_var','ProbabilityFloor','ProbabilityCeiling'], how='left')\
        #         .assign(Risk_Score = lambda x: x['Risk_Score']*np.where(x['bin_assessment']=='Inside Target Range', 1,
        #                                                     np.where(x['bin_assessment'] == 'Near Target Range', 1.5,
        #                                                     np.where(x['bin_assessment'] == 'Bad', 3, 2))))\
        #                                                         .drop(columns =['bin_assessment', 'ProbabilityFloor','ProbabilityCeiling'])
        return(df)

    def get_optimal_portfolio(self, df_this_week, max_bets = None, verbose = False):
        required_cols = ['response_var', 'Model_Probability', 'Odds', 'Position', 'Week', 'label', 'team', 'opponent_team', 'EVProfitPer100', 'Risk_Score', 'Type']
        if not set(required_cols).issubset(df_this_week.columns):
            raise Exception (f"The parameter df must contain the following columns: {', '.join(required_cols)}")
        if df_this_week['Week'].nunique() != 1:
            raise ValueError(
                'get_optimal_portfolio must receive data for exactly one week.'
            )
        df_this_week.index = df_this_week['label'].astype(str) + ' ' + df_this_week['response_var'].astype(str)
        if verbose:
            print('Estimating Sigma...')
        start_sigma = datetime.now()
        mu, Sigma = get_mu_sigma(df_this_week, self.correlations, verbose)
        if verbose:
            print(f"Sigma, correlations, and risk scores took: {(datetime.now() - start_sigma).seconds/60} minutes")
        if len(mu) == 1:
            selected_rows = df_this_week.copy()
            selected_rows['Portfolio_Weight'] = 1.0
            selected_rows['Portfolio_Mu'] = mu.iloc[0]
            selected_rows['Portfolio_Var'] = Sigma.iloc[0, 0]
            selected_rows['Portfolio_SD'] = np.sqrt(Sigma.iloc[0, 0])
            selected_rows['Portfolio_Sharpe'] = (
                selected_rows['Portfolio_Mu'].iloc[0] / selected_rows['Portfolio_SD'].iloc[0]
                if selected_rows['Portfolio_SD'].iloc[0] > 0
                else np.nan
            )
            selected_rows['portfolio_id'] = 0
            return(selected_rows)
        start_weights = datetime.now()
        weights = calculate_weights(mu, Sigma, max_bets, verbose)
        if verbose:
            print(f"Weights took: {(datetime.now() - start_weights).seconds/60} minutes")
        full_weights_list = []
        for wgt in range(len(weights)):
            these_weights = weights[wgt]
            portfolio_metrics = measure_portfolio_metrics(these_weights, mu, Sigma, df_this_week)
            if portfolio_metrics is not None:
                full_weights_list.append(portfolio_metrics)
        if len(full_weights_list) == 0:
            return None
        all_portfolios = get_portfolios_from_weights(full_weights_list, False)
        if len(all_portfolios) > 0:
            return all_portfolios

    def get_best_portfolio(self, portfolios, total_bet, sd_cap, min_stake = 0.5):
        best_port = find_best_portfolio_by_mu_with_sd_cap(portfolios, sd_cap)
        best_port_with_normalized_amounts = get_amounts_from_portfolio(best_port, total_bet, min_stake)
        return best_port_with_normalized_amounts

class BetsRequest(BaseModel):
    season: int
    data: dict[str, list]
    penalize_by_model: bool = False
    positive_only: bool = True
    apply_ev_caps: bool = True

class PortfolioRequest(BaseModel):
    season: int
    data: dict[str, list]
    max_bets: int | None = None
    total_amount: float
    sd_cap: float
    min_stake: float =  0.5

@app.post('/get_bets_with_ev')
def get_bets_with_ev(request: BetsRequest):
    bets = Portfolio(request.season)
    df = pd.DataFrame(request.data)
    screened = remove_large_monotonicity_violations(df).drop(columns=['number', 'root', 'previous_probability'])
    res = bets.calculate_ev_and_risk(screened, request.penalize_by_model, request.positive_only, request.apply_ev_caps)
    return res.to_dict(orient='records')

@app.post('/get_portfolio')
def get_portfolio(request: PortfolioRequest):
    bets = Portfolio(request.season)
    df = pd.DataFrame(request.data)
    all_portfolios = bets.get_optimal_portfolio(df, request.max_bets)
    res = bets.get_best_portfolio(all_portfolios, request.total_amount, request.sd_cap, request.min_stake)
    return res.to_dict(orient='records')

#%%

