import numpy as np
import quadprog
import pandas as pd
import random
from datetime import datetime
from statsmodels.stats.correlation_tools import cov_nearest
from model_functions import calculate_probability_ranges

#make sure the df is already filtered on Odds range, risk range, manual player removal, etc
def get_optimized_by_gamma(mu, Sigma, max_bets, gamma = 1):
    if not isinstance(mu, pd.Series):
        mu = pd.Series(mu)
    Sigma = pd.DataFrame(Sigma, index=mu.index, columns=mu.index)
    valid = mu.notna() & np.isfinite(mu)
    mu = mu.loc[valid]
    Sigma = Sigma.loc[valid, valid]
    n = len(mu)
    if n == 0:
        return None
    Dmat = 2 * gamma * Sigma.to_numpy(copy=True) + 1e-8 * np.eye(n)
    dvec = mu.to_numpy(copy=True)
    C = np.column_stack([np.ones(n), np.eye(n)])
    b = np.concatenate([[1.0], np.zeros(n)])
    try:
        sol = quadprog.solve_qp(Dmat, dvec, C, b, meq=1)
    except Exception as e:
        print(e)
        return None
    w = pd.Series(sol[0], index=mu.index)
    if max_bets is None:
        num_bets = len(w)
    else:
        num_bets = min(max_bets, len(w))
    new_w = w.sort_values(ascending=False).head(num_bets)
    if new_w.sum() <= 0:
        return None
    new_w = new_w / new_w.sum()
    return new_w


EV_CAPS = {
    'team_differential': 20,
    'team_win': 30,
    'passing_yards': 150,
    'receiving_yards': 250,
    'receptions': 500,
    'rushing_yards': 250,
    'rushing_receiving_yards': 250,
    'anytime_td_scorer': 250,
}

# def calculate_ev_and_risk(df, model_lookup_table, penalize_by_model = True, penalize_by_bin = True, positive_only = True, apply_ev_caps = False):
#     ev_caps = pd.DataFrame({'Type': list(EV_CAPS.keys()), 'cap': list(EV_CAPS.values())})
#     df = df.assign(Odds = lambda x: pd.to_numeric(x['Odds'], errors='coerce'),
#                    ProfitPer100 = lambda x: np.where(x['Odds'] > 0, x['Odds'], 100**2/(-1*x['Odds'])),
#                    EVProfitPer100 = lambda x: x['Model_Probability']*x['ProfitPer100'] - 100*(1-x['Model_Probability']),
#                    Type = lambda x: np.where(
#                     x['response_var'].isin(['anytime_td_scorer', 'team_win']),
#                     x['response_var'],
#                     x['response_var'].str.rsplit('_',n=1).str[0]
#                    ),
#                    Risk_Raw = lambda x: x['Model_Probability']*(1 - x['Model_Probability'])*(x['ProfitPer100']/100 + 1)**2)
#     if apply_ev_caps:
#         df = df.merge(ev_caps, on = 'Type', how = 'left').assign(cap=lambda x: x["cap"].fillna(np.inf)).query('EVProfitPer100 <= cap').drop(columns = 'cap')
#     if positive_only:
#         df = df.query('EVProfitPer100 > 0')
#     if penalize_by_model:
#         df = df.merge(model_lookup_table[['response_var', 'model_assessment']].drop_duplicates(), on='response_var', how='left')\
#             .assign(Risk_Score = lambda x: x['Risk_Raw']*np.where(x['model_assessment']=='Extra Strong', 1,
#                                                         np.where(x['model_assessment'] == 'Strong', 1.5,
#                                                         np.where(x['model_assessment'] == 'Medium', 2, 3))))\
#                                                             .drop(columns = 'model_assessment')
#     else:
#         df = df.assign(Risk_Score = lambda x: x['Risk_Raw'])
#     if penalize_by_bin:
#         df = calculate_probability_ranges(df).merge(model_lookup_table[['response_var','ProbabilityFloor','ProbabilityCeiling', 'bin_assessment']].drop_duplicates(), on = ['response_var','ProbabilityFloor','ProbabilityCeiling'], how='left')\
#             .assign(Risk_Score = lambda x: x['Risk_Score']*np.where(x['bin_assessment']=='Inside Target Range', 1,
#                                                         np.where(x['bin_assessment'] == 'Near Target Range', 1.5,
#                                                         np.where(x['bin_assessment'] == 'Bad', 3, 2))))\
#                                                             .drop(columns =['bin_assessment', 'ProbabilityFloor','ProbabilityCeiling'])



#     return(df)

def remove_large_monotonicity_violations(bets, tolerance=0.05):
    bets_with_ordering = bets[~bets['response_var'].isin(['anytime_td_scorer','team_win'])]
    bets_without_ordering = bets[bets['response_var'].isin(['anytime_td_scorer','team_win'])]
    bets_with_ordering['number'] = bets_with_ordering['response_var'].str.replace('minus','-').str.extract(r"(-?\d+(?:\.\d+)?)", expand=False).astype(float)
    bets_with_ordering['root'] = bets_with_ordering['response_var'].str.replace(r"_(?:minus)?\d+(?:\.\d+)?$", "", regex=True)
    bets_with_ordering['previous_probability'] = bets_with_ordering.sort_values(['label','Week','root','number']).groupby(['label','Week','root'])['Model_Probability'].shift(1)
    non_violations = bets_with_ordering[~((bets_with_ordering['previous_probability'].notna()) & (bets_with_ordering['Model_Probability'] > bets_with_ordering['previous_probability'] + tolerance))]
    return pd.concat([bets_without_ordering, non_violations],axis=0)

def get_mu_sigma(df_this_week, correlations, verbose = False):
    cov_matrix = pd.DataFrame(
                            np.zeros((len(df_this_week), len(df_this_week)), dtype=float),
                            index=df_this_week.index,
                            columns=df_this_week.index
                        )
    if verbose:
        print('Calculating correlations and risk scores...')
    for i in range(len(cov_matrix)):
        for j in range(i, len(cov_matrix)):
            if i == j:
                cov_matrix.iloc[i,j] = df_this_week['Risk_Score'].iloc[i]
            else:
            #if player is the same: correlation = 1
            #if player fits in one of the correlation categories, assign the correct correlation based on the correlations spreadsheet
            #otherwise, correlation = 0
                if (df_this_week['label'].iloc[i] == df_this_week['label'].iloc[j]) & (df_this_week['Type'].iloc[i] == df_this_week['Type'].iloc[j]):
                    cor = 1
                #same player, different bet type:
                elif (df_this_week['label'].iloc[i] == df_this_week['label'].iloc[j]) & (df_this_week['Type'].iloc[i] != df_this_week['Type'].iloc[j]):
                    type_i = df_this_week['Type'].iloc[i]
                    type_j = df_this_week['Type'].iloc[j]
                    sub = correlations[
                        (correlations['Correlation_Type'] == 'same_player') &
                        (
                            ((correlations['Var1'] == type_i) & (correlations['Var2'] == type_j)) |
                            ((correlations['Var1'] == type_j) & (correlations['Var2'] == type_i))
                        )
                    ]
                    cor = 0 if sub.empty else sub['Cor'].iloc[0]
                #same team:
                elif df_this_week['team'].iloc[i] == df_this_week['team'].iloc[j]:
                    pos_i = str(df_this_week['Position'].iloc[i])
                    pos_j = str(df_this_week['Position'].iloc[j])
                    sub = correlations[
                        (correlations['Correlation_Type'] == 'same_team') &
                        (correlations['Var1'] == df_this_week['Type'].iloc[i]) &
                        (correlations['Var2'] == df_this_week['Type'].iloc[j])
                    ]
                    if not sub.empty and {'Position1', 'Position2'}.issubset(sub.columns):
                        sub = sub[
                            sub.apply(
                                lambda row:
                                    (pd.isna(row['Position1']) or pos_i in str(row['Position1'])) and
                                    (pd.isna(row['Position2']) or pos_j in str(row['Position2'])),
                                axis=1
                            )
                        ]
                    cor = 0 if sub.empty else sub['Cor'].iloc[0]
                #opposing teams:
                elif df_this_week['team'].iloc[i] == df_this_week['opponent_team'].iloc[j]:
                    pos_i = str(df_this_week['Position'].iloc[i])
                    pos_j = str(df_this_week['Position'].iloc[j])
                    sub = correlations[
                        (correlations['Correlation_Type'] == 'opp_team') &
                        (correlations['Var1'] == df_this_week['Type'].iloc[i]) &
                        (correlations['Var2'] == df_this_week['Type'].iloc[j])
                    ]
                    if not sub.empty and {'Position1', 'Position2'}.issubset(sub.columns):
                        sub = sub[
                            sub.apply(
                                lambda row:
                                    (pd.isna(row['Position1']) or pos_i in str(row['Position1'])) and
                                    (pd.isna(row['Position2']) or pos_j in str(row['Position2'])),
                                axis=1
                            )
                        ]
                    cor = 0 if sub.empty else sub['Cor'].iloc[0]
                #unrelated games:
                else:
                    cor = 0
                cov_matrix.iloc[i, j] = (cor * np.sqrt(df_this_week['Risk_Score'].iloc[i])* np.sqrt(df_this_week['Risk_Score'].iloc[j]))
                cov_matrix.iloc[j, i] = cov_matrix.iloc[i, j]
    mu = df_this_week['EVProfitPer100']/100
    mu.index = cov_matrix.columns
    Sigma = (cov_matrix + cov_matrix.T) / 2
    Sigma = Sigma.astype(float)
    eigvals = np.linalg.eigvalsh(Sigma.to_numpy())
    min_eig = eigvals.min()
    if min_eig > -1e-8:
        # Matrix is already PSD up to floating-point noise.
        # Add tiny ridge to make it strictly positive definite.
        Sigma = Sigma + np.eye(len(Sigma)) * 1e-8
    else:
        game_keys = np.array(["||".join(sorted((str(team), str(opp))))
            for team, opp in zip(df_this_week["team"], df_this_week["opponent_team"])
        ])
        for game_key in np.unique(game_keys):
            idx = np.where(game_keys == game_key)[0]
            block = Sigma.iloc[idx, idx].to_numpy(copy=True)
            min_eig = np.linalg.eigvalsh(block).min()
            if min_eig > -1e-8:
                block = block + np.eye(len(block)) * 1e-8
            else:
                block = cov_nearest(
                    block,
                    method="nearest",
                    threshold=1e-10
                )
            Sigma.iloc[idx, idx] = block
    return [mu, Sigma]

def calculate_weights(mu, Sigma, max_bets, verbose = False):
    gammas = 10 ** np.linspace(-3, 3, 100)
    n = len(Sigma)
    max_bets_this_week = n if max_bets is None else min(max_bets, n)
    if verbose:
        print('Calculating weights...')
    weights = [get_optimized_by_gamma(mu, Sigma, max_bets_this_week, gamma = g) for g in gammas]
    return weights

def measure_portfolio_metrics(these_weights, mu, Sigma, df_this_week):
    if these_weights is not None and these_weights.notna().all():
        mu_portfolio = sum(these_weights * mu[these_weights.index])
        var_portfolio = these_weights.to_numpy() @ Sigma.loc[these_weights.index, these_weights.index].to_numpy() @ these_weights.to_numpy()
        sd_portfolio = np.sqrt(var_portfolio)
        sharpe_val = mu_portfolio / sd_portfolio if sd_portfolio > 0 else np.nan
        weights_df = these_weights.to_frame('Portfolio_Weight')
        bet_rows = df_this_week.loc[weights_df.index].copy()
        portfolio_id = random.randint(0, 10000000)
        return {
            'portfolio_id': portfolio_id,
            'weights': weights_df,
            'bet_rows': bet_rows,
            'mu': mu_portfolio,
            'var': var_portfolio,
            'sd': sd_portfolio,
            'sharpe': sharpe_val
        }

def get_portfolios_from_weights(full_weights_list, return_best_only = True):
    if len(full_weights_list) == 0:
        return None
    valid_portfolios = [
        x for x in full_weights_list
        if x is not None and np.isfinite(x["sharpe"])
    ]
    if len(valid_portfolios) == 0:
        return None
    
    best_indx = np.nanargmax([x["sharpe"] for x in valid_portfolios])
    best_portfolio = valid_portfolios[best_indx]
    if return_best_only:
        portfolios_to_loop = [best_portfolio]
    else:
        portfolios_to_loop = valid_portfolios
    selected_rows_df = pd.DataFrame()
    for p in portfolios_to_loop:
        selected_rows = p['bet_rows'].copy()
        selected_rows['portfolio_id'] = p['portfolio_id']
        selected_rows['Portfolio_Weight'] = p['weights']['Portfolio_Weight']
        selected_rows['Portfolio_Mu'] = p['mu']
        selected_rows['Portfolio_Var'] = p['var']
        selected_rows['Portfolio_SD'] = p['sd']
        selected_rows['Portfolio_Sharpe'] = p['sharpe']
        selected_rows_df = pd.concat([selected_rows_df, selected_rows])
    return selected_rows_df
         
#portfolio was run on m bets and we want the top n normalized
def getTopNFromPortfolio(portfolio, n):
    if n < len(portfolio):
        new_portfolio = portfolio.head(n)
        total_weights = sum(new_portfolio['Portfolio_Weight'])
        #normalize:
        new_portfolio['Portfolio_Weight'] = new_portfolio['Portfolio_Weight']/total_weights
        #calculate new metrics:
        portfolio_metrics = measure_portfolio_metrics(these_weights, mu, Sigma, df_this_week)


def find_best_portfolio_by_mu_with_sd_cap(df, sd_cap):
    eligible = df[df['Portfolio_SD'] < sd_cap].copy()
    if eligible.empty:
        return eligible
    chosen = eligible[['Week', 'portfolio_id', 'Portfolio_Mu']]\
        .drop_duplicates()\
        .sort_values(['Week', 'Portfolio_Mu'], ascending=[True, False])\
        .groupby('Week', as_index=False).head(1)
    return eligible.merge(chosen[['Week', 'portfolio_id']], on=['Week', 'portfolio_id'], how='inner')


def get_amounts_from_portfolio(portfolios, total_bet, min_stake):
  portfolios['bet_amount'] = total_bet*portfolios['Portfolio_Weight']
  portfolios = portfolios[portfolios['bet_amount'] > min_stake].copy()
  new_total_bet_amounts = portfolios.groupby(['Week','portfolio_id'],as_index=False).agg(new_total_bet_amount = ('bet_amount','sum'))
  portfolios = portfolios.merge(new_total_bet_amounts, on = ['Week','portfolio_id'], how='left')
  portfolios['new_weight'] = portfolios['bet_amount']/portfolios['new_total_bet_amount']
  portfolios['Portfolio_Weight'] = portfolios['new_weight']
  portfolios['bet_amount'] = portfolios['new_weight']*total_bet
  return portfolios.drop(columns = ['new_total_bet_amount','new_weight'])