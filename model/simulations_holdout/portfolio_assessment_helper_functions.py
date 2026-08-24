
import quadprog
import pandas as pd
import numpy as np
from model_functions import  _execute_with_retry

def aggregate_portfolios(portfolios, field, bet_amount = 100):
  return(portfolios.assign(amount = lambda x: x['Portfolio_Weight']*bet_amount, result_payout = lambda x: (x['BetHit']*(x['amount'] + ((x['ProfitPer100']/100)*x['amount']))))\
  .groupby(field,as_index = False).agg(payout = ('result_payout','sum'), spent = ('amount','sum'), num_bets = ('amount','size'), num_won_bets = ('BetHit','sum'), med_odds = ('Odds','median'))\
    .assign(total_return = lambda x: (x['payout']-x['spent'])/x['spent']))

def calculate_adjusted_return(portfolio):
  adjusted_bet_returns = []
  for b in np.where(portfolio["BetHit"].to_numpy())[0]:
    adjusted_bets = portfolio.copy()
    adjusted_bets.loc[b,'BetHit'] = False
    new_weekly_returns = aggregate_portfolios(adjusted_bets, 'Week')
    adjusted_bet_returns.append((sum(new_weekly_returns['payout'])-sum(new_weekly_returns['spent']))/sum(new_weekly_returns['spent']))
  return min(adjusted_bet_returns)

def calculate_top_k_win_stress(portfolio, k_values=[1, 2, 3, 5, 10]):
    portfolio = portfolio.copy().reset_index(drop=True)
    baseline_weekly_returns = aggregate_portfolios(portfolio, 'Week')
    baseline_return = (sum(baseline_weekly_returns['payout'])-sum(baseline_weekly_returns['spent']))/sum(baseline_weekly_returns['spent'])
    winning_bets = (portfolio.loc[portfolio["BetHit"].fillna(False).astype(bool)].copy()\
      .sort_values("ProfitPer100", ascending=False)
    )
    results = []
    for k in k_values:
        adjusted_bets = portfolio.copy()
        wins_to_remove = winning_bets.head(k)
        adjusted_bets.loc[wins_to_remove.index, "BetHit"] = False
        new_weekly_returns = aggregate_portfolios(adjusted_bets, 'Week')
        adjusted_return = (sum(new_weekly_returns['payout'])-sum(new_weekly_returns['spent']))/sum(new_weekly_returns['spent'])
        results.append({
            "k_wins_removed": k,
            "baseline_return": baseline_return,
            "adjusted_return": adjusted_return,
            "return_drop": baseline_return - adjusted_return
        })

    return pd.DataFrame(results)

def calculate_top_pct_win_stress(portfolio, pct_values=[0.05, 0.10, 0.20, 0.30]):
    portfolio = portfolio.copy().reset_index(drop=True)
    baseline_weekly_returns = aggregate_portfolios(portfolio, 'Week')
    baseline_return = (sum(baseline_weekly_returns['payout'])-sum(baseline_weekly_returns['spent']))/sum(baseline_weekly_returns['spent'])
    winning_bets = (portfolio.loc[portfolio["BetHit"].fillna(False).astype(bool)].copy().sort_values("ProfitPer100", ascending=False))
    results = []
    for pct in pct_values:
        k = int(np.ceil(len(winning_bets) * pct))
        adjusted_bets = portfolio.copy()
        wins_to_remove = winning_bets.head(k)
        adjusted_bets.loc[wins_to_remove.index, "BetHit"] = False
        new_weekly_returns = aggregate_portfolios(adjusted_bets, 'Week')
        adjusted_return = (sum(new_weekly_returns['payout'])-sum(new_weekly_returns['spent']))/sum(new_weekly_returns['spent'])
        results.append({
            "pct_wins_removed": pct,
            "k_wins_removed": k,
            "num_wins": len(winning_bets),
            "baseline_return": baseline_return,
            "adjusted_return": adjusted_return,
            "return_drop": baseline_return - adjusted_return,
            "pct_return_retained": adjusted_return / baseline_return if baseline_return != 0 else np.nan
        })

    return pd.DataFrame(results)

def wins_to_break_even(portfolio):
    portfolio = portfolio.copy().reset_index(drop=True)
    baseline_weekly_returns = aggregate_portfolios(portfolio, 'Week')
    baseline_return = (sum(baseline_weekly_returns['payout'])-sum(baseline_weekly_returns['spent']))/sum(baseline_weekly_returns['spent'])
    winning_bets = (portfolio.loc[portfolio["BetHit"].fillna(False).astype(bool)].copy().sort_values("ProfitPer100", ascending=False))

    for k in range(1, len(winning_bets) + 1):
        adjusted = portfolio.copy()
        adjusted.loc[winning_bets.head(k).index, "BetHit"] = False
        new_weekly_returns = aggregate_portfolios(adjusted, 'Week')
        adjusted_return = (sum(new_weekly_returns['payout'])-sum(new_weekly_returns['spent']))/sum(new_weekly_returns['spent'])
        if adjusted_return <= 0:
            return pd.DataFrame([{
                "baseline_return": baseline_return,
                "num_wins": len(winning_bets),
                "wins_to_break_even": k,
                "pct_wins_to_break_even": k / len(winning_bets),
                "return_after_removal": adjusted_return
            }])

    return pd.DataFrame([{
        "baseline_return": baseline_return,
        "num_wins": len(winning_bets),
        "wins_to_break_even": np.nan,
        "pct_wins_to_break_even": np.nan,
        "return_after_removal": np.nan
    }])

def weekly_reliability_summary(portfolio, strategy_name):
    weekly = aggregate_portfolios(portfolio, "Week").copy()

    return pd.DataFrame([{
        "strategy": strategy_name,
        "season_return": (
            weekly["payout"].sum() - weekly["spent"].sum()
        ) / weekly["spent"].sum(),
        "avg_weekly_return": weekly["total_return"].mean(),
        "median_weekly_return": weekly["total_return"].median(),
        "weekly_sd": weekly["total_return"].std(),
        "min_week": weekly["total_return"].min(),
        "pct_profitable_weeks": (weekly["total_return"] > 0).mean(),
        "pct_weeks_down_100": (weekly["total_return"] <= -1).mean(),
        "num_weeks": weekly["Week"].nunique()
    }])

def print_return_summary(df, bet_amount):
  weekly_returns = aggregate_portfolios(df, 'Week', bet_amount)
  print(f"Total return: {(sum(weekly_returns['payout'])-sum(weekly_returns['spent']))/sum(weekly_returns['spent']):.3f}")
  print(f"# of wins: {sum(df['BetHit'])}")
  print(calculate_top_k_win_stress(df))
  print(calculate_top_pct_win_stress(df))
  print(wins_to_break_even(df))