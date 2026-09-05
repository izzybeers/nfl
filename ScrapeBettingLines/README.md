# Betting Lines Scraper


This directory contains the DraftKings betting-line scraper, to store betting lines for different bets at different points in time. 
The scraper runs multiple times per day, with increased frequency closer to game times.

## Betting Markets Scraped

The scraper currently collects the following NFL betting markets from DraftKings:

* **Passing Yards** — player passing-yard props
* **Rushing Yards** — player rushing-yard props
* **Receiving Yards** — player receiving-yard props
* **Rushing + Receiving Yards** — combined player rushing and receiving-yard props
* **Receptions** — player reception-total props
* **Anytime Touchdown Scorer** — player anytime touchdown props
* **Moneyline** — game moneyline markets
* **Spread and Alternate Spread** — alternate point-spread markets

Where available, the scraper collects the associated event, market, selection, line/point value, American odds, decimal odds, and true odds, along with the UTC event and scrape timestamps.


## Files

- `ScrapeBettingLines.R` — entry point for running the scraper.
- `ScrapeBettingLinesHelperFunctions.R` — helper functions for retrieving DraftKings market data, transforming the results, and writing them to Supabase.

## Requirements

The scraper uses the following R packages:

- `httr`
- `jsonlite`
- `dplyr`
- `stringr`
- `lubridate`

The following environment variables must be available in `.Renviron`:

- `SUPABASE_URL`
- `SUPABASE_KEY`
- `DRAFTKINGS_BASE_URL_ROOT`

## Running Manually

From this directory:

```bash
Rscript ScrapeBettingLines.R
```

