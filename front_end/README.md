# NFL Bet Recommender — Front End

The NFL Bet Recommender is an interactive **R Shiny application** that turns the outputs of the larger NFL modeling pipeline into actionable betting recommendations.

The application combines:

- machine-learning predictions for NFL player and team outcomes
- current sportsbook odds
- expected-value calculations
- portfolio optimization
- bet tracking and performance analysis

The front end is deployed on **Posit Connect Cloud** and communicates with Supabase, a GCP-hosted API, and a residential DraftKings line-retrieval service.

## What the App Does

The app is intended to provide a single interface for using the outputs of the NFL modeling system during the season.

### Bet Recommendations

The app combines model predictions with current sportsbook lines to identify betting opportunities.

For each available bet, it can display information such as:

- model probability
- sportsbook odds
- market-implied probability
- expected return
- player and team information
- matchup history
- recent and prior-season performance
- opponent information
- game time and location

Supported markets include both player and team bets, including:

- passing yards
- rushing yards
- receiving yards
- rushing + receiving yards
- receptions
- anytime touchdown scorer
- moneylines
- spreads

The application matches current sportsbook markets to the corresponding model response variables so that predictions and live betting lines can be evaluated together.

### Portfolio Optimization

Rather than evaluating bets only one at a time, the application can also construct optimized portfolios of positive-EV bets.

Users can control portfolio constraints including:

- total amount to wager
- maximum number of bets
- minimum individual stake
- maximum portfolio standard deviation

The available bets are sent to the project's GCP API, which performs the portfolio optimization and returns:

- selected bets
- recommended stake amounts per bet
- expected portfolio return
- portfolio variance
- portfolio standard deviation

This allows the application to account for the combined risk and correlations between bets, and return of a group of bets rather than simply ranking bets independently.

### Bet Tracking

Users must use the DraftKings app directly to place the bet, but for tracking purposes, can log bets directly through the Shiny application.

Stored information includes:

- bettor
- season and week
- player or team
- bet type
- sportsbook line
- odds
- amount wagered
- game
- game time
- submission time
- whether the bet came from an individual recommendation or through the portfolio optimizer

After games are completed, results can be recorded and used for historical performance analysis.

### Betting Performance

The application provides summaries of historical betting results, including breakdowns by:

- bettor
- week
- bet type
- individual bets vs. optimized portfolios
- odds range

These summaries are used to evaluate how the system and bettors have performed over time.

### Weekly Bet Cheat Sheet

The application can generate an HTML cheat sheet for the upcoming NFL slate using `BetWeeklySummary.Rmd`, to help bettors keep track of which players and teams to root for as they watch the games!

The report organizes bets by game window (Sunday early window, Sunday Late Window, Sunday Night, etc) and game, and allows the bettor to see which bets they placed for that game, along with any survivor pool picks, and the jersey number of the players they bet on to help them follow the player throughout the game.

It also supports survivor-pool selections, allowing bets and survivor picks to be viewed together in game order.

### Survivor Pool

The application includes a small dashboard to help players who are playing Survivor Pool (unrelated to the models). It shows the top teams ordered by how many DraftKings points they are favored by, and displays game and team information, along with a list of the team's upcoming opponents for the rest of the season.

## Front-End Architecture

The front end depends on several services from the larger NFL betting project.

```text
                         ┌───────────────────────┐
                         │       Supabase        │
                         │                       │
                         │ Model predictions     │
                         │ NFL/reference data    │
                         │ Bets and results      │
                         └───────────┬───────────┘
                                     │
                                     ▼
                         ┌───────────────────────┐
                         │     R Shiny App       │
                         │  Posit Connect Cloud  │
                         └───────┬────────┬──────┘
                                 │        │
              EV + Portfolio Optimization │   │ Current DK lines
                                 │        │
                                 ▼        ▼
                    ┌────────────────┐  ┌────────────────────┐
                    │    GCP API     │  │ Cloudflare Tunnel  │
                    │                │  │                    │
                    │ Expected value │  │ api.izzybeers.com  │
                    │ Portfolio      │  └─────────┬──────────┘
                    │ optimization   │            │
                    └────────────────┘            ▼
                                        ┌────────────────────┐
                                        │  Residential Mac   │
                                        │                    │
                                        │ Plumber API for    │
                                        │ DraftKings lines   │
                                        └─────────┬──────────┘
                                                  │
                                                  ▼
                                        ┌────────────────────┐
                                        │     DraftKings     │
                                        │   Sportsbook API   │
                                        └────────────────────┘
```

The major responsibilities are separated as follows:

- **Posit Connect Cloud** — hosts the R Shiny front end
- **Supabase** — stores predictions, NFL data, bets, results, and other application data
- **Google Cloud Platform** — hosts the expected-value and portfolio-optimization API
- **Residential Plumber API** — retrieves current DraftKings sportsbook data
- **Cloudflare Tunnel** — exposes the residential DraftKings service to the hosted application

## Files

### `app.R`

Main R Shiny application.

This file contains the UI and server logic responsible for:

- loading application data
- displaying bet recommendations
- filtering available bets
- refreshing sportsbook lines
- displaying detailed bet information
- requesting EV calculations
- requesting optimized portfolios
- logging bets
- updating bet results
- displaying historical betting summaries
- generating weekly cheat sheets
- survivor-pool functionality

### `app_helper_funs.R`

Contains shared functions used throughout the front end.

Major responsibilities include:

- Supabase reads and writes
- pulling current player and team predictions
- player and team name normalization
- retrieving sportsbook markets
- converting DraftKings markets into model response variables
- matching predictions to sportsbook lines
- calculating implied probabilities
- preparing player, team, opponent, and matchup information for display
- formatting detailed recommendation information

### `BetWeeklySummary.Rmd`

Parameterized R Markdown document used to create the weekly bet cheat sheet.

Inputs include:

- logged bets
- detailed bet information
- survivor selections
- team lookup information
- NFL season
- NFL week

The generated HTML report groups upcoming bets and survivor selections by game window and matchup.

### `dk_api.R`

Plumber API responsible specifically for retrieving **DraftKings sportsbook data**.

It runs on a residential Mac rather than in the same cloud environment as the Shiny application.

The API exposes endpoints used by the front end such as:

```text
/get_bet_ids_by_category
/lines_by_bet_id?bet_id=<id>
```

This service is separate from the GCP API used for expected-value calculations and portfolio optimization.

## GCP API

A separate API deployed on **Google Cloud Platform** provides computational services used by the front end.

It currently handles:

- expected-value calculations
- portfolio optimization

The Shiny application accesses this service through:

```text
API_BASE_URL
```

For portfolio optimization, the app sends the current set of eligible bets along with user-defined portfolio constraints and receives the optimized portfolio.

## Supabase

Supabase provides persistent storage used by the application and the larger modeling pipeline.

Front-end data includes:

- player predictions
- team predictions
- team and player lookup information
- NFL reference data
- generated recommendations
- logged bets
- bet results
- application error logs

The app retrieves the most recent prediction run for each player/team response variable before matching those predictions to available sportsbook markets.

## Environment Variables

The front end currently uses four environment variables:

```text
SUPABASE_URL
SUPABASE_KEY
API_BASE_URL
CLOUDFLARE_URL
```

Their roles are:

| Variable | Purpose |
| --- | --- |
| `SUPABASE_URL` | Supabase project URL |
| `SUPABASE_KEY` | Supabase authentication key |
| `API_BASE_URL` | GCP-hosted EV and portfolio API |
| `CLOUDFLARE_URL` | DraftKings line-retrieval API |

The current production DraftKings proxy is:

```text
CLOUDFLARE_URL=https://api.izzybeers.com
```

For local development, values can be stored in `.Renviron`:

```r
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

SUPABASE_URL = Sys.getenv("SUPABASE_URL")
SUPABASE_KEY = Sys.getenv("SUPABASE_KEY")
api_base_url = Sys.getenv("API_BASE_URL")
dk_proxy_url = Sys.getenv("CLOUDFLARE_URL")
```

`.Renviron` is excluded from both Git and the Posit Connect Cloud application bundle.

## Running the Shiny App Locally

From the front-end directory:

```r
shiny::runApp()
```

The required environment variables must already be available to the R session.

Most application functionality can communicate with Supabase and the GCP API directly.

Live DraftKings functionality additionally requires the residential DraftKings service to be available.

## Running the DraftKings Service

For manual development and testing, the Plumber API can be started with:

```bash
Rscript -e 'plumber::plumb("dk_api.R")$run(host="127.0.0.1", port=8000)'
```

The Cloudflare Tunnel is run separately:

```bash
cloudflared tunnel run --token <CLOUDFLARE_TUNNEL_TOKEN>
```

Cloudflare routes:

```text
https://api.izzybeers.com
```

to:

```text
http://127.0.0.1:8000
```

The long-term residential deployment is intended to run both `cloudflared` and the Plumber API as background services rather than requiring open Terminal sessions.

## Deployment

### Front End

The Shiny application is deployed to:

**Posit Connect Cloud**

The deployment synchronizes the following environment variables from the local R environment:

```text
SUPABASE_URL
SUPABASE_KEY
API_BASE_URL
CLOUDFLARE_URL
```

### EV and Portfolio API

Expected-value calculations and portfolio optimization are deployed separately on:

**Google Cloud Platform**

### DraftKings Line API

DraftKings sportsbook retrieval runs separately on:

```text
Residential Mac
      ↓
Cloudflare Tunnel
      ↓
api.izzybeers.com
```

`dk_api.R` is therefore excluded from the Posit Connect Cloud application bundle.

## Security

Secrets should never be committed to Git.

This includes:

- `.Renviron`
- `.env`
- Supabase credentials
- Cloudflare tunnel tokens
- API credentials
- other local secret files

The application uses environment variables to keep credentials separate from the source code.