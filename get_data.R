###############################################
# Data pipeline for the POTUS Economic Scorecard
#
# Fetches every indicator, validates each one, and writes the JSON the site
# loads in the browser.
#
# Output, all under data/:
#   <id>.json        one file per indicator: metadata plus parallel date/value arrays
#   indicators.json  metadata for every indicator, no series -- drives the selector
#   presidents.json  the presidents table
###############################################

library(dplyr)
library(lubridate)
library(quantmod)
library(jsonlite)

OUT_DIR <- "data"

#-------------------------------------------
# Indicator registry
#-------------------------------------------

# One row per indicator. `type` decides how the app plots it: percent_change
# re-indexes each presidency to 0 at its reference date, absolute plots the
# value as reported.
indicators <- tibble::tribble(
  ~id,                   ~name,                        ~source,  ~symbol,      ~type,             ~y_label,
  "sp500",               "S&P 500",                    "yahoo",  "^GSPC",      "percent_change",  "Percent Change (%)",
  "djia",                "Dow Jones",                  "yahoo",  "^DJI",       "percent_change",  "Percent Change (%)",
  "nasdaq",              "NASDAQ",                     "yahoo",  "^IXIC",      "percent_change",  "Percent Change (%)",
  "dxy",                 "US Dollar Index",            "yahoo",  "DX-Y.NYB",   "percent_change",  "Percent Change (%)",
  "unemployment",        "Unemployment Rate",          "fred",   "UNRATE",     "absolute",        "Unemployment Rate (%)",
  "inflation",           "Inflation Rate",             "fred",   "CPIAUCSL",   "absolute",        "Inflation Rate (YoY %)",
  "treasury10yr",        "10-Year Treasury Yield",     "fred",   "DGS10",      "absolute",        "10-Year Treasury Yield (%)",
  "housing",             "Home Price Index",           "fred",   "CSUSHPISA",  "absolute",        "Home Price Index",
  "gdp",                 "Real GDP",                   "fred",   "GDPC1",      "absolute",        "Real GDP (Billions of 2017 $)",
  "debt_gdp",            "Federal Debt to GDP Ratio",  "fred",   "GFDEGDQ188S","absolute",        "Federal Debt to GDP Ratio (%)",
  "labor_participation", "Labor Force Participation",  "fred",   "CIVPART",    "absolute",        "Labor Force Participation Rate (%)"
)

descriptions <- c(
  sp500               = "The S&P 500 is a stock market index tracking the performance of 500 large companies listed on U.S. exchanges. Source: Standard & Poor's.",
  djia                = "The Dow Jones Industrial Average is a price-weighted index of 30 significant stocks traded on the NYSE and NASDAQ. Source: S&P Dow Jones Indices.",
  nasdaq              = "The NASDAQ Composite Index includes all companies listed on the NASDAQ stock market, weighted by market capitalization. Source: NASDAQ, Inc.",
  dxy                 = "The US Dollar Index measures the dollar against a basket of foreign currencies. A rising value indicates a stronger dollar, which can be good for imports but challenging for exports. Source: ICE Futures U.S.",
  unemployment        = "The Unemployment Rate represents the percentage of the labor force that is unemployed but actively seeking employment. Source: U.S. Bureau of Labor Statistics.",
  inflation           = "The Inflation Rate measures the year-over-year percentage change in consumer prices as captured by the Consumer Price Index (CPI). Source: U.S. Bureau of Labor Statistics.",
  treasury10yr        = "The 10-Year Treasury Yield reflects market expectations about future growth and inflation. Lower yields generally indicate economic pessimism or lower inflation expectations. Source: U.S. Department of the Treasury.",
  housing             = "The Case-Shiller Home Price Index tracks changes in the value of residential real estate nationwide. Source: S&P CoreLogic.",
  gdp                 = "Real Gross Domestic Product is the inflation-adjusted value of all goods and services produced by an economy. Source: U.S. Bureau of Economic Analysis.",
  debt_gdp            = "The Federal Debt to GDP Ratio shows government debt as a percentage of annual economic output, a measure of fiscal sustainability. Source: U.S. Treasury and Bureau of Economic Analysis.",
  labor_participation = "The Labor Force Participation Rate shows the percentage of the population that is either employed or actively seeking employment. Source: U.S. Bureau of Labor Statistics."
)

# How stale a series is allowed to be before it counts as a fetch failure.
# Monthly and quarterly series are published well after the period they cover,
# so they need a much longer leash than the daily market series.
max_staleness_days <- c(
  sp500 = 7, djia = 7, nasdaq = 7, dxy = 7, treasury10yr = 10,
  unemployment = 70, inflation = 70, labor_participation = 70,
  housing = 130, gdp = 200,
  # Debt-to-GDP is quarterly and lands roughly two quarters after the period it
  # covers, so it is routinely ~8 months behind.
  debt_gdp = 300
)

#-------------------------------------------
# Presidents
#-------------------------------------------

presidents_data <- tryCatch({
  d <- read.csv("presidents_data.csv", stringsAsFactors = FALSE)
  d$inauguration_date <- as.Date(d$inauguration_date)
  d$election_date <- as.Date(d$election_date)
  d
}, error = function(e) {
  stop("Failed to read presidents_data.csv: ", conditionMessage(e))
})

earliest_date <- min(presidents_data$election_date) - days(30)

#-------------------------------------------
# Fetchers
#-------------------------------------------

# FRED's keyless CSV endpoint. quantmod::getSymbols.FRED still points at
# /series/<id>/downloaddata/<id>.csv, which now returns a 301 and fails
# intermittently over HTTP/2; this endpoint returns 200 for every series we use.
fetch_fred_series <- function(symbol) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", symbol)
  raw <- read.csv(url, stringsAsFactors = FALSE, na.strings = c(".", "", "NA"))

  # The date column has been renamed over the years; take whichever is present.
  date_col <- intersect(c("observation_date", "DATE"), names(raw))
  if (length(date_col) == 0) {
    stop("no recognisable date column in ", url, " (got: ", paste(names(raw), collapse = ", "), ")")
  }

  tibble::tibble(
    date = as.Date(raw[[date_col[1]]]),
    value = suppressWarnings(as.numeric(raw[[symbol]]))
  )
}

# quantmod directly rather than through tidyquant: tq_get(get = "stock.prices")
# is a wrapper over exactly this call, and dropping it removes a heavy
# dependency from the CI build.
fetch_yahoo_series <- function(symbol) {
  raw <- getSymbols(symbol, src = "yahoo", from = earliest_date, to = Sys.Date(),
                    auto.assign = FALSE, warnings = FALSE)
  adjusted <- raw[, grep("\\.Adjusted$", colnames(raw))[1]]
  tibble::tibble(
    date = as.Date(zoo::index(raw)),
    value = as.numeric(adjusted)
  )
}

# CPI is published as an index level; the app shows the year-over-year rate.
# The lag is by position, so it is only correct on a complete monthly series --
# hence the gap check before it runs.
to_yoy <- function(series) {
  series <- arrange(series, date)

  gaps <- as.numeric(diff(series$date))
  if (length(gaps) > 0 && max(gaps) > 45) {
    stop("CPI series has a gap of ", max(gaps), " days; a positional 12-month lag would be wrong")
  }

  series %>%
    mutate(value = (value / lag(value, 12) - 1) * 100) %>%
    filter(!is.na(value))
}

#-------------------------------------------
# Fetch every indicator, collecting failures rather than stopping at the first
#-------------------------------------------

series_list <- list()
failures <- character()

for (i in seq_len(nrow(indicators))) {
  ind <- indicators[i, ]
  message("Fetching ", ind$name, " (", ind$symbol, ") from ", ind$source, " ...")

  series <- tryCatch({
    s <- if (ind$source == "fred") fetch_fred_series(ind$symbol) else fetch_yahoo_series(ind$symbol)
    if (ind$id == "inflation") s <- to_yoy(s)
    s
  }, error = function(e) {
    failures <<- c(failures, paste0(ind$id, ": ", conditionMessage(e)))
    NULL
  })

  if (is.null(series)) next

  # Drop missing observations at the source rather than leaving them for the
  # app to trip over. This is what blanked out the dollar index: its reference
  # value fell on a federal holiday, came back NA, and nulled the whole series.
  series <- series %>%
    filter(!is.na(date), !is.na(value), is.finite(value)) %>%
    distinct(date, .keep_all = TRUE) %>%
    arrange(date) %>%
    filter(date >= earliest_date)

  if (nrow(series) == 0) {
    failures <- c(failures, paste0(ind$id, ": no usable observations after filtering"))
    next
  }

  stale_days <- as.numeric(Sys.Date() - max(series$date))
  allowed <- max_staleness_days[[ind$id]]
  if (stale_days > allowed) {
    failures <- c(failures, sprintf(
      "%s: most recent observation is %s, %d days old (allowed: %d)",
      ind$id, max(series$date), round(stale_days), allowed
    ))
    next
  }

  series_list[[ind$id]] <- series
  message("  ", nrow(series), " observations, ", min(series$date), " to ", max(series$date))
}

# Fail loudly and completely. The old script only aborted when *both* sources
# came back empty, so a single-source outage silently published a partial
# dataset that looked fine.
if (length(failures) > 0) {
  stop(
    "Data fetch failed for ", length(failures), " of ", nrow(indicators), " indicators:\n  ",
    paste(failures, collapse = "\n  ")
  )
}

#-------------------------------------------
# Write the JSON the site loads
#-------------------------------------------

dir.create(OUT_DIR, showWarnings = FALSE)

# Parallel arrays rather than an array of objects: repeating the key names on
# every observation roughly triples the file size for no gain.
write_series_json <- function(ind, series, meta) {
  payload <- c(meta, list(
    dates = format(series$date, "%Y-%m-%d"),
    values = round(series$value, 4)
  ))
  write_json(payload, file.path(OUT_DIR, paste0(ind$id, ".json")), auto_unbox = TRUE, digits = NA)
}

meta_list <- list()

for (i in seq_len(nrow(indicators))) {
  ind <- indicators[i, ]
  series <- series_list[[ind$id]]

  # Which presidencies this series can actually cover. Several start well after
  # 1957 -- the dollar index and the Dow especially -- and the app needs to be
  # able to say so instead of drawing a blank chart.
  covered <- presidents_data$president[presidents_data$inauguration_date >= min(series$date)]

  meta <- list(
    id = ind$id,
    name = ind$name,
    type = ind$type,
    y_label = ind$y_label,
    description = unname(descriptions[[ind$id]]),
    symbol = ind$symbol,
    source = ind$source,
    first_date = format(min(series$date), "%Y-%m-%d"),
    last_date = format(max(series$date), "%Y-%m-%d"),
    n = nrow(series),
    covers = covered
  )

  write_series_json(ind, series, meta)
  meta_list[[length(meta_list) + 1]] <- meta

  message(sprintf("  wrote %s.json (%d obs, covers %d of %d presidencies)",
                  ind$id, nrow(series), length(covered), nrow(presidents_data)))
}

write_json(meta_list, file.path(OUT_DIR, "indicators.json"), auto_unbox = TRUE, digits = NA)

write_json(
  presidents_data %>%
    mutate(
      inauguration_date = format(inauguration_date, "%Y-%m-%d"),
      election_date = format(election_date, "%Y-%m-%d")
    ),
  file.path(OUT_DIR, "presidents.json"),
  auto_unbox = TRUE
)

message("\nDone. ", nrow(indicators), " indicators written to ", OUT_DIR, "/")
