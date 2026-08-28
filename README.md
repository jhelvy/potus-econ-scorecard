
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Quarto](https://img.shields.io/badge/Quarto-website-blue)](https://quarto.org/)
[![GitHub
stars](https://img.shields.io/github/stars/jhelvy/potus-econ-scorecard?style=social)](https://github.com/jhelvy/potus-econ-scorecard)

## Source code for the [POTUS Economic Scorecard](https://jhelvy.github.io/potus-econ-scorecard/) app

<figure>
<img
src="https://github.com/jhelvy/potus-econ-scorecard/blob/main/screenshot.png?raw=true"
alt="POTUS Economic Scorecard Interface" />
<figcaption aria-hidden="true">POTUS Economic Scorecard
Interface</figcaption>
</figure>

## Overview

POTUS Economic Scorecard is an interactive web app that compares
economic performance across presidential administrations. Pick an
indicator, a reference point, and a set of presidents, and the app
charts each presidency on a common day-zero axis so the terms line up
side by side.

Eleven indicators are available:

**Markets** (charted as percent change from the reference date)

- S&P 500
- Dow Jones Industrial Average
- NASDAQ Composite
- US Dollar Index

**Economic indicators** (charted as absolute values)

- Unemployment Rate
- Inflation Rate (year-over-year CPI)
- 10-Year Treasury Yield
- Case-Shiller Home Price Index
- Real GDP
- Federal Debt to GDP Ratio
- Labor Force Participation Rate

Indicators start at different points in history, so not every one covers
all 19 presidencies. Presidents the selected indicator cannot cover are
dimmed in the sidebar, and selecting only uncovered presidents shows a
message explaining when the series begins.

## Features

- **Eleven Economic Indicators**: Four market indices and seven economic
  series.
- **Flexible Reference Points**: Choose between “Inauguration Day” and
  “Day Before Election” as day zero.
- **Party Filtering**: Filter presidents by political party.
- **Customizable Time Period**: Adjust the number of days shown, from 10
  to 1460.
- **Data Export**: Download the chart as a PNG or the underlying series
  as a CSV.

## How It Works

Historical data is downloaded daily by a GitHub Actions workflow and
committed to the
[repository](https://github.com/jhelvy/potus-econ-scorecard) as JSON,
one file per indicator. The page loads only the indicator you select and
does all of the indexing and charting in the browser, so it renders in
about a second.

For market indices, each presidency is indexed to its first observation
on or after the reference date and plotted as percent change from that
value. For the economic indicators, absolute values are plotted
directly.

## Data Sources

- Market data (S&P 500, Dow Jones, NASDAQ, US Dollar Index) comes from
  Yahoo Finance.
- Economic indicators come from FRED (Federal Reserve Economic Data).

## Technology

This application is built using:

- [Observable JS](https://quarto.org/docs/interactive/ojs/) and
  [Observable Plot](https://observablehq.com/plot/) for the interactive
  chart, which runs entirely in the browser with no server and no R.
- [Quarto](https://quarto.org/) for website publishing.
- [R](https://www.r-project.org/), with
  [quantmod](https://www.quantmod.com/) and
  [jsonlite](https://jeroen.r-universe.dev/jsonlite), for the daily data
  pipeline.

## Local Deployment

To run the site locally:

1.  Clone the repository:

    ``` bash
    git clone https://github.com/jhelvy/potus-econ-scorecard.git
    cd potus-econ-scorecard
    ```

2.  Refresh the data (optional – the repository already carries a recent
    copy):

    ``` bash
    Rscript get_data.R
    ```

3.  Preview the site:

    ``` bash
    quarto preview
    ```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1.  Fork the repository
2.  Create your feature branch
    (`git checkout -b feature/amazing-feature`)
3.  Commit your changes (`git commit -m 'Add some amazing feature'`)
4.  Push to the branch (`git push origin feature/amazing-feature`)
5.  Open a Pull Request

## License

This project is licensed under a CC-BY-SA-4.0 license - see the
[LICENSE](https://github.com/jhelvy/potus-econ-scorecard/blob/main/LICENSE.md)
file for details.
