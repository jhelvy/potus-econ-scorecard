
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Shinylive](https://img.shields.io/badge/Shinylive-2.0.0-blue)](https://posit.co/blog/shinylive-for-r/)
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

POTUS Economic Scorecard is an interactive
[Shiny](https://shiny.posit.co/) application that allows users to
compare economic performance metrics across different presidential
administrations. The app provides visual representations of various
economic indicators, including:

- S&P 500
- Dow Jones Industrial Average
- NASDAQ
- Unemployment Rate
- Inflation Rate

## Features

- **Multiple Economic Indicators**: Compare performance using S&P 500,
  Dow Jones, NASDAQ, Unemployment Rate, and Inflation Rate.
- **Flexible Reference Points**: Choose between “Inauguration Day” or
  “Day Before Election” as your reference point.
- **Party Filtering**: Filter presidents by political party.
- **Customizable Time Period**: Adjust the number of days to display for
  comparison.
- **Data Export**: Download the plot or raw data for your own analysis

## How It Works

Historical economic data is downloaded daily and stored in the app
[GitHub repository](https://github.com/jhelvy/potus-econ-scorecard). The
app loads this data and calculates performance metrics relative to your
chosen reference date. For market indices (S&P 500, Dow Jones, NASDAQ),
performance is shown as percent change from the reference date. For
economic indicators (Unemployment Rate, Inflation Rate), absolute values
are displayed.

## Data Sources

- Market data (S&P 500, Dow Jones, NASDAQ) is sourced from Yahoo Finance
- Economic indicators are sourced from FRED (Federal Reserve Economic
  Data)

## Technology

This application is built using:

- [R Shiny](https://shiny.posit.co/) for the interactive web
  application.
- [shinylive](https://posit-dev.github.io/r-shinylive/) for
  browser-based execution without a server.
- [Quarto](https://quarto.org/) for website publishing.
- [plotly](https://plotly.com/r/) for interactive visualizations.

## Local Deployment

To run the app locally:

1.  Clone the repository:

    ``` bash
    git clone https://github.com/jhelvy/potus-econ-scorecard.git
    cd potus-econ-scorecard
    ```

2.  Open the project in RStudio or run from the R console:

    ``` r
    library(shiny)
    runApp()
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
