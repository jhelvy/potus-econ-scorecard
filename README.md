# POTUS Economic Scorecard

[![Shinylive](https://img.shields.io/badge/Shinylive-2.0.0-blue)](https://posit.co/blog/shinylive-for-r/)
[![GitHub stars](https://img.shields.io/github/stars/jhelvy/potus-econ-scorecard?style=social)](https://github.com/jhelvy/potus-econ-scorecard)

> Interactive visualization of economic performance under different U.S. presidents

## Overview

POTUS Economic Scorecard is an interactive [Shiny](https://shiny.posit.co/) application that allows users to compare economic performance metrics across different presidential administrations. The app provides visual representations of various economic indicators, including:

- S&P 500
- Dow Jones Industrial Average
- NASDAQ
- Unemployment Rate
- Inflation Rate

## Features

- **Multiple Economic Indicators**: Compare performance using S&P 500, Dow Jones, NASDAQ, Unemployment Rate, and Inflation Rate.
- **Flexible Reference Points**: Choose between "Inauguration Day" or "Day Before Election" as your reference point.
- **Party Filtering**: Filter presidents by political party.
- **Customizable Time Period**: Adjust the number of days to display for comparison.
- **Data Export**: Download the plot or raw data for your own analysis

## Usage

Access the app directly at: [https://jhelvy.github.io/potus-econ-scorecard/](https://jhelvy.github.io/potus-econ-scorecard/)

It should look like this:

![POTUS Economic Scorecard Interface](https://raw.githubusercontent.com/jhelvy/potus-econ-scorecard/main/screenshot.png)

## How It Works

Historical economic data is downloaded daily and stored in the app [GitHub repository](https://github.com/jhelvy/potus-econ-scorecard). The app loads this data and calculates performance metrics relative to your chosen reference date. For market indices (S&P 500, Dow Jones, NASDAQ), performance is shown as percent change from the reference date. For economic indicators (Unemployment Rate, Inflation Rate), absolute values are displayed.

## Data Sources

- Market data (S&P 500, Dow Jones, NASDAQ) is sourced from Yahoo Finance
- Economic indicators are sourced from FRED (Federal Reserve Economic Data)

## Technology

This application is built using:

- [R Shiny](https://shiny.posit.co/) for the interactive web application.
- [shinylive](https://posit-dev.github.io/r-shinylive/) for browser-based execution without a server.
- [Quarto](https://quarto.org/) for website publishing.
- [plotly](https://plotly.com/r/) for interactive visualizations.

## Local Deployment

To run the app locally:

1. Clone the repository:

   ```bash
   git clone https://github.com/jhelvy/potus-econ-scorecard.git
   cd potus-econ-scorecard
   ```

2. Open the project in RStudio or run from the R console:
   ```r
   library(shiny)
   runApp()
   ```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
