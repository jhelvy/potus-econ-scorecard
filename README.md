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

- **Multiple Economic Indicators**: Choose from several key economic metrics
- **Flexible Reference Points**: Compare data from either inauguration day or the day before election
- **Customizable Time Frame**: Adjust the display period from 10 days to 4 years
- **Party and President Filtering**: Filter by political party or select specific presidents
- **Interactive Visualizations**: Powered by Plotly for detailed hover information
- **Data Export**: Download the plot or raw data for your own analysis

## Usage

Access the app directly at: [https://jhelvy.github.io/potus-econ-scorecard/](https://jhelvy.github.io/potus-econ-scorecard/)

### Interface Overview

![POTUS Economic Scorecard Interface](https://raw.githubusercontent.com/jhelvy/potus-econ-scorecard/main/screenshot.png)

1. **Sidebar Controls**:
   - Select an economic indicator
   - Choose a reference date (inauguration or election)
   - Set the number of days to display
   - Filter by political party
   - Select/deselect specific presidents

2. **Main Panel**:
   - Interactive plot showing the selected economic indicator over time
   - Download buttons for both the visualization and underlying data

### Interpreting the Results

- For market indices (S&P 500, Dow Jones, NASDAQ), the y-axis shows percent change from the reference date
- For economic indicators (Unemployment, Inflation), the y-axis shows absolute values
- Lines are color-coded by party (blue for Democratic, red for Republican)
- Each line ends with a label showing the president's name and final value

## Data Sources

The app uses two primary datasets:

1. **Market Data**: Historical values for economic indicators, sourced from public economic databases
2. **Presidents Data**: Information about U.S. presidents, including inauguration and election dates

Data is loaded dynamically from the GitHub repository to ensure the latest information is displayed.

## Technical Details

The application is built with:

- R Shiny for the interactive web framework
- ggplot2 and Plotly for visualizations
- dplyr and tidyr for data manipulation
- Shinylive for browser-based execution without a backend server

## Local Development

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

## Acknowledgments

- Economic data provided by [SOURCE NAME]
- Presidential data compiled from public records
- Built with [Shinylive](https://posit.co/blog/shinylive-for-r/)
