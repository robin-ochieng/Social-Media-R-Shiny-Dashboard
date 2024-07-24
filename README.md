# Advertising Report Shiny Dashboard

![image](https://github.com/user-attachments/assets/b7700584-1971-42e7-98f6-9e5467fb913a)

This Shiny app is an interactive dashboard for visualizing and analyzing advertising metrics from various social media platforms. It includes data for LinkedIn, Facebook, Instagram, and X (Twitter).

## Features

- **Responsive UI**: Built with `bs4Dash` for a modern and responsive user interface.
- **Data Visualization**: Interactive DataTables for displaying KPIs and detailed reports.
- **Dynamic Value Boxes**: Real-time summary statistics for key performance indicators.
- **Custom Theme**: A tailored theme for a consistent and visually appealing presentation.

## Installation

To run this application locally, you need to have R installed. Follow these steps:

1. **Clone the Repository**:
    ```sh
    git clone https://github.com/your-repo/advertising-report-dashboard.git
    cd advertising-report-dashboard
    ```

2. **Install Required Packages**:
    ```R
    install.packages(c("shiny", "bs4Dash", "DT"))
    ```

3. **Run the Shiny App**:
    ```R
    shiny::runApp()
    ```

## File Structure

- **app.R**: Main application file.
- **ui.R**: User interface definition.
- **server.R**: Server logic for handling inputs and outputs.
- **data/**: Directory containing CSV files with advertising data.
- **www/**: Directory for custom styles and images.

## Usage

Once the application is running, you can interact with the dashboard to view and analyze advertising metrics. The following components are included:

- **Overview**: Summary of key performance indicators.
- **Detailed Reports**: Drill down into specific metrics for LinkedIn, Facebook, Instagram, and X.
- **DataTables**: Interactive tables for detailed views of the data.

## Contributing

We welcome contributions to improve this Shiny dashboard. Please submit a pull request or open an issue for any suggestions or bug reports.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgements

- Built with [Shiny](https://shiny.rstudio.com/).
- UI components provided by [bs4Dash](https://rinterface.github.io/bs4Dash/).

![image](https://github.com/user-attachments/assets/e2e466eb-44a5-47e2-8995-b36fc4ff1573)

