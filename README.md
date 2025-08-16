# Revolut-Spending
# Revolut Debit Card Spending Analysis  

This repository contains an end-to-end time series analysis and forecasting project using **Revolut debit card spending data**. The project investigates transaction trends in the UK, with a focus on age demographics, seasonality, and forecasting future spending patterns.  

---

## Repository Structure  

1. **`spendings_dataset.csv`**  
   - Dataset provided by Revolut.  
   - Includes monthly spending values broken down by demographics and transaction type (online vs in-store).  
   - Covers a 5-year period.  

2. **`revolut_spending.R`**  
   - Main R script containing the analysis, model building, evaluation, and visualisation.  
   - Implements multiple forecasting approaches including SARIMA, SARIMAX, VAR, ETS, and Linear Regression.  

---

## Project Objectives  

- Explore and clean Revolut’s debit card spending dataset.  
- Perform **Exploratory Data Analysis (EDA)** and **time series decomposition** to identify trends and seasonality.  
- Apply and compare forecasting models:  
  - SARIMA (baseline)  
  - SARIMAX (with exogenous variables, e.g. Age 35–54 spending)  
  - Vector Autoregression (VAR)  
  - Exponential Smoothing State Space Model (ETS)  
  - Linear Regression (benchmark)  
- Select the best-performing model using evaluation metrics (MAE, RMSE, MAPE).  
- Forecast **future spending for 24 months**, with visualisation of actual, predicted, and forecasted values.  

---

## Key Findings  

- Revolut’s **total debit card spending has increased steadily over the last 5 years**, though growth rates vary by age group.  
- Younger adults (18–34) show the most consistent growth, while older age groups (35–54, 55+) exhibit slower and less steady increases.  
- Seasonality is observed in online vs in-store transaction percentages.  
- The **SARIMAX model (with Age 35–54 as an exogenous factor)** outperformed other models in forecasting accuracy.  

---

## Business Impact  

- Forecasting helps Revolut anticipate **spending demand** and optimise financial planning.  
- Insights into age-specific spending patterns support **targeted product development and marketing**.  
- Seasonal behaviour analysis improves **resource allocation** for digital and in-store transaction services.  

---

## Tools & Libraries  

- **Language:** R  
- **Libraries:**  
  - `forecast`  
  - `tseries`  
  - `urca`  
  - `ggplot2`  
  - `vars`  
  - `fpp3`  
  - `lmtest`  

---

## How to Run  

1. Clone the repository:  
   ```bash
   git clone https://github.com/your-username/revolut-spending-analysis.git
   cd revolut-spending-analysis
2. Open R or RStudio.

3. Load the dataset:

R
Copy
Edit
data <- read.csv("spendings_dataset.csv")
Run the analysis script:

R
4. Copy
5.Edit
source("revolut_spending.R")
View the generated plots and results in your RStudio environment.

