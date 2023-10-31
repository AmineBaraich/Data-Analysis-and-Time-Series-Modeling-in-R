# Data-Analysis-and-Time-Series-Modeling-in-R
# Analysis of Financial Sources
## Project Overview

This project involves a analysis of the sources of financing for the Moroccan economy. The analysis consists of two main parts:

### Part 1: Descriptive Analysis

In this part, we aim to provide empirical evidence regarding the importance of financial markets (direct finance) and the banking sector (indirect finance) in both
economies. The analysis will highlight the presence of agency problems arising from issues like information asymmetry and limited commitment within the financial system. 
This will be demonstrated through graphical and statistical representations of financial time series data, including risk premiums that are indicative of financial frictions.

### Part 2: Quantitative Analysis

In the second part, we will conduct a quantitative analysis by reviewing the relevant literature and proposing a macroeconomic model that incorporates suitable financial 
frictions. This model will be used to empirically test the relevance and relative importance of agency problems in the two economies.

## Code Description

The provided R code performs the following tasks:

1. Data Import and Preparation: Import and prepare the dataset from "data_indicateur.xlsx" for analysis.

2. Principal Component Analysis (PCA): Perform PCA on the data, visualize the results, and provide a summary.

3. Export PCA Results: Export PCA results, including Eigenvalues, Correlations, Coordinates, and Contributions, to separate files.

4. Time Series Data Transformation: Transform selected variables into time series data, conduct stationarity tests, and differentiate the time series.

5. VAR Model Building: Create a Vector AutoRegressive (VAR) model to analyze the relationships between the differentiated time series data.

6. Model Impact Simulation: Simulate the impact of external factors on the VAR model and visualize the results.


