# Predicting Song Popularity using Linear Regression

## Project Overview
This project aims to identify and model the musical factors that influence song popularity. Using a dataset of over **18,000 tracks**, we built and refined a linear regression model to predict popularity based on features like acousticness, energy, and tempo.

## Key Features & Methodology
* **Data Cleaning:** Systematic removal of outliers, such as songs with 0 BPM or excessive duration (above 500,000 ms).
* **Feature Engineering:**
    * Converted `time_signature` to binary (Standard/Complex).
    * Categorized `tempo` into Low, Medium, and High categories.
    * Introduced significant interaction terms, including `danceability × energy` and `acousticness × instrumentalness`.
* **Model Selection:** Utilized the **Backward Elimination** algorithm based on the AIC criterion to achieve the most efficient model.
* **Advanced Optimization:** Added a polynomial transformation (squared energy - $energy^2$) which resulted in a 31-point reduction in AIC.

## Final Results
The model refinement process led to a significant improvement in predictive metrics:
* **Initial AIC:** 168,746.73
* **Final AIC:** 168,455.71 (Total Reduction of **291.02**)
* **Adjusted R-squared:** Improved from **0.0397** to **0.0547**.

## Technologies Used
* **Language:** R
* **Key Libraries:** `dplyr`, `MASS`, `strucchange`, `ggplot2`
