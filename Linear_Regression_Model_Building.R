# Part B: Linear Regression Project

install.packages("dplyr")
library(readxl)
library(dplyr)
library(MASS)

# Loading and Cleaning Data (Recreating Part A Status)
# Outlier Removal (Based on Part A logic)
raw_dataset <- read_excel(file.choose())
dataset <- raw_dataset %>%
  filter(tempo > 0, time_signature > 0, song_duration_ms < 600000)



#2

#2.1

# Our First Thoughts:
cor(dataset$song_popularity, dataset$audio_mode)
table(dataset$time_signature)
  
# ALGO':
dataset_algo <- subset(dataset, select = -song_name)
# Step 1: Fit the Full Model
full_model <- lm(song_popularity ~ ., data = dataset_algo)

# Step 2: Run Backward Elimination
backward_model <- step(full_model, direction = "backward")

# Step 3: Check which variables were kept
print("--- Final Variables in the Model ---")
print(names(coef(backward_model)))


# 1. Fit Full Model (All variables)
model_full <- lm(song_popularity ~ ., data = subset(dataset, select = -song_name))

# 2. Fit Reduced Model (Without 'audio_mode')
model_reduced <- lm(song_popularity ~ ., data = subset(dataset, select = -c(song_name, audio_mode)))

# 3. Print Adjusted R-squared for comparison
print(paste("Adj R2 (Full Model):", summary(model_full)$adj.r.squared))
print(paste("Adj R2 (Reduced Model):", summary(model_reduced)$adj.r.squared))

# Update 'dataset'
dataset <- subset(dataset, select = -c(audio_mode))



#2.2

# 2.2.1 Feature Engineering: Adjustment 1 (Time Signature)

# AIC Check BEFORE adjustment
aic_before <- AIC(lm(song_popularity ~ . - song_name, data = dataset))
print(aic_before)

# Perform the Adjustment
# Create binary variable: 4/4 is 'Standard', others are 'Complex'
dataset$time_signature_bin <- as.factor(ifelse(dataset$time_signature == 4, "Standard", "Complex"))

# Remove the old numeric variable 'time_signature'
dataset$time_signature <- NULL

# AIC Check AFTER adjustment
aic_after <- AIC(lm(song_popularity ~ . - song_name, data = dataset))
print(aic_after)



# 2.2.2 Check Adjustment 2 (Tempo) - "On the Fly" Check

# 1. Calculate AIC on the CURRENT dataset X
aic_before <- AIC(lm(song_popularity ~ . - song_name, data = dataset))

# 2. Calculate AIC on a TEMPORARY transformation 
aic_after <- AIC(lm(song_popularity ~ . - song_name, 
                    data = transform(dataset, 
                                     tempo_cat = cut(tempo, 
                                                     breaks = c(0, 100, 130, Inf), 
                                                     labels = c("Low", "Medium", "High")),
                                     tempo = NULL)))

print(paste("AIC Before:", round(aic_before, 2)))
print(paste("AIC After: ", round(aic_after, 2)))

#Update 'dataset'
# Create the categorical variable 'tempo_cat' in the real dataset
dataset$tempo_cat <- cut(dataset$tempo, 
                         breaks = c(0, 100, 130, Inf), 
                         labels = c("Low", "Medium", "High"))

# Remove the original continuous variable 'tempo'
dataset$tempo <- NULL



# 2.2.3 Check Adjustment 3 (Electronic Score) 

# 1. Calculate AIC on current dataset (Before change)
aic_before <- AIC(lm(song_popularity ~ . - song_name, data = dataset))

# 2. Calculate AIC with the PROPOSED change
# Transformation:
# a) Create 'electronic_score' = energy - acousticness
# b) Remove 'energy'
# c) Remove 'acousticness'
aic_after <- AIC(lm(song_popularity ~ . - song_name, 
                    data = transform(dataset, 
                                     electronic_score = energy - acousticness,
                                     energy = NULL,
                                     acousticness = NULL)))

print(paste("AIC Before (Two variables):", round(aic_before, 2)))
print(paste("AIC After (Combined variable):", round(aic_after, 2)))




#2.3 

#  Create Dummy Variables
dataset$tempo_cat_Medium <- ifelse(dataset$tempo_cat == "Medium", 1, 0)
dataset$tempo_cat_High <- ifelse(dataset$tempo_cat == "High", 1, 0)




#2.4

# Interaction Check 1: Danceability * Energy

#  Build Base Model (Current state)
model_base <- lm(song_popularity ~ . - song_name, data = dataset)

#  Build Interaction Model (With danceability:energy)
model_inter_1 <- lm(song_popularity ~ . - song_name + danceability:energy, data = dataset)

#  Extract Metrics
sum_base <- summary(model_base)
sum_inter <- summary(model_inter_1)

#  Print Comparison Table
print(" Model Comparison: Base vs. Interaction ")

# Compare Adjusted R-Squared (Higher is better)
print(paste("Adj. R2 (Base): ", round(sum_base$adj.r.squared, 6)))
print(paste("Adj. R2 (Inter):", round(sum_inter$adj.r.squared, 6)))

# Compare AIC (Lower is better)
print(paste("AIC (Base): ", round(AIC(model_base), 2)))
print(paste("AIC (Inter):", round(AIC(model_inter_1), 2)))

#  Check Significance (P-value) of the interaction term
p_val <- sum_inter$coefficients["danceability:energy", "Pr(>|t|)"]
print("--- Interaction Significance ---")
print(paste("P-value for 'danceability:energy':", format(p_val, scientific = FALSE)))



# Interaction Check 2: Audio Valence * Energy

# Build Base Model (Current state)
model_base <- lm(song_popularity ~ . - song_name, data = dataset)

# Build Interaction Model (With audio_valence:energy)
model_inter_val_en <- lm(song_popularity ~ . - song_name + audio_valence:energy, data = dataset)

# Extract Metrics
sum_base <- summary(model_base)
sum_inter <- summary(model_inter_val_en)

# Print Comparison Table
print("--- Model Comparison: Base vs. Interaction ---")

# Compare Adjusted R-Squared (Higher is better)
print(paste("Adj. R2 (Base): ", round(sum_base$adj.r.squared, 6)))
print(paste("Adj. R2 (Inter):", round(sum_inter$adj.r.squared, 6)))

# Compare AIC (Lower is better)
print(paste("AIC (Base): ", round(AIC(model_base), 2)))
print(paste("AIC (Inter):", round(AIC(model_inter_val_en), 2)))

# Check Significance (P-value) of the interaction term
print("--- Interaction Significance ---")

# FIX: Automatically find the interaction row (works regardless of variable order)
inter_name <- grep(":", rownames(sum_inter$coefficients), value = TRUE)
p_val <- sum_inter$coefficients[inter_name, "Pr(>|t|)"]

print(paste("P-value for Interaction:", format(p_val, scientific = FALSE)))



# Interaction Check 3: Acousticness * Instrumentalness

# Build Base Model (Current state)
model_base <- lm(song_popularity ~ . - song_name, data = dataset)

# Build Interaction Model (With acousticness:instrumentalness)
model_inter_ac_inst <- lm(song_popularity ~ . - song_name + acousticness:instrumentalness, data = dataset)

# Extract Metrics
sum_base <- summary(model_base)
sum_inter <- summary(model_inter_ac_inst)

# Print Comparison Table
print("--- Model Comparison: Base vs. Interaction ---")

# Compare Adjusted R-Squared (Higher is better)
print(paste("Adj. R2 (Base): ", round(sum_base$adj.r.squared, 6)))
print(paste("Adj. R2 (Inter):", round(sum_inter$adj.r.squared, 6)))

# Compare AIC (Lower is better)
print(paste("AIC (Base): ", round(AIC(model_base), 2)))
print(paste("AIC (Inter):", round(AIC(model_inter_ac_inst), 2)))

# Check Significance (P-value) of the interaction term
print("--- Interaction Significance ---")

# FIX: Automatically find the interaction row (works regardless of variable order)
inter_name <- grep(":", rownames(sum_inter$coefficients), value = TRUE)
p_val <- sum_inter$coefficients[inter_name, "Pr(>|t|)"]

print(paste("P-value for Interaction:", format(p_val, scientific = FALSE)))






# 1. Create Dummy Variables Manually

# Create a dummy ranking for Medium (1 if Medium, otherwise 0)
dataset$tempo_cat_Medium <- ifelse(dataset$tempo_cat == "Medium", 1, 0)

#Create a dummy variable for High (1 if High, otherwise 0)
dataset$tempo_cat_High <- ifelse(dataset$tempo_cat == "High", 1, 0)


# 2. Final Model Fit

final_model <- lm(
  song_popularity ~ 
  # Original explanatory variables (without tempo_cat))
  acousticness +
  danceability +
  energy +
  instrumentalness +
  liveness +
  speechiness +
  audio_valence +
  song_duration_ms +
  time_signature_bin +
  
  # The new dummy variables (instead of the original variable))
  tempo_cat_Medium +
  tempo_cat_High +
  
  # interactions
  danceability:energy +
  audio_valence:energy +
  acousticness:instrumentalness,
  
  data = dataset
)


# 3. Model Performance Metrics


# Adjusted R-squared
print("--- Adjusted R-squared ---")
print(summary(final_model)$adj.r.squared)

# AIC
print("--- AIC ---")
print(AIC(final_model))

# Full Model Summary
print("--- Full Model Summary ---")
summary(final_model)






#3#


# 3.1 Model Selection using AIC (Backward vs. Forward)

# Define the full model
full_model <- lm(
  song_popularity ~ 
    acousticness + danceability + energy + instrumentalness + 
    liveness + speechiness + audio_valence + song_duration_ms + 
    time_signature_bin + 
    tempo_cat_Medium + tempo_cat_High + 
    danceability:energy + audio_valence:energy + acousticness:instrumentalness,
  data = dataset
)

# Define the null model 
null_model <- lm(song_popularity ~ 1, data = dataset)

# Run Backward Elimination algorithm based on AIC
final_backward_aic <- step(full_model, direction = "backward", trace = 0)

# Run Forward Selection algorithm based on AIC
final_forward_aic <- step(null_model, 
                          scope = list(lower = null_model, upper = full_model), 
                          direction = "forward", 
                          trace = 0)

# Display results and comparison

print("--- Algorithm 1: Backward Elimination (Best AIC) ---")
print(paste("Final AIC:", round(AIC(final_backward_aic), 2)))
print("Variables Selected:")
print(names(coef(final_backward_aic)))

print(" ") # Empty line

print("--- Algorithm 2: Forward Selection (Best AIC) ---")
print(paste("Final AIC:", round(AIC(final_forward_aic), 2)))
print("Variables Selected:")
print(names(coef(final_forward_aic)))




# 3.2 Visual Diagnostics (Residuals & Q-Q Plot)

# Set up the plotting area to show 2 plots side-by-side
par(mfrow = c(1, 2))

# Plot 1: Standardized Residuals vs. Fitted Values 
plot(fitted(final_backward_aic), rstandard(final_backward_aic),
     main = "Standardized Residuals vs. Fitted",
     xlab = "Fitted Values (Predictions)",
     ylab = "Standardized Residuals",
     pch = 20, col = "gray")

# Add a horizontal reference line at 0 (Red)
abline(h = 0, col = "red", lwd = 2)

# Add a smooth trend line (Blue) to help detect non-linearity
lines(lowess(fitted(final_backward_aic), rstandard(final_backward_aic)), 
      col = "blue", lwd = 2)


# Plot 2: Normal Q-Q Plot 
qqnorm(rstandard(final_backward_aic), 
       main = "Normal Q-Q Plot",
       pch = 20, col = "gray")
qqline(rstandard(final_backward_aic), col = "red", lwd = 2)

# Reset plotting area to default (1x1)
par(mfrow = c(1, 1))




#3.3

# PART 1: Formal Test for Normality Assumption

# Extract standardized residuals from the final model to standardize the scale
std_residuals <- rstandard(final_model)
# Kolmogorov-Smirnov (KS) Test 
print("--- KS Test Results ---")
ks_result <- ks.test(std_residuals, "pnorm")
print(ks_result)

# Shapiro-Wilk Test 
#  The shapiro test function has a limit of 5000 observations.
#  we take a random sample of 5000 residuals.
print("--- Shapiro-Wilk Test Results ---")

set.seed(123) 

shapiro_result <- shapiro.test(sample(std_residuals, 5000))
print(shapiro_result)



# PART 2: Formal Test for Linearity Assumption

install.packages("strucchange")
library(strucchange)

print("--- M-fluctuation (Chow) Test Results ---")
chow_test <- sctest(final_model, type = "Rec-CUSUM")
print(chow_test)




#4

# 1. Update the original model by adding a squared transformation of 'energy'
final_model_poly <- update(final_model, . ~ . + I(energy^2))

# 2. View the summary to see if the new squared variable is significant
summary(final_model_poly)

# 3. Compare model efficiency using AIC 
AIC(final_model, final_model_poly)







# 1. Executive Summary


# Function to extract and print a linear regression model
pretty_lm_equation <- function(model, digits = 2) {
  
# Extract coefficients from the model
coeffs <- coef(model)
  
# Helper function for rounding numbers
format_num <- function(x) {
format(round(x, digits), nsmall = digits, trim = TRUE)
  }
  
# Clean variable names for presentation
clean_name <- function(name) {
name <- gsub("`", "", name)              # remove backticks
name <- gsub(":", " × ", name)           # interactions
name <- gsub("I\\((.+)\\)", "\\1", name) # remove I()
name <- gsub("\\^2", "²", name)          # square notation
name
  }
  
# Start with intercept
equation_parts <- c(format_num(coeffs["(Intercept)"]))
  
# Add remaining coefficients
for (var in names(coeffs)[names(coeffs) != "(Intercept)"]) {
    beta <- coeffs[var]
    sign <- ifelse(beta >= 0, " + ", " - ")
    equation_parts <- c(
      equation_parts,
      paste0(sign, format_num(abs(beta)), "(", clean_name(var), ")")
    )
  }
  
# Final equation
equation <- paste0("ŷ = ", paste(equation_parts, collapse = ""))
return(equation)
}


# Model after variable selection (before polynomial)
eq_selected_model <- pretty_lm_equation(final_backward_aic)
cat(eq_selected_model, "\n\n")


# Final improved model (including squared transformation)
eq_final_model <- pretty_lm_equation(final_model_poly)
cat(eq_final_model, "\n")




# Final Project Summary: Total Model Improvement Check (AIC & Adj R2)

# Calculate Start Point (Original Full Model)
dataset_original <- raw_dataset %>%
  filter(tempo > 0, time_signature > 0, song_duration_ms < 600000)

# Fit model on original data
model_start <- lm(song_popularity ~ . - song_name, data = dataset_original)

# Extract Metrics for Start Point
aic_start <- AIC(model_start)
r2_start  <- summary(model_start)$adj.r.squared

# Get End Point (Final Improved Model)
aic_end <- AIC(final_model_poly)
r2_end  <- summary(final_model_poly)$adj.r.squared

# Print Report
print("==========================================")
print("       PROJECT IMPROVEMENT REPORT         ")
print("==========================================")

# AIC Report 
print("--- AIC ---")
print(paste("1. Start Point: ", round(aic_start, 2)))
print(paste("2. End Point:   ", round(aic_end, 2)))
print(paste("TOTAL REDUCTION:", round(aic_start - aic_end, 2)))

print(" ") # Empty line

# Adjusted R-squared Report 
print("--- Adjusted R-squared ---")
print(paste("1. Start Point: ", round(r2_start, 6)))
print(paste("2. End Point:   ", round(r2_end, 6)))
print(paste("TOTAL INCREASE: ", round(r2_end - r2_start, 6)))

