# ================================================================
# ROTTEN TOMATOES MOVIE RATINGS ANALYSIS
# Structure: Data Prep → Descriptive → Inferential → Predictive
# ================================================================

setwd("C:\\Users\\LENOVO\\Desktop\\rtest\\rotten\\csv")

# ---- Packages ----
required_packages <- c(
  "tidyverse", "stringr", "lubridate", "caret", "randomForest",
  "gbm", "xgboost", "ggplot2", "scales", "gridExtra", "corrplot"
)
new_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(tidyverse)
library(stringr)
library(lubridate)
library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(ggplot2)
library(scales)
library(gridExtra)
library(corrplot)

# ================================================================
# 1. LOAD DATA
# ================================================================
movies_raw <- read.csv(
  "C:\\Users\\LENOVO\\Desktop\\rtest\\rotten\\csv\\rotten_tomatoes_movies.csv",
  stringsAsFactors = FALSE
)

movies_clean <- movies_raw %>%
  select(
    movie_title,
    tomatometer_rating,
    audience_rating,
    genres,
    directors,
    actors,
    content_rating,
    runtime,
    original_release_date
  )




# ================================================================
# 2. DATA PREPROCESSING
# ================================================================

# Missing value summary
missing_summary <- data.frame(
  column      = names(movies_clean),
  missing     = colSums(is.na(movies_clean) | movies_clean == ""),
  missing_pct = round(colSums(is.na(movies_clean) | movies_clean == "") /
                        nrow(movies_clean) * 100, 2)
)
print(missing_summary)

# Drop missing target/predictor + invalid ratings + duplicates
movies_clean <- movies_clean %>%
  filter(!is.na(tomatometer_rating) & !is.na(audience_rating)) %>%
  filter(tomatometer_rating >= 0 & tomatometer_rating <= 100,
         audience_rating    >= 0 & audience_rating    <= 100) %>%
  distinct(movie_title, .keep_all = TRUE)

boxplot(movies_clean$audience_rating)

# Runtime cleaning + median imputation
movies_clean$runtime <- as.numeric(movies_clean$runtime)
movies_clean <- movies_clean %>%
  filter(is.na(runtime) | (runtime >= 30 & runtime <= 240))
movies_clean$runtime[is.na(movies_clean$runtime)] <-
  median(movies_clean$runtime, na.rm = TRUE)

# ---- Date features from original_release_date ----
movies_clean$release_date <- ymd(movies_clean$original_release_date)
fallback_idx <- is.na(movies_clean$release_date) &
  !is.na(movies_clean$original_release_date) &
  movies_clean$original_release_date != ""
if (any(fallback_idx)) {
  movies_clean$release_date[fallback_idx] <-
    parse_date_time(movies_clean$original_release_date[fallback_idx],
                    orders = c("mdy", "dmy", "Ymd", "Y-m-d"))
}
movies_clean <- movies_clean %>% filter(!is.na(release_date))

movies_clean <- movies_clean %>%
  mutate(
    release_year   = year(release_date),
    #release_month  = month(release_date),
    movie_age      = as.numeric(year(Sys.Date()) - release_year),
  ) %>%
  filter(release_year >= 1900 & release_year <= year(Sys.Date()))

# ---- Clean text columns ----
movies_clean$genres <- as.character(movies_clean$genres) %>%
  str_replace_all("&", ",") %>% str_squish()
movies_clean <- movies_clean %>% filter(!is.na(genres) & genres != "")

movies_clean$directors <- as.character(movies_clean$directors) %>%
  str_squish() %>% str_replace_all("&", ",")
movies_clean$directors[is.na(movies_clean$directors) |
                         movies_clean$directors == ""] <- "Unknown"

movies_clean$actors <- as.character(movies_clean$actors) %>%
  str_squish() %>% str_replace_all("&", ",")
movies_clean$actors[is.na(movies_clean$actors) |
                      movies_clean$actors == ""] <- "Unknown"

movies_clean$content_rating <- movies_clean$content_rating %>%
  str_to_upper() %>% str_squish() %>%
  str_replace_all("NOT RATED|UNRATED|N/A", "NR")
rating_counts <- table(movies_clean$content_rating)
rare_ratings  <- names(rating_counts[rating_counts < 50])
movies_clean$content_rating[movies_clean$content_rating %in% rare_ratings] <- "Other"
movies_clean$content_rating[is.na(movies_clean$content_rating) |
                              movies_clean$content_rating == ""] <- "Other"
movies_clean$content_rating <- as.factor(movies_clean$content_rating)

# Outlier removal on runtime
Q1 <- quantile(movies_clean$runtime, 0.25)
Q3 <- quantile(movies_clean$runtime, 0.75)
IQR_val <- Q3 - Q1
movies_clean <- movies_clean %>%
  filter(runtime >= Q1 - 1.5 * IQR_val & runtime <= Q3 + 1.5 * IQR_val)

cat("Final dataset:", nrow(movies_clean), "rows\n")



# ================================================================
# 3. DESCRIPTIVE ANALYSIS
# ================================================================
cat("\n========== DESCRIPTIVE ANALYSIS ==========\n")

# Summary statistics
cat("\nSummary of Tomatometer Rating:\n")
print(summary(movies_clean$tomatometer_rating))
cat("\nSummary of Audience Rating:\n")
print(summary(movies_clean$audience_rating))
cat("\nSummary of Runtime:\n")
print(summary(movies_clean$runtime))

# Distribution of Tomatometer Rating
ggplot(movies_clean, aes(x = tomatometer_rating)) +
  geom_histogram(bins = 30, fill = "tomato", color = "white", alpha = 0.85) +
  labs(title = "Distribution of Tomatometer Rating",
       x = "Tomatometer Rating", y = "Frequency") +
  theme_minimal()

# Distribution of Audience Rating
ggplot(movies_clean, aes(x = audience_rating)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.85) +
  labs(title = "Distribution of Audience Rating",
       x = "Audience Rating", y = "Frequency") +
  theme_minimal()

# Tomatometer by Content Rating
ggplot(movies_clean, aes(x = content_rating, y = tomatometer_rating,
                         fill = content_rating)) +
  geom_boxplot(alpha = 0.85) +
  labs(title = "Tomatometer Rating by Content Rating",
       x = "Content Rating", y = "Tomatometer Rating") +
  theme_minimal() +
  theme(legend.position = "none")

# Movies released per year
movies_clean %>%
  count(release_year) %>%
  ggplot(aes(x = release_year, y = n)) +
  geom_col(fill = "#2E86AB", alpha = 0.85) +
  labs(title = "Number of Movies Released per Year",
       x = "Release Year", y = "Count") +
  theme_minimal()

# Average Tomatometer over time
movies_clean %>%
  group_by(release_year) %>%
  summarise(avg_score = mean(tomatometer_rating), n = n()) %>%
  filter(n >= 10) %>%
  ggplot(aes(x = release_year, y = avg_score)) +
  geom_line(color = "#A23B72", size = 0.7) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(title = "Average Tomatometer Score Over Time",
       x = "Release Year", y = "Average Score") +
  theme_minimal()

#boxplot of audience and tomatometer rating
ratings_long <- movies_clean %>%
  select(tomatometer_rating, audience_rating) %>%
  pivot_longer(cols = everything(),
               names_to = "rating_type",
               values_to = "rating")

ggplot(ratings_long, aes(x = rating_type, y = rating, fill = rating_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot of Audience vs Critic Ratings",
       x = "Rating Type",
       y = "Score") +
  theme_minimal()

# Correlation heatmap of numeric features
numeric_data <- movies_clean %>%
  select(tomatometer_rating, audience_rating, runtime,
         release_year)
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#C73E1D", "white", "#2E86AB"))(200),
         title = "Correlation Heatmap", mar = c(0, 0, 2, 0))





# ================================================================
# 4. INFERENTIAL ANALYSIS
# ================================================================
cat("\n========== INFERENTIAL ANALYSIS ==========\n")

# ---- Hypothesis Framework ----
# H0: No relationship exists between audience rating and critic rating
# H1: Audience rating is significantly related to critic rating
# Significance level: α = 0.05

# ---------------------------------------------------------------
# 4.1  Normality check (Shapiro–Wilk caps at 5000)
# ---------------------------------------------------------------
cat("\n--- 4.1 Shapiro–Wilk Normality Tests ---\n")
safe_shapiro <- function(x, name) {
  x <- x[!is.na(x)]
  res <- shapiro.test(sample(x, min(5000, length(x))))
  cat(sprintf("%-22s W = %.4f, p = %.4g\n",
              name, res$statistic, res$p.value))
}
safe_shapiro(movies_clean$tomatometer_rating, "tomatometer_rating")
safe_shapiro(movies_clean$audience_rating,    "audience_rating")
safe_shapiro(movies_clean$runtime,            "runtime")

par(mfrow = c(1, 2))
qqnorm(movies_clean$tomatometer_rating, main = "Q-Q: Tomatometer")
qqline(movies_clean$tomatometer_rating, col = "red", lwd = 2)
qqnorm(movies_clean$audience_rating, main = "Q-Q: Audience")
qqline(movies_clean$audience_rating, col = "blue", lwd = 2)
par(mfrow = c(1, 1))

plot(density(movies_clean$tomatometer_rating), main = "Density Plot of Data", xlab = "Data Values", col = "orange")
plot(density(movies_clean$audience_rating), main = "Density Plot of Data", xlab = "Data Values", col = "blue")



# ============================================================
# METHOD 1: LOG TRANSFORMATION
# ============================================================

# Apply log transformation (+1 to handle zeros)
log_audience <- log(movies_clean$audience_rating + 1)
log_tomato   <- log(movies_clean$tomatometer_rating + 1)

# Visualize with density plots
par(mfrow = c(1, 2))  # side-by-side plots

plot(density(log_audience), 
     main = "Log-Audience Distribution", 
     xlab = "Log(Audience + 1)", 
     col = "orange")

plot(density(log_tomato), 
     main = "Log-Tomato Distribution", 
     xlab = "Log(Tomato + 1)", 
     col = "blue")

# Test normality
safe_shapiro(log_audience, "log_audience")
safe_shapiro(log_tomato,   "log_rating")


# ============================================================
# METHOD 2: YEO-JOHNSON TRANSFORMATION
# ============================================================

# Define pre-processing (Yeo-Johnson automatically finds optimal lambda)
preProc <- preProcess(movies_clean[, c("tomatometer_rating", "audience_rating")],
                      method = c("YeoJohnson"))

# Apply transformation
movies_transformed <- predict(preProc, movies_clean)

# Check normality after Yeo-Johnson
safe_shapiro(movies_transformed$audience_rating, "movies_transformed$audience_rating")
safe_shapiro(movies_transformed$tomatometer_rating, "movies_transformed$tomatometer_rating")

# Optional: density plots for Yeo-Johnson results
par(mfrow = c(1, 2))
plot(density(movies_transformed$audience_rating), 
     main = "Yeo-Johnson: Audience", 
     col = "darkgreen")
plot(density(movies_transformed$tomatometer_rating), 
     main = "Yeo-Johnson: Tomato", 
     col = "purple")

safe_shapiro(movies_transformed$audience_rating, "log_audience")
safe_shapiro(movies_transformed$tomatometer_rating,   "log_rating")


# ---------------------------------------------------------------
# 4.2  Spearman correlation (audience vs critic)
# ---------------------------------------------------------------
cat("\n--- 4.2 Correlation Test: audience_rating vs tomatometer_rating ---\n")
spear_at <- cor.test(movies_clean$audience_rating,
                     movies_clean$tomatometer_rating, method = "spearman")
print(spear_at)

# ---------------------------------------------------------------
# 4.6  Inferential summary
# ---------------------------------------------------------------
inference_summary <- data.frame(
  Test = c(
           "Spearman ρ (audience vs critic)",
           "Spearman ρ (runtime vs critic)",
           "Spearman ρ (year vs critic)"),
  Statistic = c(
                round(spear_at$estimate, 3),
                round(cor(movies_clean$runtime,
                          movies_clean$tomatometer_rating,
                          method = "spearman"), 3),
                round(cor(movies_clean$release_year,
                          movies_clean$tomatometer_rating,
                          method = "spearman"), 3)),
  P_value = c(
              format.pval(spear_at$p.value, eps = .001),
              "see output", "see output")
)
cat("\n--- 4.6 Inferential Summary ---\n")
print(inference_summary)

# ================================================================
# 5. PREDICTIVE ANALYSIS
# ================================================================
cat("\n========== PREDICTIVE ANALYSIS ==========\n")

# ---------------------------------------------------------------
# Feature engineering for modeling
# ---------------------------------------------------------------

# One-hot encode genres
all_genres <- movies_clean$genres %>%
  str_split(",") %>% unlist() %>% str_trim() %>% unique() %>% na.omit()
for (g in all_genres) {
  col_name <- paste0("genre_", str_replace_all(g, "[^A-Za-z0-9]", "_"))
  movies_clean[[col_name]] <- as.integer(str_detect(movies_clean$genres, fixed(g)))
}

# Director features
movies_clean$num_directors <- str_count(movies_clean$directors, ",") + 1
director_counts <- movies_clean %>%
  separate_rows(directors, sep = ",") %>%
  mutate(directors = str_trim(directors)) %>%
  count(directors, name = "director_movie_count")
movies_clean$director_popularity <- sapply(movies_clean$directors, function(d) {
  if (is.na(d) || d == "Unknown") return(0)
  dirs <- str_trim(str_split(d, ",")[[1]])
  matched <- director_counts$director_movie_count[director_counts$directors %in% dirs]
  if (length(matched) == 0) 0 else max(matched)
})

# Director's average historical score
director_avg_score <- movies_clean %>%
  separate_rows(directors, sep = ",") %>%
  mutate(directors = str_trim(directors)) %>%
  group_by(directors) %>%
  summarise(dir_avg_score = mean(tomatometer_rating, na.rm = TRUE))
movies_clean$director_avg_score <- sapply(movies_clean$directors, function(d) {
  if (is.na(d) || d == "Unknown") return(median(movies_clean$tomatometer_rating))
  dirs <- str_trim(str_split(d, ",")[[1]])
  matched <- director_avg_score$dir_avg_score[director_avg_score$directors %in% dirs]
  if (length(matched) == 0) median(movies_clean$tomatometer_rating) else mean(matched)
})

# Actor features
movies_clean$num_actors <- str_count(movies_clean$actors, ",") + 1
actor_counts <- movies_clean %>%
  separate_rows(actors, sep = ",") %>%
  mutate(actors = str_trim(actors)) %>%
  count(actors, name = "actor_movie_count")
movies_clean$actor_popularity <- sapply(movies_clean$actors, function(a) {
  if (is.na(a) || a == "Unknown") return(0)
  acts <- str_trim(str_split(a, ",")[[1]])
  matched <- actor_counts$actor_movie_count[actor_counts$actors %in% acts]
  if (length(matched) == 0) 0 else max(matched)
})

# Build modeling dataset
model_data <- movies_clean %>%
  select(tomatometer_rating, audience_rating, runtime, content_rating,
         release_year, movie_age,
         num_directors, director_popularity, director_avg_score,
         num_actors, actor_popularity,
         starts_with("genre_")) %>%
  na.omit()

# Train/test split
set.seed(123)
train_index <- createDataPartition(model_data$tomatometer_rating,
                                   p = 0.8, list = FALSE)
train_data  <- model_data[train_index, ]
test_data   <- model_data[-train_index, ]
actual <- test_data$tomatometer_rating
cat("Train:", nrow(train_data), "| Test:", nrow(test_data), "\n")

# Helper to evaluate model
evaluate_model <- function(actual, predicted, model_name) {
  data.frame(
    Model = model_name,
    RMSE  = round(sqrt(mean((actual - predicted)^2)), 3),
    MAE   = round(mean(abs(actual - predicted)), 3),
    R2    = round(cor(actual, predicted)^2, 3)
  )
}





# ---------------------------------------------------------------
# 5.1  SIMPLE LINEAR REGRESSION
#         tomatometer_rating ~ audience_rating
# ---------------------------------------------------------------
# H0: slope (β1) = 0  →  audience rating has NO linear effect
# H1: slope (β1) ≠ 0  →  audience rating predicts critic rating

cat("\n--- 5.1 Simple Linear Regression: tomatometer ~ audience ---\n")
slr_model <- lm(tomatometer_rating ~ audience_rating, data = train_data)
print(summary(slr_model))

cat("\n95% Confidence Intervals:\n")
print(round(confint(slr_model), 4))

slr_coef    <- coef(slr_model)
slr_summary <- summary(slr_model)
slr_r2      <- slr_summary$r.squared
slr_fstat   <- slr_summary$fstatistic
slr_pvalue  <- pf(slr_fstat[1], slr_fstat[2], slr_fstat[3], lower.tail = FALSE)

cat("\n----- Plain-English Interpretation -----\n")
cat(sprintf("Regression equation:\n"))
cat(sprintf("  tomatometer = %.3f + %.3f × audience_rating\n",
            slr_coef[1], slr_coef[2]))
cat(sprintf("R-squared : %.4f  (%.1f%% variance explained)\n",
            slr_r2, slr_r2 * 100))
cat(sprintf("F p-value : %.4g\n", slr_pvalue))

# Predict on test set
slr_pred <- predict(slr_model, newdata = test_data)
slr_res  <- evaluate_model(actual, slr_pred, "Simple Linear Regression")

# Visualize the SLR
ggplot(train_data, aes(x = audience_rating, y = tomatometer_rating)) +
  geom_point(alpha = 0.25, color = "#2E86AB", size = 0.8) +
  geom_smooth(method = "lm", color = "red", fill = "pink", se = TRUE) +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen",
              linetype = "dashed") +
  annotate("text", x = 5, y = 95,
           label = sprintf("y = %.2f + %.2f·x\nR² = %.3f",
                           slr_coef[1], slr_coef[2], slr_r2),
           hjust = 0, size = 4, fontface = "bold", color = "darkred") +
  labs(title = "Simple Linear Regression: Tomatometer vs Audience Rating",
       subtitle = "Red = fitted line | Green dashed = y = x reference",
       x = "Audience Rating", y = "Tomatometer Rating") +
  xlim(0, 100) + ylim(0, 100) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))



# ---------------------------------------------------------------
# 5.2  MULTIPLE LINEAR REGRESSION
# ---------------------------------------------------------------
cat("\n--- 5.2 Multiple Linear Regression ---\n")
mlr_model <- lm(tomatometer_rating ~ ., data = train_data)
print(summary(mlr_model))

mlr_pred <- predict(mlr_model, newdata = test_data)
mlr_res  <- evaluate_model(actual, mlr_pred, "Multiple Linear Regression")

pred <- predict(mlr_model, newdata = train_data)

# Plot
plot(train_data$tomatometer_rating, pred,
     xlab = "Actual Tomatometer Rating",
     ylab = "Predicted Tomatometer Rating",
     main = "Actual vs Predicted Ratings")

abline(0, 1)

# ---------------------------------------------------------------
# 5.3  RANDOM FOREST
# ---------------------------------------------------------------
cat("\n--- 5.3 Random Forest ---\n")
set.seed(123)
rf_model <- randomForest(
  tomatometer_rating ~ .,
  data       = train_data,
  ntree      = 500,
  mtry       = floor(sqrt(ncol(train_data) - 1)),
  importance = TRUE
)
print(rf_model)
rf_pred <- predict(rf_model, newdata = test_data)
rf_res  <- evaluate_model(actual, rf_pred, "Random Forest")

# Predicted vs actual (RF)
ggplot(data.frame(actual = actual, predicted = rf_pred),
       aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red",
              linetype = "dashed", size = 1) +
  labs(title = "Random Forest: Predicted vs Actual",
       subtitle = paste0("RMSE = ", rf_res$RMSE,
                         " | R² = ", rf_res$R2),
       x = "Actual", y = "Predicted") +
  xlim(0, 100) + ylim(0, 100) +
  theme_minimal()

# Feature importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)
importance_df <- importance_df %>%
  arrange(desc(`%IncMSE`)) %>% head(15)

ggplot(importance_df, aes(x = reorder(Feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Most Important Features (Random Forest)",
       x = "Feature", y = "% Increase in MSE") +
  theme_minimal()



# ---------------------------------------------------------------
# 5.4  GRADIENT BOOSTING (Tuned)
# ---------------------------------------------------------------
cat("\n--- 5.4 Gradient Boosting ---\n")
set.seed(123)
gbm_model <- gbm(
  tomatometer_rating ~ .,
  data              = train_data,
  distribution      = "gaussian",
  n.trees           = 5000,
  interaction.depth = 6,
  shrinkage         = 0.01,
  bag.fraction      = 0.8,
  cv.folds          = 5,
  n.minobsinnode    = 10,
  verbose           = FALSE
)
best_iter <- gbm.perf(gbm_model, method = "cv", plot.it = FALSE)
cat("Optimal trees:", best_iter, "\n")

gbm_pred <- predict(gbm_model, newdata = test_data, n.trees = best_iter)
gbm_res  <- evaluate_model(actual, gbm_pred, "Gradient Boosting")

# GBM feature importance
gbm_imp <- summary(gbm_model, n.trees = best_iter, plotit = FALSE) %>%
  head(15)
ggplot(gbm_imp, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 15 Features — Gradient Boosting",
       x = "Feature", y = "Relative Influence (%)") +
  theme_minimal()

print(gbm_res$R2)


# ---------------------------------------------------------------
# 5.5  XGBOOST
# ---------------------------------------------------------------
cat("\n--- 5.5 XGBoost ---\n")
train_matrix <- model.matrix(tomatometer_rating ~ . - 1, data = train_data)
test_matrix  <- model.matrix(tomatometer_rating ~ . - 1, data = test_data)
dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$tomatometer_rating)
dtest  <- xgb.DMatrix(data = test_matrix,  label = test_data$tomatometer_rating)

xgb_params <- list(
  objective        = "reg:squarederror",
  eta              = 0.05,
  max_depth        = 6,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  eval_metric      = "rmse"
)

set.seed(123)
xgb_model <- xgb.train(
  params    = xgb_params,
  data      = dtrain,
  nrounds   = 2000,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30,
  verbose   = 0
)
xgb_pred <- predict(xgb_model, dtest)
xgb_res  <- evaluate_model(actual, xgb_pred, "XGBoost")




# ---------------------------------------------------------------
# 5.6  MODEL COMPARISON
# ---------------------------------------------------------------
all_results <- bind_rows(slr_res, mlr_res, rf_res, gbm_res, xgb_res) %>%
  arrange(RMSE)

cat("\n========== MODEL PERFORMANCE COMPARISON ==========\n")
print(all_results)

# RMSE chart
ggplot(all_results, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = RMSE), vjust = -0.4, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Model Comparison — RMSE (lower is better)",
       x = "", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

# R² chart
ggplot(all_results, aes(x = reorder(Model, R2), y = R2, fill = Model)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = R2), vjust = -0.4, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Model Comparison — R² (higher is better)",
       x = "", y = "R²") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

# Predicted vs Actual — all models
preds_df <- data.frame(
  actual                       = actual,
  `Simple Linear Regression`   = slr_pred,
  `Multiple Linear Regression` = mlr_pred,
  `Random Forest`              = rf_pred,
  `Gradient Boosting`          = gbm_pred,
  `XGBoost`                    = xgb_pred,
  check.names = FALSE
) %>%
  pivot_longer(-actual, names_to = "Model", values_to = "predicted")

ggplot(preds_df, aes(x = actual, y = predicted, color = Model)) +
  geom_point(alpha = 0.3, size = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black",
              linetype = "dashed") +
  facet_wrap(~ Model, ncol = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Predicted vs Actual — All Models",
       x = "Actual", y = "Predicted") +
  xlim(0, 100) + ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none")

# ================================================================
# 6. FINAL CONCLUSION
# ================================================================
best_model <- all_results %>% slice(1)
cat("\n========== FINAL CONCLUSION ==========\n")
cat("Best model:", best_model$Model, "\n")
cat("RMSE      :", best_model$RMSE, "\n")
cat("MAE       :", best_model$MAE,  "\n")
cat("R²        :", best_model$R2,   "\n")
cat("======================================\n")
