🎬 Do Movies That Resonate with Audiences Also Earn Stronger Critical Reception?
---
📌 Project Overview
This project investigates whether movies that resonate with general audiences also tend to earn stronger critical reception. Using the Rotten Tomatoes Movies and Critic Reviews Dataset, we apply descriptive, inferential, and predictive analytics to statistically evaluate the relationship between audience ratings and Tomatometer (critic) ratings.
Research Question:  
Are higher fan ratings statistically associated with higher critic ratings?
---

📊 Dataset
Source: Rotten Tomatoes Movies and Critic Reviews Dataset (Kaggle)
Property	Value
Type	Secondary dataset
Raw records	17,713
Fields	22
Records after cleaning	15,165 (14.4% removed)
---
🔧 Setup Instructions
Step 1 — Install R and RStudio
Download and install:
R (≥ 4.0)
RStudio
Step 2 — Clone the Repository
```bash
git clone https://github.com/<your-username>/rotten-tomatoes-analysis.git
cd rotten-tomatoes-analysis
```
Step 3 — Update the File Path in the Script
Open `final_script.R` and update these two lines at the top to match your local path:
```r
# Line 6 — update setwd() to your project folder
setwd("path/to/rotten-tomatoes-analysis")

# Line 32 — update the CSV path
movies_raw <- read.csv("data/rotten_tomatoes_movies.csv", stringsAsFactors = FALSE)
```
Step 4 — Install Required Packages
The script auto-installs missing packages on first run. You can also install them manually in R:
```r
install.packages(c(
  "tidyverse", "stringr", "lubridate", "caret",
  "randomForest", "gbm", "xgboost", "ggplot2",
  "scales", "gridExtra", "corrplot"
))
```
Step 5 — Run the Script
Open `final_script.R` in RStudio and press `Ctrl+Shift+Enter` (Windows) or `Cmd+Shift+Enter` (Mac) to run the full analysis.
---
🧪 Analysis Pipeline
The script is structured into 5 sections:
1. Data Loading
Loads the raw CSV and selects 9 relevant columns: `movie_title`, `tomatometer_rating`, `audience_rating`, `genres`, `directors`, `actors`, `content_rating`, `runtime`, `original_release_date`.
2. Data Preprocessing
Removed rows with missing `tomatometer_rating` or `audience_rating`
Validated ratings are within 0–100 range
Removed duplicate movie titles
Cleaned and imputed `runtime` (median imputation; filtered 30–240 min range)
Parsed `original_release_date` into `release_year` and `movie_age`
Standardised `genres`, `directors`, `actors`, and `content_rating` text fields
Removed runtime outliers using IQR method
3. Descriptive Analysis
Produces the following visualisations:
Summary statistics for Tomatometer and Audience ratings
Histograms of both rating distributions
Boxplot comparing audience vs critic ratings
Tomatometer rating by content rating
Movies released per year (bar chart)
Average Tomatometer score over time (trend line)
Correlation heatmap of numeric features
Top 10 genres by audience vs critic ratings
Top 10 directors by average Tomatometer score
Modern directors: audience vs critic comparison
4. Inferential Analysis
Normality test: Shapiro-Wilk — both variables rejected normality
Correlation test: Spearman's rank correlation (non-parametric)
ρ = 0.667, p < 2.2e-16 → H₀ rejected
Interpretation: significant positive monotonic relationship between audience and critic ratings
5. Predictive Modelling
Five models trained on an 80/20 train-test split:
Model	RMSE	MAE	R²
Simple Linear Regression	21.384	17.096	0.432
Multiple Linear Regression	14.047	10.669	0.755
Random Forest	13.541	10.024	0.778
Gradient Boosting	12.917	8.936	0.792
XGBoost ⭐	12.834	8.748	0.795
Best model: XGBoost — lowest RMSE, lowest MAE, highest R².  
Top predictive features: `director_avg_score`, `audience_rating`, `director_popularity`.
---
📈 Key Findings
Audience and critic ratings share a moderate-to-strong positive relationship (Spearman ρ = 0.667)
Critics are more polarising than audiences (SD: 28.46 vs 20.39), though both groups average around 60
Documentary, Manga, and Anime genres score highest with both audiences and critics
Average Tomatometer scores have declined over decades as more films are released each year
Director reputation is the strongest predictor of critic ratings in the XGBoost model
---
✅ Conclusion
The statistical evidence supports the hypothesis:
> *Content that aligns with audience preferences receives better critical feedback.*
However, critic ratings are also significantly influenced by director quality, genre, runtime, and content rating — audience preference alone does not fully explain critic scores.
---
📎 Presentation
The full project presentation is available in `presentation/TPSM__2_.pdf`.
---
