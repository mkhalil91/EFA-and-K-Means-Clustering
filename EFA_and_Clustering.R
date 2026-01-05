# -----------------------------------------------------------------------------
# R SCRIPT: EXPLORATORY FACTOR ANALYSIS & K-MEANS CLUSTERING
# -----------------------------------------------------------------------------
# Description:
# This script performs data cleaning, Exploratory Factor Analysis (EFA),
# and K-Means clustering on survey data.
#
# Dependencies:
# psych, factoextra, NbClust, tidyverse, readr, readxl, corrplot, fpc, tableone
# -----------------------------------------------------------------------------

# --- 1. SETUP: LOAD REQUIRED LIBRARIES ---
if(!require(pacman)) install.packages("pacman")
pacman::p_load(psych, factoextra, NbClust, tidyverse, readr, readxl, 
               knitr, corrplot, fpc, tableone)

# --- 2. DATA LOADING AND PREPARATION ---

# A. Load dataset
# NOTE: Update file path to local directory before running
raw_data <- read_excel("path/to/your/data_file.xlsx")

# B. Variable Selection
# Variables selected based on theoretical relevance to risk, compassion, 
# and power outage actions.
efa_vars_names <- c(
  "Power_Outage_Actions_Evacuate",
  "Power_Outage_Actions_Travel",
  "Power_Outage_Actions_Bring_Supplies",
  "Power_Outage_Actions_Travel_to_Charge_EV",
  "Compassion_to_Strangers",
  "Compassion_to_All",
  "Helping_Others",
  "Power_Outage_Actions_Lights",
  "Power_Outage_Actions_Fridge",
  "Power_Outage_Actions_Cooling_System",
  "Power_Outage_Actions_Heating_System",
  "Power_Outage_Actions_Power_Medical_Equipment",
  "Power_Outage_Actions_Charge_Medical_Mobility_Equipment",
  "Power_Outage_Actions_Sharing_Electricity"
)

selected_data <- raw_data %>% select(all_of(efa_vars_names))

# C. Recode Categorical Responses to Numeric
# Converting Likert-scale responses to a centered numeric scale (-2 to +2)
data_numeric <- selected_data %>%
  mutate(across(everything(), ~ case_when(
    # Agreement scales
    . %in% c("Not true at all", "Not all true", "Greatly decrease", "Very unlikely") ~ -2,
    . %in% c("Somewhat untrue", "Somewhat decrease", "Somewhat unlikely") ~ -1,
    . %in% c("Neither true nor untrue", "Neither increase nor decrease", "Neither likely nor unlikely") ~ 0,
    . %in% c("Somewhat true", "Somewhat increase", "Somewhat likely") ~ 1,
    . %in% c("Very true", "Greatly increase", "Very likely") ~ 2,
    TRUE ~ NA_real_
  )))

# Remove rows with missing values to ensure valid EFA
# Note: This may reduce sample size.
data_numeric <- na.omit(data_numeric)


# --- 3. PRELIMINARY ANALYSIS ---

# Correlation Matrix Visualization
# Visual check for factorability (looking for correlations > 0.3)
cor_matrix <- cor(data_numeric, use = "pairwise.complete.obs")

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.5,
         tl.cex = 0.5,
         title = "Correlation Matrix of EFA Variables",
         mar = c(0,0,2,0))


# --- 4. EXPLORATORY FACTOR ANALYSIS (EFA) ---

# A. Suitability Tests
print("--- EFA Suitability Tests ---")
print(KMO(data_numeric))
print(cortest.bartlett(cor(data_numeric), n = nrow(data_numeric)))

# B. Determine Number of Factors (Parallel Analysis)
fa.parallel(data_numeric, fa = "fa", fm = "pa", main = "Parallel Analysis Scree Plot")

# C. Model Execution
# Based on parallel analysis, extracting 3 factors using Varimax rotation
# NOTE: Adjust 'nfactors' based on the scree plot results
n_factors <- 3 
efa_model <- fa(r = data_numeric, nfactors = n_factors, rotate = "varimax", fm = "pa")

print("--- EFA Factor Loadings ---")
print(efa_model$loadings, cutoff = 0.4, sort = TRUE)

# D. Extract Factor Scores
factor_scores <- as.data.frame(efa_model$scores)
# Optional: Rename columns based on interpretation
# names(factor_scores) <- c("Factor1_Label", "Factor2_Label", "Factor3_Label")


# --- 5. CLUSTER ANALYSIS (K-MEANS) ---

# A. Cluster Verification (Hierarchical Dendrogram)
# Used to visualize natural groupings and validate the choice of K
dist_matrix <- dist(factor_scores, method = "euclidean")
hc_model <- hclust(dist_matrix, method = "ward.D2")
fviz_dend(hc_model, k = 4, main = "Dendrogram for Cluster Count Verification")

# B. Cluster Stability Check (Bootstrapping)
# Validating stability of 4 clusters using Jaccard Index (100 resamples)
set.seed(123)
k_to_test <- 4
cboot_result <- clusterboot(factor_scores, B = 100, bootmethod = "boot",
                            clustermethod = kmeansCBI, krange = k_to_test, seed = 123, count = FALSE)

print("--- Cluster Stability (Jaccard Index) ---")
print(data.frame(Cluster = 1:k_to_test, Stability = round(cboot_result$bootmean, 3)))

# C. Final K-Means Model
set.seed(123)
kmeans_model <- kmeans(factor_scores, centers = k_to_test, nstart = 50)

# D. Merge Clusters back to Data
final_data <- data_numeric %>%
  bind_cols(factor_scores) %>%
  mutate(cluster = as.factor(kmeans_model$cluster))


# --- 6. DEMOGRAPHIC PROFILING ---

# A. Prepare Demographic Data
demog_data <- read_excel("path/to/your/demographics_file.xlsx")

# IMPORTANT: Ensure row alignment.
# Because we ran na.omit() in Step 2C, we must ensure the demographic file
# matches the rows remaining in 'final_data'.
# Ideally, merge by a unique ID. If relying on row order, ensure NAs are handled similarly.

# Simple row binding (Assumes demographic file corresponds exactly to the analyzed data)
# If NAs were dropped in Step 2, you may need to filter 'demog_data' to match.
demog_clean <- demog_data %>%
  # Example alignment if IDs exist: filter(ID %in% rownames(final_data))
  mutate(Cluster = final_data$cluster)

# B. Define Variables for TableOne
numeric_features <- c('Age', 'HH Size', 'Number of Children under 18', 
                      'Number of Seniors above 65', 'Number of HH Member with Disability')

categorical_features <- c('Gender', 'Disability?', 'HH Income in 2022', 
                          'Highest Level of Education', 'Current Employment Status', 
                          'Marital Status', 'Do you own your residence?')

# C. Generate Statistical Table
tab1 <- CreateTableOne(vars = c(numeric_features, categorical_features),
                       strata = "Cluster",
                       data = demog_clean,
                       test = TRUE)

print("--- Demographic Comparison by Cluster ---")
print(tab1, smd = TRUE, showAllLevels = FALSE)


# --- 7. CLUSTER VISUALIZATION ---

# Plot Mean Factor Scores per Cluster
cluster_profile <- final_data %>%
  group_by(cluster) %>%
  summarise(across(all_of(names(factor_scores)), mean)) %>%
  pivot_longer(cols = -cluster, names_to = "factor", values_to = "mean_score")

ggplot(cluster_profile, aes(x = factor, y = mean_score, fill = factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "Cluster Profiles based on Factor Scores", y = "Mean Score", x = "Latent Factor")