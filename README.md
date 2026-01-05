# EFA-and-K-Means-Clustering
This repository contains the R source code required to replicate the statistical analysis presented in the research paper: **"Exploring Attitudinal Cluster Behaviour for EV-enabled Actions During Emergencies"**.

## Overview

This project utilizes survey data to identify distinct behavioral segments among residents regarding their preparedness, risk perception, and compassion during power outages. The analysis pipeline performs **Exploratory Factor Analysis (EFA)** to identify latent constructs, followed by **K-Means Clustering** to profile respondent personas.

## Methodology

The analysis proceeds in the following stages:

1.  **Data Preprocessing**: Conversion of Likert-scale responses to numeric vectors and data quality checks.
2.  **Exploratory Factor Analysis (EFA)**:
    * **Extraction Method**: Principal Axis Factoring (`pa`).
    * **Rotation**: Varimax (orthogonal).
    * **Selection Criteria**: Parallel Analysis and Scree Plot inspection.
3.  **Cluster Analysis**:
    * **Method**: K-Means Clustering on extracted factor scores.
    * **Validation**: Hierarchical dendrograms for visual inspection and Bootstrap Stability Analysis (Jaccard Index) to ensure robust cluster assignments.
4.  **Demographic Profiling**: Statistical comparison of clusters against demographic variables using `TableOne`.

## Dependencies

The analysis was performed using **R (Version 4.x.x)**. The script utilizes the `pacman` package manager to automatically install and load the required libraries.

The core dependencies are:
* `tidyverse`: Data manipulation and visualization (ggplot2).
* `psych`: Psychometric analysis and factor analysis.
* `factoextra`: Visualization of clustering results.
* `fpc`: Flexible procedures for clustering (Bootstrapping).
* `NbClust`: Determining the optimal number of clusters.
* `tableone`: Creating "Table 1" summary statistics.
