# masters-thesis
R Code and Supplemental Material
# Master's Thesis: An Exploratory Analysis of Changes in Depressive Symptom Severity and Smartphone App Usage Patterns: Insights from the iCAN Pilot Study SMARDY

This repository contains the R code and supplementary materials for a psychology thesis analyzing data from the iCAN pilot study SMARDY.

[![DOI](https://zenodo.org/badge/953910362.svg)](https://doi.org/10.5281/zenodo.15077127)

## Repository Structure

```
main
 |- R
      |- Masterarbeit.Rproj      # R Project file
      |- renv.lock               # R environment dependencies
      |- figures                 # Generated figures
           |- phq_trajectory.svg
           |- corr_heatmap.svg
           |- phq_trajectory_legend.svg
      |- code                    # Analysis code
           |- all.R                 # Data integration script
           |- app_ranking.R         # App usage analysis
           |- BigData_hehe.R        # Raw data preparation
           |- boot.R                # Bootstrap analysis
           |- cluster.R             # Clustering methods
           |- corr.R                # Correlation analysis
           |- data_import.R         # Data import utilities
           |- data_processing.R     # Data processing utilities
           |- descriptive_stats.R   # Descriptive statistics
           |- lpa.R                 # Latent Profile Analysis
           |- shiny_app.R           # Interactive visualization app
           |- utils.R               # Utility functions
           |- visualization.R       # Static visualizations
      |- data                     # Data directory (currently empty)
           |- [empty]
 |- README.md                   # This file
```

## Project Description

This research investigates the relationship between digital mental health app usage, smartphone usage patterns, and mental health outcomes (primarily measured through PHQ-9 scores). The analysis includes longitudinal tracking of participants' PHQ scores, smartphone screen time, app usage across different categories, and user experience metrics.

## Data

**Note**: The `data` directory is currently empty. Data files will be added after resolving legal and ethical considerations regarding the publication of research data.

The analysis expects the following data files (to be placed in the `data` directory):
- `codebook.csv`: Participant information and study metadata
- `ema.csv`: Ecological Momentary Assessment data (including PHQ-9 scores)
- `screenstate.csv`: Screen time data
- `app_use.csv`: App usage statistics
- `sosci.csv`: Questionnaire data
- `ue.csv`: User experience metrics
- `modules.csv`: Intervention module usage
- `apps.csv`: App classification data

## Key Components

### Data Processing

- `data_import.R`: Functions for importing raw CSV data with appropriate type conversions
- `data_processing.R`: Data cleaning, transformation, and feature generation
- `all.R`: Integration of all data sources into a unified dataset

### Analysis

- `descriptive_stats.R`: Comprehensive framework for generating descriptive statistics
- `corr.R`: Correlation analysis and visualization
- `boot.R`: Bootstrap analysis for mixed models
- `lpa.R`: Latent Profile Analysis for app usage patterns
- `cluster.R`: Time series clustering for PHQ trajectories

### Visualization

- `visualization.R`: Static visualization of PHQ trajectories and other key metrics
- `shiny_app.R`: Interactive data exploration tool

## Interactive Visualization

The repository includes a Shiny application (`shiny_app.R`) that provides interactive visualization of:
- PHQ-9 trajectories over time
- App usage patterns
- Participant grouping and filtering
- Statistical distributions and outliers

To run the Shiny app:

```r
# From the R directory
source("code/shiny_app.R")
```

## Setup and Usage

### Prerequisites

- R (version 4.0.0 or higher recommended)
- RStudio (for Rproj file)

### Dependencies

This project uses `renv` for dependency management. To restore the required packages:

```r
# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Restore dependencies
renv::restore()
```

Main package dependencies include:
- dplyr, tidyr, tidyverse (data manipulation)
- ggplot2, plotly (visualization)
- lme4, lmerTest (mixed models)
- mclust, dtwclust (clustering)
- shiny, shinyWidgets (interactive app)
- lubridate (date/time handling)

### Running the Analysis

1. Clone the repository
2. Open the R project (`Masterarbeit.Rproj`)
3. Restore dependencies using `renv::restore()`
4. Add data files to the `data` directory
5. Run analyses by sourcing the relevant scripts

Example:

```r
# Load utility functions
source("code/utils.R")

# Process data
source("code/data_processing.R")

# Create integrated dataset
source("code/all.R")
```

## Figures

The `figures` directory contains key visualizations from the thesis:
- `phq_trajectory.svg`: PHQ-9 score trajectories over time
- `corr_heatmap.svg`: Correlation heatmap of key variables
- `phq_trajectory_legend.svg`: Legend for PHQ trajectory visualizations

## Contact

Jonas Culmsee
University of Greifswald
Institute of Psychology

## Acknowledgments
