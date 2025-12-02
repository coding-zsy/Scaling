Urban Scaling Law of Crimes
Overview
This repository contains the R code and data used for analyzing the scaling relationships between crime incidence and urban population size. The project investigates whether different categories of crimes follow superlinear, linear, or sublinear scaling patterns across cities.
The analysis is implemented in a single R script (Crime_scaling_code.R) and uses crime datasets stored in the Crime_data/ directory.
Repository Structure
├── Crime_scaling_code.R     # Main R script for data preprocessing, analysis, and visualization
├── Crime_data/              # Crime and population datasets used in the analysis
│     ├── ...                # CSV/Shapefile files
└── README.md                # Project documentation
How to Run the Code
1. Install Required R Packages
The script depends on the following R packages:
install.packages(c(
  "ggplot2", "dplyr", "tidyr", "stringr",
  "openxlsx", "ggpubr", "patchwork",
  "sf", "ggbreak", "ggforce", "ggpmisc"
))
2. Set Working Directory
Update the working directory to match your local environment:
setwd("path/to/your/project")
(The script currently uses: C:/Users/13593/Desktop/crime_scaling)
3. Run the Script
Simply execute:
source("Crime_scaling_code.R")
This will run the entire workflow, including:
•	Data loading
•	Data cleaning and transformation
•	Scaling-law regression
•	Visualization of crime–population scaling patterns
Output
The script generates:
•	Comparison of the frequency distribution of different crime categories
•	Scatterplots of crime counts vs. population (log–log scale)
•	Temporal variation of scaling exponents for each crime category
•	Comparative visualizations using patchwork and ggpubr
•	Verification of the constructed crime model
These outputs help evaluate how different crime categories scale with city size.
Data Description
All datasets required for the analysis are stored in the Crime_data/ folder.
These may include:
•	Crime incident counts by city
•	Population data
•	Spatial data (e.g., shapefiles)
The raw data files are not publicly distributed here unless explicitly permitted.
Notes
•	R version recommended: 4.2 or later
•	Ensure all data filenames inside Crime_data/ match the paths used in the script
•	For reproducibility, avoid hard-setting the working directory inside the script 
