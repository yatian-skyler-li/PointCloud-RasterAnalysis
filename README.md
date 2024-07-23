# PointCloud-RasterAnalysis
This repository includes projects using lidr, terra, tidyverse packages in R for Light Detection and Ranging (lidar) and Raster data analysis under the forest management scenario. The original design of these project are from Prof. Nicholas Coops (nicholas.coops@ubc.ca) and Liam Irwin (lakirwin@ubc.ca). The scripts are developed by Skyler Li.

# 1. A suite of lidar products for Alex Fraser Research Forest (AFRF)
- Dataset
  - 24 .las tiles for the AFRF were used (under AFRFLAS folder). These data were collected in 2008 with a discrete return sensor. The point density is approximately 3 returns / m2. Fixed-radius plot data were collected between 1997-2010 across our study area, with an approximate radius of 10 m for each plot. Individual tree measurements were made, allowing for the calculation of total above ground biomass (AGB), dominant tree height, and other attributes of interest.
  - The table Plot_Table.csv contains plot ids, plot locations (X and Y), and estimates of our two attributes: total AGB (kg/ha) and dominant tree height (m).
  - The RGB Aerial Photo of AFRF is also avaible for reference.

- Main steps
  - Digital Elevation Model (DEM) and Canopy Height Model (CHM) were developed
  - Extract point clouds for plot locations listed in Plot_Table.csv and calculate metrics for each plot
  - Develop statistical models between lidar metrics, toal AGB and dominant tree height

# 2. Individual tree detection using lidR
- Dataset
  - 20 .las tiles for Malcolm Knapp Research Forest (MKRF) under MKRFLAS folder were used. The lidar data was collected over the Malcolm Knapp Research Forest (MKRF) in Maple Ridge, BC. This data was collected in May 2010 by a discrete return sensor with an approximate point density of 3.1 returns / m2.
  - MKRF_Plots.csv shows four plot locations for this project.

- Main steps
  - Extract point clouds for each plot location and remove outliers below 0 m and above 65 m
  - Detect individual trees using a point cloud with segment_trees function and Li et al., 2012 algorithm
  - Detect individual trees using a CHM. Dalponte and Coomes 2016 algorithm was applied.
