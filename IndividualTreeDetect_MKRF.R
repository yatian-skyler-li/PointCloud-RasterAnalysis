## ---------------------------
##
## Script name: Individual tree segmentation in Malcolm Knapp Research Forest
##
## Author: Skyler Li
##
## Date Created: 2024-03-10
##
## ---------------------------

# install packages, lidR, terra, tidyverse if they haven't been installed
library(lidR)
library(terra)
library(tidyverse)
library(rgl)

# set working directory where data was stored
setwd("C:/Users/skylerli.stu/Data")
# create variable string of wd
wd = "C:/Users/skylerli.stu/Data"

#1. Filter out the points below 0m and above 65m
# read normalized las into catalog to continue processing
norm_cat_mkrf <- readLAScatalog("Normalized")
# add LAScatalog enginge option to filter undersired data points
opt_filter(norm_cat_mkrf) <- '-drop_z_below 0 -drop_z_above 65'
# ensure the entire study area was processed
plot(norm_cat_mkrf)
# check the summary of the catelog after applying the filter of height
summary(norm_cat_mkrf)

#2. Extract multiple plots
# read 'Plot_Table.csv'
plot_table <- read.csv("Plots.csv")

# define plot radius
radius <- 154/2

# for loop to extract multiple plots
for(i in 1:nrow(plot_table)){ #run the loop until i = the number of rows in 'plot_table' (20)
  plot_cent <- c(plot_table$X[i], plot_table$Y[i]) #extract plot center
  plot_las <- clip_circle(norm_cat_mkrf, plot_cent[1], plot_cent[2], radius) #clip plot from norm_cat_las
  output_file <- paste("Plots/MKRF_Plot_", i, ".las", sep = "") #output directory as string
  writeLAS(assign(paste("MKRF_Plot_", i, sep = ""), plot_las), output_file) #write'MKRF_Plot_i' to output dir.
}

# see the raw data of these plots
plot(MKRF_Plot_1)
plot(MKRF_Plot_2)
plot(MKRF_Plot_3)
plot(MKRF_Plot_4)

lasFiles <- c(MKRF_Plot_1, MKRF_Plot_2, MKRF_Plot_3, MKRF_Plot_4)


for (lasFile in lasFiles) {
  x=1
  print(lasFile)
  li <- segment_trees(lasFile,li2012(dt1=1.5, dt2=2, R = 2, Zu=15, speed_up = 10, hmin = 2))
  plotli <- plot(li, color = "treeID")
}

#3. Produce CHM from normalized catalog
# Create CHM for Plot 2, 0.5m resolution, using pitfree algorithm
chm_plot2_0.5 <- rasterize_canopy(MKRF_Plot_2, 0.5, pitfree(thresholds = c(0,10,20,30), max_edge = c(0,1), subcircle = 0.2))
plot(chm_plot2_0.5, main = "Plot 2 CHM with 0.5 m resolution") #plot in 2D

# reapeat with other resolutions
dalpontePlot2 <- segment_trees(MKRF_Plot_2,
                               dalponte2016(chm_plot2_0.5,
                                            find_trees(chm_plot2_0.5, lmf(ws=5, hmin = 2, shape = "circular"))))
plot(dalpontePlot2, color = "treeID")

# for loop could be applied if multiple resolution CHM is required
# Create CHM for Plot 1, 1m resolution, using pitfree algorithm
chm_plot1_1 <- rasterize_canopy(MKRF_Plot_1, 1, pitfree(thresholds = c(0,10,20,30), max_edge = c(0,1), subcircle = 0.2))
plot(chm_plot1_1, main = "Plot 1 CHM with 1 m resolution") #plot in 2D
# determine tree tops for plot1 using lmf algorithm
plot1_treeTop <- find_trees(chm_plot1_1, lmf(ws=5, hmin = 2, shape = "circular"))
plot(plot1_treeTop, add = TRUE)
