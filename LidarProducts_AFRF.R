## ---------------------------
##
## Script name: A suite of lidar products for Alex Fraser Research Forest
##
## Author: Skyler Li
##
## Date Created: 2024-03-10
##
## ---------------------------

##load 'lidR', 'terra', 'tidyverse' pacakges, install them first if you haven't
library(lidR)
library(terra)
library(tidyverse)

# set working directory to where the data was stored
setwd("C:/Users/skylerli/OneDrive/AFRF")
# create variable string of wd
wd = "C:/Users/skylerli.stu/OneDrive/AFRF"

# 1. Load the RGB Aerial Photo
# Load RGB image and explore structure
rgb_afrf <- rast('Aerial_Photo/AFRF_Aerial_Photo.tif')
# compactly display the structure of rgb_afrf object
str(rgb_afrf)
# plot the SpatRaster in RGB true colour
plotRGB(rgb_afrf) 

# 2. Read multiple .las files into LAScatalog object
#Create LAScatalog object from afrf las tiles
cat_afrf <- readLAScatalog("LAS")
#use las_check to perform a deep inspection of the LASCatalog object
las_check(cat_afrf) #the normalization has been done
# use summary() to provide succinct information
summary(cat_afrf)
# plot the overview of the location of tiles relative to each other
plot(cat_afrf)

# 3. Check duplication
# it seems no duplication
#Examine single .las tile to determine duplication, afrf_Tile1.las
tile_1_afrf <- readLAS("LAS/AFRF_Tile1.las")
# check afrf_Tile1.las tile 1 for more detailed diagnosis
las_check(tile_1_afrf)

# 4. Normalize catelog
#Create DEM based on the filtered catelog, 2m resolution
dem_allLAS_afrf <- rasterize_terrain(cat_afrf, 2, tin())
#Create color palette
col_1 <- height.colors(50) 

#Plot DEM using color palette
plot(dem_allLAS_afrf, col = col_1) #plot in 2D

#define LAScatalog engine options
opt_output_files(cat_afrf) <- paste(wd, "/Normalized LAS/norm_afrf_{ID}", sep = "")
#normalize all tiles in cat_afrf with the DEM 
norm_tiles_afrf <- normalize_height(cat_afrf, dem_allLAS_afrf)

#read normalized las into catalog to continue processing
norm_cat_afrf <- readLAScatalog("Normalized LAS")
#add LAScatalog enginge option to filter undersired data points
opt_filter(norm_cat_afrf) <- '-drop_z_below 0 -drop_z_above 55'
#ensure the entire study area was processed
plot(norm_cat_afrf)
# check the summary of the catelog after applying the filter of height
summary(norm_cat_afrf)

# 4. Produce CHM from normalized catalog
#Create CHM for all normalized afrf Tiles
chm_afrf <- rasterize_canopy(norm_cat_afrf, 2, p2r()) 
plot(chm_afrf, col = col_1) #plot in 2D

### Optional steps to create the plot metrics by ourselves
# 5. Plot extraction
#Extract multiple plots
#read 'Plot_Table.csv'
plot_table <- read.csv("Plots/Plot_Table.csv")

#define plot radius
radius <- 10

#for loop to extract multiple plots
for(i in 1:nrow(plot_table)){ #run the loop until i = the number of rows in 'plot_table' (38)
  plot_cent <- c(plot_table$X[i], plot_table$Y[i]) #extract plot center
  plot_las <- clip_circle(norm_cat_afrf, plot_cent[1], plot_cent[2], radius) #clip plot from norm_cat_las
  output_file <- paste("Plots/afrf_Plot_", i, ".las", sep = "") #output directory as string
  writeLAS(assign(paste("afrf_Plot_", i, sep = ""), plot_las), output_file) #write'afrf_Plot_i' to output dir.
}

# 6. Calculating Cloud Metrics
#Calculate cloud metrics for all plots
#create empty dataframe
afrf_plot_metrics <- data.frame() 

#For loop to calculate cloud metrics for all plots and add them to 'afrf_cloud_metrics'
for(i in 1:nrow(plot_table)){ #for loop == number of rows in plot_table (20)
  #read the plot we created in the Plots folder
  plot <- readLAS(paste("Plots/afrf_Plot_", i, ".las", sep= ""), filter = "-keep_first -drop_z_below 2")
  metrics <- cloud_metrics(plot, .stdmetrics) #compute standard metrics
  afrf_plot_metrics <- rbind(afrf_plot_metrics, metrics) #add the new 'metrics' to 'afrf_cloud_metrics'
}

#Export afrf_plot_metrics as .csv in Plots folder
write_csv(afrf_plot_metrics, "Plots/afrf_Plot_Metrics.csv")

# 7. Statistic models
#Read in relevant .csvs
afrf_plot_metrics <- read_csv("Plots/afrf_Plot_Metrics.csv")

#Add column to "afrf_plot_metrics' called Plot_ID (join key)
afrf_plot_metrics$Plot_ID = 1:38

#Join 'Plot_Table' and 'afrf_Plot_Metrics' into 'data_table'
data_table <- plot_table %>% 
  full_join(afrf_plot_metrics)

#7.1 Explore the relationship between lidar metrics and AGB and dominant tree height
#plot the relationship between zmax and total AGB
plot(Total_AGB ~ zmax, data = data_table)
# explore zq90 and dominant tree height
plot(Dominant_Height ~ zq90, data = data_table)

#7.2 Explore the correlation between our lidar metrics
#Create a matrix of scatterplots between less correlated variables
pairs(~ zq90 + zmax + pzabove2 +pzabovezmean + zentropy + zskew, data = data_table)

#7.3 MODEL DEVELOPMENT
#Forward Variable Selection using the F-test
#start with no variables in our models
modelAGB = lm(Total_AGB ~ 1, data = data_table)
model_dominant_height = lm(Dominant_Height ~ 1, data = data_table)

### 1st round checking parameters of the models
#Add each selected variable to our model one by one, to see which variable is the most significant
#predictor of AGB, Selected Variables zq90 + zmax + zentropy + zskew + pzabove2 +zkurt
add1(modelAGB,~  zq90 + zmax + zentropy + zskew + pzabove2 +zkurt, test = 'F') # f test
# Selected Variables for dominant height, zq90 + zkurt+ zpcum2 + pzabove2 + zentropy + zskew
add1(model_dominant_height,~ zq90 + zkurt+ zpcum2 + pzabove2 + zentropy + zskew, test = 'F')
#zmax was the most significant (lowest Pr(>F)), so we add it to our AGB model
modelAGB = lm(Total_AGB ~ zmax, data = data_table)
#zq90 was the most significant (lowest Pr(>F)), so we add it to our dominant height model
model_dominant_height = lm(Dominant_Height ~ zq90, data = data_table)

### 2nd round checking parameters of the models
#check each remaining variable to the new model one by one, to see 
#if any variable is a significant addition
add1(modelAGB,~  zq90 + zmax + zentropy + zskew +pzabove2 + zkurt, test = 'F') # f test
add1(model_dominant_height,~ zq90 + zkurt+ zpcum2 + pzabove2 + zentropy + zskew, test = 'F')
#zkurt was the most significant addition (lowest Pr(>F)), so we add it to AGB model
modelAGB = lm(Total_AGB ~ zmax + zkurt, data = data_table)
#zentropy was the most significant addition, so we add it to dominant tree height model
model_dominant_height = lm(Dominant_Height ~ zq90 + zentropy, data = data_table)

### 3rd round checking parameters of the models
#Again, we test all the remaining variables one by one
add1(modelAGB,~  zq90 + zmax + zentropy + zskew + pzabove2 +zkurt, test = 'F')
add1(model_dominant_height,~  zq90 + zkurt + zpcum2 + pzabove2 + zentropy + zskew , test = 'F')
#zentropy was the most significant addition, so we add it to AGB model
modelAGB = lm(Total_AGB ~ zmax+zkurt+zentropy, data = data_table)
#No additional variables were significant, so we can stop building dominant height model

### 4th round checking parameters of the models
add1(modelAGB,~  zq90 + zmax + zentropy + zskew + pzabove2 +zkurt, test = 'F')
#No additional variables were significant, so we can stop building our model

#Get the summary of the final models
summary(modelAGB)
summary(model_dominant_height)
#Plot our predicted AGB against our measured AGB
plot(Total_AGB ~ modelAGB$fitted,data = data_table,xlab = 'Predicted',ylab = 'Measured',
     main = "AGB Model")
abline(0,1) #Adds a one to one line
#Plot our predicted dominant tree height against our measured dominant tree height
plot(Dominant_Height ~ model_dominant_height$fitted,data = data_table,xlab = 'Predicted',
     ylab = 'Measured', main = "Dominant tree height model")
abline(0,1) #Adds a one to one line
#Get the coefficients to our models
modelAGB$coefficients
model_dominant_height$coefficients

#7.4 GRID METRICS
# Calculate grid_metrics for all afrf 
norm_cat_afrf <- readLAScatalog("Normalized LAS")
opt_filter(norm_cat_afrf) <- '-keep_z_above 0 -drop_z_above 55'
plot(norm_cat_afrf)
#Calculate grid metrics of mean Z at 20 m resolution for entire study area
# since the radius of the plot is about 10 m
pixel_metrics_afrf <- pixel_metrics(norm_cat_afrf, .stdmetrics_z, 20) 

#subset "zq90" from grid_metrics_afrf RasterBrick
zq90_afrf <- terra::subset(pixel_metrics_afrf, "zq90")
plot(zq90_afrf)
#subset "zkurt" from grid_metrics_afrf RasterBrick
zkurt_afrf <- terra::subset(pixel_metrics_afrf, "zkurt")
plot(zkurt_afrf)
#subset "zentropy" from grid_metrics_afrf RasterBrick
zentropy_afrf <- terra::subset(pixel_metrics_afrf, "zentropy")
plot(zentropy_afrf)
#subset "zmax" from grid_metrics_afrf RasterBrick
zmax_afrf <- terra::subset(pixel_metrics_afrf, "zmax")
plot(zmax_afrf)

# Create a histogram for the raster data
histogram_data_zkurt <- hist(zkurt_afrf, plot = FALSE)
plot(histogram_data_zkurt, main = "Raster Histogram", xlab = "Pixel Values", ylab = "Frequency")
# exclude extreme value in zkurt_afrf
clamped_zkurt_afrf <- clamp(zkurt_afrf, lower=0, upper=20, values=TRUE)

# merge the layers needed for AGB model into one raster
mergedAGB <- c(zmax_afrf, clamped_zkurt_afrf, zentropy_afrf)
# apply the function we find in model 1 for AGB
totalAGB_raster <- -526619.45 + 10898.48*mergedAGB[["zmax"]] + 
  29720.40*mergedAGB[["zkurt"]]+333369.05*mergedAGB[["zentropy"]]
# exclude extreme value of bio mass
total_AGB_clamped <- clamp(totalAGB_raster, lower=0, upper=700000, values=TRUE)
#plot the final Total AGB raster
plot(total_AGB_clamped)

# merge two raster zq90 and zentropy into one for dominant tree model
merged_dominant_height <- c(zq90_afrf, zentropy_afrf)

# apply the function we find in model 2 for dominant height
dominant_height <- -0.1700183 + 0.7589064*(merged_dominant_height[["zq90"]]) +
  12.5121359* (merged_dominant_height[["zentropy"]])
# plot the dominant tree height raster
plot(dominant_height)



