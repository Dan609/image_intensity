# The analysis of  Laser scaning confocal images of ex vivo brain tumors
# Dan E. Bobkov, 2023
# Image analysis: get the RGB pixel intensities from folders with tif images
# Red - Mitochondria staining with TMRM
# Green - Hsp70 staining with Abs
################
# options(scipen=.1)
# par(mar = c(4,4,1,.5))
# Import libraries
#install.packages("BiocManager") 
#BiocManager::install("EBImage")
library(EBImage)
library(xlsx)
library(raster)
library(ggpubr)
library(readxl)
library(rJava)
library(xlsxjars)
library(DescTools)
library(ggplot2)
library(dplyr)
library(ggsignif)
library(PMCMRplus)
library(tidyverse)
library(EnvStats)
library(PerformanceAnalytics)
#setwd("~/Yandex.Disk.localized/....")
getwd()

# Choose dir function for mac <<<<<<<<<<<<<<<<<<
choose.dir <- function() {
  system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
         intern = FALSE, ignore.stderr = TRUE)
  p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  return(ifelse(length(p), p, NA))
}
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#######################################

########################################
######## SELECT FOLDERS WITH IMAGES ####
########################################

file_list_1 <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)

file_list_2 <- list.files(path = , choose.dir(),
                          pattern = "tif",
                          all.files = FALSE,
                          full.names = TRUE, recursive = TRUE,
                          ignore.case = FALSE, include.dirs = FALSE,
                          no.. = FALSE)

file_list_1
file_list_2


##### DATA FRAME with pixel intensities

df <- data.frame(name = character(),
                      zone = character(),
                      red_mean = integer(),
                      green_mean = integer(),
                      blue_mean = integer(),
                      red_median = integer(),
                      green_median = integer(),
                      blue_median = integer())


###### do and after FTD

# Start scan
start.time <- Sys.time()
for (file_name in file_list_1) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'MRH17111961', # average pixel values in
                    zone = 'Before', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Start scan
start.time <- Sys.time()
for (file_name in file_list_2) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'MRH17111961', # average pixel values in
                    zone = 'After', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
######################################################

#write.xlsx(df_mean,
#           file = 'df_MRH17111961.xlsx',
#           sheetName = 'RGB', append = FALSE)

write.csv(df,'df_MRH17111961.csv')


######################################################

shapiro.test(df$red_mean)
ks.test(df$red_mean, 'pnorm')

x <- df[df$zone == 'Before', ]$red_median
y <- df[df$zone == 'After', ]$red_median

x <- df[df$zone == 'Before', ]$red_mean
y <- df[df$zone == 'After', ]$red_mean

wilcox.test(x, y, alternative = "two.sided")

df$zone <- factor(df$zone, ordered = TRUE, 
                     levels = c("Before", "After"))

head(df)


# Plot weight by group and color by group

ggboxplot(df, x = "zone", y = "red_mean", 
          color = "zone", palette = c("#00AFBB", "#E7B800"),
          ylab = "Fluorescense intensity, a.u.", xlab = "Groups")

# Basic violin plot
p <- ggplot(df, aes(x=zone, y=red_mean)) + 
  geom_violin(trim=FALSE)
p + geom_dotplot(binaxis='y', stackdir='center',
                position=position_dodge(1))

# Data generation
# Plot correlation matrix



data <- df[, c(3:8)]

chart.Correlation(data, histogram = TRUE, method = "pearson")





# Use semi-transparent fill
p <- ggplot(df, aes(x=red_mean, fill=zone)) +
  geom_density(alpha=0.4) + scale_fill_brewer(palette="Accent")+
  labs(title="red_mean",x="TMRM level", y = "Density")
p
# Calculate the mean of each group
mu <- ddply(df, "zone", summarise, grp.mean=mean(red_mean))
head(mu)
# Add mean lines
p + geom_vline(data=mu, aes(xintercept=grp.mean, color=zone),
    linetype="dashed") + scale_color_brewer(palette="Accent") + theme_classic()


# Use semi-transparent fill
p <- ggplot(df, aes(x=red_median, fill=zone)) +
  geom_density(alpha=0.4) + scale_fill_brewer(palette="Accent")+
  labs(title="red_median",x="TMRM level", y = "Density")
p
# Calculate the mean of each group
mu <- ddply(df, "zone", summarise, grp.mean=mean(red_median))
head(mu)
# Add mean lines
p + geom_vline(data=mu, aes(xintercept=grp.mean, color=zone),
    linetype="dashed") + scale_color_brewer(palette="Accent") + theme_classic()


