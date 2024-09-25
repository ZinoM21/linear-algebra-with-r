# Assignment 1


# 1.2 Use R and the updated Darwin and Tahiti standardized SLP data to repro-
# duce the EOFs and PCs and to plot the EOF pattern maps and PC time series.

darwin_data <- read.table("data/PSTANDdarwin.txt", header = FALSE)
tahiti_data <- read.table("data/PSTANDtahiti.txt", header = FALSE)
# ------------------------
# 2.1 Write a computer code to
# a) Read the NOAAGlobalTemp data file, and
# b) Generate a 4 × 8 space-time data matrix for the December mean
# surface air temperature anomaly data of four grid boxes and eight years.

# a)
data_2_1 <- read.csv("data/NOAAGlobalT.csv", header = TRUE)
dim(data_2_1)

# b)
sd_data <- data_2_1[1777, ] # 1777 is the row number for San Diego data
# Note: use c(1777, 2000, 2100, 2200) to select 4 different locations

dec_cols <- seq(15, ncol(sd_data), by = 12) # remove cols 1-3: i LAT LON
sd_dec <- sd_data[, dec_cols]

sd_dec_2000_2008 <- sd_dec[121:129] # choosing years 2000-2008

row.names(sd_dec_2000_2008) <- c("San Diego")
# use c("San Diego", "Location2", "Location3", "Location4") to name all rows
colnames(sd_dec_2000_2008) <- 2000:2008
sd_dec_2000_2008

darwin_data
