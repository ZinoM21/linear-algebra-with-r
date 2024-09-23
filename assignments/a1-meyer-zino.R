# Assignment 1


# 1.2 Use R and the updated Darwin and Tahiti standardized SLP data to repro-
# duce the EOFs and PCs and to plot the EOF pattern maps and PC time series.

darwin_data <- read.table("data/PSTANDdarwin.txt", header = FALSE)
tahiti_data <- read.table("data/PSTANDtahiti.txt", header = FALSE)

darwin_data
