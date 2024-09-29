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
# Select San Diego, Berlin, Tokyo, Auckland
global_data <- data_2_1[c(1777, 2019, 1828, 755), ]

# Extract December data columns (every 12th column & without first 3 columns)
dec_cols <- seq(15, ncol(global_data), by = 12)

# Get december data only
dec_data <- global_data[, dec_cols]

# Extract years 2000-2008
data_2000_2008 <- dec_data[121:129]

# Set row and column names
row.names(data_2000_2008) <- c("San Diego", "Berlin", "Tokyo", "Auckland")
colnames(data_2000_2008) <- 2000:2008
data_2000_2008


# ------------------------
# 2.2 Write a computer code to find the inverse of the following matrix.
matrix_2_2 <- matrix(c(1.7, -0.7, 1.3,
                       -1.6, -1.4, 0.4,
                       -1.5, -0.3, 0.6),
                     nrow = 3,
                     byrow = TRUE)

inverse_matrix <- solve(matrix_2_2)
print(inverse_matrix)


# ---------------
# 2.3 Write a computer code to solve the following linear system of
# equations Ax = b, where A is a 3x3 matrix and b is a 3x1 vector:

A <- matrix(c(1, 2, 3,
              4, 5, 6,
              7, 8, 0),
            nrow = 3, byrow = TRUE)
b <- c(1, -1, 0)
x <- solve(A, b)
print(x)

