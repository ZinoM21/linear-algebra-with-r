# Assignment 1



# ------------------------
# 1.2 Use R and the updated Darwin and Tahiti standardized SLP data to repro-
# duce the EOFs and PCs and to plot the EOF pattern maps and PC time series.

# Load data & keep years as row names
darwin_stand <- read.table("data/PSTANDdarwin.txt",
  header = FALSE, row.names = 1
)
years <- as.numeric(rownames(darwin_stand)) # save years for later

# Choose dec monthly data
darwin_dec <- t(darwin_stand[, 12])
colnames(darwin_dec) <- rownames(darwin_stand[, 12, drop = FALSE])

# Same for tahiti
tahiti_stand <- read.table("data/PSTANDtahiti.txt",
  header = FALSE, row.names = 1
)
tahiti_dec <- t(tahiti_stand[, 12])
colnames(tahiti_dec) <- rownames(tahiti_stand[, 12, drop = FALSE])

# Create a 2 by 65 space-time matrix from darwin and tahiti deceember data
da_ta <- rbind(darwin_dec, tahiti_dec)
dim(da_ta)
da_ta

# Calculate the SVD
da_ta_svd <- svd(da_ta)
U <- da_ta_svd$u
V <- t(da_ta_svd$v)

### Plot the EOF pattern maps
eof1 <- U[1, ]
eof2 <- U[2, ]

library(maps)
library(mapdata)

# Longitude and latitude for Darwin and Tahiti
locations <- data.frame(
  name = c("Darwin", "Tahiti"),
  lon = c(130.84, 210.58),
  lat = c(-12.46, -17.65)
)

# Initiate plot
plot.new()
par(mfrow = c(2, 1))
par(mar = c(0, 0, 0, 0)) # Zero space between (a) and (b)

# Mode 1 / EOF 1
map(database = "world2Hires", ylim = c(-70, 70), mar = c(0, 0, 0, 0))
grid(nx = 12, ny = 6)
points(locations$lon, locations$lat,
  pch = 19, col = c("blue", "red"), cex = 1.5
)
text(locations$lon, locations$lat,
  labels = paste0(locations$name, " ", round(eof1, 2)),
  col = "blue", pos = 1, cex = 1
)
axis(2,
  at = seq(-70, 70, 20),
  col.axis = "black", tck = -0.05, las = 2, line = -0.9, lwd = 0
)
axis(1,
  at = seq(0, 360, 60),
  col.axis = "black", tck = -0.05, las = 1, line = -0.9, lwd = 0
)
text(180, -50, "SLP Anomalies Darwin and Tahiti Mode 1",
  col = "purple", cex = 1.3
)
box()


# Mode 2 / EOF 2
map(database = "world2Hires", ylim = c(-70, 70), mar = c(0, 0, 0, 0))
grid(nx = 12, ny = 6)
points(locations$lon, locations$lat,
  pch = 19, col = c("blue", "red"), cex = 1.5
)
text(locations$lon, locations$lat,
  labels = paste0(locations$name, " ", round(eof2, 2)),
  col = "blue", pos = 1, cex = 1
)
axis(2,
  at = seq(-70, 70, 20),
  col.axis = "black", tck = -0.05, las = 2, line = -0.9, lwd = 0
)
axis(1,
  at = seq(0, 360, 60),
  col.axis = "black", tck = -0.05, las = 1, line = -0.9, lwd = 0
)
text(180, -50, "SLP Anomalies Darwin and Tahiti Mode 1",
  col = "purple", cex = 1.3
)
box()

dev.off()


### Plot the PC time series with matrix V
# png("images/pc-time-series-1-2.png", width = 800, height = 600)
plot(years, V[1, ],
  type = "l", col = "red",
  xlab = "Year", ylab = "PC", main = "PC Time Series"
)
lines(years, V[2, ], type = "l", col = "blue")
legend("topright", legend = c("PC1", "PC2"), col = c("red", "blue"), lty = 1)

dev.off()


# ------------------------
# 1.8
# a)
precip_data <- read.csv("data/3447060.csv", header = TRUE)

# Create the pivot table with stations as rows, years as columns
station_year_df <- xtabs(PRCP ~ STATION + DATE, data = precip_data)

# Anomaly data X (with respect to mean), and Y columns for time in years
mean_precip <- rowMeans(station_year_df)
X <- sweep(station_year_df, 1, mean_precip)
Y <- ncol(X)

# covariance matrix formula: C = X * t(X) / Y
cov_x <- (X %*% t(X)) / Y
cov_x

# b)
inv_cov_x <- solve(cov_x)
inv_cov_x

# c)
eig_cov_x <- eigen(cov_x)
eig_cov_x$values
eig_cov_x$vectors

# d)
x_svd <- svd(X)
x_svd$u
x_svd$d
x_svd$v

# e)
eig_cov_x$values
x_svd$d
x_svd$d^2 / Y
# -> the eigenvalues of the covariance matrix are
# the squares of the singular values of the data matrix,
# divided by the number of columns

# f)
eig_cov_x$vectors
x_svd$u
# -> The eigenvectors of the covariance matrix are the same
# as the SVD spatial modes of X

# g) Plot the PC time series and describe their behavior.
V_t <- t(x_svd$v)
V_t

png("images/pc-time-series.png", width = 800, height = 600)
plot(1:Y, V_t[1, ], type = "l", col = "red", xlab = "Year", ylab = "PC", main = "PC Time Series")
lines(1:Y, V_t[2, ], type = "l", col = "blue")
lines(1:Y, V_t[3, ], type = "l", col = "green")
legend("topright", legend = c("1", "2", "3"), col = c("blue", "red", "green"), lty = 1)
dev.off()

# We see that lines one and three are very similar at the end (falling near
# year 4 and rising for year 5).
# Also, lines two and three are similar at the beginning (rising for year 1-2
# and falling after year 3).
# This describes / looks like an oscillation pattern, where all the lines
# rise and fall from -0.6 to 0.5. We just see an excerpt from this
# larger pattern.
# I would guess that one oscillation cycle is roughly 6-8 years long.



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
matrix_2_2 <- matrix(
  c(
    1.7, -0.7, 1.3,
    -1.6, -1.4, 0.4,
    -1.5, -0.3, 0.6
  ),
  nrow = 3,
  byrow = TRUE
)

inverse_matrix <- solve(matrix_2_2)
print(inverse_matrix)


# ---------------
# 2.3 Write a computer code to solve the following linear system of
# equations Ax = b, where A is a 3x3 matrix and b is a 3x1 vector:

A <- matrix(
  c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 0
  ),
  nrow = 3, byrow = TRUE
)
b <- c(1, -1, 0)
x <- solve(A, b)
print(x)

# --------------
# 2.7 b)

A <- matrix(c(0, 4, -2, -7), nrow = 2, byrow = TRUE)

# eigenvalues & vectors:
eig <- eigen(A)
eigenvalues <- eig$values
eigenvectors <- eig$vectors

# Get unit eigenvectors by normalizing (divide by magnitude)
unit_eigenvectors <- apply(eigenvectors, 2, function(v) v / sqrt(sum(v^2)))
unit_eigenvectors
