# 1
anomaly_data <- read.csv("data/NOAAGlobalT.csv", header = TRUE)
dim(anomaly_data)
# [1] 2592 1648

# Define cities and years for later use
cities <- c("San Francisco", "Chicago", "London", "Tokyo")
years <- 1991:1996

# Select 4 rows for the cities
cities_rows <- anomaly_data[c(1920, 1927, 2088, 1828), ]

# Extract june data columns (every 12th column starting with the 9th)
june_cols <- seq(9, ncol(cities_rows), by = 12)

# Get june data only
june_data <- cities_rows[, june_cols]

# Extract years 91-96
W <- june_data[112:117]

# Set row and column names
row.names(W) <- cities
colnames(W) <- years

print(data.frame(W))
#                 X1991   X1992   X1993   X1994   X1995   X1996
# San Francisco -1.5714  1.7334 -0.4119 -0.1086 -0.5146  0.0023
# Chicago        2.0285 -1.4971 -1.0721  0.4447  1.3542 -0.4503
# London        -1.2829  1.2994  0.4849 -0.0177  0.3695  0.0444
# Tokyo          1.0840 -0.1687 -0.4509  0.2126 -0.4990 -0.1933


# 2
# a)

par(mar = c(3, 7, 3, 2)) # margins
image(t(W), col = heat.colors(64), axes = FALSE)

axis(1, at = seq(0, 1, length.out = 6), labels = years)
axis(2, at = seq(0, 1, length.out = 4), labels = cities, las = 2)

title(main = "Visualization of Temperature Data Matrix by Zino Meyer")
box()

dev.off()


# b)
W_svd <- svd(W)

U <- W_svd$u
U
# [,1]       [,2]       [,3]        [,4]
# [1,]  0.5309902 -0.2935757  0.7139986 -0.34938327
# [2,] -0.7094970 -0.6649731  0.2261012 -0.05747304
# [3,]  0.4113356 -0.5676375 -0.2050683  0.68303562
# [4,] -0.2132287  0.3865382  0.6301041  0.63881963

D <- W_svd$d
D
# [1] 4.206324 1.308161 1.219229 0.464540

V <- W_svd$v
V
# [,1]       [,2]       [,3]       [,4]
# [1,] -0.72092763  0.1984898  0.2319366  0.5352626
# [2,]  0.60695935 -0.2416753  0.4317358  0.5601015
# [3,]  0.19911408  0.2937743 -0.7546169  0.5353425
# [4,] -0.10122661 -0.1311811  0.1317200  0.2929954
# [5,] -0.23195046 -0.8806695 -0.3702604  0.0765775
# [6,]  0.09038491  0.1520006 -0.1895258 -0.1465547


# c)
library(maps)
library(mapdata)

EOF1 <- U[1, ]

# Combine data for easy access
locations <- data.frame(
    name = c("San Francisco", "Chicago", "London", "Tokyo"),
    lon = c(-122.4194, -87.6298, -0.1276, 139.6917),
    lat = c(37.7749, 41.8781, 51.5074, 35.6895),
    EOF_value = EOF1,
    pos = c(2, 1, 2, 4),
    offset = c(0.5, 0.7, 2, 0.6)
)

# Set color dependant on value
locations$cols <- ifelse(locations$EOF_value > 0, "red", "blue")

# Plot Mode 1
plot.new()
par(mar = c(0, 0, 0, 0))

map(database = "worldHires", ylim = c(-70, 70), mar = c(2, 2, 2, 2))
grid(nx = 12, ny = 6)

points(locations$lon, locations$lat,
    pch = 19,
    col = locations$cols,
    cex = (abs(locations$EOF_value) * 2)
    # point size is the absolute EOF value respectively (scaled by 2)
)

text(locations$lon, locations$lat,
    labels = paste(locations$name, round(locations$EOF_value, 2)),
    col = locations$cols,
    pos = locations$pos,
    offset = locations$offset,
    cex = 0.8
)

axis(1,
    at = seq(-180, 180, 60), col.axis = "black",
    tck = -0.05, las = 1, line = -0.9, lwd = 0
)
axis(2,
    at = seq(-70, 70, 20), col.axis = "black",
    tck = -0.05, las = 2, line = -0.9, lwd = 0
)

text(40, -55,
    "Surface Temperature Anomalies Mode 1",
    col = "purple", cex = 1.3
)
box()

dev.off()


# 3
# a)
et_data <- read.csv("data/EarthTemperatureData.csv", header = TRUE)
dim(et_data)
# [1] 166  14

monthly_data <- et_data[, 2:13]
monthly_seq <- c(t(monthly_data))
monthly_seq[1:7] # first seven elements in the array
# [1] -0.702 -0.284 -0.732 -0.570 -0.325 -0.213 -0.128

# b)
monthly_seq_1901_2000 <- monthly_seq[600:1800]
time <- seq(1901, 2000, len = length(monthly_seq_1901_2000))

par(mar = c(5, 5, 2, 2))
plot(time, monthly_seq_1901_2000,
    type = "l", col = "blue", lwd = 2,
    xlab = "Time", ylab = "Temp anomalies [deg C]",
    main = "Monthly Global Average Temp Anomalies"
)

dev.off()


# 5
# b)
H <- matrix(
    c(
        2, 3, 4,
        4, 6, 0,
        0, 0, 1
    ),
    ncol = 3
)
det_H <- det(H)
det_H
# [1] 6.661338e-16
# This is a very small number close to zero, can be considered zero

# c)
B <- matrix(
    c(
        1, 2, 3, 4,
        4, 5, 6, 0,
        7, 1, 9, 0,
        0, 1, 0, 8
    ),
    ncol = 4
)

inv_B <- solve(B)
inv_B
#            [,1]        [,2]        [,3]        [,4]
# [1,] -0.8666667 -0.13333333  0.68888889  0.01666667
# [2,]  0.2333333  0.26666667 -0.21111111 -0.03333333
# [3,]  0.1333333 -0.13333333  0.02222222  0.01666667
# [4,]  0.4333333  0.06666667 -0.34444444  0.11666667

# d)
det_B <- det(B)
det_B
# [1] -360

# e)
eig_B <- eigen(B)

eigenvalues_B <- eig_B$values
unit_eigenvectors_B <- eig_B$vectors
# the eigen() func already normalizes the eigenvectors to unit length

eigenvalues_B
# [1] 13.200401  7.269414  3.578543 -1.048358

unit_eigenvectors
# [,1]       [,2]       [,3]       [,4]
# [1,] -0.5027881  0.1720017  0.3888892 -0.8857564
# [2,] -0.2586260 -0.1561739 -0.6696376  0.2046320
# [3,] -0.7285304  0.2432922  0.5259025  0.1422598
# [4,] -0.3867302 -0.9417187 -0.3518199  0.3915656

# f)
dot_product <- sum(unit_eigenvectors[, 1] * unit_eigenvectors[, 2])
dot_product
# [1] 0.1408555
# The unit eigenvectors are not orthogonal
# since the dot product between them is
# not close to zero
