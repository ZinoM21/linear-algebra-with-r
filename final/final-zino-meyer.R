# 1)
# a)
# Darwin data
darwin_data <- read.table("data/PSTANDdarwin.txt", header = F)
dim(darwin_data)
# [1] 65 13

darwin_jan <- darwin_data[11:50, 2] # rows for 1961-2000, Jan column

# Tahiti data
tahiti_data <- read.table("data/PSTANDtahiti.txt", header = F)
dim(tahiti_data)
# [1] 65 13

tahiti_jan <- tahiti_data[11:50, 2] # rows for 1961-2000, Jan column

# Bind to space-time data matirx
da_ta_jan <- rbind(darwin = darwin_jan, tahiti = tahiti_jan)
da_ta_jan[, 1:6] # print first 6 columns
#        [,1] [,2] [,3] [,4] [,5] [,6]
# darwin  0.5 -1.4 -0.7  0.6 -0.3  0.7
# tahiti  0.1  1.8  1.0  0.0 -1.0 -1.5


# b)
svd_da_ta_jan <- svd(da_ta_jan)
svd_da_ta_jan
# $d
# [1] 9.231838 6.429866
#
# $u
#            [,1]      [,2]
# [1,] -0.6063979 0.7951614
# [2,]  0.7951614 0.6063979
#
# $v
#              [,1]          [,2]
# [1,] -0.024229499  0.0712643902
# [2,]  0.246998228 -0.0033763944
# [3,]  0.132112366  0.0077427595
# [4,] -0.039411301  0.0742001203
# [5,] -0.066426865 -0.1314096266


# c)
plot(1961:2000,
    svd_da_ta_jan$v[, 1],
    type = "l",
    xlab = "Year", ylab = "PC1",
    main = "PC1 over time"
)

# 3)
# a)
A <- matrix(c(1, 2, 3, 4,
              4, 5, 6, 0,
              7, 8, 0, 9,
              3, 1, 2, 9), nrow = 4, byrow = TRUE)

b <- c(2, 5, 0, 1)

x <- solve(A, b)
x
# [1]  0.116402116  0.005291005  0.751322751 -0.095238095


# c) plot vectors
a <- c(2, 1)
b <- c(1, 2)

plot(c(0, 3), c(0, 3), type = "n", 
     xlab = "", ylab = "", asp = 1)
grid()
title("Two 2D vectors and the angle between them",
      cex.main = 0.85, line = 0.5)

# vector a
arrows(0, 0, a[1], a[2], col = "black")
text(2.1, 0.7, "a", pos = 3, col = "black", font = 2)

# vector b
arrows(0, 0, b[1], b[2], col = "blue")
text(1.1, 1.9, "b", pos = 3, col = "black", font = 2)

# angle
text(0.5, 0.5, labels = expression(theta), col = "black", font = 2)



# 4 )
# d)
u <- c(2,1,0)
v <- c(1,2,0)

library(pracma)

cross_product <- cross(u, v) 
cross_product
# [1] 0 0 3



# 5)
# c)
# iii)
A <- matrix(c(5, 1, 0, 3), nrow = 2, byrow = TRUE)
A
#      [,1] [,2]
# [1,]    5    1
# [2,]    0    3

ATA <- t(A) %*% A
ATA
#      [,1] [,2]
# [1,]   25    5
# [2,]    5   10

eigen_result <- eigen(ATA)

eigenvalues <- eigen_result$values
print(eigenvalues)
# [1] 26.513878  8.486122

eigenvectors <- eigen_result$vectors
print(eigenvectors)
#            [,1]       [,2]
# [1,] -0.9570920  0.2897841
# [2,] -0.2897841 -0.9570920


# iv)
x <- eigenvectors[,1]
x
# [1] -0.9570920 -0.2897841

x1 <- x[1]
x2 <- x[2]

# Calculate the polynomial
polyn <- 25 * x1^2 + 10 * x1 * x2 + 10 * x2^2
polyn
# [1] 26.51388



# 6)
# a)
library("imager")
sam <- load.image("data/SamPhoto.png")
dim(sam)
# [1] 430 460   1   3

graydat <- grayscale(sam)
dim(graydat)
# [1] 430 460   1   1

print(graydat[1:4, 1:3])
#           [,1]      [,2]      [,3]
# [1,] 0.6075294 0.6126667 0.6215686
# [2,] 0.6072549 0.6112549 0.6186667
# [3,] 0.6053725 0.6098431 0.6107843
# [4,] 0.6158431 0.6192941 0.6158431


# b)
print(max(graydat))
# [1] 1

print(min(graydat))
# [1] 0.1607843


# c)
plot(graydat, 
     xlim = c(0, 430), ylim = c(460, 0),
     main = 'B/W Sam')
points(200, 100, col = "blue", pch = 19)



# 7)
# a)
N = 3; K = 2
mydata <- matrix(c(1, 1, 1, 0, 3, 4), 
                 nrow = N, byrow = TRUE)
p1 = mydata[1, ]
p2 = mydata[2, ]
p3 = mydata[3, ]

### Manual calculation
# Case 1: C1 = (P1, P2)
mu1_c1 = (p1 + p2)/2 # mean of points in C1
mu2_c1 = p3 # mean of points in C2
tWCSS_c1 = norm(p1 - mu1_c1, type = '2')^2 + 
  norm(p2 - mu1_c1, type = '2')^2 + 
  norm(p3 - mu2_c1, type = '2')^2
tWCSS_c1
# [1] 0.5

# Case 2: C1 = (P1, P3)
mu1_c2 = (p1 + p3)/2
mu2_c2 = p2
tWCSS_c2 = norm(p1 - mu1_c2, type = '2')^2 + 
  norm(p3 - mu1_c2, type = '2')^2 +
  norm(p2 - mu2_c2, type = '2')^2 
tWCSS_c2
# [1] 6.5

# Case 3: C1 = (P2, P3)
mu1_c3 = (p2 + p3)/2
mu2_c3 = p1
tWCSS_c3 = norm(p2 - mu1_c3, type = '2')^2 + 
  norm(p3 - mu1_c3, type = '2')^2 +
  norm(p1 - mu2_c3, type = '2')^2 
tWCSS_c3
# [1] 10

# Use first case to get optimal cluster means:
mu1_c1
# [1] 1.0 0.5
mu2_c1
# [1] 3 4

### Calculation using kmeans() function
Kclusters = kmeans(mydata, K) 

Kclusters$tot.withinss
# [1] 0.5

centers <- Kclusters$centers
C1 <- centers[1, ]
C1
# [1] 1.0 0.5

C2 <- centers[2, ]
C2
# [1] 3 4


# b)
cluster <- Kclusters$cluster
cluster
# [1] 2 2 1

# Plot
plot(mydata[, 1], mydata[, 2],
     lwd = 2,
     xlim = c(0, 4), ylim = c(0, 4),
     xlab = "x", ylab = "y",
     col = cluster * 2,
     main = "K-means clustering for
three points and two clusters",
     cex.lab = 1.4, cex.axis = 1.4
)
points(centers[, 1], centers[, 2],
       col = c(2, 4), pch = 4
)
for (i in K:1) {
  text(centers[i, 1] + 0.15, centers[i, 2] - 0.15,
       bquote(C[.(i)]),
       cex = 1.4, col = i * 2)
}
for (i in 1:N) {
  text(mydata[i, 1] - 0.2, mydata[i, 2],
       bquote(P[.(i)]),
       cex = 1.4, col = cluster[i] * 2)
}

dev.off()


# c)
x <- mydata
y <- Kclusters$cluster

# Train SVM
library(e1071)
svm3P = svm(y ~ ., data = data.frame(x, y = as.factor(y)), 
            kernel = "linear", cost = 10, 
            scale = FALSE, 
            type = 'C-classification')

# Find hyper-planes
w = t(svm3P$coefs) %*% svm3P$SV 
w
#              X1         X2
# [1,] -0.3076923 -0.4615385

b = svm3P$rho
b
# [1] -1.769231

# Calculate x1 and x2 for the hyper-planes
x1 = seq(0, 6, len = 31)
x20 = (b - w[1]*x1)/w[2]
x2p = (1 + b - w[1]*x1)/w[2]
x2n = (-1 + b - w[1]*x1)/w[2]

# Plot the SVM results
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y * 2, pch = 19,
     xlim = c(0, 6), ylim = c(0, 6),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'SVM for three points labeled in two categories')
axis(2, at = (-2):8, tck = 1, lty = 2, 
     col = "grey", labels = NA)
axis(1, at = (-2):8, tck = 1, lty = 2, 
     col = "grey", labels = NA)

lines(x1, x20, lwd = 1.5, col = 'purple') # separating lane
lines(x1, x2p, lty = 2, col = 2) # hyper plane 1
lines(x1, x2n, lty = 2, col = 4) # hyper plane 2

text(4,5.5, 
     "Two red points and a blue point 
     are training data for an SVM ", 
     cex = 1.3, col = 4)

# d)
x1_sv = svm3P$SV

# support vector 1
x1_sv1 = x1_sv[1]
x2p_sv = (1 + b - w[1]*x1_sv1)/w[2]
sv1 <- c(x1_sv1, x2p_sv)
sv1
# [1] 1 1

# support vector 2
x1_sv2 = x1_sv[2]
x2n_sv = (-1 + b - w[1]*x1_sv2)/w[2]
sv2 <- c(x1_sv2, x2n_sv)
sv2
# [1] 3 4

# maximum margin of separation
d_m = 2/norm(w, type ='2')
d_m
# [1] 3.605551