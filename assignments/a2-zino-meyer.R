# 2.9
A <- matrix(
    c(
        1, 2, 3, 4, 5,
        6, 7, 8, 9, 10,
        11, 12, 13, 14, 15
    ),
    nrow = 3,
    byrow = TRUE
)

svd_A <- svd(A)

U <- svd_A$u
U
#            [,1]       [,2]       [,3]
# [1,] -0.2016649  0.8903171  0.4082483
# [2,] -0.5168305  0.2573316 -0.8164966
# [3,] -0.8319961 -0.3756539  0.4082483

D <- diag(svd_A$d) # convert from vector to diagonal matrix
D
#          [,1]     [,2]      [,3]
# [1,] 35.12722 0.000000 0.000e+00
# [2,]  0.00000 2.465397 0.000e+00
# [3,]  0.00000 0.000000 4.162e-16

V <- svd_A$v
V
#            [,1]        [,2]         [,3]
# [1,] -0.3545571 -0.68868664 -0.516261856
# [2,] -0.3986964 -0.37555453  0.822628731
# [3,] -0.4428357 -0.06242242 -0.091458089
# [4,] -0.4869750  0.25070970 -0.219922590
# [5,] -0.5311143  0.56384181  0.005013804


# ----------------
# 2.10
# Since I converted D to a diagonal matrix in the previous exercise,
# I use D[1, 1] to get the scalar d1
d1 <- D[1, 1]
u1 <- U[, 1]
v1 <- V[, 1]

# Then, it is the scalar times the outer product of u1 and v1
B <- d1 * outer(u1, v1) # same as d1 * u1 %*% t(v1) because u1 and v1 are vectors
B
#           [,1]      [,2]      [,3]      [,4]      [,5]
# [1,]  2.511657  2.824337  3.137016  3.449696  3.762376
# [2,]  6.436920  7.238261  8.039602  8.840944  9.642285
# [3,] 10.362183 11.652185 12.942188 14.232191 15.522194


# ----------------
# 2.11
A <- matrix(
    c(
        1.2, -0.5, 0.9, -0.6,
        1.0, -0.7, -0.4, 0.9,
        -0.2, 1.1, 1.6, -0.4
    ),
    nrow = 3, byrow = TRUE
)

# a)
a_a_t <- A %*% t(A)
eig_a_a_t <- eigen(a_a_t)

eig_a_a_t$values
# [1] 5.3834039 3.2895832 0.6170129

eig_a_a_t$vectors
#            [,1]      [,2]       [,3]
# [1,]  0.1586046 0.8912905  0.4247892
# [2,] -0.5272625 0.4402097 -0.7267803
# [3,]  0.8347687 0.1087047 -0.5397634

# b)
a_t_a <- t(A) %*% A
eig_a_t_a <- eigen(a_t_a)

eig_a_t_a$values
# [1] 5.383404e+00 3.289583e+00 6.170129e-01 6.087397e-16

eig_a_t_a$vectors
#            [,1]       [,2]      [,3]       [,4]
# [1,]  0.2171740  0.8204225 0.1388675 -0.5103511
# [2,] -0.5206528 -0.3496772 0.3785963 -0.6806698
# [3,] -0.7280696  0.4410850 0.2426462  0.4652756
# [4,]  0.3894493 -0.1003834 0.8823284  0.2444361

# c)
svd_A <- svd(A)
svd_A$d
# [1] 2.3202163 1.8137208 0.7855017

U <- svd_A$u
U
#            [,1]      [,2]       [,3]
# [1,]  0.1586046 0.8912905  0.4247892
# [2,] -0.5272625 0.4402097 -0.7267803
# [3,]  0.8347687 0.1087047 -0.5397634

V <- svd_A$v
V
#            [,1]       [,2]       [,3]
# [1,] -0.2171740  0.8204225 -0.1388675
# [2,]  0.5206528 -0.3496772 -0.3785963
# [3,]  0.7280696  0.4410850 -0.2426462
# [4,] -0.3894493 -0.1003834 -0.8823284

# e)
U_orthonormal <- t(U) %*% U
U_orthonormal
#               [,1]          [,2]         [,3]
# [1,]  1.000000e+00 -2.743475e-16 2.729620e-17
# [2,] -2.743475e-16  1.000000e+00 2.145436e-17
# [3,]  2.729620e-17  2.145436e-17 1.000000e+00

V_orthonormal <- t(V) %*% V
V_orthonormal
#               [,1]          [,2]          [,3]
# [1,]  1.000000e+00 -1.089398e-16  3.797139e-16
# [2,] -1.089398e-16  1.000000e+00 -1.643707e-16
# [3,]  3.797139e-16 -1.643707e-16  1.000000e+00


# ----------------
# Problem #8:
# Display the first three rows and the first five columns of the grayscale
# data matrix A of a color photo of your choice.
# Use the grayscale data matrix A to plot a black-white photo based on the
# color photo in Part (a)
library(imager)
sunset_img <- load.image("images/sunset.jpeg") # 36 KB file size

# a)
A <- grayscale(sunset_img)
A[1:3, 1:5]
#           [,1]      [,2]      [,3]      [,4]      [,5]
# [1,] 0.6625882 0.6625882 0.6625882 0.6625882 0.6625882
# [2,] 0.6625882 0.6625882 0.6625882 0.6625882 0.6625882
# [3,] 0.6625882 0.6625882 0.6625882 0.6625882 0.6625882

# b)
plot(A,
    xlim = c(0, 960), ylim = c(0, 500),
    main = "B/W Sunset"
)
dev.off()


# ----------------
# Problem #9:
# a)
# SVD analysis of grayscale A from above
svd_A <- svd(A)

D <- svd_A$d
D[1:5]
# [1] 336.85945  31.45088  14.88263  11.22749   9.48810

U <- svd_A$u
U[1:5, 1:5]
#             [,1]       [,2]       [,3]       [,4]        [,5]
# [1,] -0.02882415 0.02142493 0.01815178 0.02985490 -0.08097674
# [2,] -0.02889038 0.02141685 0.02113961 0.03609065 -0.09044643
# [3,] -0.02894194 0.02172832 0.02292550 0.03965408 -0.09658090
# [4,] -0.02891078 0.02176661 0.02186279 0.03627760 -0.09376029
# [5,] -0.02891545 0.02132887 0.02201846 0.03589219 -0.10129097

V <- svd_A$v
V[1:5, 1:5]
#             [,1]        [,2]        [,3]        [,4]        [,5]
# [1,] -0.07250391 -0.04013077 0.008661080 -0.01334525 0.009426218
# [2,] -0.07248727 -0.04014685 0.008563544 -0.01317204 0.009528562
# [3,] -0.07239671 -0.04021180 0.009082291 -0.01339862 0.009017418
# [4,] -0.07229404 -0.04000343 0.009052688 -0.01311987 0.009939243
# [5,] -0.07221855 -0.03992743 0.008515128 -0.01305185 0.009673950


# b)
# Scree plot of first 30 modes of A
variance_percent_D <- 100 * (D^2) / sum(D^2)
cum_percent_D <- cumsum(variance_percent_D)
modeK <- 1:length(D)
K <- 30

plot(modeK[1:K], variance_percent_D[1:K],
    type = "o", col = "blue",
    xlab = "Mode number", pch = 16,
    ylab = "Percentage of mode variance",
    main = "Scree Plot of first 30 eigenvalues of B/W Sunset Photo"
)

dev.off()

# Scree plot of first 30 modes of A, incl. cumulative variance
par(mar = c(4, 4, 2, 4), mgp = c(2.2, 0.7, 0))
plot(1:K,
    variance_percent_D[1:K],
    ylim = c(0, 100),
    type = "o",
    ylab = "Percentage of Variance [%]",
    xlab = "EOF Mode Number",
    cex.lab = 1.2, cex.axis = 1.1, lwd = 2,
    main = "Scree Plot of first 30 eigenvalues of B/W Sunset Photo"
)
legend(3, 30,
    col = c("black"), lty = 1, lwd = 2.0,
    legend = c("Percentange Variance"), bty = "n",
    text.font = 2, cex = 1.0, text.col = "black"
)

par(new = TRUE)
plot(1:K, cum_percent_D[1:K],
    ylim = c(90, 100), type = "o",
    col = "blue", lwd = 2, axes = FALSE,
    xlab = "", ylab = ""
)
legend(3, 94.5,
    col = c("blue"), lty = 1, lwd = 2.0,
    legend = c("Cumulative Percentage Variance"), bty = "n",
    text.font = 2, cex = 1.0, text.col = "blue"
)
axis(4, col = "blue", col.axis = "blue", mgp = c(3, 0.7, 0))
mtext("Cumulative Variance [%]",
    col = "blue",
    cex = 1.2, side = 4, line = 2
)

dev.off()


# c)
# Reconstruct A from its first 30 modes
A_recon_30 <- U[, 1:30] %*% diag(D)[1:30, 1:30] %*% t(V[, 1:30])
A_recon_30[1:5, 1:5]
#           [,1]      [,2]      [,3]      [,4]      [,5]
# [1,] 0.6705306 0.6702721 0.6715154 0.6707233 0.6698029
# [2,] 0.6700016 0.6696815 0.6710111 0.6696757 0.6687013
# [3,] 0.6682787 0.6679999 0.6691361 0.6676661 0.6670194
# [4,] 0.6676286 0.6673559 0.6683376 0.6669393 0.6661647
# [5,] 0.6679623 0.6676991 0.6686842 0.6672425 0.6665847


# d)
# Plot reconstructed image next to original image
par(mfrow = c(1, 2), mar = c(3, 3, 3, 3))

plot(as.cimg(A_recon_30),
    xlim = c(0, 960), ylim = c(0, 500),
    main = "Reconstructed B/W Sunset from first 30 modes"
)
plot(A,
    xlim = c(0, 960), ylim = c(0, 500),
    main = "B/W Sunset"
)

dev.off()
