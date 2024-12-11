################## R Codes for #################################
# Interactive Linear Algebra with R and Python
# A Cambridge University Press book 
# By SSP Shen 
# 
# The R code was written by 
# Samuel Shen, Distinguished Professor
# San Diego State University
# Email: sshen@sdsu.edu
# https://shen.sdsu.edu 
# R code Version 2.0: August 2024 San Diego, California, USA
################################################################


################################################################
#
# Chapter 1: Basic Matrix Operations
#
################################################################


#Sam says many studeent do not they want a job
#Sam is idiot

#Create your working directory named LinAlg and go there
setwd('/Users/sshen/LinAlg')
#/Users/sshen/LinAlg
getwd() #Verify that you are in the directory/folder
#[1] "/Users/sshen/LinAlg"

1 + 2
 1 - 3


plot(1, col = 'red' , cex = 2, pch = 3)

x = c(1, 2)
y = c(3, 2)
plot(x, y, xlim = c(-2, 2), ylim = c(-3,3),
     col = 'blue', type = 'o')

plot(sin, c(-9,9), lwd =3, col = 'purple',
     main = 'Sam\'s color plot')


A = matrix(c(1,1,1,-1), nrow=2)
A

B = matrix(c(1,3,2,0), nrow=2)
B
A - B

#Matrix subtraction
matrix(c(1,1,1,-1), nrow=2) - matrix(c(1,3,2,0), nrow=2) 
# [,1] [,2]
#[1,] 0 -1
#[2,] -2 -1

A - B

#Dot product
#install.packages('geometry')
library(geometry)
a = c(1, 1)
b = c(1, 0)
# Calculating dot product using dot()
dot(a, b)

#Another way to compute dot product is to 
a%*%b
#[,1]
#[1,]    1

#Calculate the angle between two vectors
am = norm(a, type="2")  
am
#[1] 1.414214
bm = norm(b, type="2") 
bm
#[1] 1
angleab = acos(dot(a, b)/(am*bm))*180/pi
angleab
#[1] 45 degrees

#Cross product of two 3D vectors
library(pracma)
x = 1:3
y = 4:6
cross(x, y)	
#[1] -3  6 -3
dot(x, y)
#[1] 32

#Scalar times a matrix
A = matrix(c(1,1,1,-1), ncol = 2, byrow=T)
A
#[,1] [,2]
#[1,]    1    1
#[2,]    1   -1
3*A
#[,1] [,2]
#[1,]    3    3
#[2,]    3   -3

#Matrix multiplication by command %*%
A = matrix(c(1,1,1,-1), nrow=2) 
B = matrix(c(1,2,3,4), nrow=2)
A%*%B
#     [,1] [,2]
#[1,]    3    7
#[2,]   -1   -1
B%*%A
#     [,1] [,2]
#[1,]    4   -2
#[2,]    6   -2

#Matrix transpose
A = matrix(c(1,2,3,4), ncol =2, byrow=T)
A
#     [,1] [,2]
#[1,]    1    2
#[2,]    3    4
t(A)
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#Generate a diagonal matrix
D = diag(c(2, 1, -3))
round(D, digits = 0)
#     [,1] [,2] [,3]
#[1,]    2    0    0
#[2,]    0    1    0
#[3,]    0    0   -3

#Generate an 3-dimensional identity matrix
I = diag(3)
I
#     [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1

#Generate a 2-by-3 zero matrix
M = matrix(0, 2, 3)
M
#     [,1] [,2] [,3]
#[1,]    0    0    0
#[2,]    0    0    0

#Compute the inverse of a matrix
A = matrix(c(1,1,1,-1), nrow=2)
invA = solve(A)
invA
#     [,1] [,2]
#[1,]  0.5  0.5
#[2,]  0.5 -0.5

#example of inverse
##par(mfrow) lets u plot mutliple things beside eachother
par(mfrow = c(1,2))
H = matrix(rnorm(64), nrow = 8)
image(H)
G = solve(H)
image(G)
dev.off
#dev.off to turn of plots also resets

filled.contour(H, grid())

#Verify the inverse
invA %*% A
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1


###9/9/2024 class
m = 9
n = 8
A = matrix(2, m, n)
A # print A

# preferred, more descriptive syntax
A = matrix(2, nrow = 9, ncol = 8) 
A

# Fill with range
A = matrix(1:72, nrow = 9, ncol = 8) # <- preferred syntax, more descriptive
A

# visualize a matrix
# this is how photos are made!
image(A)

# Normal distribution fill
A = matrix(rnorm(800 * 1000), nrow = 800, ncol = 1000)
image(A)

# Inspect the normal distribution
rnorm(4000)
hist(rnorm(4000), breaks = 99) # -> this will show the bell curve


#Delete rows or/and columns
A = matrix(1:9, nrow = 3)
A
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8
#[3,]    3    6    9

A[-3,]
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8

A[,-1]
#     [,1] [,2]
#[1,]    4    7
#[2,]    5    8
#[3,]    6    9
A[-3,-1]
#     [,1] [,2]
#[1,]    4    7
#[2,]    5    8

A[1:2, 2:3] #Sub-matrix
#     [,1] [,2]
#[1,]    4    7
#[2,]    5    8

A[1:2, ] #keep all the columns
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8

#Insert a row or column to a matrix
A = matrix(1:4, nrow = 2)
br = 5:6
bc = 7:8
rbind(A, br)
#   [,1] [,2]
#1    3
#2    4
#br    5    6

rbind(br, A)
#   [,1] [,2]
#br    5    6
#      1    3
#      2    4

rbind(rbind(A[1,], br), A[2,])
#   [,1] [,2]
#      1    3
#br    5    6
#      2    4
Abc = cbind(A, bc)
Abc
Abc = cbind(A, bc)
#         bc
#[1,] 1 3  7
#[2,] 2 4  8

cbind(Abc, A)#stack two matrices
#         bc    
#[1,] 1 3  7 1 3
#[2,] 2 4  8 2 4


#
#
#Row or column statistics
A = matrix(1:6, nrow = 2)
A

rowSums(A)
#[1]  9 12

rowMeans(A)
#[1] 3 4

install.packages("matrixStats")
library(matrixStats) #SD needs the library

rowCumsums(A) #cumulative sum
#     [,1] [,2] [,3]
#[1,]    1    4    9
#[2,]    2    6   12

colMeans(A)
#[1] 1.5 3.5 5.5

rowSds(A) 
#[1] 2 2

colSds(A)
#[1] 0.7071068 0.7071068 0.7071068

x = rnorm(10000) # mean sd = 1 (unit normal / standard normal)
x
hist(x, breaks = 1000)
sd(x)
mean(x, mean = 3, sd = 2)


#Sweep a matrix by a vector using subtraction
A = matrix(1:6, nrow=2, byrow = TRUE)
A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    4    5    6

u = 1:3
Br = sweep(A, 2, u) #2 means sweep every row
Br
#     [,1] [,2] [,3]
#[1,]    0    0    0
#[2,]    3    3    3

v= 1:2
Bc = sweep(A, 1, v) #1 means sweep every column
Bc
#     [,1] [,2] [,3]
#[1,]    0    1    2
#[2,]    2    3    4

c = colMeans(A) #means of each column
sweep(A, 2, c) #anomaly data matrix
#[1,] -1.5 -1.5 -1.5
#[2,]  1.5  1.5  1.5

sin(A)#function operation on each matrix element
#           [,1]       [,2]       [,3]
#[1,]  0.8414710  0.9092974  0.1411200
#[2,] -0.7568025 -0.9589243 -0.2794155

A^2 #not equal to A%*%A
#     [,1] [,2] [,3]
#[1,]    1    4    9
#[2,]   16   25   36

#Sweep a matrix by a vector using multiplication
A = matrix(1:6, nrow=2, byrow = TRUE)
w = 1:2
w*A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    8   10   12

A*w # yields the same result as w*A
w3 = 1:3

#sweep each row by transposing A
t(w3*t(A))
#     [,1] [,2] [,3]
#[1,]    1    4    9
#[2,]    4   10   18

w3*A #multiplication sweep by row-dimensions not matching
#     [,1] [,2] [,3]
#[1,]    1    6    6
#[2,]    8    5   18

A/w #sweeping by division
#     [,1] [,2] [,3]
#[1,]    1  2.0    3
#[2,]    2  2.5    3


#Conversions between a Vector and a Matrix
v = c(60, 58, 67, 70, 55, 53)
M = matrix(v, nrow = 2) #from vector to matrix
M
#     [,1] [,2] [,3]
#[1,]   60   67   55
#[2,]   58   70   53
c(M) #from matrix to vector by column
#[1] 60 58 67 70 55 53
c(t(M)) #from matrix to vector by row
#[1] 60 67 55 58 70 53

#Reduce the dimension of an nD array
x <- array(1:(2*3*4), dim=c(2,3,4))
dim(x)
#[1] 2 3 4
x #a stack of four 2-by-3 matrices
#, , 1 #the first of the 3rd dim index

#[,1] [,2] [,3]
#[1,]    1    3    5
#[2,]    2    4    6
# ...
#install.packages('R.utils')
library(R.utils)
#flat all the other dim except the 3rd one
#flat the 1st and 2nd dim
y <- wrap(x, map=list(3, NA)) 
dim(y)
#[1] 4 6
y
#     [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    1    2    3    4    5    6
#[2,]    7    8    9   10   11   12
#[3,]   13   14   15   16   17   18
#[4,]   19   20   21   22   23   24

#back to the original 3D array
array(t(y), dim = c(2,3,4))


#Solve linear equations
A = matrix(c(1,1,1,-1), nrow = 2)
b = c(20,4)
solve(A, b)
#[1] 12  8  #This is the result x1=12, and x2=8.

#Spatial covariance matrix
dat = matrix(c(0,-1,1,2,3,4), nrow=3)
dat
colMeans(dat)
A = sweep(dat, 2, colMeans(dat))
A
#     [,1] [,2]
#[1,]    0   -1
#[2,]   -1    0
#[3,]    1    1
covm=(1/(dim(A)[2]))*A%*%t(A)
covm #is the covariance matrix.
#     [,1] [,2] [,3]
#[1,]  0.5  0.0 -0.5
#[2,]  0.0  0.5 -0.5
#[3,] -0.5 -0.5  1.0

u = c(1, 1, 0)
v = covm %*% u
v
#     [,1]
#[1,]  0.5
#[2,]  0.5
#[3,] -1.0
#u and v are in different directions

#Eigenvectors of a covariance matrix 
ew = eigen(covm)
ew
#$values
#[1] 1.500000e+00 5.000000e-01 1.332268e-15

#$vectors
#           [,1]          [,2]      [,3]
#[1,] -0.4082483 -7.071068e-01 0.5773503
#[2,] -0.4082483  7.071068e-01 0.5773503
#[3,]  0.8164966  8.881784e-16 0.5773503
#This is the first eigenvector

#Verify the eigenvectors and eigenvalues
covm%*%ew$vectors[,1]/ew$values[1]
#           [,1]
#[1,] -0.4082483
#[2,] -0.4082483
#[3,]  0.8164966

w = ew$vectors[,1] # is an eigenvector

#R code for SVD
#Develop a 2-by-3 space-time data matrix for SVD
A=matrix(c(1,-1,2,0,3,1),nrow=2)
A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]   -1    0    1
#Perform SVD calculation
msvd=svd(A)
msvd
msvd$d
#[1] 3.784779 1.294390
msvd$u
#          [,1]       [,2]
#[1,] -0.9870875 -0.1601822
#[2,] -0.1601822  0.9870875
msvd$v
#           [,1]       [,2]
#[1,] -0.2184817 -0.8863403
#[2,] -0.5216090 -0.2475023
#[3,] -0.8247362  0.3913356
#One can verify that A=UDV', where V' is transpose of V.
verim=msvd$u%*%diag(msvd$d)%*%t(msvd$v)
verim
#     [,1]         [,2] [,3]
#[1,]    1 2.000000e+00    3
#[2,]   -1 1.665335e-16    1
round(verim)
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]   -1    0    1
#This is the original data matrix A

covm = (1/(dim(A)[2]))*A%*%t(A)
eigcov = eigen(covm)
eigcov$values
#[1] 4.7748518 0.5584816
eigcov$vectors
#           [,1]       [,2]
#[1,] -0.9870875  0.1601822
#[2,] -0.1601822 -0.9870875

((msvd$d)^2)/(dim(A)[2])
#[1] 4.7748518 0.5584816
eigcov$values
#[1] 4.7748518 0.5584816

x1=c(1,2,3) #Given the coordinates of the 3 points
x2=c(2,1,3)
y=c(-1,2,1)
df=data.frame(x1,x2,y) #Put data into the data.frame format
fit <- lm(y ~ x1 + x2, data=df)
fit#Show the regression results
#Call:
#  lm(formula = y ~ x1 + x2, data = df)
#Coefficients:
#  (Intercept)           x1           x2  
#-5.128e-16    1.667e+00   -1.333e+00  

1.667*x1-1.333*x2  #Verify that 3 points determining a plane
#[1] -0.999  2.001  1.002


#Multilinear regression
u=c(1,2,3,1)
v=c(2,4,3,-1)
w=c(1,-2,3,4)
mydata=data.frame(u,v,w)
myfit <- lm(w ~ u + v, data=mydata)
summary(myfit)#Show the result
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)   1.0000     1.8708   0.535    0.687
#u             2.0000     1.2472   1.604    0.355
#v            -1.5000     0.5528  -2.714    0.225

#Multilinear regression example for more data
dat=matrix(rnorm(40),nrow=10, dimnames=list(c(letters[1:10]), c(LETTERS[23:26])))
fdat=data.frame(dat)
fit=lm(Z~ W + X + Y, data=fdat)
summary(fit)

#Coefficients\index{linear regression!coefficients}
#            Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.36680    0.16529   2.219   0.0683 
#W            0.11977    0.20782   0.576   0.5853  
#X           -0.53277    0.19378  -2.749   0.0333 
#Y           -0.04389    0.14601  -0.301   0.7739


#Image analysis using SVD
#Ref: imager: an R package for image processing
#https://dahtah.github.io/imager/imager.html 

install.packages('imager') #run this if not installed yet
library(imager)
dat <- load.image('images/golf.jpeg') #355 KB file size

# Flip the image horizontally
dat <- imrotate(dat, angle = 90)

dim(dat)
# [1] 3024 4032   1   3
# 3024 rows and 4032 columns, 3024*4032 = 12,192,768 pixels
# 1 photo frame, 3 RGB colors 
# If a video, 1 will become 150 frames or more

# Show part of the dat
dat[1:3, 1:4,1,1]

# Plot the color figure
plot(dat, 
     xlim = c(0, 3024), ylim = c(0, 4032),
     main = 'Golfing :D')

dev.off()

# Show the image with only one color value (3 for red, 2 for )
image(dat[, , 1,3])
dev.off()

# Make the photo black-and-white / grayscale
graydat <- grayscale(dat)
dim(graydat)
#[1] 3024 4032   1   1 -> 1 grayscale [0, 1]

# plot the gray b/w photo
plot(graydat, 
     xlim = c(0, 3024), ylim = c(0, 4032),
     main = "B/W Zino Golf")
dev.off()

#Plot the color and b/w photos together
par(mfrow = c(1, 2))
plot(dat,
     xlim = c(0, 3024), ylim = c(0, 4032),
     main = "Color Golfo")

plot(graydat,
     xlim = c(0, 3024), ylim = c(0, 4032),
     main = "B/W Golfo")
dev.off()

# SVD analysis of the grayscale data
svdDat <- svd(graydat)
SVDd <- svdDat$d
percentD = 100*(SVDd^2)/sum(SVDd^2)
cumpercentD = cumsum(percentD)
modeK = 1:length(SVDd)
dev.off()
plot(modeK[1:20], percentD[1:20], 
     type = 'o', col = 'blue',
     xlab = 'Mode number', pch = 16,
     ylab = 'Percentage of mode variance',
     main = 'Scree Plot of SVD B/W Photo Data')
K = 20
lam = (svdDat$d)^2
lamK=lam[1:K]
lamK

#setEPS() #Plot the figure and save the file
#postscript("fig0608.eps", width = 6, height = 4)
par(mar=c(4,4,2,4), mgp=c(2.2,0.7,0))
plot(1:K, 100*lamK/sum(lam), ylim=c(0,100), type="o", 
     ylab="Percentage of Variance [%]",
     xlab="EOF Mode Number", 
     cex.lab=1.2, cex.axis = 1.1, lwd=2, 
     main="Scree Plot of the First 20 Eigenvalues")
legend(3,30, col=c("black"),lty=1, lwd=2.0,
       legend=c("Percentange Variance"),bty="n",
       text.font=2,cex=1.0, text.col="black")
par(new=TRUE)
plot(1:K,cumsum(100*lamK/sum(lam)),
     ylim = c(90,100), type="o",
     col="blue",lwd=2, axes=FALSE,
     xlab="",ylab="")
legend(3,94.5, col=c("blue"),lty=1,lwd=2.0,
       legend=c("Cumulative Percentage Variance"),bty="n",
       text.font=2,cex=1.0, text.col="blue")
axis(4, col="blue", col.axis="blue", mgp=c(3,0.7,0))
mtext("Cumulative Variance [%]",col="blue", 
      cex=1.2, side=4,line=2)

# Reconstructing b/w photo from SVD modes
U = svdDat$u
V = svdDat$v
D = diag(svdDat$d)
dim(D)

#100% reconstruction using all the modes
recon = U%*%D%*%t(V)
dim(recon)
#[1] 430 460
image(recon)

dev.off()
par(mfrow = c(2, 2))
kR = 430 #Recon from all 413 modes
R430 = as.cimg(U[,1:kR]%*%D[1:kR, 1:kR]%*%t(V[, 1:kR]))
plot(R430, main = "All 430 modes")
kB = 20 #The first 20 modes
R20 = as.cimg(U[,1:kB]%*%D[1:kB, 1:kB]%*%t(V[, 1:kB]))
plot(R20, main = "The first 20 modes")
k = 3 #Recon from the first 3 modes
R3 = as.cimg(U[,1:k]%*%D[1:k, 1:k]%*%t(V[, 1:k]))
plot(R3, main = "The first 3 modes")
k1 = 21; k2 = 100 #Recon from 21st to 100th modes
Rk1_k2 = as.cimg(U[,k1:k2]%*%D[k1:k2, k1:k2]%*%t(V[, k1:k2]))
plot(Rk1_k2, main = "21st to 100th modes")
dev.off()

#Three monotone photos and their color photo
dim(dat) #4D array of a color photo
#[1] 430 460   1   3
dev.off()
par(mfrow = c(2, 2))
R = as.cimg(apply(t(dat[,, 1, 1]), 1, rev))
plot(R, main = 'Monotone color R')
G = as.cimg(apply(t(dat[,, 1, 2]), 1, rev))
plot(G, main = 'Monotone color G')
B = as.cimg(apply(t(dat[,, 1, 3]), 1, rev))
plot(B, , main = 'Monotone color B')
#Bind the three monotone photos into one 
trippy = imappend(list(R,G,B), "c") 
dim(trippy) #color figure data
#[1] 430 460   1   3
plot(trippy, main ='Blend RGB Colors')
dev.off()


################################################################
#
# Chapter 2: Matrix Theory and Visualization
#
################################################################


#Create your working directory named LinAlg and go there
setwd('/Users/sshen/LinAlg') 
getwd() #Verify that you are in the directory/folder
#[1] "/Users/sshen/LinAlg"

#R code: Computational examples of matrices
A = matrix(c(1,0,0,4,3, 2), nrow = 3, byrow = TRUE)
B = matrix(c(0,1,-1,2), nrow = 2) #form a matrix by columns
C = A%*%B #matrix multiplication
C
#[1,]    0   -1
#[2,]    4    8
#[3,]    2    1
t(C) # transpose matrix of C
#[1,]    0    4    2
#[2,]   -1    8    1

A = matrix(c(1, -1, 1, 2), nrow =2, byrow = TRUE)
solve(A) #compute the inverse of A
#[1,]  0.6666667 0.3333333
#[2,] -0.3333333 0.3333333
A%*%solve(A) #verify the inverse of A
#[1,] 1.000000e+00    0
#[2,] 1.110223e-16    1

#Solve linear equations
A = matrix(c(30, 40, 1, 1), nrow =2, byrow = TRUE)
b = c(1000, 30)
solve(A,b)
#[1] 20 10
solve(A)%*%b #Another way to solve the equations
det(A) #compute the determinant
#[1] -10

library(Matrix)
rankMatrix(A) #Find the rank of a matrix
#[1] 2 #rank(A) = 2

#Orthogonal matrices
p = sqrt(2)/2
Q = matrix(c(p,-p,p,p), nrow=2) 
Q #is an orthogonal matrix 
#           [,1]      [,2]
#[1,]  0.7071068 0.7071068
#[2,] -0.7071068 0.7071068
Q%*%t(Q) #verify O as an orthogonal matrix
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
det(Q) #The determinant of an orthogonal matrix is 1 or -1
#[1] 1

#R code for eigenvectors and eigenvalues
A = matrix(c(1, 2, 2, 1), nrow=2)
eigen(A)
#$values
#[1]  3 -1
#$vectors
#[,1]       [,2]
#[1,] 0.7071068 -0.7071068
#[2,] 0.7071068  0.7071068

# Plot Fig. 2.2
#R plot eigenvector v vs a non-eigenvector u
setwd('/Users/sshen/LinAlg')
setEPS() #Plot the figure and save the file
postscript("fig0502.eps", width = 6)
par(mar=c(4.5,4.5,2.0,0.5))
plot(9,9,
     main = 'An eigenvector vs a non-eigenvector',
     cex.axis = 1.4, cex.lab = 1.4,
     xlim = c(0,3), ylim=c(0,3),
     xlab = bquote(x[1]), ylab = bquote(x[2]))
arrows(0,0, 1,0, length = 0.25, 
       angle = 15, lwd = 5, col = 'blue')
arrows(0,0, 1,2, length = 0.3, 
       angle = 15, lwd = 3, col = 'blue',  lty = 3)
arrows(0,0, 1,1, length = 0.25, 
       angle = 15, lwd = 5, col='red') 
arrows(0,0, 3,3, length = 0.3, 
       angle = 15, lwd = 3, col='red', lty = 3)
text(1.4,0.1, 'Non-eigenvector u', cex =1.4, col = 'blue')
text(1.0,2.1, 'Au', cex =1.4, col = 'blue')
text(1.5,0.9, 'Eigenvector v', cex =1.4, col = 'red')xq
text(2.8, 2.95, 'Av', cex =1.4, col = 'red')
dev.off()

# Verify diagonalization and decomposition: R code
C = matrix(c(2,1,1,2), nrow = 2)
eigen(C)
#$values
#[1] 3 1
#$vectors
#  [,1]       [,2]
#[1,] 0.7071068 -0.7071068
#[2,] 0.7071068  0.7071068
Q = eigen(C)$vectors
D = t(Q)%*%C%*%Q #Matrix diagonalization
D
#[1,]    3    0
#[2,]    0    1
Q%*%D%*%t(Q) #Matrix decomposition
#[1,]    2    1
#[2,]    1    2
D[1,1]*Q[,1]%*%t(Q[,1]) + D[2,2]*Q[,2]%*%t(Q[,2])
#[1,]    2    1
#[2,]    1    2

#Hadamard product of two matrices
#install.packages('matrixcalc')
library(matrixcalc)
A = matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
B = matrix( c( 2, 4, 6, 8 ), nrow=2, byrow=TRUE )
hadamard(A, B)
#     [,1] [,2]
#[1,]    2    8
#[2,]   18   32

#Jordan product of A and B
A = matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
B = matrix( c( 2, 1, 2, 1 ), nrow=2, byrow=TRUE )
(A%*%B + B%*%A)/2
#     [,1] [,2]
#[1,]  5.5  5.5
#[2,]  9.5  7.5

#R commutator of A and B
A = matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
B = matrix( c( 2, 1, 2, 1 ), nrow=2, byrow=TRUE )
A%*%B - B%*%A
#     [,1] [,2]
#[1,]    1   -5
#[2,]    9   -1
#install.packages('psych')
library(psych)
tr(A%*%B - B%*%A) #tr for trace
#[1] 0

#Cross product
library(pracma)
x = 1:3
y = 4:6
cross(x, y)	
#[1] -3  6 -3
dot(x, y)
#[1] 32

#Outer product of two vectors
a = 1:2
b = 1:4
a%o%b #outer product a_2-by-1 times t(b)_1-by-4
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]    2    4    6    8


#Outer product of A_mn and B_nq
A = matrix(1:4, ncol = 2)
B = matrix(1:6, ncol = 3)
A%o%B
dim(A%o%B)
#[1] 2 2 2 3

#Outer product of A_mn and B_pq
A = matrix(1:4, ncol = 2)
B = matrix(1:9, ncol = 3)
A%o%B
dim(A%o%B)
#[1] 2 2 2 3

#Kronecker product
library(fastmatrix)
A <- diag(1:2)
B <- matrix(1:4, ncol = 2)
kronecker.prod(A, B)
#     [,1] [,2] [,3] [,4]
#[1,]    1    3    0    0
#[2,]    2    4    0    0
#[3,]    0    0    2    6
#[4,]    0    0    4    8

# an example with vectors
ones <- rep(1, 2)
y <- 1:4
kronecker.prod(ones, t(y)) # 2-by-4 matrix
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]    1    2    3    4

#Cholesky decomposition
dat = matrix(1:4, ncol = 2)
A = dat%*%t(dat)
chol(A)

#Direct sum of two matrices
A = matrix(1:4, ncol = 2)
B = matrix(1:6, ncol = 3)
A1 = rbind(A, matrix(rep(0, 4), ncol = 2))
B1 = rbind(matrix(rep(0, 6), ncol = 3), B)
C = cbind(A1, B1) #= direct sum of A and B
C
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    1    3    0    0    0
#[2,]    2    4    0    0    0
#[3,]    0    0    1    3    5
#[4,]    0    0    2    4    6

#Express the direct sum by Kronecker products
kronecker.prod(diag(1,0), A) + kronecker.prod(diag(0,1), B)
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    1    3    0    0    0
#[2,]    2    4    0    0    0
#[3,]    0    0    1    3    5
#[4,]    0    0    2    4    6

#SVD example for a 2-by-3 matrix: R code
A=matrix(c(-1,1,0,2,-2,3),nrow=2)
A #Show the 2-by-3 matrix
#     [,1] [,2] [,3]
#[1,]   -1    0   -2
#[2,]    1    2    3
svdA=svd(A) #Compute the SVD of A and put the results in svdA
svdA #Show SVD results: d, U, and V
round(svdA$d, digits=2) #Show only the singular values
#[1] 4.22 1.09
round(svdA$u, digits=2) #Show only matrix U
#      [,1] [,2]
#[1,] -0.48 0.88
#[2,]  0.88 0.48
round(svdA$v, digits=2)#Show only matrix V
#     [,1]  [,2]
#[1,] 0.32 -0.37
#[2,] 0.42  0.88
#[3,] 0.85 -0.29
sqrt(eigen(A%*%t(A))$values)
#[1] 4.221571 1.085514


#Data reconstruction by singular vectors: R code
round(svdA$d[1]*svdA$u[,1]%*%t(svdA$v[,1]), 
      digits=1)
#     [,1] [,2] [,3]
#[1,] -0.7 -0.8 -1.7
#[2,]  1.2  1.5  3.2

round(svdA$d[1]*svdA$u[,1]%*%t(svdA$v[,1]) + 
        svdA$d[2]*svdA$u[,2]%*%t(svdA$v[,2]), 
      digits =2)
#     [,1] [,2] [,3]
#[1,]   -1    0   -2
#[2,]    1    2    3


#R plot schematic diagram of SVD
setwd('/Users/sshen/climstats')
setEPS() #Plot the figure and save the file
postscript("fig0503.eps", width = 11)
par(mar=c(0,0,0,0))
plot(200, axes = FALSE,
     xlab = "", ylab = "",
     xlim = c(-3,28), ylim = c(-3,16))
text(13,15.5, cex=2.2,
     bquote("SVD:" ~ A==UDV^t~ "when n > m or n < m"))
#Space-time data matrix A when n>m
segments(x0 = c(0,0,3,3),
         y0 = c(6,12,12,6) +1,
         x1 = c(0,3,3,0),
         y1 = c(12,12,6,6) +1, 
         col = c('blue','red','blue','red'),lwd =3)
segments(x0 = c(0.5,1.0),
         y0 = c(6,6)+1,
         x1 = c(0.5,1.0),
         y1 = c(12,12)+1,
         lwd =1.3, lty = 3)
text(-.8, 9+1, 'n', srt=90, col ='blue', cex = 1.4)
text(1.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(2.0, 9+1, '...',  cex = 1.4)
text(2, 5+1, bquote(A[n%*%m]),  cex = 2.5)
text(5, 9+1, '=',  cex = 3)
#Spatial matrix U
segments(x0 = c(7,7,10,10),
         y0 = c(6,12,12,6)+1,
         x1 = c(7,10,10,7),
         y1 = c(12,12,6,6)+1, 
         col = c('blue','blue','blue','blue'), lwd =3)
segments(x0 = c(7.5,8),
         y0 = c(6,6)+1,
         x1 = c(7.5,8),
         y1 = c(12,12)+1,
         lwd =1.3, lty = 3, col = 'blue')
text(6.2, 9+1, 'n', srt=90, col ='blue', cex = 1.4)
text(8.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(9, 9+1, '...',  cex = 1.4, col='blue')
text(8.7, 5.0+1, bquote(U[n%*%m]),  cex = 2.5, col= 'blue')
#Singular value diagonal matrix D
segments(x0 = c(12,12,15,15),
         y0 = c(9,12,12,9)+1,
         x1 = c(12,15,15,12),
         y1 = c(12,12,9,9)+1, 
         col = c('brown','brown','brown','brown'), lwd =3)
segments(x0 = 12, y0 = 12+1, x1 = 15, y1 = 9+1, lty=3,
         col = c('brown'), lwd =1.3)#diagonal line
text(11.2, 10.5+1, 'm', srt=90, col ='red', cex = 1.4)
text(13.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(14.1, 11.3+1, '0', col = 'brown',  cex = 1.4)
text(12.9, 10.0+1, '0', col = 'brown',  cex = 1.4)
text(13.9, 8.0+1, bquote(D[m%*%m]),  cex = 2.5, col='brown')
#Temporal matrix V
segments(x0 = c(17,17,20,20),
         y0 = c(9,12,12,9)+1,
         x1 = c(17,20,20,17),
         y1 = c(12,12,9,9)+1, 
         col = c('red','red','red','red'), lwd =3)
segments(x0 = c(17,17),
         y0 = c(11.5,10.8)+1,
         x1 = c(20,20),
         y1 = c(11.5,10.8)+1, 
         col = c('red','red'), lty=3, lwd =1.3)
text(16.2, 10.5+1, 'm', srt=90, col ='red', cex = 1.4)
text(18.5, 12.5+1, 'm', col = 'red',  cex = 1.4)
text(19.5, 8+1, bquote((V^t)[m%*%m]),  cex = 2.5, col='red')
text(18.5, 10+1, '...',  col='red', srt=90, cex =1.4)
# Space-time data matrix B when n < m
segments(x0 = c(0,0,6,6),
         y0 = c(0,3,3,0),
         x1 = c(0,6,6,0),
         y1 = c(3,3,0,0), 
         col = c('blue','red','blue','red'), lwd =3)
segments(x0 = c(1,2,5),
         y0 = c(0,0,0),
         x1 = c(1,2,5),
         y1 = c(3,3,3),
         lwd =1.3, lty = 3)
text(-0.8, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(3, 3.8, 'm', col = 'red',  cex = 1.4)
text(3.5, 1.5, '...',  cex = 1.4)
text(3, -1.5, bquote(A[n%*%m]),  cex = 2.5)
text(8, 1.5, '=',  cex = 3)
#Spatial matrix U
segments(x0 = c(11,11,14,14),
         y0 = c(0,3,3,0),
         x1 = c(11,14,14,11),
         y1 = c(3,3,0,0), 
         col = c('blue','blue','blue','blue'), lwd =3)
segments(x0 = c(11.5,12.2),
         y0 = c(0,0),
         x1 = c(11.5,12.2),
         y1 = c(3,3),
         lwd =1.3, lty = 3, col = 'blue')
text(10.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(12.5, 3.8, 'n', col = 'blue',  cex = 1.4)
text(13.2, 1.5, '...',  cex = 1.4, col='blue')
text(12.5, -1.5, bquote(U[n%*%n]),  cex = 2.5, col= 'blue')
#Singular value diagonal matrix D
segments(x0 = c(16,16,19,19),
         y0 = c(0,3,3,0),
         x1 = c(16,19,19,16),
         y1 = c(3,3,0,0), 
         col = c('brown','brown','brown','brown'), lwd =3)
segments(x0 = 16, y0 = 3, x1 = 19, y1 = 0, lty=3,
         col = c('brown'), lwd =1.3)#diagonal line
text(15.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(17.5, 3.8, 'n', col = 'blue',  cex = 1.4)
text(18.1, 2.3, '0', col = 'brown',  cex = 1.4)
text(16.9, 1.0, '0', col = 'brown',  cex = 1.4)
text(17.5, -1.5, bquote(D[n%*%n]),  cex = 2.5, col='brown')
#Temporal matrix V
segments(x0 = c(21,21,27,27),
         y0 = c(0,3,3,0),
         x1 = c(21,27,27,21),
         y1 = c(3,3,0,0), 
         col = c('red','red','red','red'),
         lwd =3)
segments(x0 = c(21,21),
         y0 = c(2.5,1.8),
         x1 = c(27,27),
         y1 = c(2.5,1.8), 
         col = c('red','red'), lty=3, lwd =1.3)
text(20.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(24, 3.8, 'm', col = 'red',  cex = 1.4)
text(24, -1.5, bquote((V^t)[n%*%m]),  cex = 2.5, col='red')
text(24, 1, '...',  col='red', srt=90, cex =1.4)
dev.off()

#R SVD analysis for the weighted SOI from SLP data
#for seven years: 2009-2015
setwd("/Users/sshen/climmath")
Pda<-read.table("data/PSTANDdarwin.txt", header=F)
dim(Pda) 
#[1] 65 13 #Monthly Darwin data from 1951-2015
pdaDec<-Pda[,13] #Darwin Dec standardized SLP anomalies data
Pta<-read.table("data/PSTANDtahiti.txt", header=F)
ptaDec=Pta[,13] #Tahiti Dec standardized SLP anomalies
ptada1 = cbind(pdaDec, ptaDec) #space-time data matrix

#Space-time data format
ptada = t(ptada1[59:65,]) #2009-2015 data
colnames(ptada)<-2009:2015
rownames(ptada)<-c("Darwin", "Tahiti")
ptada #6 year of data for two stations
#       2009 2010 2011 2012 2013 2014 2015
#Darwin  0.5 -2.3 -2.2  0.3  0.3  0.1 -0.4
#Tahiti -0.7  2.5  1.9 -0.7  0.4 -0.8 -1.3
svdptd = svd(ptada) #SVD for the 2-by-6 matrix
U=round(svdptd$u, digits=2)
U
#[1,] -0.66 0.75
#[2,]  0.75 0.66
D=round(diag(svdptd$d), digits=2)
D
#[1,]  4.7 0.00
#[2,]  0.0 1.42
V =round(svdptd$v, digits=2)
t(V)
#[1,] -0.18  0.72  0.61 -0.15 0.02 -0.14 -0.15
#[2,] -0.06 -0.06 -0.28 -0.17 0.34 -0.32 -0.82


#R SVD analysis for the weighted SOI from SLP data
#For the entire period of 1951-2015
setwd("/Users/sshen/LinAlg")
Pda<-read.table("data/PSTANDdarwin.txt", header=F)
dim(Pda) 
#[1] 65 13 #Monthly Darwin data from 1951-2015
pdaDec<-Pda[,13] #Darwin Dec standardized SLP anomalies data
Pta<-read.table("data/PSTANDtahiti.txt", header=F)
ptaDec=Pta[,13] #Tahiti Dec standardized SLP anomalies
ptada1 = cbind(pdaDec, ptaDec) #space-time data matrix

#Space-time data format
ptada = t(ptada1) #2009-2015 data
colnames(ptada)<-1951:2015
rownames(ptada)<-c("Darwin", "Tahiti")
ptada[,1:6] #6 year of data for two stations
#       1951 1952 1953 1954 1955 1956
#Darwin  1.4  0.9  0.6 -0.8  0.2 -1.6
#Tahiti  0.1 -1.1 -0.1  1.5  1.8  0.1
#df = data.frame(ptada)
dim(ptada)
#[1]  2 65
svdptd = svd(ptada) #SVD for the 2-by-65 matrix
U=round(svdptd$u, digits=2)
U
#      [,1] [,2]
#[1,] -0.75 0.66
#[2,]  0.66 0.75
D=round(diag(svdptd$d), digits=2)
D
#      [,1] [,2]
#[1,] 10.02 0.00
#[2,]  0.00 7.09
V =round(svdptd$v, digits=2)
t(V)[,1:5] #The first five year from 1951-1955
#      [,1]  [,2]  [,3] [,4] [,5]
#[1,] -0.10 -0.14 -0.05 0.16 0.10
#[2,]  0.14 -0.03  0.05 0.08 0.21

#
plot(1951:2015, -V[,1], type = 'o', 
     ylab = "PC", xlab = 'Year',
     main = "Tahiti-Darwin SLP Principal Components",
     col = 'black', ylim = c(-0.45, 0.45))
lines(1951:2015, -V[,2], type = 'l', 
      lty = 2, col = 'purple')
legend(1950, 0.5, lwd = 2, c("PC1", "PC2"), col = c('blue', 'Purple'),
       lty = c(1,2), bty="n")
#El Nino samples
points(c(1982, 1997), c(-V[32,1], -V[47,1]), 
       pch = 16, col = 'red')
text(1982, -V[32,1] + 0.07, "EN", col = 'red')
text(1997, -V[47,1] + 0.07, "EN", col = 'red')
#La Nina samples
points(c(1975, 2010), c(-V[25,1], -V[60,1]), 
       pch = 16, col = 'blue')
text(1975, -V[25,1] - 0.07, "LN", col = 'blue')
text(2010, -V[60,1] - 0.07, "EN", col = 'blue')

t = 1951:2015
y = -3*V[,1] + 0.3*cumsum(V[,1]) - 0.3*cumsum(V[,2])
plot(t, y, type = 'o', 
     ylab = "PC", xlab = 'Year',
     main = "Tahiti-Darwin SLP Principal Components",
     col = 'black', ylim = c(-1, 1))
lines(1951:2015, -V[,2], type = 'l', 
      lty = 2, col = 'purple')
legend(1950, 0.5, lwd = 2, c("PC1", "PC2"), col = c('blue', 'Purple'),
       lty = c(1,2), bty="n")
#El Nino samples
points(c(1982, 1997), c(-V[32,1], -V[47,1]), 
       pch = 16, col = 'red')
text(1982, -V[32,1] + 0.07, "EN", col = 'red')
text(1997, -V[47,1] + 0.07, "EN", col = 'red')
#La Nina samples
points(c(1975, 2010), c(-V[25,1], -V[60,1]), 
       pch = 16, col = 'blue')
text(1975, -V[25,1] - 0.07, "LN", col = 'blue')
text(2010, -V[60,1] - 0.07, "EN", col = 'blue')


t= 1951:2015
plot(t, cumsum(V[,1]), type = 'l', 
     ylim = c(-1,1), ylab = "Cumsum index")
lines(t, cumsum(V[,2]), 
      type = 'l', col = 'red') 
lines(t, -cumsum(V[,1]) + cumsum(V[,2]), 
      type = 'l', col = 'blue') 

#For a better computer display
library(plotly)
PC = data.frame(1951:2015, V)
colnames(PC) = c("Year", "PC1", "PC2")
plot_ly(data = PC, x = ~Year, y = ~(-PC1), 
        type = 'lines+markers') %>%
  layout(yaxis = list(title = 'PC1'))


#Minors and co-factor
minors <- function(b){
  n <- nrow(b)
  a <- matrix(NA, n, n)
  for(i in 1:n)
    for(j in 1:n)
      a[i, j] = det(b[-i, -j])
  a
}
b = matrix(c(2,3,5,6,7,1,9,4,5), 
           nrow = 3, ncol = 3)
minors(b) 
#     [,1] [,2] [,3]
#[1,]   31   -5  -32
#[2,]   21  -35  -28
#[3,]  -39  -19   -4


cofactors <- function(b) (-1)^(row(b)+col(b)) *minors(b)
cofactors(b)
#     [,1] [,2] [,3]
#[1,]   31    5  -32
#[2,]  -21  -35   28
#[3,]  -39   19   -4
#Adjoint matrix, aka, adjugate of a square matrix
A <- matrix(c(1,4,5,3,7,2,2,8,3),nrow=3,ncol=3)
A
#install.packages('RConics')
library(RConics)
B <- adjoint(A)
B
#B = det(A) * inverse of A
C = det(A)*solve(A)
C - B


#Characteristic polynomials, Cayley-Hamilton theorem, Jordon forms
A = matrix(c(1,1,0,2), nrow = 2)
A
det(A)
# [1] 2
library(pracma)
cpA = charpoly(A, info = TRUE)
cpA
#[1]  1 -3  2
cpA$cp
#[1]  1 -3  2
#1 x^2 - 3 x + 2
cpA$inv
#= solve(A)
cpA$det
#= det(A)

cpA$inv %*% A
#zapsmall(cpA$inv %*% A)
# is equal to a 2-by-2 identity matrix

#Cayley-Hamilton theorem
#cp(x) = a2 x^2 + a1 x + a0
#cp(A) = a2 A^2 + a1 A + a0 I = 0
library(expm)
A%^%2 - 3*A%^%1 + 2*diag(2) 


#Jordan normal form
#install.packages("pracma")
library(pracma)
A = matrix(c(2, 1, 0, 2), nrow = 2)
A
jordan(A)

#install.packages("pracma")
library(pracma)
A <- matrix(c(2, 2, 0, 2), nrow = 2, byrow = TRUE)
J <- jordan(A)
J
#     [,1] [,2]
#[1,]    2    1
#[2,]    0    2


#Jordan matrix
#install.packages('mcompanion')
library(mcompanion)
Jordan_matrix(4, 2)
eigen(Jordan_matrix(4, 2))
#$values
#[1] 4 4  #repeated eigenvalues 2 times

Jordan_matrix(5, 3)
eigen(Jordan_matrix(5, 3))
#$values
#[1] 5 5 5 #repeated eigenvalues 3 times

Jordan_matrix(6, 1)
Jordan_matrix(6, 4)
eigen(Jordan_matrix(6, 4))
#$values
#[1] 6 6 6 6 #repeated eigenvalues 4 times

A = Jordan_matrix(4, 3)
A
cpA = charpoly(A, info = TRUE)
cpA
#$cp
#[1]   1 -12  48 -64
library(expm)
A%^%3 - 12*A%^%2 + 48*A%^%1- 64*diag(3) 

#Cayley-Hamilton theorem

## a matrix with the above 3 blocks
Jordan_matrix(c(4, 5, 6), c(2, 3, 1))
## a matrix with a 2x2 Jordan block for eval 1 and two simple 0 eval's
m <- make_mcmatrix(eigval = c(1), co = cbind(c(1,1,1,1), c(0,1,0,0)),
                   dim = 4, len.block = c(2))
m
m.X <- cbind(c(1,1,1,1), c(0,1,0,0), c(0,0,1,0), c(0,0,0,1))
m.X
m.J <- cbind(c(1,0,0,0), c(1,1,0,0), rep(0,4), rep(0,4))
m.J
from_Jordan(m.X, m.J) # == m
m.X %*% m.J %*% solve(m.X) # == m
all(m == from_Jordan(m.X, m.J)) && all(m == m.X %*% m.J %*% solve(m.X))
## TRUE
## which column(s) in m.X correspond to 1st Jordan block?
chain_ind(1, c(2,1,1)) # c(1, 2) since 2x2 Jordan block
## which column(s) in m.X correspond to 2nd Jordan block?
chain_ind(2, c(2,1,1)) # 3, simple eval
## which column(s) in m.X correspond to 1st and 2nd Jordan blocks?
chain_ind(c(1, 2), c(2,1,1)) # c(1,2,3)
## non-contiguous subset are ok:
chain_ind(c(1, 3), c(2,1,1)) # c(1,2,4)
## split the chains into a list of matrices
chains_to_list(m.X, c(2,1,1))
m.X %*% m.J
m %*% m.X # same
all(m.X %*% m.J == m %*% m.X) # TRUE
m %*% c(1,1,1,1) # = c(1,1,1,1), evec for eigenvalue 1
m %*% c(0,1,0,0) # gen.e.v. for eigenvalue 1
## indeed:
all( m %*% c(0,1,0,0) == c(0,1,0,0) + c(1,1,1,1) ) # TRUE
## m X = X jordan.block
cbind(c(1,1,1,1), c(0,1,0,0)) %*% cbind(c(1,0), c(1,1))
m %*% cbind(c(1,1,1,1), c(0,1,0,0))


#Image analysis using SVD
#Ref: imager: an R package for image processing
#https://dahtah.github.io/imager/imager.html 

setwd("~/mathmodel")
#install.packages('imager') #run this if not installed yet
library(imager)
dat <- load.image('data/SamPhoto.png') #355 KB file size
dim(dat)
#[1] 430 460   1   3
#430 rows and 460 columns, 430*460 = 197,800 pixels
#1 photo frame, 3 RGB colors 
#If a video, 1 will become 150 frames or more

dat[1:3, 1:4,1,1]
#Show part of the daat

#plot the color figure
plot(dat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'A Color Photo of Sam')

#Make the photo black-and-white
graydat = grayscale(dat)
dim(graydat)
#[1] 430 460   1   1
#430 rows and 460 columns,  1 photo frame, 1 grayscale [0, 1]
#plot the gray b/w photo
plot(graydat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'B/W Gray Sam')
#Plot the color and b/w photos together
par(mfrow = c(1, 2))
plot(dat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'Color Sam')
plot(graydat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'B/W Sam')
dev.off()

#SVD analysis of the grayscale data
svdDat = svd(graydat)
SVDd = svdDat$d
percentD = 100*(SVDd^2)/sum(SVDd^2)
cumpercentD = cumsum(percentD)
modeK = 1:length(SVDd)
dev.off()
plot(modeK[1:20], percentD[1:20], 
     type = 'o', col = 'blue',
     xlab = 'Mode number', pch = 16,
     ylab = 'Percentage of mode variance',
     main = 'Scree Plot of SVD B/W Photo Data')
K = 20
lam = (svdDat$d)^2
lamK=lam[1:K]
lamK
#setEPS() #Plot the figure and save the file
#postscript("fig0608.eps", width = 6, height = 4)
par(mar=c(4,4,2,4), mgp=c(2.2,0.7,0))
plot(1:K, 100*lamK/sum(lam), ylim=c(0,100), type="o", 
     ylab="Percentage of Variance [%]",
     xlab="EOF Mode Number", 
     cex.lab=1.2, cex.axis = 1.1, lwd=2, 
     main="Scree Plot of the First 20 Eigenvalues")
legend(3,30, col=c("black"),lty=1, lwd=2.0,
       legend=c("Percentange Variance"),bty="n",
       text.font=2,cex=1.0, text.col="black")
par(new=TRUE)
plot(1:K,cumsum(100*lamK/sum(lam)),
     ylim = c(90,100), type="o",
     col="blue",lwd=2, axes=FALSE,
     xlab="",ylab="")
legend(3,94.5, col=c("blue"),lty=1,lwd=2.0,
       legend=c("Cumulative Percentage Variance"),bty="n",
       text.font=2,cex=1.0, text.col="blue")
axis(4, col="blue", col.axis="blue", mgp=c(3,0.7,0))
mtext("Cumulative Variance [%]",col="blue", 
      cex=1.2, side=4,line=2)

#Reconstructing b/w photo from SVD modes
U = svdDat$u
V = svdDat$v
D = diag(svdDat$d)
dim(D)

#100% reconstruction using all the modes
recon = U%*%D%*%t(V)
dim(recon)
#[1] 430 460
image(recon)

dev.off()
par(mfrow = c(2, 2))
kR = 430 #Recon from all 413 modes
R430 = as.cimg(U[,1:kR]%*%D[1:kR, 1:kR]%*%t(V[, 1:kR]))
plot(R430, main = "All 430 modes")
kB = 20 #The first 20 modes
R20 = as.cimg(U[,1:kB]%*%D[1:kB, 1:kB]%*%t(V[, 1:kB]))
plot(R20, main = "The first 20 modes")
k = 3 #Recon from the first 3 modes
R3 = as.cimg(U[,1:k]%*%D[1:k, 1:k]%*%t(V[, 1:k]))
plot(R3, main = "The first 3 modes")
k1 = 21; k2 = 100 #Recon from 21st to 100th modes
Rk1_k2 = as.cimg(U[,k1:k2]%*%D[k1:k2, k1:k2]%*%t(V[, k1:k2]))
plot(Rk1_k2, main = "21st to 100th modes")
dev.off()

#Three monotone photos and their color photo
dim(dat) #4D array of a color photo
#[1] 430 460   1   3
dev.off()
par(mfrow = c(2, 2))
R = as.cimg(apply(t(dat[,, 1, 1]), 1, rev))
plot(R, main = 'Monotone color R')
G = as.cimg(apply(t(dat[,, 1, 2]), 1, rev))
plot(G, main = 'Monotone color G')
B = as.cimg(apply(t(dat[,, 1, 3]), 1, rev))
plot(B, , main = 'Monotone color B')
#Bind the three monotone photos into one 
trippy = imappend(list(R,G,B), "c") 
dim(trippy) #color figure data
#[1] 430 460   1   3
plot(trippy, main ='Blend RGB Colors')
dev.off()


################################################################
#
# Chapter 3: Basics of Machine Learning
#
################################################################

#
#tWCSS calculation for N = 3 and K = 2
N = 3; K = 2
mydata <- matrix(c(1, 1, 2, 1, 3, 3.5), 
                 nrow = N, byrow = TRUE)
x1 = mydata[1, ]
x2 = mydata[2, ]
x3 = mydata[3, ]

#Case C1 = (P1, P2)
c1 = (mydata[1, ] + mydata[2, ])/2
c2 = mydata[3, ]
tWCSS = norm(x1 - c1, type = '2')^2 + 
  norm(x2 - c1, type = '2')^2 + 
  norm(x3 - c2, type = '2')^2
tWCSS
#[1] 0.5

#Case C1 = (P1, P3)
c1 = (mydata[1, ] + mydata[3, ])/2
c2 = mydata[2, ]
norm(x1 - c1, type = '2')^2 + 
  norm(x3 - c1, type = '2')^2 + 
  norm(x2 - c2, type = '2')^2
#[1] 5.125

#Case C1 = (P2, P3)
c1 = (mydata[2, ] + mydata[3, ])/2
c2 = mydata[1, ]
norm(x2 - c1, type = '2')^2 + 
  norm(x3 - c1, type = '2')^2 + 
  norm(x1 - c2, type = '2')^2
#[1] 3.625

#The case C1 = (P1, P2) can be quickly found by 
kmeans(mydata, 2) 
#Clustering vector:
#[1] 1 1 2 #points P1, P2 in C1
#
#
#
# R plot Fig. 9.1: K-means for N = 3 and K = 2
N = 3 #The number of data points
K = 2 #Assume K clusters
mydata = matrix(c(1, 1, 2, 1, 3, 3.5), 
                nrow = N, byrow = TRUE)
mydata
Kclusters = kmeans(mydata, K) 
Kclusters #gives the K-means results, 
#e.g., cluster centers and WCSS 
#Cluster means:
#[,1] [,2]
#1  1.5  1.0
#2  3.0  3.5
#Within cluster sum of squares by cluster:
#  [1] 0.5 0.0
Kclusters$centers
par(mar = c(4,4,2.5,0.5))
plot(mydata[,1], mydata[,2], lwd = 2,
     xlim =c(0, 4), ylim = c(0, 4),
     xlab = 'x', ylab = 'y', col = c(2, 2, 4),
     main = 'K-means clustering for 
     three points and two clusters',
     cex.lab = 1.4, cex.axis = 1.4)
points(Kclusters$centers[,1], Kclusters$centers[,2],
       col = c(2, 4), pch = 4)
text(1.5, 0.8, bquote(C[1]), col = 'red', cex = 1.4)
text(3.2, 3.5, bquote(C[2]), col = 'skyblue', cex = 1.4)
text(1, 1.2, bquote(P[1]), cex = 1.4, col = 'red')
text(2, 1.2, bquote(P[2]), cex = 1.4, col = 'red')
text(3, 3.3, bquote(P[3]), cex = 1.4, col = 'skyblue')

#
#R for Fig. 9.2: K-means clustering for 2001 daily weather 
#data at Miami International Airport, Station ID USW00012839
dat = read.csv("data/MiamiIntlAirport2001_2020.csv", 
               header=TRUE)
dim(dat)
#[1] 7305   29
tmin = dat[,'TMIN']
wdf2 = dat[,'WDF2']

# plot the scatter diagram Tmin vs WDF2
#setEPS() #Plot the data of 150 observations
#postscript("fig0902a.eps",  width=5, height=5)
par(mar=c(4.5, 4.5, 2, 4.5))
plot(tmin[1:365], wdf2[1:365], 
     pch =16, cex = 0.5,
     xlab = 'Tmin [deg C]',
     ylab = 'Wind Direction [deg]', 
     type = 'p')
title('(a) 2001 Daily Miami Tmin vs WDF2', cex.main = 0.9, line = 1)
axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
     lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
mtext('Wind Direction', side = 4, line =3)
dev.off()


plot(tmin[1:365], type="o")
plot(wdf2[1:365], type="o")

#K-means clustering 
K = 2 #assuming K = 2, i.e., 2 clusters
mydata = cbind(tmin[1:365], wdf2[1:365])
fit = kmeans(mydata, K) # K-means clustering
fit$totss

#Output the coordinates of the cluster centers
fit$centers 
#1 18.38608 278.8608
#2 21.93357 103.9161

fit$tot.withinss # total WCSS
#[1] 457844.9 for # the value may vary for each run

#Visualize the clusters by kmeans.ani()
mycluster <- data.frame(mydata, fit$cluster)
names(mycluster)<-c('Tmin [deg C]', 
                    'Wind Direction [deg]',
                    'Cluster')
library(animation)
kmeans_animation <- function() {
  par(mar = c(4.5, 4.5, 2, 4.5))
  kmeans.ani(mycluster, centers = K, pch=1:K, col=1:K, hints = '')
  title(main = "(b) K-means Clusters for Daily Tmin vs WDF2", cex.main = 0.8)
  axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
       lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
  mtext('Wind Direction', side = 4, line = 3)
}

# Run this to see the animation
kmeans_animation()

# Run this to save the animation
saveGIF(kmeans_animation(), movie.name = "kmeans_anim_miami_temp_wind.gif", interval = 0.1)


#
#R plot Fig. 9.3: tWCSS(K) and pWCSS(K)
dat = read.csv("data/MiamiIntlAirport2001_2020.csv", 
               header=TRUE)
dim(dat)
#[1] 7305   29
tmin = dat[,'TMIN']
tmax = dat[,'TMAX']
wdf2 = dat[,'WDF2']
mydata = cbind(tmin[1:365], wdf2[1:365])
twcss = c() # Create empty array
for(K in 1:8){
  mydata=cbind(tmax[1:365], wdf2[1:365])
  twcss[K] = kmeans(mydata, K)$tot.withinss # Save twcss at index K
}
twcss # 8 different twcss
par(mar = c(4.5, 6, 2, 0.5))
par(mfrow=c(1,2))
plot(twcss/100000, type = 'o', lwd = 2,
     xlab = 'K', ylab = bquote('tWCSS [ x' ~  10^5 ~ ']'),
     main = '(a) The elbow principle from tWCSS scores',
     cex.lab = 1.5, cex.axis = 1.5)
points(2, twcss[2]/100000, pch =16, cex = 3, col = 'blue')
text(4, 5, 'Elbow at K = 2', cex = 1.5, col = 'blue')
#compute percentage of variance explained
pWCSS = 100*(twcss[1]- twcss)/twcss[1] 
plot(pWCSS, type = 'o', lwd = 2,
     xlab = 'K', ylab = 'pWCSS [percent]',
     main = '(b) The knee principle from pWCSS variance',
     cex.lab = 1.5, cex.axis = 1.5)
points(2, pWCSS[2], pch =16, cex = 3, col = 'blue')
text(4, 80, 'Knee at K = 2', cex = 1.5, col = 'blue')
dev.off()


for(K in 1:8){
  mydata=cbind(tmax[2:366], wdf2[2:366])
  clusterK <- kmeans(mydata, K) # 5 cluster solution
  twcss[K] = fit$tot.withinss
}
twcss
plot(twcss, type = 'o')
pvariance = 100*(twcss[1]- twcss)/twcss[1] 
plot(pvariance, type = 'o')

#R plot Fig. 9.3b: pWCSS(K) for the knee principle


######################
#scale the data because of different units
#equivalent to standardized anomalies
mydata = data.frame(cbind(tmin[2:366], wdf2[2:366]))
tminS = scale(tmin[2:366])
wdf2S = scale(wdf2[2:366])
K = 2 #assuming K = 2, i.e., 2 clusters
mydataS = data.frame(cbind(tminS, wdf2S))
clusterK = kmeans(mydataS, K) # K-means clustering
#Output the scaled coordinates of the cluster centers
clusterK$centers 
#1 -0.9165034  1.7527775
#2  0.2252159 -0.4307167
clusterK$tot.withinss # total WCSS
#[1] 377.103 for # the value may vary for each run
#Centers in non-scaled units
Centers = matrix( nrow = 2, ncol = 2) 
Centers[,1] =
  clusterK$centers[,1] * attr(tminS, 
                              "scaled:scale") + attr(tminS, "scaled:center")
Centers[,2] =
  clusterK$centers[,2] * attr(wdf2S, 
                              "scaled:scale") + attr(wdf2S, "scaled:center")
Centers
#[1,] 17.14306 282.5000
#[2,] 22.15427 107.2014

#Plot clusters using convex hull
i = 1
N = 365
mycluster = clusterK$cluster
plotdat = cbind(1:N, mydata, mycluster)
X = plotdat[which(mycluster == i), 1:3]
plot(X[,2:3], pch = 16, cex = 0.5,
     xlim = c(0, 30), ylim = c(0, 365),
     col = 'red')
grid(5, 5)
hpts <- chull(X[, 2:3])
hpts <- c(hpts, hpts[1])
lines(X[hpts, 2:3], col = 'red')
for(j in 1:length(X[,1])){
  text(X[j,2], X[j,3] + 8, paste("", X[j,1]), 
       col = 'red', cex = 0.8)
}



d1 = tminS[1:365] * attr(tminS, "scaled:scale") + attr(tminS, "scaled:center")
d1 - tmin[2:366]
#Visualize the clusters by fviz_cluster()
library(factoextra)
fviz_cluster(clusterK, data = mydataS, stand = FALSE,
             main = 'K-means Clustering for the Miami Daily Tmin vs WDF2')

#Visualize the clusters by kmeans.ani()
mycluster <- data.frame(mydata, clusterK$cluster)
names(mycluster)<-c('Daily Tmin [deg C]', 
                    'Wind Direction [deg]',
                    'Cluster')
library(animation)
par(mar = c(4.5, 4.5, 2, 0.5))
kmeans.ani(mycluster, centers = K, pch=1:K, col=1:K,
           hints = '')
title(main=
        "(b) K-means for Tmin vs WDF2")

#Visualize the clusters by kmeans.ani()
mycluster <- data.frame(mydata, fit$cluster)
names(mycluster)<-c('Daily Tmin [deg C]', 
                    'Wind Direction [deg]',
                    'Cluster')
library(animation)
par(mar = c(4.5, 4.5, 2, 0.5))
kmeans.ani(mycluster, centers = K, pch=1:K, col=1:K,
           hints = '')
title(main=
        "(b) K-means for Tmin vs WDF2")

#
#
#R plot Fig. 9.4: Convex hull for a cluster 
setwd("~/climstats")
dat = read.csv("data/MiamiIntlAirport2001_2020.csv", 
               header=TRUE)
dim(dat)
#[1] 7305   29
tmin = dat[,'TMIN']
wdf2 = dat[,'WDF2']
#K-means clustering 
K = 2 #assuming K = 2, i.e., 2 clusters
mydata = cbind(tmin[2:366], wdf2[2:366]) #2001 data
clusterK = kmeans(mydata, K) # K-means clustering
mycluster <- data.frame(mydata, clusterK$cluster)
plotdat = cbind(1:N, mycluster)

par(mar = c(4.5, 6, 2, 4.5))#set up the plot margin
i = 2 # plot Cluster 1
N = 365 #Number of data points
X = plotdat[which(mycluster[,3] == i), 1:3]
colnames(X)<-c('Day', 'Tmin [deg C]', 'Wind Direction [deg]')
plot(X[,2:3], pch = 16, cex = 0.5, col = i,
     xlim = c(0, 30), ylim = c(0, 365),
     main = 'Cluster 1 of Miami Tmin vs WDF2' )
grid(5, 5)
#chull() finds the boundary points of a convex hull
hpts = chull(X[, 2:3]) 
hpts1 = c(hpts, hpts[1]) #close the boundary
lines(X[hpts1, 2:3], col = i)
for(j in 1:length(X[,1])){
  text(X[j,2], X[j,3] + 8, paste("", X[j,1]), 
       col = i, cex = 0.8)
} #Put the data order on the cluster
axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
     lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
mtext('Wind Direction', side = 4, line =3)


## Support Vector Machines
#
#
#R plot Fig. 9.5: Maximum difference between two points
x = matrix(c(1, 1, 3, 3), 
           ncol = 2, byrow = TRUE)#Two points
y= c(-1, 1) #Two labels -1 and 1
#Plot the figure and save it as a .eps file
# setEPS() 
# postscript("fig0905.eps", height=7, width=7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y + 3, pch = 19, 
     xlim = c(-2, 6), ylim = c(-2, 6),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5, 
     main = "Maximum Difference Between Two Points")
axis(2, at = (-2):6, tck = 1, lty = 2, 
     col = "grey", labels = NA)
axis(1, at = (-2):6, tck = 1, lty = 2, 
     col = "grey", labels = NA)
segments(1, 1, 3, 3)
arrows(1, 1, 1.71, 1.71, lwd = 2,
       angle = 9, length= 0.2)
text(1.71, 1.71-0.4, quote(bold('n')), cex = 1.5)
arrows(4, 0, 4 + 0.5, 0 + 0.5, lwd = 3,
       angle = 15, length= 0.2, col = 'green' )
text(4.7, 0.5 -0.2, quote(bold('w')), 
     cex = 1.5, col = 'green')
x1 = seq(-2, 6, len = 31)
x20 = 4 - x1
lines(x1, x20, lwd = 1.5, col = 'purple')
x2m = 2 - x1
lines(x1, x2m, lty = 2, col = 2)
x2p = 6 - x1
lines(x1, x2p, lty = 2, col = 4)
text(1-0.2,1-0.5, bquote(P[1]), cex = 1.5, col = 2)
text(3+0.2,3+0.5, bquote(P[2]), cex = 1.5, col = 4)
text(1-0.2,1-0.5, bquote(P[1]), cex = 1.5, col = 2)
text(0,4.3, 'Separating Hyperplane', 
     srt = -45, cex = 1.2, col = 'purple')
text(1.5, 4.8, 'Positive Hyperplane', 
     srt = -45, cex = 1.2, col = 4)
text(-1, 3.3, 'Negative Hyperplane', 
     srt = -45, cex = 1.2, col = 2)
dev.off()

#
#
# SVM with two points
x = matrix(c(1, 1, 3, 3), 
           ncol = 2, byrow = TRUE)
y = c(1, -1) # two categories 1 and -1

library(e1071)
dat = data.frame(x, y = as.factor(y))
svm2P = svm(y ~ ., data = dat, 
            kernel = "linear", cost = 10, 
            scale = FALSE, 
            type = 'C-classification')
svm2P #This is the trained SVM

# Find hyperplane, normal vector, and SV (wx + b = 0)
w = t(svm2P$coefs) %*% svm2P$SV 
w # [1,] -0.5 -0.5

b = svm2P$rho
b # [1] -2

2/norm(w, type = "2")

#
#
#R plot Fig. 9.6: SVM for three points
#training data x
x = matrix(c(1, 1, 2, 1, 3, 3.5), 
           ncol = 2, byrow = TRUE)
y = c(1, 1, -1) #two categories 1 and -1
plot(x, col = y + 3, pch = 19,
     xlim = c(-2, 8), ylim = c(-2, 8))
library(e1071)
dat = data.frame(x, y = as.factor(y))
svm3P = svm(y ~ ., data = dat, 
            kernel = "linear", cost = 10, 
            scale = FALSE, 
            type = 'C-classification')
svm3P #This is the trained SVM

xnew = matrix(c(0.5, 2.5, 4.5, 4), 
              ncol = 2, byrow = TRUE) #New data
predict(svm3P, xnew) #prediction using the trained SVM
# 1  2 
# 1 -1

# Find hyperplane, normal vector, and SV (wx + b = 0)
w = t(svm3P$coefs) %*% svm3P$SV 
w
#[1,] -0.2758621 -0.6896552

b = svm3P$rho
b
#[1] -2.241379

2/norm(w, type ='2') #maximum margin of separation
#[1] 2.692582

x1 = seq(0, 5, len = 31)
x2 = (b - w[1]*x1)/w[2]
x2p = (1 + b - w[1]*x1)/w[2]
x2m = (-1 + b - w[1]*x1)/w[2]
x20 = (b - w[1]*x1)/w[2]


#plot the SVM results
# setEPS() 
# postscript("fig0906.eps", height=7, width=7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y + 3, pch = 19,
     xlim = c(0, 6), ylim = c(0, 6),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'SVM for three points labeled in two categories')
axis(2, at = (-2):8, tck = 1, lty = 2, 
     col = "grey", labels = NA)
axis(1, at = (-2):8, tck = 1, lty = 2, 
     col = "grey", labels = NA)
lines(x1, x2p, lty = 2, col = 4)
lines(x1, x2m, lty = 2, col = 2)
lines(x1, x20, lwd = 1.5, col = 'purple')
xnew = matrix(c(0.5, 2.5, 4.5, 4), 
              ncol = 2, byrow = TRUE)
points(xnew, pch = 18, cex = 2)
for(i in 1:2){
  text(xnew[i,1] + 0.5, xnew[i,2] , paste('Q',i),
       cex = 1.5, col = 6-2*i)
}
text(2.2,5.8, "Two blue points and a red point 
are training data for an SVM ", 
     cex = 1.5, col = 4)
text(3.5,4.7, "Two black diamond points 
         are to be predicted by the SVM", 
     cex = 1.5)
dev.off()

#
#
#--------R plot Fig. 9.7: SVM for many points---------
#Training data x and y
x = matrix(c(1, 6, 2, 8, 3, 7.5, 1, 8, 4, 9, 5, 9, 
             3, 7, 5, 9, 1, 5,
             5, 3, 6, 4, 7, 4,   8, 6, 9, 5, 10, 6, 
             5, 0, 6, 5, 8, 2, 2, 2, 1, 1), 
           ncol = 2, byrow = TRUE)
y= c(1, 1, 1, 1, 1, 1, 
     1, 1, 1,
     2, 2, 2, 2, 2, 2 ,
     2, 2, 2, 2, 2)

# Train SVM based on x & y
library(e1071)
dat = data.frame(x, y = as.factor(y))
svmP = svm(y ~ ., data = dat, 
           kernel = "linear", cost = 10, 
           scale = FALSE, 
           type = 'C-classification')
svmP

# Number of Support Vectors:  3
svmP$SV #SVs are #x[9,], x[17,], x[19,]
#9   1  5
#17  6  5
#19  2  2

# Find SVM parameters: w, b, SV (wx+c=0)
# In essence this finds the hyper plane that separates our points
w = t(svmP$coefs) %*% svmP$SV 
w
#[1,] -0.39996 0.53328

b <- svmP$rho
b
#[1] 1.266573

# Maximum margin
2/norm(w, type ='2')
#[1] 3.0003

x1 = seq(0, 10, len = 31)
x2 = (b - w[1]*x1)/w[2]
x2p = (1 + b - w[1]*x1)/w[2]
x2m = (-1 + b - w[1]*x1)/w[2]
x20 = (b - w[1]*x1)/w[2]

# Plot the svm results
# setEPS() 
# postscript("fig0907.eps", height=7, width=7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y + 9, pch = 19, cex =1.5,
     xlim = c(0, 10), ylim = c(0, 10),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'SVM application to many points of two labels')
axis(2, at = 0:10, tck = 1, lty = 3, 
     col = "grey", labels = NA)
axis(1, at = 0:10, tck = 1, lty = 3, 
     col = "grey", labels = NA)
lines(x1, x2p, lty = 2, col = 10)
lines(x1, x2m, lty = 2, col = 11)
lines(x1, x20, lwd = 1.5, col = 'purple')

# Deg angle of the hyperplane
thetasvm = atan(-w[1]/w[2])*180/pi
thetasvm
#[1] 36.8699 #36.9

# Linear equations for the hyperplanes
delx = 1.4
dely = delx * (-w[1]/w[2])
text(5 + 2*delx, 6.5 + 2*dely, bquote(bold(w%.%x) - b == 0), 
     srt = thetasvm, cex = 1.5, col = 'purple')
text(5 - delx, 7.6 - dely, bquote(bold(w%.%x) - b == 1), 
     srt = thetasvm, cex = 1.5, col = 10)
text(5, 4.8, bquote(bold(w%.%x) - b == -1), 
     srt = thetasvm, cex = 1.5, col = 11)

# Normal direction of the hyperplanes
arrows(2, 3.86, 2 + w[1], 4 + w[2], lwd = 2,
       angle = 15, length= 0.1, col = 'blue' )
text(2 + w[1] + 0.4, 4 + w[2], bquote(bold(w)), 
     srt = thetasvm, cex = 1.5, col = 'blue')

# New data points to be predicted
xnew = matrix(c(0.5, 2.5, 7, 2, 6, 9), 
              ncol = 2, byrow = TRUE)
points(xnew, pch = 17, cex = 2)
predict(svmP, xnew)
#1 2 3 
#2 2 1

for(i in 1:3){
  text(xnew[i,1], xnew[i,2] - 0.4 , 
       paste('Q',i), cex = 1.5)
}

dev.off()


#
#
#R plot Fig. 9.8: R.A. Fisher data of three iris species
setwd('~/climstats')

data(iris) #read the data already embedded in R
dim(iris)
#[1] 150   5

iris[1:2,] # Check the first two rows of the data
#    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#1          5.1         3.5          1.4         0.2  setosa
#2          4.9         3.0          1.4         0.2  setosa

str(iris) # Check the structure of the data
#'data.frame':	150 obs. of  5 variables:
#$ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#$ Species     : Factor w/ 3 levels "setosa","versicolor",...

# setEPS() #Plot the data of 150 observations
# postscript("fig0908.eps",  width=7, height=5)
par(mar= c(4.5, 4.5, 2.5, 0.2))
plot(iris[,1], type = 'o', pch = 16, cex = 0.5, ylim = c(-1, 9), 
     xlab = 'Sorted order of the flowers for measurement', 
     ylab = 'Length or width [cm]',
     main = 'R. A. Fisher data of irish flowers', col = 1, 
     cex.lab = 1.3, cex.axis = 1.3)
lines(iris[,2], type = 'o', pch = 16, cex = 0.5, col=2)
lines(iris[,3], type = 'o', pch = 16, cex = 0.5, col=3)
lines(iris[,4], type = 'o', pch = 16, cex = 0.5, col=4)
legend(-5, 9.5, legend = c('Sepal length', 'Sepal width', 
                          'Petal length', 'Petal width'), 
       col = 1:4, lty = 1, lwd = 2, bty = 'n', 
       y.intersp = 0.8, cex = 1.2)
text(25, -1, 'Setosa 1-50', cex = 1.3)
text(75, -1, 'Versicolor 51-100', cex = 1.3)
text(125, -1, 'Virginica 101 - 150', cex = 1.3)

dev.off()

#
#
#R code of the RF experiment on the Fisher iris data 
# install.packages("randomForest") 
library(randomForest)
#set.seed(8)  # run this line to get the same result
#randomly select 120 observations as training data 
train_data = iris[train_id, ]

dim(train_data)
#[1] 120   5

#use the remaining 30 as the new data for prediction
new_data = iris[-train_id, ]
dim(new_data)
#[1] 30  5

#train the RF model
classifyRF = randomForest(x = train_data[, 1:4],
                          y = train_data[, 5], ntree = 800)
classifyRF #output RF training result
#Type of random forest: classification
#Number of trees: 800
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 4.17%
#Confusion matrix:
#  		      setosa versicolor virginica class.error
#setosa         41          0         0  0.00000000
#versicolor      0         34         2  0.05555556
#virginica       0          3        40  0.06976744

plot(classifyRF, 
     main='RF model error rate for each tree') 
#plot the errors vs RF trees Fig. 9.9a

#This plots the error rate data in the 
#matrix named classifyRF$err.rate
errRate = classifyRF$err.rate
dim(errRate)
#[1] 800   4

#Fig. 9.9a is a plot for this matrix data
#Fig. 9.9a can also be plotted by the following code
tree_num = 1:800
plot(tree_num, errRate[,1], 
     type = 's', col='black',
     ylim = c(0, 0.2),
     xlab = 'Trees', ylab = 'Error rate',
     main = 'RF model error rate for each tree',
     cex.axis = 1.3, cex.lab = 1.3)
lines(tree_num, errRate[,2], lwd =1.8,
      lty = 2, type = 's', col='red')
lines(tree_num, errRate[,3], lwd =1.8,
      lty = 3, type = 's', col='green')
lines(tree_num, errRate[,4], lwd =1.8,
      lty = 4, type = 's', col='skyblue')
legend(400, 0.21, lwd = 2.5,
       legend = c('OOB','Setosa', 'Vericolor', 'Virginica'),
       col=c('black', 'red', 'green', 'skyblue'), 
       lty=1:4, cex=1.2, y.intersp = 0.6,
       text.font = 2,  box.lty=0)

classifyRF$importance #classifyRF$ has many outputs
#             MeanDecreaseGini
#Sepal.Length         8.313131
#Sepal.Width          1.507188
#Petal.Length        31.075960
#Petal.Width         38.169763

#plot the importance result Fig. 9.9b
varImpPlot(classifyRF, sort = FALSE, 
           lwd = 1.5, pch = 16,
           main = 'Importance plot of the RF model',
           cex.axis = 1.3, cex.lab = 1.3)

classifyRF$confusion

#RF prediction for the new data based on the trained trees
predict(classifyRF, newdata = new_data[,1:4])
# 2          4         10         11         12         14 
#setosa     setosa     setosa     setosa     setosa     setosa 

#It got two wrong: 120 versicolor and 135 versicolor 

#Another version of the randomForest() command
anotherRF = randomForest(Species ~ ., 
                         data = train_data, ntree = 500)

#
#
#R plot Fig. 9.10: RF regression for ozone data
library(randomForest)
airquality[1:2,] #use R's RF benchmark data "airquality"
#  Ozone Solar.R Wind Temp Month Day
#1    41     190  7.4   67     5   1
#2    36     118  8.0   72     5   2
dim(airquality)
#[1] 153   6
ozoneRFreg = randomForest(Ozone ~ ., data = airquality, 
                          mtry = 2, ntree = 500, importance = TRUE, 
                          na.action = na.roughfix)
#na.roughfix allows NA to be replaced by medians 
#to begin with when training the RF trees
ozonePred = ozoneRFreg$predicted #RF regression result
t0 = 1:153
n1 = which(airquality$Ozone > 0) #positions of data
n0 = t0[-n1] #positions of missing data
ozone_complete = ozone_filled= airquality$Ozone 
ozone_complete[n0] = ozonePred[n0] #filled by RF
ozone_filled = ozonePred #contains the RF reg result
ozone_filled[n1] <- NA #replace the n1 positions by NA 
t1 = seq(5, 10, len = 153) #determine the time May - Sept

par(mfrow = c(2, 1))
par(mar = c(3, 4.5, 2, 0.1))
plot(t1, airquality$Ozone,   
     type = 'o', pch = 16, cex = 0.5, ylim = c(0, 170), 
     xlab = '', ylab = 'Ozone [ppb]', xaxt="n",
     main = '(a) Ozone data: Observed (black) and RF filled (blue)', 
     col = 1, cex.lab = 1.3, cex.axis = 1.3) 
MaySept = c("May","Jun", "Jul", "Aug", "Sep")
axis(side=1, at=5:9, labels = MaySept, cex.axis = 1.3)
points(t1, ozone_filled, col = 'blue', 
       type = 'o', pch = 16, cex = 0.5)#RF filled data

#Plot the complete data
par(mar = c(3, 4.5, 2, 0.1))
plot(t1, ozone_complete, 
     type = 'o', pch = 16, cex = 0.5, ylim = c(0, 170), 
     xlab = '', ylab = 'Ozone [ppb]', 
     xaxt="n", col = 'brown', 
     main = '(b) RF-filled complete ozone data series', 
     cex.lab = 1.3, cex.axis = 1.3)
MaySept = c("May","Jun", "Jul", "Aug", "Sep")
axis(side=1, at=5:9, labels = MaySept, cex.axis = 1.3)


#R plot of Fig. 9.11: A tree in a random forest 
#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)

setwd('~/climstats')
setEPS() #Plot the data of 150 observations
postscript("fig0911.eps",  width=6, height=6)
par(mar = c(0, 2, 1, 1))
iris_tree = rpart( Species ~. , data = train_data)
rpart.plot(iris_tree, 
           main = 'An decision tree for the RF training data')
dev.off()

#
#
#R code for NN recruitment decision and Fig. 9.12
TKS = c(20,10,30,20,80,30)
CSS = c(90,20,40,50,50,80)
Recruited = c(1,0,0,0,1,1)
# Here, you will combine multiple columns or features into a single set of data
df = data.frame(TKS, CSS, Recruited)
df

require(neuralnet) # load 'neuralnet' library

# fit neural network
set.seed(123)
nn = neuralnet(Recruited ~ TKS + CSS, data = df, 
               hidden=5, act.fct = "logistic",
               linear.output = FALSE)
plot(nn) #Plot Fig 9.12: A neural network

TKS=c(30,51,72) #new data for decision
CSS=c(85,51,30) #new data for decision
test=data.frame(TKS,CSS)
Predict=neuralnet::compute(nn,test)
Predict$net.result #the result is probability
#[1,] 0.99014936
#[2,] 0.58160633
#[3,] 0.01309036

# Converting probabilities into decisions
ifelse(Predict$net.result > 0.5, 1, 0) #threshold = 0.5
#[1,]    1
#[2,]    1
#[3,]    0

#print bias and weights
nn$weights[[1]][[1]]
#print the last bias and weights before decision
nn$weights[[1]][[2]]
#print the random start bias and weights
nn$startweights 
#print error and other technical indices of the nn run
nn$result.matrix #results data


#
#
#R plot Fig. 9.13: Curve of a logistic function
z = seq(-2, 4, len = 101)
k = 3.2
z0 = 1
setEPS() #Automatically saves the .eps file
postscript("fig0913.eps", height=5, width=7)
par(mar = c(4.2, 4.2, 2, 0.5))
plot(z, 1/(1 + exp(-k*(z - z0))),
     type = 'l', col = 'blue', lwd =2,
     xlab = 'z', ylab = 'g(z)',
     main = bquote('Logistic function for k = 3.2,'~z[0]~'= 1.0'),
     cex.lab = 1.3, cex.axis = 1.3)
dev.off()

#
#
#R NN code for the Fisher iris flower data
#Ref: https://rpubs.com/vitorhs/iris 
data(iris) #150-by-5 iris data
#attach True or False columns to iris data
iris$setosa = iris$Species == "setosa" 
iris$virginica = iris$Species == "virginica"
iris$versicolor = iris$Species == "versicolor"
p = 0.9 # assign 50% of data for training
train.idx = sample(x = nrow(iris), size = p*nrow(iris))
train = iris[train.idx,] #determine the training data
test = iris[-train.idx,] #determine the test data
dim(train) #check the train data dimension
#[1] 75  8

#training a neural network
library(neuralnet)
#use the length, width, True and False data for training 
iris.nn = neuralnet(setosa + versicolor + virginica ~ 
                      Sepal.Length + Sepal.Width + 
                      Petal.Length + Petal.Width, 
                    data = train, hidden=c(10, 10), 
                    rep = 5, err.fct = "ce", 
                    linear.output = F, lifesign = "minimal", 
                    stepmax = 1000000, threshold = 0.001)

plot(iris.nn, rep="best") #plot the neural network

#Prediction for the rest data
prediction = neuralnet::compute(iris.nn, test[,1:4])
#prediction$net.result is 75-by-3 matrix
prediction$net.result[1:3,] #print the first 3 rows
#1    1 7.882440e-13 2.589459e-42
#2    1 7.890670e-13 2.586833e-42
#6    1 7.848803e-13 2.600133e-42
#The largest number in a row indicates species

#find which column is for the max of each row
pred.idx <- apply(prediction$net.result, 1, which.max)
pred.idx[70:75] #The last 6 rows
#140 142 144 148 149 150 
#3   3   3   3   3   3 

#Assign 1 for setosa, 2 for versicolor, 3 for virginica
predicted <- c('setosa', 'versicolor', 'virginica')[pred.idx]
predicted[1:6] #The prediction result
#[1] "setosa" "setosa" "setosa" "setosa" "setosa" "setosa"

#Create confusion matrix: table(prediction,observation)
table(predicted, test$Species)
#predicted    setosa versicolor virginica
#setosa         27          0         0
#versicolor      0         19         2
#virginica       0          1        26


################################################################
#
# Chapter 4: Matrix Applications to Regression Models
#
################################################################

#Plot Fig. 4.1: Colorado temperature lapse rate: R code
x= c(
  1671.5, 1635.6, 2097.0, 1295.4, 1822.7, 2396.9, 2763.0, 1284.7,
  1525.2, 1328.6, 1378.9, 2323.8, 2757.8, 1033.3, 1105.5, 1185.7,
  2343.9, 1764.5, 1271.0, 2347.3, 2094.0, 2643.2, 1837.9, 1121.7)
y= c(
  22.064, 23.591, 18.464, 23.995, 20.645, 17.175, 13.582, 24.635,
  22.178, 24.002, 23.952, 16.613, 13.588, 25.645, 25.625, 25.828,
  17.626, 22.433, 24.539, 17.364, 17.327, 15.413, 22.174, 24.549)
setEPS() # save the .eps figure 
postscript("fig0401.eps",  width = 8)
par(mar=c(4.5,4.5,2.5,0.5))
plot(x,y, 
     xlab="Elevation [m]",
     ylab=expression("Temperature ["~degree~"C]"),
     main="Colorado Elevation and July Tmean: 1981-2010 Average",
     cex.lab=1.5, cex.axis=1.5, cex.main =1.2)
reg=lm(y~x)
reg
#(Intercept)            x  
# 33.476216    -0.006982   #-7.0 degC/1km
summary(reg)
#R-squared:  0.9631
abline(reg,lwd=3)
text(2100, 25.5, 
     expression("Temperature lapse rate: 7.0"~degree~"C/1.0km"), 
     cex=1.5)
text(2350, 24, "y = 33.48 - 0.0070 x", cex=1.5)
text(2350, 22.5,"R-squared = 0.96", cex=1.5)
dev.off()

#R code for the Colorado TLR regression analysis
lm(y ~ x)
#(Intercept)            x  
#33.476216    -0.006982 
reg = lm(y ~ x)
round(reg$residuals, digits = 5)
#       1        2        3        4        5        6 
#0.25792  1.53427 -0.37129 -0.43697 -0.10542  0.43358 
#7        8        9       10       11       12 
#-0.60335  0.12833 -0.64953 -0.19817  0.10302 -0.63880 
#13       14       15       16       17       18 
#-0.63366 -0.61692 -0.13283  0.63012  0.51454  1.27623 
#19       20       21       22       23       24 
#-0.06333  0.27628 -1.52923  0.39122  1.52971 -1.09572 

mean(reg$residuals)
#[1] 1.62043e-17

xa = x - mean(x)
sum(xa*reg$residuals)
#[1] -2.83773e-13

sum((reg$residuals)^2)/(length(y) -2)
#[1] 0.6096193


#
#Plot Fig. 4.2: R code
setEPS() 
postscript("fig0402.eps",  height = 5, width = 8)
par(mar=c(0.0,0.5,0.0,0.5))
plot(0,0, xlim=c(0,5.2), ylim=c(0,2.2),
     axes = FALSE, xlab="", ylab="")
arrows(0,0,4,0, angle=5, code=2, lwd=3, length=0.5)
arrows(4,0,4,2, angle=5, code=2, lwd=3, length=0.5)
arrows(0,0,4,2, angle=5, code=2, lwd=3, length=0.5)
arrows(5,0,4,2, angle=7, code=2, lwd=2, lty=3, length=0.5)
arrows(0,0,5,0, angle=7, code=2, lwd=2, lty=3, length=0.5)
arrows(3,0,4,2, angle=7, code=2, lwd=2, lty=3, length=0.5)
arrows(0,0,3,0, angle=7, code=2, lwd=2, lty=3, length=0.5)
segments(3.9,0, 3.9, 0.1)
segments(3.9, 0.1, 4.0, 0.1)
text(2,0.2, expression(hat(b)~bold(x)[a]), cex=2)
text(2,1.2, expression(bold(y)[a]), cex=2)
text(4.1,1, expression(bold(e)), cex=2)
text(3.8,0.6, expression(paste("Shortest ",bold(e))), 
     cex=1.5, srt=90)
text(3.4,1.1, expression(paste("Longer ",bold(e))), 
     cex=1.5, srt=71)
text(4.6,1.1, expression(paste("Longer ",bold(e))), 
     cex=1.5, srt=-71)
dev.off()

#
#
#R code for estimating regression slope b
#
#Method 1: Using vector projection
xa = x - mean(x)  #Anomaly the x data vector
nxa = sqrt(sum(xa^2)) #Norm of the anomaly data vector
ya = y - mean(y)
nya=sqrt(sum(ya^2))
sum(ya*(xa/nxa))/nxa #Compute b
#[1] -0.006981885  #This is an estimate for b

#Method 2:  Using correlation: R code
corxy=cor(xa, ya) #Compute the correlation between xa and ya
corxy
#[1] -0.9813858 #Very high correlation
corxy*nya/nxa #Compute b
#[1] -0.006981885 #This is an estimate for b

#
#R code for computing MV 
var(reg$fitted.values)
#[1] 15.22721 
#Or another way
yhat = reg$fitted.values
var(yhat)
#[1] 15.22721 
#Or still another way
n = 24
sum((yhat - mean(yhat))^2)/(n-1)
#[1] 15.22721 

#R code for computing YV 
sum((y - mean(y))^2)/(n-1)
# [1] 15.81033  
#Or another way
var(y)
#[1] 15.81033

#R code for computing R-squared value 
var(reg$fitted.values)/var(y)
#[1] 0.9631181  #This is the R-squared value

cor(x,y)
#[1] -0.9813858
(cor(x,y))^2
#[1] 0.9631181 #This is the R-squared value


#
#
qt(c(.025, .975), df=22)
#[1] -2.073873  2.073873

summary(reg)

-0.0069818 - 2.073873 * 0.0002913
#[1] -0.007585919
-0.0069818 + 2.073873 * 0.0002913
#[1] -0.006377681

33.4762157 - 2.073873*0.5460279
#[1] 32.34382
33.4762157 + 2.073873*0.5460279
#[1] 34.60861

(-0.0069818-(-0.0073))/0.0002913
#[1] 1.092345


#R plot Fig. 4.3: Confidence interval of a regression model
setwd("/Users/sshen/climstats")
#Confidence interval of the linear model
x1 = seq(max(x), min(x),len=100)
n = 24
xbar = mean(x)
reg = lm(y ~ x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modTLR = 33.476216 + -0.006982*x1
xbar = mean(x)
Sxx = sum((x-xbar)^2)
CIupperModel= modTLR + qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIlowerModel= modTLR - qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIupperResponse= modTLR + qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)
CIlowerResponse= modTLR - qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0403.eps", height = 8, width = 8)
par(mar=c(4.5,4.5,2.0,0.5))
plot(x,y, 
     ylim=c(10,30), xlim=c(1000,3000),
     xlab="Elevation [m]",
     ylab=bquote("Temperature ["~degree~"C]"),
     main="Colorado Elevation and July Tmean: 1981-2010 Average",
     cex.lab=1.5, cex.axis=1.5)
lines(x1,CIupperModel,type="l",col='red')
lines(x1,CIlowerModel,type="l",col='red')
lines(x1,CIupperResponse,type="l",col='blue')
lines(x1,CIlowerResponse,type="l",col='blue')
abline(reg,lwd=3)
text(2280, 26, 
     bquote("Temperature lapse rate: 7.0"~degree~"C/km"), 
     cex=1.5)
text(2350, 27.5,"R-squared = 0.96", cex=1.5)
text(2350, 29, "y= 33.48 - 0.0070 x", cex=1.5)
text(1600, 15,"Blue lines: CI of July Tmean RV", 
     col="blue", cex=1.5)
text(1600, 13.5,"Red lines: CI of the fitted model", 
     col="red", cex=1.5)
dev.off()


#
#Plot Fig. 4.4: R code
reg = lm(y~x)
setEPS() #Plot the figure and save the file
postscript("fig0404.eps", height = 4.5, width = 8)
par(mar=c(4.5,4.5,2.0,0.5))
plot(x, reg$residuals, pch=5,
     ylim=c(-2,2), xlim=c(1000,2800),
     xlab="Elevation [m]",
     ylab=bquote("Residual Temp ["~degree~"C]"),
     main="Residuals of the Colorado 1981-2010 July Tmean vs. Elevation",
     cex.lab=1.5, cex.axis=1.5, cex.main = 1.2)
dev.off()

#
#Plot Fig. 4.5: R code
reg = lm(y ~ x)
setEPS() #Plot the figure and save the file
postscript("fig0405.eps", height = 6, width = 6)
par(mar=c(4.5,4.5,2.0,0.5))
qqnorm(reg$residuals, pch=5,
       main="QQ-Normal Plot for the Colorado TLR Residuals",
       cex.lab = 1.4, cex.axis = 1.4)
qqline(reg$residuals, lty=2)
dev.off()

#
#R code for the DW-test for independence
#install.packages("lmtest")
library(lmtest)
ElevTemp=cbind(x,y, 1:24)
#Sort the data for ascending elevation
ElevTemp=ElevTemp[order(x),]
reg1=lm(ElevTemp[,2] ~ ElevTemp[,1])
dwtest(reg1)
#DW = 2.3072, p-value = 0.7062


#
#R code for the Mann-Kendall test
#install.packages("trend")
library(trend)
ElevTemp=cbind(x, y, 1:24)
#Sort the data for ascending elevation
ElevTemp=ElevTemp[order(x),]
reg1=lm(ElevTemp[,2] ~ ElevTemp[,1])
ElevTemp[,3]=reg1$residuals
mk.test(ElevTemp[,3])
#data:  ElevTemp[, 3]
#z = 0.47128, n = 24, p-value = 0.6374
mk.test(ElevTemp[,2])
#z = -5.9779, n = 24, p-value = 2.261e-09

#
#
lat=c(39.9919, 38.4600, 39.2203, 38.8236, 39.2425, 37.6742,
      39.6261, 38.4775, 40.6147, 40.2600, 39.1653, 38.5258,
      37.7717, 38.0494, 38.0936, 38.0636, 37.1742, 38.4858,
      38.0392, 38.0858, 40.4883, 37.9492, 37.1786, 40.0583)
lon=c(
  -105.2667, -105.2256, -105.2783, -102.3486, -107.9631, -106.3247,
  -106.0353, -102.7808, -105.1314, -103.8156, -108.7331, -106.9675,
  -107.1097, -102.1236, -102.6306, -103.2153, -105.9392, -107.8792,
  -103.6933, -106.1444, -106.8233, -107.8733, -104.4869, -102.2189)

#R code for the TLR multivariate linear regression
elev = x; temp = y #The x and y data were entered earlier
dat = cbind(lat, lon, elev, temp)
datdf = data.frame(dat)
datdf[1:2,] #Show the data of the first two stations
#      lat       lon   elev     temp
# 39.9919 -105.2667 1671.5 22.064
# 38.4600 -105.2256 1635.6 23.591

#Multivariate linear regression
reg=lm(temp ~ lat + lon + elev, data = datdf) 
summary(reg)  #Display the regression results 
#                  Estimate   Std. Error   t value  Pr(>|t|)    
#(Intercept) 36.4399561  9.4355746   3.862 0.000971 ***
#  lat         -0.4925051  0.1320096  -3.731 0.001319 ** 
#  lon         -0.1630799  0.0889159  -1.834 0.081564 .  
#  elev        -0.0075693  0.0003298 -22.953 7.67e-16 ***
#Residual standard error: 0.6176 on 20 degrees of freedom
#Multiple R-squared:  0.979

round(reg$coefficients, digits=5)
colnames(datdf) <- c('x1', 'x2', 'x3', 'y')
reg=lm(y ~ x1 + x2 + x3, data = datdf)
reg

#
# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

CIupperModelr= modT + 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModelr= modT - 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponser= modT + 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponser= modT - 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["*degree*"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x,CIupperModel,type="l",col='red')
lines(x,CIlowerModel,type="l",col='red')
lines(x,CIupperResponse,type="l",col='blue')
lines(x,CIlowerResponse,type="l",col='blue')

lines(x,CIupperModelr,type="l", lty = 3, col='red')
lines(x,CIlowerModelr,type="l", lty = 3, col='red')
lines(x,CIupperResponser,type="l",lty = 3, col='blue')
lines(x,CIlowerResponser,type="l",lty = 3, col='blue')

text(1940, 0.5, 
     bquote("Linear trend: 0.7348"*degree*"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["*degree*"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#
#
#Kolmogorov-Smirnov (KS) test for normality
library(fitdistrplus)
resi_mean = mean(reg$residuals)
resi_sd = sd(reg$residuals)
test_norm = rnorm(length(reg$residuals), 
                  mean = 0, sd = 1)
testvar = (reg$residuals - resi_mean)/resi_sd
ks.test(testvar, test_norm)
#D = 0.057554, p-value = 0.9754
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

#degrees of freedom and critical t values
rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof #[1] 5.183904 effective degrees of freedom
qt(.975, df=137) #[1] 1.977431 critical t value
qt(.975, df=5) #[1] 2.570582 critical t value

#
#Plot Fig. 4.7: R code
#Polynomial fitting by multiple linear  regression
x1=x
x2=x1^2
x3=x1^3
dat3=data.frame(cbind(x1,x2,x3,y))
reg3 = lm(y ~ x1 + x2 + x3, data=dat3)
# simply use
# reg3 = lm(y ~ x + I(x^2) + I(x^3))
setEPS() #Plot the figure and save the file
postscript("fig0407.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  type="o", xaxt="n",
     cex.lab=1.4, cex.axis=1.4, xlab="Year", 
     ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
lines(x, predict(reg3), col="black", lwd=3)
reg3
#(Intercept)           x1           x2           x3  
#-1.426e+03    2.333e+00   -1.271e-03    2.308e-07  
text(1940, 0.3, 
     "The third order polynomial fit",
     col="black",cex=1.4)
text(1880, 0.58, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x1, reg3$residuals, 
     pch=5, cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Residuals ["~degree~"C]"))
text(1880, 0.32, "(b)", cex=1.4)
dev.off()


######Below is for an earlier Fig. 4.6 in the book draft 
# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = x1 = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x1) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x1)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x1
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x1, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x1,CIupperModel,type="l",col='red')
lines(x1,CIlowerModel,type="l",col='red')
lines(x1,CIupperResponse,type="l",col='blue')
lines(x1,CIlowerResponse,type="l",col='blue')
text(1940, 0.5, 
     bquote("Linear trend: 0.7348"~degree~"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x1, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["~degree~"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#Kolmogorov-Smirnov (KS) test for normality
ks.test(reg$residuals,test_norm)
#D = 0.086331, p-value = 0.6782
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof # [1] 5.183904
qt(.975, df=137) #[1] 1.977431
qt(.975, df=5) #[1] 2.570582


################## New Fig. 4.6

# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x,CIupperModel,type="l",col='red')
lines(x,CIlowerModel,type="l",col='red')
lines(x,CIupperResponse,type="l",col='blue')
lines(x,CIlowerResponse,type="l",col='blue')
text(1940, 0.5, 
     bquote("Linear trend: 0.7348"~degree~"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["~degree~"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#Kolmogorov-Smirnov (KS) test for normality
ks.test(reg$residuals,test_norm)
#D = 0.086331, p-value = 0.6782
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof # [1] 5.183904
qt(.975, df=137) #[1] 1.977431
qt(.975, df=5) #[1] 2.570582

### New ### Fig. 4.6

# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

CIupperModelr= modT + 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModelr= modT - 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponser= modT + 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponser= modT - 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

setEPS() #Plot figure Fig. 4.6 and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x,CIupperModel,type="l",col='red')
lines(x,CIlowerModel,type="l",col='red')
lines(x,CIupperResponse,type="l",col='blue')
lines(x,CIlowerResponse,type="l",col='blue')

lines(x,CIupperModelr,type="l", lty = 3, col='red')
lines(x,CIlowerModelr,type="l", lty = 3, col='red')
lines(x,CIupperResponser,type="l",lty = 3, col='blue')
lines(x,CIlowerResponser,type="l",lty = 3, col='blue')

text(1940, 0.5, 
     bquote("Linear trend: 0.7348"~degree~"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["~degree~"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#Kolmogorov-Smirnov (KS) test for normality
ks.test(reg$residuals, test_norm)
#D = 0.086331, p-value = 0.6782
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof # [1] 5.183904
qt(.975, df=137) #[1] 1.977431
qt(.975, df=5) #[1] 2.570582


yautoc = acf(y) #Auto-correlation function
yautoc$acf[2]
#[1] 0.9270817

cor(y[1:138], y[2:139])
#[1] 0.9459649

length(y)
#[1] 139

#Confidence interval of the linear model
x1 = x
xbar = mean(x1)
reg8018 = lm(y ~ x1)
SSE = sum((reg8018$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x1
xbar = mean(x)
Sxx = sum((x-xbar)^2)
CIupperModel= modT + qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIlowerModel= modT - qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIupperResponse= modT + qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)
CIlowerResponse= modT - qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)

plot(x1, y, 
     xlab="Elevation [m]",
     ylab=bquote("Temperature ["~degree~"C]"),
     main="Colorado Elevation and July Tmean: 1981-2010 Average",
     cex.lab=1.5, cex.axis=1.5)
lines(x1,CIupperModel,type="l",col='red')
lines(x1,CIlowerModel,type="l",col='red')
lines(x1,CIupperResponse,type="l",col='blue')
lines(x1,CIlowerResponse,type="l",col='blue')


#Linear regression diagnostics: Check the assumptions
#Normality test by Q-Q plot
par(mfrow=c(1,1))
par(mar=c(4.5,4.5,2.5,0.7))
qqnorm(reg8018$residuals, pch=5,
       ylim=c(-0.4,0.4),xlim=c(-3,3),
       main="QQ-Normal Plot for the NOAAGlobalTemp: 1880-2018",
       cex.lab=1.4, cex.axis=1.4)
qqline(reg8018$residuals, lty=2, col = 'red')


#Kolmogorov-Smirnov (KS) test for normality
resi_sd=sd(reg8018$residuals)
resi_mean=mean(reg8018$residuals)
test_norm = rnorm(length(x1), mean = resi_mean,
                  sd=resi_sd)
ks.test(reg8018$residuals,test_norm)
#D = 0.086331, p-value = 0.6782
#Conclusion: Normal distribution is not rejected. 

#Check independence by Durbin-Watson (DW) test 
dwtest(reg8018)
#DW = 0.45235, p-value < 2.2e-16
#Conclusion: There is a significant serial correlation. 

yautoc = acf(y)
yautoc$acf[2]

cor(y,y)

#rm(list=ls()) #R forgets all the defined variables 

############################################################################
#
# Appendix A: A Tutorial of R and R Studio
#
############################################################################

1+4
#[1] 5  This is the result
2+pi/4-0.8
#[1] 1.985398 
x<-1
y<-2
z<-4
t<-2*x^y-z
t
#[1] -2
u=2        # "=" sign and "<-" are almost equivalent
v=3        # The text behind the "#" sign is comments
u+v
#[1] 5
sin(u*v)    # u*v = 6 in the sine function is considered radian by R 
#[1] -0.2794155

#Enter temperature data in c() 
tmax <- c(77, 72, 75, 73, 66, 64, 59)
#Show the data
tmax
#[1] 77 72 75 73 66 64 59

#Generate a sequence using different methods
seq(1,8)
seq(8)
seq(1,8, by=1)
seq(1,8, length=8)
seq(1,8, length.out =8)

#Define a function
samfctn <- function(x) x*x
samfctn(4)
#[1] 16
fctn2 <- function(x,y,z) x+y-z/2
fctn2(1,2,3)
#[1] 1.5 

#Plot temperature data
plot(1:7, c(77, 72, 75, 73, 66, 64, 59))

#More plot examples
plot(sin, -pi, 2*pi)   #plot the curve of y=sin(x) from -pi to 2 pi

square <- function(x) x*x   #Define a function
plot(square, -3,2)   # Plot the defined function

# Plot a 3D surface
x <- seq(-1, 1, length=100)
y <- seq(-1, 1, length=100)
z <- outer(x, y, function(x, y)(1-x^2-y^2))  
#outer (x,y, function) renders z function on the x, y grid
persp(x,y,z, theta=330) 
# yields a 3D surface with perspective angle 330 deg

#Contour plot
contour(x,y,z) #lined contours
filled.contour(x,y,z) #color map of contours

#Symbolic calculations by R

D(expression(x^2,'x'), 'x') 
# Take derivative of x^2 w.r.t. x 
2 * x #The answer is 2x

fx= expression(x^2,'x')  #assign a function  
D(fx,'x') #differentiate the function w.r.t. x
2 * x  #The answer is 2x

fx= expression(x^2*sin(x),'x') 
#Change the expression and use the same derivative command
D(fx,'x')
2 * x * sin(x) + x^2 * cos(x)

fxy = expression(x^2+y^2, 'x','y') 
#One can define a function of 2 or more variables
fxy #renders an expression of the function in terms of x and y
#expression(x^2 + y^2, "x", "y")
D(fxy,'x') #yields the partial derivative with respect to x: 2 * x
D(fxy,'y') #yields the partial derivative with respect to y: 2 * y

square = function(x) x^2
integrate (square, 0,1) 
#Integrate x^2 from 0 to 1 equals to 1/3 with details below
#0.3333333 with absolute error < 3.7e-15

integrate(cos,0,pi/2) 
#Integrate cos(x) from 0 to pi/2 equals to 1 with details below
#1 with absolute error < 1.1e-14

#Vectors and matrices by R

c(1,6,3,pi,-3) #c() gives a vector, considered a 4X1 column vector
#[1]  1.000000  6.000000  3.000000  3.141593 -3.000000
seq(2,6) #Generate a sequence from 2 to 6
#[1] 2 3 4 5 6
seq(1,10,2) # Generate a sequence from 1 to 10 with 2 increment 
#[1] 1 3 5 7 9
x=c(1,-1,1,-1)
x+1 #1 is added to each element of x
#[1] 2 0 2 0
2*x #2 multiplies each element of x
#[1]  2 -2  2 -2
x/2 # Each element of x is divided by 2
#[1]  0.5 -0.5  0.5 -0.5
y=seq(1,4)
x*y  # This multiplication * multiples each pair of elements 
#[1]  1 -2  3 -4
x%*%y #This is the dot product of two vectors and yields 
#     [,1]
#[1,]   -2
t(x)  # Transforms x into a row 1X4 vector
#     [,1] [,2] [,3] [,4]
#[1,]    1   -1    1   -1
t(x)%*%y #This is equivalent to dot product and forms 1X1 matrix
#     [,1]
#[1,]   -2
x%*%t(y) #This column times row yields a 4X4 matrix
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]   -1   -2   -3   -4
#[3,]    1    2    3    4
#[4,]   -1   -2   -3   -4
my=matrix(y,ncol=2) 
#Convert a vector into a matrix of the same number of elements
#The matrix elements go by column, first column, second, etc
#Commands matrix(y,ncol=2, nrow=2)  or matrix(y,2)  
#or matrix(y,2,2) does the same job
my
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
dim(my)  #find dimensions of a matrix
#[1] 2 2
as.vector(my) #Convert a matrix to a vector, again via columns
#[1] 1 2 3 4
mx <- matrix(c(1,1,-1,-1), byrow=TRUE,nrow=2)
mx*my #multiplication between each pair of elements
#     [,1] [,2]
#[1,]    1    3
#[2,]   -2   -4
mx/my #division between each pair of elements
#     [,1]       [,2]
#[1,]  1.0  0.3333333
#[2,] -0.5 -0.2500000
mx-2*my  
#     [,1] [,2]
#[1,]   -1   -5
#[2,]   -5   -9
mx%*%my #This is the real matrix multiplication in matrix theory
#     [,1] [,2]
#[1,]    3    7
#[2,]   -3   -7
det(my) #determinant
#[1] -2
myinv = solve(my) #yields the inverse of a matrix
myinv
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
myinv%*%my #verifies the inverse of a matrix
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
diag(my) #yields the diagonal vector of a matrix
#[1] 1 4
myeig=eigen(my) #yields eigenvalues and unit eigenvectors
myeig
myeig$values
#[1]  5.3722813 -0.3722813
myeig$vectors
#           [,1]       [,2]
#[1,] -0.5657675 -0.9093767
#[2,] -0.8245648  0.4159736
mysvd = svd(my) #SVD decomposition of a matrix M=UDV'
#SVD can be done for a rectangular matrix of mXn
mysvd$d
#[1] 5.4649857 0.3659662
mysvd$u
#           [,1]       [,2]
#[1,] -0.5760484 -0.8174156
#[2,] -0.8174156  0.5760484
mysvd$v
#           [,1]       [,2]
#[1,] -0.4045536  0.9145143
#[2,] -0.9145143 -0.4045536

ysol=solve(my,c(1,3)) 
#solve linear equations matrix %*% x = b
ysol  #solve(matrix, b)
#[1]  2.5 -0.5
my%*%ysol #verifies the solution
#     [,1]
#[1,]    1
#[2,]    3

#Simple statistics by R

x=rnorm(10) #generate 10 normally distributed numbers
x
#[1]  2.8322260 -1.2187118  0.4690320 -0.2112469  0.1870511
#[6]  0.2275427 -1.2619005  0.2855896  1.7492474 -0.1640900
mean(x)
#[1] 0.289474
var(x)
#[1] 1.531215
sd(x)
#[1] 1.237423
median(x)
#[1] 0.2072969
quantile(x)
#        0%        25%        50%        75%       100% 
#-1.2619005 -0.1994577  0.2072969  0.4231714  2.8322260 
range(x) #yields the min and max of x
#[1] -1.261900  2.832226
max(x)
#[1] 2.832226

boxplot(x) #yields the box plot of x
w=rnorm(1000)

summary(rnorm(12)) #statistical summary of the data sequence
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1.9250 -0.6068  0.3366  0.2309  1.1840  2.5750 

hist(w) 
#yields the histogram of 1000 random numbers with a normal distribution

#Linear regression and linear trend line
#2007-2016 data of the global temperature anomalies
#Source: NOAAGlobalTemp data
t=2007:2016
T=c(.36,.30, .39, .46, .33, .38, .42, .50, .66, .70)
lm(T ~ t) #Linear regression model of temp vs time
#(Intercept)            t  
#-73.42691      0.03673 
#Temperature change rate is 0.03673  deg C/yr or 0.37 deg C/decade
plot(t,T, type="o",xlab="Year",ylab="Temperature [deg C]",
     main="2007-2016 Global Temperature Anomalies 
     and Their Linear Trend [0.37 deg C/decade] ")
abline(lm(T ~ t), lwd=2, col="red") #Regression line

#The R packages and the datasets used in this book are 
#listed below and can be downloaded and installed first 
#before proceeding to the R codes in the rest of the book. 
#The R packages: 
#animation, chron, e1071, fields, ggplot2, lattice, 
#latticeExtra, maps, mapdata, mapproj, matrixStats, ncdf, 
#NLRoot, RColorBrewer, rgdal, rasterVis, raster, sp, TTR

#To load the package "animation", you can do 
#library(animation)

#You can also load all these packages in one shot
#using pacman
#install.packages("pacman")
library(pacman)
pacman::p_load(animation, chron, e1071, fields, ggplot2, lattice, 
               latticeExtra, maps, mapdata, mapproj, matrixStats, ncdf4, 
               NLRoot, RColorBrewer, rgdal, rasterVis, raster, sp, TTR)

#The zipped data file:
# https://cambridge.org/climatemathematics/data.zip

#On your computer, you can create a directory called 
#climmath under your user name. 
#The one used in the book is Users/sshen/climmath  
#You unzip the data and move the data folder under 
#the Users/sshen/climmath directory. 
#A data folder will created:  
#Users/sshen/climmath/data. 
#The data folder contains about 400 MB of data. 
#Place all the R codes in the directory  Users/sshen/climmath. 
#Then, you can run all the codes in this book after replacing sshen 
#by your user name on your own computer. 


################################################################
#
# Appendix B: Visualization of Matrices
#
################################################################


# R plot Fig. 1.1: A simple line graph of data
# go to your working directory
setwd("/Users/sshen/climstats") 
# read the data file from the folder named "data"
NOAAtemp = read.table(
  "data/aravg.ann.land_ocean.90S.90N.v4.0.1.201907.txt",
  header=FALSE) #Read from the data folder
# check the data matrix dimension
dim(NOAAtemp)
#[1] 140   6 
#140 years from 1880 to 2019
#2019 will be excluded since data only up to July 2019
#col1 is year, col2 is anomalies, col3-6 are data errors
# set the plot margins and the positions of labels
par(mar=c(3.5,3.5,2.5,1), mgp=c(2,0.8,0))
plot(NOAAtemp[1:139,1], NOAAtemp[1:139,2],
     type ="o", col="brown", lwd=3,
     main ="Global Land-Ocean Average Annual Mean 
     Surface Temperature Anomalies: 1880-2018",
     cex.lab=1.2,cex.axis=1.2,
     xlab="Year", 
     ylab=expression(
       paste("Temperature Anomaly [", degree,"C]"))
)

#R plot Fig. 1.2: Staircase chart of data
plot(NOAAtemp[1:139,1], NOAAtemp[1:139,2],
     type="s", #staircase curve for data
     col="black", lwd=2,
     main="Global Land-Ocean Average Annual Mean 
     Surface Temperature Anomalies: 1880-2018",
     cex.lab=1.2,cex.axis=1.2,
     xlab="year",
     ylab=expression(paste(
       "Temperature Anomaly [", degree,"C]"))
)

# R plot Fig. 1.3: A color bar chart of data
x <- NOAAtemp[,1]
y <- NOAAtemp[,2]
z <- rep(-99, length(x))
# compute 5-point moving average
for (i in 3:length(x)-2) z[i] = 
  mean(c(y[i-2],y[i-1],y[i],y[i+1],y[i+2]))
n1 <- which(y>=0); x1 <- x[n1]; y1 <- y[n1]
n2 <- which(y<0); x2 <- x[n2]; y2 <- y[n2]
x3 <- x[2:length(x)-2]
y3 <- z[2:length(x)-2]
plot(x1, y1, type="h", #bars for data
     xlim = c(1880,2016), lwd=3, 
     tck = 0.02,  #tck>0 makes ticks inside the plot
     ylim = c(-0.7,0.7), xlab="Year", col="red",
     ylab = expression(paste(
       "Temperature Anomaly [", degree,"C]")),
     main ="Global Land-Ocean Average Annual Mean 
     Surface Temperature Anomalies: 1880-2018",
     cex.lab = 1.2, cex.axis = 1.2)
lines(x2, y2, type="h",
      lwd = 3, tck = -0.02, col = "blue")
lines(x3, y3, lwd = 2)

#
#R code for computing statistical indices
setwd("/Users/sshen/climstats")
NOAAtemp = read.table(
  "data/aravg.ann.land_ocean.90S.90N.v4.0.1.201907.txt",
  header=FALSE)
temp2018=NOAAtemp[1:139,2] #use the temp data up to 2018
head(temp2018) #show the first six values
#[1] -0.370221 -0.319993 -0.320088 -0.396044 -0.458355 -0.470374
mean(temp2018) #mean
#[1] -0.1858632
sd(temp2018) #standard deviation
#[1] 0.324757
var(temp2018) #variance
#[1] 0.1054671
library(e1071) 
#This R library is needed to compute the following parameters
#install.packages("e1071") #if it is not in your computer
skewness(temp2018) 
#[1] 0.7742704
kurtosis(temp2018)
#[1] -0.2619131
median(temp2018)
#[1] -0.274434
quantile(temp2018,probs= c(0.05, 0.25, 0.75, 0.95))
#       5%        25%        75%        95% 
#  -0.5764861 -0.4119770  0.0155245  0.4132383 

#
#R plot Fig. 1.4. Histogram nad its fit
par(mar=c(3.5,3.5,2.5,1), mgp=c(2,0.8,0))
h <- hist(NOAAtemp[1:139, 2], 
          main="Histogram of 1880-2018 Temperature Anomalies",
          xlab=expression(paste(
            "Temperature anomalies [", degree, "C]")), 
          xlim=c(-1,1), ylim=c(0,30),
          breaks=10, cex.lab=1.2, cex.axis=1.2) 
xfit <- seq(-1, 1, length=100)
areat <- sum((h$counts)*diff(h$breaks[1:2]))#Normalization area
yfit <- areat*dnorm(xfit, 
                    mean=mean(NOAAtemp[1:139,2]), 
                    sd=sd(NOAAtemp[1:139,2]))
#Plot the normal fit on the histogram
lines(xfit, yfit, col="blue", lwd=3) 

#
# R plot Fig. 1.5: Box plot
boxplot(NOAAtemp[1:139, 2], ylim = c(-0.8, 0.8), 
        ylab=expression(paste(
          "Temperature anomalies [", degree, "C]")),
        width=NULL, cex.lab=1.2, cex.axis=1.2)

#
# R plot Fig. 1.6: Q-Q plot for the standardized 
# global average annual mean temperature anomalies
temp2018 <- NOAAtemp[1:139,2]
tstand <- (temp2018 - mean(temp2018))/sd(temp2018)
set.seed(101)
qn <- rnorm(139) #simulate 139 points by N(0,1)
qns <- sort(qn) # sort the points
qq2 <- qqnorm(qns,col="blue",lwd = 2)

setEPS() #Automatically saves the .eps file
postscript("fig0106.eps", height=7, width=7)
par(mar = c(4.5,5,2.5,1), xaxs = "i", yaxs = "i")
qt = qqnorm(tstand, 
            main = "Q-Q plot for the Standardized Global Average 
  Annual Mean Temperature Anomalies vs N(0,1)", 
            ylab="Quantile of Temperature Anomalies", 
            xlab="Quantile of N(0,1)", 
            xlim=c(-3,3), ylim = c(-3,3),
            cex.lab = 1.3, cex.axis = 1.3)
qqline(tstand, col = "red", lwd=3)
points(qq2$x, qq2$y, pch = 19, 
       col ="purple")
dev.off()

# R plot Fig. 1.7: Data line graph with a linear trend line
par(mar=c(3.5,3.5,2.5,1), mgp=c(2,0.8,0))
plot(NOAAtemp[1:139,1], NOAAtemp[1:139,2],
     type="l", col="brown", lwd=3,
     main=" Global Land-Ocean Average Annual Mean 
Surface Temperature Anomalies: 1880-2018",
     cex.lab=1.2,cex.axis=1.2,
     xlab="Year", 
     ylab=expression(paste(
       "Temperature Anomaly [", degree,"C]"))
)
abline(lm(NOAAtemp[1:139,2] ~ NOAAtemp[1:139,1]),
       lwd=3, col="blue")
lm(NOAAtemp[1:139,2] ~ NOAAtemp[1:139,1])
#       (Intercept)  NOAAtemp[1:139, 1]  
#-13.872921            0.007023 
#Trend 0.7023 degC/100a
text(1930, 0.5, 
     expression(paste("Linear trend: 0.7023",
                      degree,"C/100a")),
     cex = 1.5, col="blue")


#
# Rea read the netCDF data: NOAAGlobalTemp 
setwd("/Users/sshen/climstats")  
#install.packages("ncdf4")
library(ncdf4)
nc = ncdf4::nc_open("data/air.mon.anom.nc")
nc # describes details of the dataset
Lat <- ncvar_get(nc, "lat")
Lat # latitude data
#[1] -87.5 -82.5 -77.5 -72.5 -67.5 -62.5
Lon <- ncvar_get(nc, "lon")
Lon # longitude data
#[1]   2.5   7.5  12.5  17.5  22.5  27.5 
Time<- ncvar_get(nc, "time")
head(Time) # time data in Julian days
#[1] 29219 29250 29279 29310 29340 29371
library(chron) # convert Julian date to calendar date
nc$dim$time$units # .nc base time for conversion
#[1] "days since 1800-1-1 00:00:0.0"
month.day.year(29219,c(month = 1, day = 1, year = 1800))
#1880-01-01 # the beginning time of the dataset
tail(Time)
#[1] 79988 80019 80047 80078 80108 80139
month.day.year(80139,c(month = 1, day = 1, year = 1800))
#2019-06-01 # the end time of the dataset

# extract anomaly data in (lon, lat, time) coordinates
NOAAgridT <- ncvar_get(nc, "air") 
dim(NOAAgridT) # dimensions of the data array
#[1]  72   36 1674 #5-by-5, 1674 months from Jan 1880-Jun 2019


#
#Plot Fig. 1.8: Dec 2015 global surface temp anomalies map
library(maps)# requires maps package 
mapmat=NOAAgridT[,,1632]
# Julian date time 1632 corresponds to Dec 2015
mapmat=pmax(pmin(mapmat,6),-6) # put values in [-6, 6]
int=seq(-6, 6, length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 
                               'darkgreen', 'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
par(mar=c(3.5, 4, 2.5, 1), mgp=c(2.3, 0.8, 0))
filled.contour(Lon, Lat, mapmat, 
               color.palette=rgb.palette, levels=int,
               plot.title=title(main="NOAAGlobalTemp Anomalies: Dec 2015",
                                xlab="Latitude", ylab="Longitude", cex.lab=1.2),
               plot.axes={axis(1, cex.axis=1.2, las=1); 
                 axis(2, cex.axis=1.2, las=2);
                 map('world2', add=TRUE); grid()},
               key.title=title(main=expression(paste("[", degree, "C]"))),
               key.axes={axis(4, cex.axis=1.2)})


#Fig. 1.9 is plotted by Panoply
#
#R plot Fig. 1.10: Hovmoller diagram
library(maps)
mapmat=NOAAgridT[30,12:24,1309:1668]
#Longitude= 240 deg, Lat =[-30 30] deg
#Time=Jan 1989-Dec 2018: 30 years
mapmat=pmax(pmin(mapmat,2),-2) # put values in [-2,2]
par(mar=c(4,5,3,0))
int=seq(-2,2,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 
                               'darkgreen','green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
par(mar=c(3.5,3.5,2.5,1), mgp=c(2.4, 0.8, 0))
x = seq(1989, 2018, len=360)
y = seq(-30, 30, by=5)
filled.contour(x, y, t(mapmat), 
               color.palette=rgb.palette, levels=int,
               plot.title=title(main=
                                  "Hovmoller diagram of the NOAAGlobalTemp Anomalies",
                                xlab="Time",ylab="Latitude", cex.lab=1.2),
               plot.axes={axis(1, cex.axis=1.2); 
                 axis(2, cex.axis=1.2); 
                 map('world2', add=TRUE);grid()},
               key.title=title(main = 
                                 expression(paste("[", degree, "C]"))),
               key.axes={axis(4, cex.axis=1.2)})


#
#R read a 4-Dimensional netCDF file
#setwd("/Users/sshen/climstats")
library(ncdf4)
# read GODAS data 1-by-1 deg, 40 levels, Jan-Dec 2015
nc=ncdf4::nc_open("data/godas2015.nc")
nc
Lat <- ncvar_get(nc, "lat")
Lon <- ncvar_get(nc, "lon")
Level <- ncvar_get(nc, "level")
Time <- ncvar_get(nc, "time")
head(Time)
#[1] 78527 78558 78586 78617 78647 78678

library(chron)
month.day.year(78527,c(month = 1, day = 1, year = 1800))
# 2015-01-01
# potential temperature pottmp[lon, lat, level, time] 

godasT <- ncvar_get(nc, "pottmp")
dim(godasT)
#[1] 360 418  40  12, 
#i.e., 360 lon, 418 lat, 40 levels, 12 months=2015

t(godasT[246:250, 209:210, 2, 12]) 
#Dec level 2 (15-meter depth) water temperature [K] of
#a few grid boxes over the eastern tropical Pacific
#        [,1]     [,2]     [,3]     [,4]     [,5]
#[1,] 300.0655 299.9831 299.8793 299.7771 299.6641
#[2,] 300.1845 300.1006 299.9998 299.9007 299.8045

#
# R plot Fig. 1.11: The ocean potential temperature
# the 20th layer from surface: 195 meters depth
# compute 2015 annual mean temperature at 20th layer
library(maps)
climmat=matrix(0,nrow=360,ncol=418)
sdmat=matrix(0,nrow=360,ncol=418)
Jmon<-1:12
for (i in 1:360){
  for (j in 1:418){
    climmat[i,j] = mean(godasT[i,j,20,Jmon]); 
    sdmat[i,j]=sd(godasT[i,j,20,Jmon]) 
  }
}
int=seq(273,298,length.out=81)
rgb.palette=colorRampPalette(c('black','blue',
                               'darkgreen','green', 'white','yellow',
                               'pink','red','maroon'), interpolate='spline')
#setwd("/Users/sshen/climstats")
setEPS() # save the .eps figure file 
postscript("fig0111.eps", height = 5, width = 10)
par(mar=c(3.5, 3.5, 2.5, 0), mgp=c(2, 0.8, 0))
filled.contour(Lon, Lat, climmat, 
               color.palette=rgb.palette, levels=int,
               plot.title=title(main=
                                  "GODAS 2015 Annual Mean Temperature at 195 [m] Depth Level",
                                xlab="Longitude",ylab="Latitude",
                                cex.lab=1.3, cex.axis=1.3),
               plot.axes={axis(1); axis(2); map('world2', add=TRUE);grid()},
               key.title=title(main="[K]"))
dev.off()


#Jingle Bells Music in R
#by Keith McNulty in 2018
#https://paulvanderlaken.com/2017/12/18/jingle-bells-in-r/

if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)

#
#
#Jingle Bells Music in R
#by Keith McNulty in 2018
#https://paulvanderlaken.com/2017/12/18/jingle-bells-in-r/


#Original version
if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)

#Revised version

if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("F C F",
               "G D G",
               "G G C D",
               "E",
               "F F F F",
               "G B B B",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 3, 4,
              3, 1, 2,
              1, 2, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 3, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)





