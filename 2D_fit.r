library(tiff) ##require tiff package for import images
library(plotly) ##3D plot package
library(plot3D) ##3D plot package
image1 = readTIFF("/Users/hongzhan/Desktop/test.tif", as.is=T)##as.is not to rescale data
FindPeak3D<-function(image,threshold){ ## putting the image as matrix and setting the thresure value to substract backgroud
  rn <- nrow(image)
  rn_1 <- rn-1
  cn <- ncol(image)
  cn_1 <- cn-1
  bkg <- matrix(1L, nrow = rn, ncol = cn)*threshold
  image_g_remove <- image - bkg
  image_g_remove[image_g_remove < 0] <- 0
  d <- image_g_remove
  m <- matrix(, nrow = rn, ncol = cn)
  for(i in 1:rn){
    if(i > 1 & i < rn){
      for(j in 1:cn){
        if(j >1 & j < cn)
          if(d[i,j] > d[i+1,j]  & ##testing surrounding values are smaller than the middle
             d[i,j] > d[i+1,j-1] &
             d[i,j] > d[i,j-1] &
             d[i,j] > d[i-1,j-1] &
             d[i,j] > d[i-1,j] &
             d[i,j] > d[i-1,j+1] &
             d[i,j] > d[i,j+1] &
             d[i,j] > d[i+1,j+1] &
             d[i,j]/threshold > 5
          ) ## setting the max value will over 30% of the thresure value
          {
            m[i,j]<-d[i,j]
          } else { 
            m[i,j]<-0}
        else {
          m[,j]<-0
        }
      }
    }else{
      m[i,]<-0        
    }
  }
  m
}

peak3D1<-FindPeak3D(image1, 20)

CorPeak<-as.data.frame(which(peak3D1 != 0, arr.ind=TRUE)) ##select the coordinates from the matrix where there is a peak value

Cor1<-CorPeak[,1]
Cor2<-CorPeak[,2]

SelectForFit2<-function(x,y,z, pixel){
  rn<-nrow(x)
  cn<-ncol(x)
  l<-length(y)
  d<-NULL
  for(i in 1:l){
    if(y[i] > 5 & 
       y[i] <= (rn-pixel) &
       z[i] > pixel &
       z[i] <= (cn-pixel)) {
      d[i]<-list(x[(y[i]-pixel):(y[i]+pixel), (z[i]-pixel):(z[i]+pixel)])
    } else if(y[i] <= pixel &
              z[i] <= pixel
    ){
      d[i]<-list(x[1:y[i]+pixel, 1:z[i]+pixel])
    } else if(y[i] > rn-pixel &
              z[i] > cn-pixel) {
      d[i]<-list(x[y[i]-pixel:rn, z[i]-pixel:cn])
    }
  }
  d
}
PunctaList<-SelectForFit2(image1,Cor1,Cor2, pixel=6)

test1<-as.data.frame(PunctaList[14])
names(test1)<-NULL
test1<-as.matrix(test1)
z<-matrix(test1, nrow=13*13, ncol=1, byrow=F)
x<-matrix(matrix(1:13, nrow=13, ncol=13, byrow=T), nrow=13*13, ncol=1, byrow=F)
y<-matrix(1:13, nrow=13*13, ncol=1, byrow=T)
df<-data.frame(x, y, z)


fit<-nls( z ~ I + A*exp(-((x-x0)^2/2*sigmax^2 + (y-y0)^2/2*sigmay^2)), start=list(I=60, A = 179, x0 = 10, y0 = 7, sigmax = 1, sigmay = 1), algorithm="port")
I<-coef(fit)[["I"]]
A<-coef(fit)[["A"]]
x0<-coef(fit)[["x0"]]
y0<-coef(fit)[["y0"]]
sigmax<-coef(fit)[["sigmax"]]
sigmay<-coef(fit)[["sigmay"]]
z_fit <- I + A*exp(-((x-8)^2/2*sigmax^2 + (y-8)^2/2*sigmay^2))
z_fit <- matrix(z_fit, nrow=13, ncol=13, byrow=F)

p <- plot_ly(z=z_fit, type = "surface", opacity=0.9) %>% add_trace(data = df, x = ~x, y = ~y, z = ~z, mode = "markers", type = "scatter3d", marker = list(size = 2, color = "black", symbol = 50))

p2 <- plot_ly(data = df, x = ~x, y = ~y, z = ~z, mode = "markers", type = "scatter3d", marker = list(size = 2, color = "black", symbol = 50)) %>% add_markers()
p3 <- plot_ly(z=test1, type = "surface")


test2<-as.data.frame(PunctaList[14])


t<-c(1, 2, 3, 4, 5, 6, 7)
pl.t<-t^2 + 3
df.test<-data.frame(t, pl.t)

library(ggplot2)
p1.1<-ggplot(df.test, aes(x=t, y=pl.t)) + geom_point()
