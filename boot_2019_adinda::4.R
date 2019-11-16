###### Start  ##################################################
###### Check, installation and loading of required packages #######
requiredPackages = c( "triangle", "RColorBrewer") # list of required packages
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 

library(robustHD)
library(boot)
library(boot)
library(ggplot2)
library(robustHD)
library(cluster)
library(factoextra)

sample_all<-load("~/Documents/Fall Semester 2019:2020/2. Applied Microeconomics/assignment_2/sample_all.Rdata")# load the sample 
sample_all
par(mfrow=c(1,1))
# the estimation of based on the sample
summary(sample_all[,2:3 ])
mnk <- lm(sample_all$y~sample_all$x)
mnk
alfa <- round(summary(mnk)$coefficients[,1][1], digits = 0 )  
beta <- round(summary(mnk)$coefficients[,1][2], digits = 3 )  
plot(sample_all$x, sample_all$y, pch=19, 
     main = paste0("Estimation based on sample y(t) = ", alfa," + ",beta,"*x(t)" ))
abline(h=(1:15)*1000, lty=3)
abline(v=(1:15)*1000, lty=3)
abline(mnk)

# START the bootsrap procedure

# input object to the loop
sample_b_ind  <-0
alfa <- 0
beta <- 0

# bootstrap 
B <- 1000 # Bootsprap iterations
for(i in 1:B){
    sample_b_ind <-sample(1:nrow(sample_all), nrow(sample_all), replace=TRUE) #label draw
  sample_b <- sample_all[sample_b_ind,] 
  mnk_b <- lm(sample_b$y~sample_b$x)
  
  alfa[i] <- summary(mnk_b)$coefficients[,1][1]  
  beta[i] <- summary(mnk_b)$coefficients[,1][2]  }
# END  


# dystribution of a and b estimators
par(mfrow=c(2,1))
hist(alfa, main="Histogram of a", col="aquamarine3", breaks=50, probability=TRUE)
lines(density(alfa), lwd=3)
summary(alfa)
hist(beta, main="Histogram of b", col="aquamarine3", breaks=50, probability=TRUE)
lines(density(beta), lwd=3)
summary(beta)
par(mfrow=c(1,1))


# dystribution of a and b as XY plot
plot_colorByDensity = function(x1,x2,
                               ylim=c(min(x2),max(x2)),
                               xlim=c(min(x1),max(x1)),
                               xlab="",ylab="",main="") {
  
  df <- data.frame(x1,x2)
  x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
  df$dens <- col2rgb(x)[1,] + 1L
  cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
  df$col <- cols[df$dens]
  plot(x2~x1, data=df[order(df$dens),], 
       ylim=ylim,xlim=xlim,pch=20,col=col,
       cex=2,xlab=xlab,ylab=ylab,
       main=main)
}


plot_colorByDensity(alfa,beta,xlab="a",ylab="b",main="XY plot of bootstrapped estimators [a,b]")
abline(h=mean(beta), v=mean(alfa), lwd=2)


######################my solutions#################################
cl<-data.frame(alfa,beta)
#visualitation data to check outliers
boxplot(cl)
#from boxplot, we can see that there are so many outliers
#instead of deleting outliers in data, I prefer to replace outliers number with winsorized mean
new_x<-winsorize(cl$alfa)
new_y<-winsorize(cl$beta)

cl_2<-data.frame(x=new_x,y=new_y)

#now there is no outlier again.
boxplot(cl_2)

#clustering
pam_<-eclust(cl_2, "pam", k=5)
summary(pam_)
sil_width<-pam_$silinfo
z<-data.frame(sil_width)
est_par<-mean(z[,4])
# allows boot to select sample 
bs <- function(formula, data, indices) {
  d <- data[indices,] 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=cl_2, statistic=bs, 
                R=1000, formula=cl_2$x~cl_2$y)
results
est_par
#estimation parameter alfa=5421.700
#estimation parameter beta=0.5300555



