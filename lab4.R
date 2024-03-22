#Outlier Examples
#Cars Dataset built in Rstudio
#you need to load cars dataset first
cars
cars1 <- cars[1:30,]#first 30 rows of the original cars dataset
head(cars1)
#Now we will introduce some additional data points that are outliers
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190,186,210,220,218))#introduce outliers
head(cars_outliers)
cars2 <- rbind(cars1, cars_outliers)
head(cars2)
help(par)#set or query graphical parameters

par(mfrow=c(1,2))
plot(cars2$speed, cars2$dist, xlim = c(0,28), ylim = c(0,230), main = "With Outliers", xlab = "Speed", ylab = "Dist", pch="*", col = "red", cex=2)
abline(lm(dist~speed, data=cars2), col = "blue", lwd=3, lty=2)

#Plot of original data without outliers. Note the change in slope (angle) of the fit line
plot(cars1$speed, cars1$dist, xlim = c(0,28), ylim = c(0,230), main = "Outliers Removed \n A much better fit!", xlab = "Speed", ylab = "Dist", pch="*", col = "red", cex=2)
abline(lm(dist~speed, data=cars2), col = "blue", lwd=3, lty=2)
