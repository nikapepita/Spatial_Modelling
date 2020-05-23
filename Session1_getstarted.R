hw <- read.csv("weight-height.csv")

#the only free height, weight, gender data set I found, 
#source unknow, data trusted with care

head(hw)
summary(hw)

library(measurements)

hw2 <- data.frame(Gender=hw$Gender,
                  Weight=conv_unit(hw$Weight,"lbs","kg"),
                  Height=conv_unit(hw$Height, "inch", "cm"))
head(hw2)
summary(hw2)

plot(hw2$Height, hw2$Weight)

library(dplyr)

#provide 10 random samples
dplyr::sample_n(hw2,10)

#how are the values for male and female?
summary(filter(hw2, Gender=="Female"))
summary(filter(hw2, Gender=="Male"))

#any obvios anomalies or indications of significant correlation between male and  female values?
boxplot(filter(hw2,Gender=="Female")$Weight, filter(hw2, Gender=="Male")$Weight, notch=T)
boxplot(filter(hw2,Gender=="Female")$Height, filter(hw2, Gender=="Male")$Height, notch=T)

shapiro.test(hw2$Weight)

shapiro.test(dplyr::sample_n(hw2,5000)$Weight)
shapiro.test(dplyr::sample_n(hw2,5000)$Height)

plot(density(hw$Weight))
plot(density(hw$Height))


plot(density(filter(hw2, Gender=="Female")$Weight), col="red")
plot(density(filter(hw2, Gender=="Male")$Weight), col="blue")


plot(density(filter(hw2, Gender=="Female")$Weight), col="red")
lines(density(filter(hw2, Gender=="Male")$Weight), col="blue")

plot(density(filter(hw2, Gender=="Female")$Height), col="red")
lines(density(filter(hw2, Gender=="Male")$Height), col="blue")

shapiro.test(dplyr::sample_n(filter(hw2, Gender=="Female"),5000)$Weight)
shapiro.test(dplyr::sample_n(filter(hw2, Gender=="Male"),5000)$Weight)

shapiro.test(dplyr::sample_n(filter(hw2, Gender=="Female"),5000)$Height)
shapiro.test(dplyr::sample_n(filter(hw2, Gender=="Male"),5000)$Height)

hw2.male <- filter(hw2, Gender=="Male")
hw.lm <- lm(formula= Weight~Height,data=hw2.male)
summary(hw.lm)

hw.new <- data.frame(name=c("Kevin","Heinz","Manuel"), Height=c(180,175,195))
hw.lw.p <- predict(object=hw.lm, newdata=hw.new)

pred.weight <- data.frame(hw.new$name,
                          weight.pred=hw.lw.p
                          )

pred.weight


####Spatial Prediction - where are the eagles

library(rgdal)
library(raster)

library(sdm)
library(kernlab)


#Vers 1
occ <- readOGR("occurence.gpkg")

class(occ)
summary(occ)
plot(occ)

bui <- readOGR("campus_buildings.gpkg")
plot(bui)

plot(occ[occ$students==1,],col='blue', pch=16,add=T)
plot(occ[occ$students==0,],col='red', pch=16,add=T)

r <- raster(bui, ncols=100, nrows=100)
rr.0 <- rasterize(bui, r, progress="text")
plot(rr.0)

rr.0.d <- distance(rr.0)

preds <- rr.0.d
plot(rr.0.d)

d <- sdmData(formula=students~layer, train=occ, predictors=preds)
d

m1 <- sdm(students~., data=d, methods=c('glm','svm'))
p1 <- predict(m1, newdata=preds, filename='sdm_preds_1.grd', overwrite=T)
plot(p1)

#Vers 2

rr <- rasterize(bui, r, progress="text", field="id")
plot(rr)


rr.1 <- rr==1
rr.1[rr.1==0] <-NA 
plot(rr.1)

rr.2 <- rr==2
rr.2[rr.2==0] <-NA 
plot(rr.2)

rr.3 <- rr==3
rr.3[rr.3==0] <-NA 
plot(rr.3)


rr.1.d <- distance(rr.1)
plot(rr.1.d)

rr.2.d <- distance(rr.2)
plot(rr.2.d)

rr.3.d <- distance(rr.3)
plot(rr.3.d)

preds <- stack(rr.1.d,rr.2.d,rr.3.d)


#prediction

d <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ, predictors=preds)
m1 <- sdm(students~.,data=d,methods=c("glm","svm"))

p1 <- predict(m1,newdata=preds,filename="sdm_preds_2.grd",overwrite=T)
plot(p1)

##add time

occ.10h <- occ[occ$time==10,]
occ.13h <- occ[occ$time==13,]
occ.22h <- occ[occ$time==22,]

d.10h <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ.10h, predictors=preds)
d.13h <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ.13h, predictors=preds)
d.22h <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ.22h, predictors=preds)

m.10h <- sdm(students~.,data=d.10h, methods=c("glm","svm"))
m.13h <- sdm(students~.,data=d.13h, methods=c("glm","svm"))
m.22h <- sdm(students~.,data=d.22h, methods=c("glm","svm"))

p.10h <- predict(m.10h, newdata=preds)
p.13h <- predict(m.13h, newdata=preds)
p.22h <- predict(m.22h, newdata=preds)

p.time <- stack(p.10h,p.13h,p.22h)


plot(p.time,1)

plotRGB(p.time, 1,3,5, stretch="lin")
plot(bui,add=T)
