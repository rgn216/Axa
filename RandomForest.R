StartTime <- (Sys.time())

generateDistribution <- function(x,name) {
      x_wo_na <- x[!is.na(x)]
      qdist <- seq(0.05,1, by = 0.05)
      if (length(x_wo_na)<(2*length(qdist))| (sd(x_wo_na,na.rm = TRUE) < 1e-3) ) {
            dist <- quantile(x_wo_na, qdist)
      } else {
            x_wo_peaks <- x_wo_na[abs(x_wo_na-mean(x_wo_na,na.rm = TRUE))
                                  < 3 * sd(x_wo_na,na.rm = TRUE)]
            dist <- quantile(x_wo_peaks, qdist)
      }
      names(dist) = paste(name,names(dist),sep='_')
      names(dist) = gsub("%", "_pcnt", names(dist))
      return(dist)
}

#calc_cinematic_features calculates acceleration and speed

calc_cinematic_features <- function(trip) {
      
      dx <- diff(trip$x,lag=2,differences=1)
      dy <- diff(trip$y,lag=2,differences=1)
      
      speed <- sqrt( dx^2 + dy^2 ) # of size nrow(trip) - 2
      ux <- dx / speed
      uy <- dy / speed
      ux[(ux == Inf)|(ux == -Inf)] <- 0
      uy[(uy == Inf)|(uy == -Inf)] <- 0
      ux[is.nan(ux)] <- 0
      uy[is.nan(uy)] <- 0
      ux[is.na(ux)] <- 0
      uy[is.na(uy)] <- 0
      dx2 <- diff(trip$x,lag=1,differences=2)
      dy2 <- diff(trip$y,lag=1,differences=2)         
      
      accel_tangential <- ux * dx2 + uy * dy2
      accel_normal <- -uy * dx2 + ux * dy2     
      
      speed_distribution <- generateDistribution(speed,'speed')
      accel_tangential_distribution <- generateDistribution(accel_tangential,'accel_tangential')
      accel_normal_distribution <- generateDistribution(accel_normal,'accel_normal')
      
      
      #roll_speed<- rollapply(speed , width = 5 , FUN = median)
      #roll_accel_tangential<- rollapply(accel_tangential , width = 5 , FUN = median)
      #roll_accel_normal<- rollapply(accel_normal , width = 5 , FUN = median)      
      #speed_distribution <- generateDistribution(roll_speed,'speed')
      #accel_tangential_distribution <- generateDistribution(roll_accel_tangential,'accel_tangential')
      #accel_normal_distribution <- generateDistribution(roll_accel_normal,'accel_normal')
      
      cinematic_features = c( speed_distribution , accel_tangential_distribution , accel_normal_distribution)
      return(cinematic_features)
}

distance <- function(trip)
{
      dx <- diff(trip$x,lag=1,differences=1)
      dy <- diff(trip$y,lag=1,differences=1)
      delta_dist <- sqrt(dx^2 + dy^2)
      dist = sum(delta_dist)
      names(dist) = paste('distance')
      return(dist)
}

tripFeatures<-function(trip,target) {
      cinematic_features <- calc_cinematic_features(trip)           
      distance <- distance(trip)
      t <- target
      names(t) <- 'target'
      rtn <- c(cinematic_features,distance,t)
      return(rtn)
}
set.seed(123)
drivers = list.files("drivers")
randomDrivers = sample(drivers, size = 5)

#randomTrips = sample(1:200, size = 10)
#refData create the training set with the 200 trips of 5 randomly chosen drivers
#nlag = 5

refData = NULL
target = 0
names(target) = "target"
for(driver in randomDrivers)
{
      #dirPath = paste0("drivers/", driver, '/')
      dirPath = paste("drivers/", driver, '/' , sep ="")
      for(i in 1:200)
      {
            trip = read.csv(paste(dirPath, i, ".csv",sep = ""))           
            features = tripFeatures(trip , target)
            refData = rbind(refData, features)
      }
}

target = 1
names(target) = "target"
submission = NULL
for(driver in drivers)
{
      print(driver)
      dirPath = paste("drivers/", driver, '/',sep = "")
      currentData = NULL
      for(i in 1:200)
      {
            trip = read.csv(paste(dirPath, i, ".csv",sep=""))
            features = tripFeatures(trip , target)
            currentData = rbind(currentData, features)
      }
      
train = rbind(currentData, refData)
train = as.data.frame(train)
rownames(train) <- 1:nrow(train)

# LOGISTIC REGRESSION

currentData = as.data.frame(currentData)
g = glm(target ~ ., data=train, family = binomial("logit"))
p =predict(g, currentData, type = "response")


# RANDOM FOREST

g = randomForest(as.factor(target) ~ ., data=train)           
p =predict(g, newdata = train[train$target == as.factor(1) , ], type = "prob")

labels = sapply(1:200, function(x) paste(driver,'_', x,sep=""))
result = cbind(labels, p[,2])
submission = rbind(submission, result)
}

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "submission.csv", row.names=F, quote=F)

print (Sys.time()- StartTime)