### Train Collisions from ICDA

require(icda)
require(MASS)
data(traincollisions)
trains = traincollisions

names(trains) = c("year", "km", "train", "trRd")
min(trains$year)
trains$yearSince = trains$year - min(trains$year)
trains$rate = trains$trRd / trains$km
head(trains)


trains.mod = glm(trRd ~ yearSince, offset = log(km), data = trains, family = poisson)
summary(trains.mod)
predict(trains.mod, newdata = data.frame(yearSince = 0, km = 436))
train.sum = summary(trains.mod)
exp(train.sum$coef[1,1] + train.sum$coef[2, 1] * 28)

trains.nb = glm.nb(trRd ~ yearSince, offset = log(trains$km), data = trains)
trains.rate.mod = glm(rate ~ yearSince, data = trains, family = poisson)
summary(trains.rate.mod)
