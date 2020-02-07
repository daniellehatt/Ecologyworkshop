install.packages("dismo")
install.packages("maptools")
install.packages("rgdal")
install.packages("raster")
install.packages("sp")
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
bioclim.data <- getData(name = "worldclim",
                        var = "bio",
                        res = 2.5,
                        path = "/Users/daniellehatt/Desktop/Ecologyworkshop/")

obs.data <- read.csv(file = "/Users/daniellehatt/Desktop/Ecologyworkshop/SDM_Data.csv")
summary(obs.data)
obs.data <- na.omit(obs.data)
summary(obs.data)
max.lat <- ceiling(max(obs.data$latitude))
min.lat <- floor(min(obs.data$latitude))
max.lon <- ceiling(max(obs.data$longitude))
min.lon <- floor(min(obs.data$longitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))
data(wrld_simpl)
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")
points(x = obs.data$longitude, 
       y = obs.data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
box()
bioclim.data <- crop(x = bioclim.data, y = geographic.extent)
bc.model <- bioclim(x = bioclim.data, p = obs.data)
obs.data <- obs.data[, c("latitude", "longitude")]
bc.model <- bioclim(x = bioclim.data, p = obs.data)
head(obs.data)
obs.data <- obs.data[, c("longitude", "latitude")]
bc.model <- bioclim(x = bioclim.data, p = obs.data)
predict.presence <- dismo::predict(object = bc.model, x = bioclim.data, ext = geographic.extent)

plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")
plot(predict.presence, add = TRUE)
plot(wrld_simpl, add = TRUE, border = "grey5")
points(obs.data$longitude, obs.data$latitude, col = "olivedrab", pch = 20, cex = 0.75)
box()
bil.files <- list.files(path = "/Users/daniellehatt/Desktop/Ecologyworkshop/wc2-5", 
                        pattern = "*.bil$", 
                        full.names = TRUE)
bil.files
mask <- raster(bil.files[1])
background <- randomPoints(mask = mask,     # Provides resolution of sampling points
                           n = nrow(obs.data),      # Number of random points
                           ext = geographic.extent, # Spatially restricts sampling
                           extf = 1.25)             # Expands sampling a little bit
head(background)                          
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95",
     main = "Presence and pseudo-absence points")
points(background, col = "grey30", pch = 1, cex = 0.75)
points(x = obs.data$longitude, 
       y = obs.data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

box()

testing.group <- 1
group.presence <- kfold(x = obs.data, k = 5)
head(group.presence)
table(group.presence)
presence.train <- obs.data[group.presence != testing.group, ]
presence.test <- obs.data[group.presence == testing.group, ]
group.background <- kfold(x = background, k = 5)
background.train <- background[group.background != testing.group, ]
background.test <- background[group.background == testing.group, ]
bc.model <- bioclim(x = bioclim.data, p = presence.train)
predict.presence <- dismo::predict(object = bc.model, 
                                   x = bioclim.data, 
                                   ext = geographic.extent)
bc.eval <- evaluate(p = presence.test,   # The presence testing data
                    a = background.test, # The absence testing data
                    model = bc.model,    # The model we are evaluating
                    x = bioclim.data)
bc.threshold <- threshold(x = bc.eval, stat = "spec_sens")

plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

plot(predict.presence > bc.threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = "olivedrab")

points(x = obs.data$longitude, 
       y = obs.data$latitude, 
       col = "black",
       pch = "+", 
       cex = 0.75)

plot(wrld_simpl, add = TRUE, border = "grey5")
box()
predict.presence > bc.threshold
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

plot(predict.presence > bc.threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = c(NA, "olivedrab"))

points(x = obs.data$longitude, 
       y = obs.data$latitude, 
       col = "black",
       pch = "+", 
       cex = 0.75)

plot(wrld_simpl, add = TRUE, border = "grey5")
box()
