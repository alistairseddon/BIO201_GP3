library("tidyverse")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library("raster")
library("caret")

source("function_script.R")

# Load data
NA_pollen_climate <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Group Project 3/data/NA_pollen_climate.RDS")
names(NA_pollen_climate)
NA_pollen_climate <- NA_pollen_climate %>% 
  filter(long > -95) %>% 
  filter(lat > 35) %>%
  filter(lat < 55) %>%
  dplyr::select(-ref) %>%
  filter(max.age > 5500)


# First thing to do is extract the data for hemlock for a given time period.
# Determine a threshold for presence data (0.1) and then plot. 

tsuga_6k <- prep_spec_data(NA_pollen_climate,
               .species.name = "Tsuga", 
               .threshold = 0.05, 
               .year = 6000 )
tsuga_5k <- prep_spec_data(NA_pollen_climate,
                           .species.name = "Tsuga", 
                           .threshold = 0.05, 
                           .year = 5000 )
tsuga_4k <- prep_spec_data(NA_pollen_climate,
                           .species.name = "Tsuga", 
                           .threshold = 0.05, 
                           .year = 4000 )
## .etc

### Mapping the data
quartz()
map_species(tsuga_6k)
map_species(tsuga_5k)
map_species(tsuga_4k)

#### Getting the climate data
tsuga_6k_clim <- get_climate_data(.data = tsuga_6k, .year = "6000") %>% drop_na()
tsuga_5k_clim <- get_climate_data(.data = tsuga_4k, .year = "5000") %>% drop_na()
tsuga_4k_clim <- get_climate_data(.data = tsuga_4k, .year = "4000") %>% drop_na()

### Checking correlation of covariables
tsuga_6k_clim %>% 
  ungroup() %>% 
  dplyr::select(-dataset.id, -lat, -long, -elev, -age, -Tsuga) %>% 
  pairs()

# Can see that these variables are highly correlated. This could be problematic when modelling 
# I think it's best to preciptation and growing degree days only

# Fitting a model to test for the relationship between hemlock presence and temperature/ precipitation
# See the function: fit.model() 
model_6ky <- fit.model(.data =tsuga_6k_clim)
quartz()
map_species(model_6ky$training.data, .var = "predicted_pres_glm")

### And you can use the Kappa statistic to evaluate the predictions of the model against the actual observed values 
caret::confusionMatrix(as.factor(model_6ky$validation.data$Tsuga),
                       as.factor(model_6ky$validation.data$predicted_pres_glm))





# It is also possible to inspect the response functions of the two variables
# First GDD Create a varible holding precipitation constant (at the mean value) and let temperature vary within the range seen in the data
meanGDD <- mean(tsuga_4k_clim$an_sum_GDD5)
minGDD <- min(tsuga_4k_clim$an_sum_GDD5)
maxGDD <- max(tsuga_4k_clim$an_sum_GDD5)

meanPRCP <- mean(tsuga_4k_clim$an_sum_PRCP)
minPRCP <- min(tsuga_4k_clim$an_sum_PRCP)
maxPRCP <- max(tsuga_4k_clim$an_sum_PRCP)

# Calculate the Response functions for GDD
new.data.GDD <- data.frame(an_sum_GDD5  = seq(from = minGDD, to = maxGDD, length.out = 200), 
                           an_sum_PRCP = rep(meanPRCP, length = 200))
predict_GDD <- predict(model_4ky_glm, type = "response", newdata= new.data.GDD )

# Calculate the Response functions for precipitation
new.data.PRCP <- data.frame(an_sum_GDD5 = rep(meanGDD, length = 200),
                           an_sum_PRCP  = seq(from = minPRCP, to = maxPRCP, length.out = 200) )
predict_PRCP <- predict(model_4ky_glm, type = "response", newdata= new.data.PRCP )

# Plot the response functions
par(mfrow = c(2,2))
plot(new.data.GDD$an_sum_GDD5, predict_GDD, type = "l", col = "red", xlab = "Annual Sum Growing Degree Days", ylab = "Model Probability")
plot(new.data.PRCP$an_sum_PRCP, predict_PRCP, type = "l", col = "blue", xlab = "Annual Sum Precipiation", ylab = "Model Probability")



