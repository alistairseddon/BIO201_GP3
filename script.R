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
# Determine a threshold for presence data (0.05) and then plot. 

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
##. etc

### Checking correlation of covariables
tsuga_6k_clim %>% 
  ungroup() %>% 
  dplyr::select(-dataset.id, -lat, -long, -elev, -age, -Tsuga) %>% 
  pairs()

# Can see that these variables are highly correlated. This could be problematic when modelling 
# I think it's best to preciptation and growing degree days only

# Fitting a model to test for the relationship between hemlock presence and temperature/ precipitation
# See the function: fit.model() 
model_results_6ky <- fit.model(.data =tsuga_6k_clim)

# map the model predictions
quartz()
map_species(model_results_6ky$training.data, .var = "predicted_pres_glm")

# inspect the model- which variables are significant?
model_glm_6ky <- model_results_6ky$model_glm
summary(model_glm_6ky)

### Use the Kappa statistic to evaluate the predictions of the model against the actual observed values (higher is better)
caret::confusionMatrix(as.factor(model_results_6ky$validation.data$Tsuga),
                       as.factor(model_results_6ky$validation.data$predicted_pres_glm))


# It is also possible to inspect the response functions of the two variables
# See the function: calc_response_functions
calc_response_functions(.data = tsuga_6k_clim, .model = model_glm_6ky )

# Now use these functions to explore hemlock dynamics over the late Holocene...

