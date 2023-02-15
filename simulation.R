rm(list = ls())
library('tidyverse')
library('jsonlite')
library('texreg')

area_overlap <- function(radius, distance) {
  2 * (radius ** 2) * acos(distance / (2 * radius)) - (1 / 2) * distance * sqrt(4 * (radius ** 2) - distance ** 2)
}

area_circles <- function(radius, distance) {
  2 * pi * (radius ** 2) - area_overlap(radius, distance)
}

proportion_overlap <- function(radius, distance) {
  area_overlap(radius, distance) / area_circles(radius, distance)
}

find_candidate <- function(number_circles) {
  number_pairs_displayed <- number_circles
  proportion_overlap_increaments <- 1 / number_pairs_displayed
  overlap_targets <- seq(0, 1, by = proportion_overlap_increaments)
  
  candidate <- tibble()
  for (overlap in overlap_targets) {
    candidate <- bind_rows(candidate,
                           data %>% 
                             filter(abs(proportion_overlap - overlap) == min(abs(proportion_overlap - overlap))))
  }
  
  candidate <- candidate %>% 
    mutate(difference_overlap = proportion_overlap - lag(proportion_overlap))
}

# do it with 1000 then divide by 10
# otherwise we do not have enough precision for the simulation
max_distance <- 1000
diameter <- max_distance:(max_distance*2)
distance <- 0:max_distance

# generate all combinations
data <- crossing(diameter, distance)

# remove candidates where distance is greater than a diameter (which means the circles are not touching),
# remove candidates where total area is +/- precision% of target area
target_area <- area_circles(radius = diameter[1]/2, 
                            distance = distance[length(distance)])
precision_area <- 0.001
data <- data %>%
  filter(diameter >= distance) %>%
  mutate(
    radius = diameter / 2,
    area_overlap = area_overlap(radius, distance),
    area_circles = area_circles(radius, distance),
    proportion_overlap = proportion_overlap(radius, distance)
  ) %>%
  filter(
    between(area_circles, 
            target_area * (1 - precision_area),
            target_area * (1 + precision_area))
  )

# find values for IOS between 2 and 20 circles
circles <- 2:20
candidates <- vector(mode = 'list', length = length(circles))
for (number_circles in circles) {
  candidates[[number_circles]] <- find_candidate(number_circles)
}

# remove unnecessary columns and save as JSON (to use in ios.js)
json_data <- candidates
for (i in 2:length(json_data)) {
  json_data[[i]] <- json_data[[i]] %>% select(diameter, distance)
}
json_data <- toJSON(json_data)
writeLines(json_data, 'json.txt')

# for the continuous we fit a model
# we do as if there are 100 circles and we fit a polynomial
data_fit <- find_candidate(100)
model <- lm(diameter ~ poly(distance, degree = 4, raw = TRUE), data = data_fit)
# assess whether the fit is good
data_fit <- data_fit %>% mutate(
  predicted_diameter = round(predict(model), digits = 0), 
  diff_prediction = diameter - predicted_diameter
  )
# look at the coefficients (used in ios.js)
summary(model)
# save latex table (for paper)
texreg(model, booktabs = TRUE, center = TRUE, use.packages = FALSE, 
  custom.coef.names = c(
  "$\\beta_0$",
  "$\\beta_1$",
  "$\\beta_2$",
  "$\\beta_3$",
  "$\\beta_4$"
),
  custom.model.names = c(" "),
  digits = trunc(20),
  caption = "Estimates from the polynomial fitted to the simulated values",
  caption.above = TRUE,
float.pos = "tbp"
)