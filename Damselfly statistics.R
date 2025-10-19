#### The impact of canopy structure and territoriality on habitat selection of male Argia oculata (Odonata: Coenagrionidae) ####

## Installing packages

library(ggplot2)
library(mgcv)
library(tidyr)
library(dplyr)
library(showtext)
library(Cairo)

showtext_auto()

## Uploading data

# Alternatively upload 'canopy_data.csv' file; I named this 'tree_data'

tree_data <- data.frame(
  distance_m = c(0, 0, 5, 5, 10, 10, 15, 15, 20, 20, 25, 25, 30, 30, 35, 35, 40, 40, 45, 45, 50, 50, 55, 55, 60, 60, 65, 65, 70, 70, 75, 75, 80, 80),
  side       = c("left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right", "left", "right"),
  height_m   = c(10.0, 9.8, 7.7, 8.7, 5.2, 9.8, 9.4, 10.3, 18.2, 9.4, 8.0, 6.4, 9.4, 6.2, 6.2, 6.2, 9.2, 7.7, 8.2, 6.6, 8.3, 6.4, 10.0, 14.1, 9.6, 12.9, 11.2, 8.0, 7.7, 6.9, 7.7, 7.7, 7.1, 11.2),
  cover      = c(88.4, 86.2, 89.5, 86.1, 88.6, 86.8, 89.5, 87.0, 89.7, 87.9, 92.1, 91.4, 88.4, 90.4, 92.1, 90.8, 90.7, 80.0, 85.7, 90.0, 89.4, 89.4, 86.8, 88.4, 90.9, 89.7, 92.9, 92.5, 90.7, 88.6, 91.2, 82.8, 89.2, 89.8)
)

## Upload the csv file 'damselfly_complete.csv' here; I named this 'damselfly_distance'

head(tree_data)
head(damselfly_distance)

##### CANOPY COVER ####

## 1. Producing scatter plot of canopy left against canopy right 

# First, data needs to be reshaped

cover_compare <- tree_data %>%
  select(distance_m, side, cover) %>%
  pivot_wider(names_from = side, values_from = cover)

head(cover_compare)

## Visualising data, comparing left and right side 

ggplot(cover_compare, aes(x = left, y = right)) +
  geom_point(color = "red", size = 3) +
  labs(x = "Canopy cover (left)",
       y = "Canopy cover (right)") +
  theme_minimal()

# Looks to be largely correlated with some anomalies 

#### 2. Test for normality (shapiro test)

shapiro.test(tree_data$cover[tree_data$side == "left"]) # p > 0.05, data is normal
shapiro.test(tree_data$cover[tree_data$side == "right"]) # p > 0.05, data is normal 

#### 3. Test for variances (F test)

var.test(cover ~ side, data = tree_data) # p < 0.05
# Variances are not equal; however this does not matter for paired t test


#### 4. Paired t-test

t.test(cover_compare$left, cover_compare$right, paired = TRUE)
# Because p > 0.05, they are not statistically significant. So, they can be combined. 

#########################################################

#### CANOPY HEIGHT ####

## 1. Producing scatter plot of canopy left against canopy right 

# First data needs to be reshaped

cover_compare_2 <- tree_data %>%
  select(distance_m, side, height_m) %>%
  pivot_wider(names_from = side, values_from = height_m)

head(cover_compare_2)

## Visualising data, comparing left and right side 

ggplot(cover_compare_2, aes(x = left, y = right)) +
  geom_point(color = "red", size = 3) +
  labs(x = "Canopy height (left)", 
       y = "Canopy height (right)") +
  theme_minimal()

# Looks to be largely correlated with some anomalies 

#### 2. Test for normality (shapiro test)

shapiro.test(tree_data$height_m[tree_data$side == "left"]) # p < 0.05, data is not normal
shapiro.test(tree_data$height_m[tree_data$side == "right"]) # p > 0.05, data is normal 

#### 3. Test for variances (F test)

var.test(height_m ~ side, data = tree_data) # p > 0.05
# Variances are not equal; however this does not matter for paired t test


#### 4. Paired Wilcoxon test


wilcox.test(cover_compare$left, cover_compare$right, paired = TRUE)

# Because p > 0.05, they are not statistically significant. So, they can be combined. 

###################################################################

#### 1. Combine canopy cover for left and right using average 

cover_avg <- cover_compare %>%
  mutate(mean_cover = (left + right) / 2)

head(cover_avg)

#### 2. Interpolate points for every 1m

interp <- approx(
  x = cover_avg$distance_m,
  y = cover_avg$mean_cover,
  xout = seq(min(cover_avg$distance_m), max(cover_avg$distance_m), by = 1)
)

# Create a new data frame
cover_interp <- data.frame(
  distance_m = interp$x,
  mean_cover = interp$y
)

head(cover_interp, 10)

#### 3. Visualise interpolation

ggplot(cover_interp, aes(x = distance_m, y = mean_cover)) +
  geom_line(color = "forestgreen", size = 1) +
  geom_point(data = cover_avg, aes(x = distance_m, y = mean_cover),
             color = "darkgreen", size = 2) +
  labs(x = "Distance (m)", y = "Mean Canopy Cover (%)") +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 12)),   # adds space between y title & labels
    plot.margin = margin(t = 10, r = 10, b = 10, l = 20)    # adds padding around the plot
  )

#### 4. Merge the damselfly and canopy cover datasets

combined_data_cover <- merge(damselfly_distance, cover_interp, by = "distance_m")

head(combined_data_cover)

###################################################################

#### 1. Combine canopy height for left and right using average 

height_avg <- cover_compare_2 %>%
  mutate(mean_height = (left + right) / 2)

head(height_avg)

#### 2. Interpolate points for every 1m

interp <- approx(
  x = height_avg$distance_m,
  y = height_avg$mean_height,
  xout = seq(min(height_avg$distance_m), max(height_avg$distance_m), by = 1)
)

# Create a new data frame
height_interp <- data.frame(
  distance_m = interp$x,
  mean_height = interp$y
)

head(height_interp, 10)

#### 3. Visualise interpolation

ggplot(height_interp, aes(x = distance_m, y = mean_height)) +
  geom_line(color = "forestgreen", size = 1) +
  geom_point(data = height_avg, aes(x = distance_m, y = mean_height),
             color = "darkgreen", size = 2) +
  labs(x = "Distance (m)", y = "Mean Canopy Height (m)") +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 12)),   # adds space between y title & labels
    plot.margin = margin(t = 10, r = 10, b = 10, l = 20)    # adds padding around the plot
  )

#### 4. Merge the damselfly and canopy cover datasets

combined_data_full <- merge(combined_data_cover, height_interp, by = "distance_m")

head(combined_data_full)

########################################################################

#### Can canopy height and cover be combined? ####

#### 1. Producing scatter plot of canopy cover and height 

combined_env_interp <- merge(cover_interp, height_interp, by = "distance_m")

head(combined_env_interp)

ggplot(combined_env_interp, aes(x = mean_height, y = mean_cover)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    x = "Mean Canopy Height (m)",
    y = "Mean Canopy Cover (%)"
  ) +
  theme_minimal(base_family = "Arial", base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    axis.title.y = element_text(margin = margin(r = 15))  # increase gap on right
  )


#### 2. Testing canopy cover and height for normality (using average 5m data) 

shapiro.test(cover_avg$mean_cover) # p > 0.05, normal
shapiro.test(height_avg$mean_height) # p > 0.05, normal

#### 3. Pearson test for correlation

cor.test(height_avg$mean_height, cover_avg$mean_cover, method = "pearson")

# Not statistically significant so there is no correlation between the two

#######################################################################

#### DAMSELFLIES LEFT AND RIGHT ####

# Difference between left and right side for males 

# Re-structuring data 

damselfly_counts <- damselfly_distance %>%
  group_by(distance_m, side) %>%
  summarise(n = n()) %>%        # count individuals
  pivot_wider(names_from = side, values_from = n, values_fill = 0)  # make Left/Right columns

head(damselfly_counts)

# Testing normality 

shapiro.test(tree_data$height_m[damselfly_counts$r]) # p < 0.05, data is not normal
shapiro.test(tree_data$height_m[damselfly_counts$l]) # p > 0.05, data is not normal 

## Paired Wilcoxon test as data is not normally distributed 

wilcox.test(damselfly_counts$l, damselfly_counts$r, paired = TRUE)

#######################################################################

#### Impact of canopy cover on damselflies ####

#### 1. Plot of canopy cover and damselfly position

combined_data_full_m <- combined_data_full %>%
  filter(sex == "m") # Filtering damselflies into only males

head(combined_data_full_m)

### Making counts of males; ie. removing females as females < 15

male_counts <- combined_data_full_m %>%
  mutate(distance_m = floor(distance_m)) %>%  # round down to nearest metre
  group_by(distance_m) %>%
  summarise(damselfly_count = n()) %>%
  ungroup()

# Fill missing metres with 0 (due to missing data)
all_distances <- data.frame(distance_m = 0:80)
male_counts <- all_distances %>%
  left_join(male_counts, by = "distance_m") %>%
  mutate(damselfly_count = ifelse(is.na(damselfly_count), 0, damselfly_count))

# Merging males with canopy cover 
combined_male_cover <- male_counts %>%
  left_join(cover_interp, by = "distance_m")

## Visualisation

head(combined_male_cover)

ggplot(combined_male_cover, aes(x = mean_cover, y = damselfly_count)) +
  geom_point(color = "dodgerblue3", size = 3) +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = TRUE, color = "black") +
  labs(
    x = "Canopy Cover (%)",
    y = "Male Abundance"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid = element_blank(),        # remove grey grid lines
    panel.background = element_blank(),  # ensure background is white
    axis.line = element_line(color = "black"),  # add clean axis lines
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#### 2. Does canopy cover impact male position? 

glm_male <- glm(damselfly_count ~ mean_cover, 
                family = poisson(link = "log"), 
                data = combined_male_cover)

summary(glm_male)

# p < 0.05, statistically significant; canopy cover negatively impacts male abundance 

###################################################################

#### IMPACT OF CANOPY HEIGHT ON DAMSELFLY POSITION ####

#### 1. Plot of canopy height and damselfly position

# Merging height and male counts 

combined_male_height <- male_counts %>%
  left_join(height_interp, by = "distance_m")

# Visualisation

ggplot(combined_male_height, aes(x = mean_height, y = damselfly_count)) +
  geom_point(color = "dodgerblue3", size = 3) +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = TRUE, color = "black") +
  labs(
    x = "Canopy Height (m)",
    y = "Male Abundance"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid = element_blank(),        # remove grey grid lines
    panel.background = element_blank(),  # ensure background is white
    axis.line = element_line(color = "black"),  # add clean axis lines
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#### 2. Does canopy height impact male position? 

glm_male_height <- glm(damselfly_count ~ mean_height,
                       family = poisson(link = "log"),
                       data = combined_male_height)

summary(glm_male_height)


##### Assessing impact of territoriality ####

#### 1. Running AIC model 

# Making another data frame including territoriality 
male_counts_2 <- combined_data_full_m %>%
  mutate(distance_m = floor(distance_m)) %>%  # round to nearest metre
  group_by(distance_m, territorial) %>%
  summarise(damselfly_count = n()) %>%
  ungroup()

male_counts_2 <- male_counts_2 %>%
  left_join(cover_interp, by = "distance_m") %>%
  left_join(height_interp, by = "distance_m")

head(male_counts_2)

## Running AIC model 

glm_full <- glm(damselfly_count ~ territorial + mean_cover + mean_height,
                family = poisson(link = "log"),
                data = male_counts_2)
summary(glm_full)


glm_best <- step(glm_full, direction = "both", trace = TRUE)
summary(glm_best)

#### 2. Does canopy cover influence territoriality?

male_counts_2 <- male_counts_2 %>%
  mutate(territorial_bin = ifelse(territorial == "y", 1, 0))

glm_territorial <- glm(territorial_bin ~ mean_cover,
                       family = binomial(link = "logit"),
                       data = male_counts_2)
summary(glm_territorial)


male_counts_2

print(male_counts_2, n = Inf)

#### 3. Do males appear in different canopy cover areas to non-territorial males? 

male_counts_2 %>%
  group_by(territorial) %>%
  summarise(p = shapiro.test(mean_cover)$p.value)

male_counts_2
 
# p > 0.05, data normal. t-test can be ran. 

t.test(mean_cover ~ territorial, data = male_counts_2)

t.test(mean_cover ~ territoriality, data = male_data)

#### Visualisation of data 

## Make sure that territoriality data exists for every 1m 

male_counts_full <- male_counts_2 %>%
  select(distance_m, territorial, damselfly_count, mean_cover, mean_height) %>%
  complete(distance_m, territorial, fill = list(damselfly_count = 0)) %>%
  group_by(distance_m) %>%
  fill(mean_cover, mean_height, .direction = "downup") %>% 
  ungroup()

# Calculating total abundance per distance
total_abundance <- male_counts_full %>%
  group_by(distance_m) %>%
  summarise(
    mean_cover = first(mean_cover),
    total_count = sum(damselfly_count),
    .groups = "drop"
  )

### Visualisation of canopy cover on male abundance 

ggplot(male_counts_full, aes(x = mean_cover, y = damselfly_count, color = territorial)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE) +   
  facet_wrap(~ territorial) +
  scale_color_manual(
    values = c("y" = "#0072B2", "n" = "#D55E00")
  ) +
  labs(
    x = "Canopy Cover (%)",
    y = "Male Abundance"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"      
  )

### Visualisation of canopy height on male abundance 

ggplot(male_counts_full, aes(x = mean_height, y = damselfly_count, color = territorial)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE) +   
  facet_wrap(~ territorial) +
  scale_color_manual(
    values = c("y" = "#0072B2", "n" = "#D55E00")
  ) +
  labs(
    x = "Canopy Height (m)",
    y = "Male Abundance"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"      
  )

##################################################################

#### Distribution of males along the transect ####

#### 1. Visualisation 

ggplot(male_counts, aes(x = distance_m, y = damselfly_count)) +
  geom_point(color = "blue", size = 3) +     
  labs(title = "Male Damselfly Counts Along Transect",
       x = "Distance along transect (m)",
       y = "Number of males") +
  theme_minimal()

#### 2. Assessing whether distribution of territorial and non-territorial differs

# Restructuring data

male_counts_2_expanded <- male_counts_2[rep(1:nrow(male_counts_2), male_counts_2$damselfly_count), ]

# Splitting based on territorial / non - territorial 
territorial <- male_counts_2_expanded$distance_m[male_counts_2_expanded$territorial == "y"]
non_territorial <- male_counts_2_expanded$distance_m[male_counts_2_expanded$territorial == "n"]

# Kolmogorov–Smirnov test

ks.test(territorial, non_territorial)

# Visualisation of territorial / non-territorial distribution

ggplot(male_counts_2_expanded, aes(x = distance_m, color = territorial)) +
  stat_ecdf(size = 1.2) +
  scale_color_manual(
    values = c("y" = "#0072B2", "n" = "#D55E00"),
    labels = c("y" = "Territorial", "n" = "Non-territorial"),
    name = "Territoriality"
  ) +
  labs(
    x = "Distance (m)",
    y = "Proportion of "
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),    # set font to Arial
    plot.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )

#########################################



