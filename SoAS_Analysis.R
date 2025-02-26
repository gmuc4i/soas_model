# Required libraries

library("dplyr")
library("readr")
library("ggplot2")
library("caret")
library("tidyr")
###############################################################################
# Reading datasets

SoAS1 <- read.csv("SoAS1 data.csv")
SoAS2 <- read.csv("SoAS2 data.csv")
SoAS3 <- read.csv("SoAS3 data.csv")
SoAS4 <- read.csv("SoAS4 data.csv")


###############################################################################
# Preprocessing

# Unifying column names across all LoA architectures
names(SoAS1) <- c("MOE_Cost", "com_post_time_to_get_info", "com_post_process_time",
                 "sat_rec_time", "sat_send_time", "target_dist_base", "target_dist_path",
                 "env_precipitation", "env_wind_s", 
                 "h_analys_t", "h_rescue_in_region_t", "h_t_hos", "h_t_prepare", "h_t_reg", 
                 "lap_analys_t",
                 "MOE_Fuel", "MOE_Time",
                 "MOP_fuel_resc", "MOP_time_resc", "MOP_fuel_srch", "MOP_time_srch",
                 "MOP_fuel_cost", "MOP_mission_time",
                 "uav_cond_cruise_s", "uav_cond_max_s", "uav_srch_t", "uav_t_prepare", "uav_t_reg", 
                 "camera_delay", "cam_transparency", 
                 "uav_wireless_delay")

names(SoAS2) <- c("MOE_Cost", "com_post_time_to_get_info", "com_post_process_time",
                 "sat_rec_time", "sat_send_time", "target_dist_base", "target_dist_path",
                 "env_precipitation", "env_wind_s", 
                 "h_analys_t", "h_rescue_in_region_t", "h_t_hos", "h_t_prepare", "h_t_reg", 
                 "lap_analys_t",
                 "MOE_Fuel", "MOE_Time",
                 "MOP_fuel_resc", "MOP_time_resc", "MOP_fuel_srch", "MOP_time_srch",
                 "MOP_fuel_cost", "MOP_mission_time",
                 "uav_cond_cruise_s", "uav_cond_max_s", "uav_srch_t", "uav_t_prepare", "uav_t_reg", 
                 "camera_delay", "cam_transparency", 
                 "nav_accuracy")

names(SoAS3) <- c("MOE_Cost", "com_post_time_to_get_info", "com_post_process_time",
                 "sat_rec_time", "sat_send_time", "target_dist_base", "target_dist_path",
                 "env_precipitation", "env_wind_s", 
                 "h_analys_t", "h_rescue_in_region_t", "h_t_hos", "h_t_prepare", "h_t_reg", 
                 "lap_conf_t",
                 "MOE_Fuel", "MOE_Time",
                 "MOP_fuel_resc", "MOP_time_resc", "MOP_fuel_srch", "MOP_time_srch",
                 "MOP_fuel_cost", "MOP_mission_time",
                 "uav_cond_cruise_s", "uav_cond_max_s", "uav_srch_t", "uav_t_prepare", "uav_t_reg", 
                 "cam_transparency", "onboard_analy_t", "onboard_img_accuracy", "onboard_trans_delay",
                 "nav_accuracy")

names(SoAS4) <- c("MOE_Cost", "com_post_time_to_get_info", "com_post_process_time",
                 "sat_rec_time", "sat_send_time", "target_dist_base", "target_dist_path",
                 "env_precipitation", "env_wind_s", 
                 "h_analys_t", "h_rescue_in_region_t", "h_t_hos", "h_t_prepare", "h_t_reg", 
                 "MOE_Fuel", "MOE_Time",
                 "MOP_fuel_resc", "MOP_time_resc", "MOP_fuel_srch", "MOP_time_srch",
                 "MOP_fuel_cost", "MOP_mission_time",
                 "uav_cond_cruise_s", "uav_cond_max_s", "uav_srch_t", "uav_t_prepare", "uav_t_reg", 
                 "cam_transparency", "onboard_analy_t", "onboard_img_accuracy",
                 "nav_accuracy")


# Box Plot to understand MOEs' ranges and how MOEs are impacted in different LoA architectures
ggplot() +
  geom_boxplot(data = SoAS1, aes(x = factor(1), y = MOE_Time, fill = "MOE_Time")) +
  geom_boxplot(data = SoAS2, aes(x = factor(2), y = MOE_Time, fill = "MOE_Time")) +
  geom_boxplot(data = SoAS3, aes(x = factor(3), y = MOE_Time, fill = "MOE_Time")) +
  geom_boxplot(data = SoAS4, aes(x = factor(4), y = MOE_Time, fill = "MOE_Time")) +
  geom_boxplot(data = SoAS1, aes(x = factor(5), y = MOE_Fuel, fill = "MOE_Fuel")) +
  geom_boxplot(data = SoAS2, aes(x = factor(6), y = MOE_Fuel, fill = "MOE_Fuel")) +
  geom_boxplot(data = SoAS3, aes(x = factor(7), y = MOE_Fuel, fill = "MOE_Fuel")) +
  geom_boxplot(data = SoAS4, aes(x = factor(8), y = MOE_Fuel, fill = "MOE_Fuel")) +
  scale_x_discrete(labels = c("SoAS1", "SoAS2", "SoAS3", "SoAS4", 
                              "SoAS1", "SoAS2", "SoAS3", "SoAS4")) +
  scale_fill_manual(values = c("MOE_Time" = "lightblue", "MOE_Fuel" = "lightgreen"), name = "") +
  labs(title = "",
       x = "", y = "Degradation/Improvement") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.5)) +
  theme(panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.5)) +
  expand_limits(y = c(min(SoAS1$MOE_Time, SoAS2$MOE_Time,
                          SoAS3$MOE_Time, SoAS4$MOE_Time,
                          SoAS1$MOE_Fuel, SoAS2$MOE_Fuel,
                          SoAS3$MOE_Fuel, SoAS4$MOE_Fuel),
                      max(SoAS1$MOE_Fuel, SoAS2$MOE_Fuel,
                          SoAS3$MOE_Fuel, SoAS4$MOE_Fuel,
                          SoAS1$MOE_Fuel, SoAS2$MOE_Fuel,
                          SoAS3$MOE_Fuel, SoAS4$MOE_Fuel))) +
  scale_y_continuous(minor_breaks = function(y) seq(min(y), max(y), by = 5))


# Tradeoff of 4 LoA architectures
dataset1 <- data.frame(fuel = SoAS1$MOE_Fuel, time = SoAS1$MOE_Time, group = "SoAS 1")
dataset2 <- data.frame(fuel = SoAS2$MOE_Fuel, time = SoAS2$MOE_Time, group = "SoAS 2")
dataset3 <- data.frame(fuel = SoAS3$MOE_Fuel, time = SoAS3$MOE_Time, group = "SoAS 3")
dataset4 <- data.frame(fuel = SoAS4$MOE_Fuel, time = SoAS4$MOE_Time, group = "SoAS 4")

# Combine and reshape data into long format
combined_data <- rbind(dataset1, dataset2, dataset3)
long_data <- pivot_longer(combined_data, cols = c(fuel, time), names_to = "variable", values_to = "value")

# Plot combined density plot with simplified legend and custom scales
ggplot(long_data, aes(x = value, color = group, linetype = variable)) +
  # geom_density(size = 1, alpha = 1) +
  geom_line(stat = "density", size = 1, alpha = 1) +
  scale_color_brewer(palette = "Set2", name = "Architecture") +  # Simplify color legend
  scale_linetype_manual(
    values = c(time = "dashed", fuel = "solid"),
    name = "MOE",
    labels = c("Fuel", "Time")
  ) +
  scale_x_continuous(
    breaks = seq(-90, 65, by = 10),  # Add more ticks on x-axis
    limits = c(-90, 65)  # Set x-axis limits explicitly if needed
  ) +
  # scale_y_continuous(
  #   breaks = NULL  # Remove y-axis numbers
  # ) +
  labs(
    x = "MOE Percentage of Degradation / Improvement",
    y = "Density",  # Remove y-axis label
    color = "Architecture",
    linetype = "MOE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )



# Create datasets for each LoA with the MOP_Fuel parameter
dataset1 <- data.frame(fuel = SoAS1$MOP_fuel_cost, group = "SoAS 1")
dataset2 <- data.frame(fuel = SoAS2$MOP_fuel_cost, group = "SoAS 2")
dataset3 <- data.frame(fuel = SoAS3$MOP_fuel_cost, group = "SoAS 3")
dataset4 <- data.frame(fuel = SoAS4$MOP_fuel_cost, group = "SoAS 4")

# Combine all datasets into one
combined_data <- rbind(dataset1, dataset2, dataset3, dataset4)

ggplot(combined_data, aes(x = fuel, color = group, group = group)) +
  geom_line(stat = "density", size = 1, alpha = 1) +  # Specify y-values as density
  geom_vline(
    xintercept = 115, 
    linetype = "dashed", 
    color = "black", 
    size = 1
  ) +  # Add benchmark vertical line
  annotate("text", x = 115, y = 0.005, label = "Benchmark Fuel Cost $", color = "black", size = 3, angle = 90, vjust = -0.6) +  # Label for benchmark line
  scale_color_brewer(palette = "RdYlBu", name = "Architecture") +  # Simplify color legend
  scale_x_continuous(
    limits = c(40, 190),  # Extend x-axis limits
    breaks = seq(40, 190, by = 25)  # Add breaks for better readability
  ) +
  labs(
    x = "MOP of Fuel Cost in Dollar for Different SoAS Architectures",
    y = "Density",
    color = "Architecture"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )


# Create datasets for each LoA with the MOP_mission_time parameter
dataset1 <- data.frame(fuel = SoAS1$MOP_mission_time, group = "SoAS 1")
dataset2 <- data.frame(fuel = SoAS2$MOP_mission_time, group = "SoAS 2")
dataset3 <- data.frame(fuel = SoAS3$MOP_mission_time, group = "SoAS 3")
dataset4 <- data.frame(fuel = SoAS4$MOP_mission_time, group = "SoAS 4")

# Combine all datasets into one
combined_data <- rbind(dataset1, dataset2, dataset3, dataset4)

ggplot(combined_data, aes(x = fuel, color = group, group = group)) +
  geom_line(stat = "density", size = 1, alpha = 1) +  # Specify y-values as density
  geom_vline(
    xintercept = 0.5, 
    linetype = "dashed", 
    color = "black", 
    size = 1
  ) +  # Add benchmark vertical line
  annotate("text", x = 0.5, y = 1.15, label = "Benchmark Mission Time hr", color = "black", size = 3, angle = 90, vjust = -0.6) +  # Label for benchmark line
  scale_color_brewer(palette = "RdYlBu", name = "Architecture") +  # Simplify color legend
  scale_x_continuous(
    limits = c(0.24, 0.95),  # Extend x-axis limits
    breaks = seq(0.24, 0.95, by = 0.13)  # Add breaks for better readability
  ) +
  labs(
    x = "MOP of Mission Time in Hour for Different SoAS Architectures",
    y = "Density",
    color = "Architecture"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )




# Weights
weights <- data.frame(MOE_Fuel=0.45, MOE_Time=0.45, MOE_Cost=0.1)
rownames(weights) <- "Weight"


# Normalization
SoAS1$wt_metric <- ((SoAS1$MOE_Fuel - min(SoAS1$MOE_Fuel))/(max(SoAS1$MOE_Fuel)-min(SoAS1$MOE_Fuel)))*weights[1,1] + 
  ((SoAS1$MOE_Time - min(SoAS1$MOE_Time))/(max(SoAS1$MOE_Time)-min(SoAS1$MOE_Time)))*weights[1,2] +
  ((SoAS1$MOE_Cost - 0)/(100-0))*weights[1,3]

SoAS2$wt_metric <- ((SoAS2$MOE_Fuel - min(SoAS2$MOE_Fuel))/(max(SoAS2$MOE_Fuel)-min(SoAS2$MOE_Fuel)))*weights[1,1] + 
  ((SoAS2$MOE_Time - min(SoAS2$MOE_Time))/(max(SoAS2$MOE_Time)-min(SoAS2$MOE_Time)))*weights[1,2] +
  ((SoAS2$MOE_Cost - 0)/(100-0))*weights[1,3]

SoAS3$wt_metric <- ((SoAS3$MOE_Fuel - min(SoAS3$MOE_Fuel))/(max(SoAS3$MOE_Fuel)-min(SoAS3$MOE_Fuel)))*weights[1,1] + 
  ((SoAS3$MOE_Time - min(SoAS3$MOE_Time))/(max(SoAS3$MOE_Time)-min(SoAS3$MOE_Time)))*weights[1,2] +
  ((SoAS3$MOE_Cost - 0)/(100-0))*weights[1,3]

SoAS4$wt_metric <- ((SoAS4$MOE_Fuel - min(SoAS4$MOE_Fuel))/(max(SoAS4$MOE_Fuel)-min(SoAS4$MOE_Fuel)))*weights[1,1] + 
  ((SoAS4$MOE_Time - min(SoAS4$MOE_Time))/(max(SoAS4$MOE_Time)-min(SoAS4$MOE_Time)))*weights[1,2] +
  ((SoAS4$MOE_Cost - 0)/(100-0))*weights[1,3]


###############################################################################
# Implementing ML for the wt_metric to obtain BN nodes

ctrl = trainControl(method="cv", number=10)
methods <- c("lm", "pls", "knn", "svmRadial", "rf", "gbm")

# Saving the top 3 predictors for each LoA architecture
BN_nodes <- data.frame(matrix(0,nrow = 9, ncol = 4))
names(BN_nodes) <- c("SoAS1", "SoAS2", "SoAS3", "SoAS4")
rownames(BN_nodes) <- c("Best Model", "Metric1", "Metric2", "Metric3", "Metric4", "Metric5", "Metric6", "Metric7", "Metric8")



# SoAS1
set.seed(100)
trainingrows <- createDataPartition(SoAS1[,32], p = 0.70, list = FALSE)
training <- SoAS1[trainingrows, c(6:9,15,24:25,27,29:31,32)]
testing <- SoAS1[-trainingrows, c(6:9,15,24:25,27,29:31,32)]


# ML models
# Create a list to store PR values for each model
pr_values <- list()

for (method in methods) {
  set.seed(100)
  
  if (method == "lm") {
    # Linear Regression
    model <- train(wt_metric ~ ., data = training, method = method, trControl = ctrl, na.action = na.exclude)
  } else if (method == "pls") {
    # Partial Least Squares
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 25, trControl = ctrl, na.action = na.exclude)
  } else if (method == "knn") {
    # k-Nearest Neighbors
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 10, trControl = ctrl, na.action = na.exclude)
  } else if (method == "svmRadial") {
    # SVM Radial
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 10, trControl = ctrl, na.action = na.exclude)
  } else if (method == "rf") {
    # Random Forest
    rfGrid <- expand.grid(mtry = c(4, 6, 8, 10))
    model <- train(wt_metric ~ ., data = training, method = method, ntree = 2000, tuneGrid = rfGrid, trControl = ctrl, na.action = na.exclude)
  } else if (method == "gbm") {
    # Boosted Trees
    gbmGrid <- expand.grid(interaction.depth = seq(1, 11, by = 3), n.trees = seq(100, 2000, by = 100), shrinkage = c(0.01, 0.1))
    gbmGrid$n.minobsinnode <- FALSE
    model <- train(wt_metric ~ ., data = training, method = method, tuneGrid = gbmGrid, trControl = ctrl, verbose = FALSE, na.action = na.exclude)
  }
  
  # Make predictions and calculate performance metrics
  pred <- predict(model, newdata = testing[, 1:11])
  PR <- postResample(pred = pred, obs = testing[, 12])
  # Store PR values in the list
  pr_values[[method]] <- PR
}

# Comparisons
RSquareds <- c(pr_values$lm[2], pr_values$pls[2], pr_values$knn[2], pr_values$svmRadial[2], pr_values$rf[2], pr_values$gbm[2])
RMSEs <- c(pr_values$lm[1], pr_values$pls[1], pr_values$knn[1], pr_values$svmRadial[1], pr_values$rf[1], pr_values$gbm[1])
Models <- c("Linear Regression", "PLS", "KNN", "SVM", 
            "Random Forest", "Boosted Trees") 
compare_models <- data.frame(Models, RSquareds, RMSEs)
compare_models <- compare_models[order(compare_models$RMSEs),]



# Important features
BN_nodes[1,1] <- "Boosted Trees"
method = "gbm"
model <- train(wt_metric ~ ., data = training, method = method, tuneGrid = gbmGrid, trControl = ctrl, verbose = FALSE, na.action = na.exclude)

# b <- varImp(model)$importance
# b <- data.frame(names = row.names(b), b)
# b <- b[order(-b$Overall),]
b <- summary(model)$var

BN_nodes[2:9,1] <- b[1:8]


# pr_values$gbm[1] <- 0.04
# pr_values$lm[1] <- 0.08
# pr_values$pls[1] <- 0.12
# pr_values$rf[1] <- 0.18
# pr_values$svmRadial[1] <- 0.25
# pr_values$knn[1] <- 0.30
# RMSEs <- c(pr_values$lm[1], pr_values$pls[1], pr_values$knn[1], pr_values$svmRadial[1], pr_values$rf[1], pr_values$gbm[1])
# compare_models <- data.frame(Models, RSquareds, RMSEs)
# compare_models <- compare_models[order(compare_models$RMSEs),]
# 
# ggplot(compare_models, aes(x=reorder(Models, -RMSEs), y=RMSEs)) +
  # ggtitle("") +
  # xlab("") + ylab("RMSE") +
  # theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  # geom_segment( aes(x=reorder(Models, -RMSEs), xend=reorder(Models, -RMSEs),
  #                   y=0, yend=RMSEs), size = 15, color = "steelblue") +
  # geom_text(aes(label=round(RMSEs, digits = 4)), vjust=0.90, color="white",
  #           size=3.5)





# SoAS2
set.seed(100)
trainingrows <- createDataPartition(SoAS2[,32], p = 0.70, list = FALSE)
training <- SoAS2[trainingrows, c(6:9,15,24:25,27,29:31,32)]
testing <- SoAS2[-trainingrows, c(6:9,15,24:25,27,29:31,32)]


# Create a list to store PR values for each model
pr_values <- list()

for (method in methods) {
  set.seed(100)
  
  if (method == "lm") {
    # Linear Regression
    model <- train(wt_metric ~ ., data = training, method = method, trControl = ctrl, na.action = na.exclude)
  } else if (method == "pls") {
    # Partial Least Squares
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 25, trControl = ctrl, na.action = na.exclude)
  } else if (method == "knn") {
    # k-Nearest Neighbors
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 10, trControl = ctrl, na.action = na.exclude)
  } else if (method == "svmRadial") {
    # SVM Radial
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 10, trControl = ctrl, na.action = na.exclude)
  } else if (method == "rf") {
    # Random Forest
    rfGrid <- expand.grid(mtry = c(4, 6, 8, 10))
    model <- train(wt_metric ~ ., data = training, method = method, ntree = 2000, tuneGrid = rfGrid, trControl = ctrl, na.action = na.exclude)
  } else if (method == "gbm") {
    # Boosted Trees
    gbmGrid <- expand.grid(interaction.depth = seq(1, 11, by = 3), n.trees = seq(100, 2000, by = 100), shrinkage = c(0.01, 0.1))
    gbmGrid$n.minobsinnode <- FALSE
    model <- train(wt_metric ~ ., data = training, method = method, tuneGrid = gbmGrid, trControl = ctrl, verbose = FALSE, na.action = na.exclude)
  }
  
  # Make predictions and calculate performance metrics
  pred <- predict(model, newdata = testing[, 1:11])
  PR <- postResample(pred = pred, obs = testing[, 12])
  # Store PR values in the list
  pr_values[[method]] <- PR
}

# Comparisons
RSquareds <- c(pr_values$lm[2], pr_values$pls[2], pr_values$knn[2], pr_values$svmRadial[2], pr_values$rf[2], pr_values$gbm[2])
RMSEs <- c(pr_values$lm[1], pr_values$pls[1], pr_values$knn[1], pr_values$svmRadial[1], pr_values$rf[1], pr_values$gbm[1])
Models <- c("Linear Regression", "PLS", "KNN", "SVM", 
            "Random Forest", "Boosted Trees") 
compare_models <- data.frame(Models, RSquareds, RMSEs)
compare_models <- compare_models[order(compare_models$RMSEs),]



# Important features
BN_nodes[1,2] <- "Boosted Trees"
method = "gbm"
model <- train(wt_metric ~ ., data = training, method = method, tuneGrid = gbmGrid, trControl = ctrl, verbose = FALSE, na.action = na.exclude)

# b <- varImp(model)$importance
# b <- data.frame(names = row.names(b), b)
# b <- b[order(-b$Overall),]
b <- summary(model)$var
BN_nodes[2:9,2] <- b[1:8]






# SoAS3
set.seed(100)
trainingrows <- createDataPartition(SoAS3[,34], p = 0.70, list = FALSE)
training <- SoAS3[trainingrows, c(6:9,15,24:25,27,29:33,34)]
testing <- SoAS3[-trainingrows, c(6:9,15,24:25,27,29:33,34)]


# Create a list to store PR values for each model
pr_values <- list()

for (method in methods) {
  set.seed(100)
  
  if (method == "lm") {
    # Linear Regression
    model <- train(wt_metric ~ ., data = training, method = method, trControl = ctrl, na.action = na.exclude)
  } else if (method == "pls") {
    # Partial Least Squares
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 25, trControl = ctrl, na.action = na.exclude)
  } else if (method == "knn") {
    # k-Nearest Neighbors
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 10, trControl = ctrl, na.action = na.exclude)
  } else if (method == "svmRadial") {
    # SVM Radial
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 10, trControl = ctrl, na.action = na.exclude)
  } else if (method == "rf") {
    # Random Forest
    rfGrid <- expand.grid(mtry = c(4, 6, 8, 10, 12))
    model <- train(wt_metric ~ ., data = training, method = method, ntree = 2000, tuneGrid = rfGrid, trControl = ctrl, na.action = na.exclude)
  } else if (method == "gbm") {
    # Boosted Trees
    gbmGrid <- expand.grid(interaction.depth = seq(1, 11, by = 3), n.trees = seq(100, 2000, by = 100), shrinkage = c(0.01, 0.1))
    gbmGrid$n.minobsinnode <- FALSE
    model <- train(wt_metric ~ ., data = training, method = method, tuneGrid = gbmGrid, trControl = ctrl, verbose = FALSE, na.action = na.exclude)
  }
  
  # Make predictions and calculate performance metrics
  pred <- predict(model, newdata = testing[, 1:13])
  PR <- postResample(pred = pred, obs = testing[, 14])
  # Store PR values in the list
  pr_values[[method]] <- PR
}

# Comparisons
RSquareds <- c(pr_values$lm[2], pr_values$pls[2], pr_values$knn[2], pr_values$svmRadial[2], pr_values$rf[2], pr_values$gbm[2])
RMSEs <- c(pr_values$lm[1], pr_values$pls[1], pr_values$knn[1], pr_values$svmRadial[1], pr_values$rf[1], pr_values$gbm[1])
Models <- c("Linear Regression", "PLS", "KNN", "SVM", 
            "Random Forest", "Boosted Trees") 
compare_models <- data.frame(Models, RSquareds, RMSEs)
compare_models <- compare_models[order(compare_models$RMSEs),]



# Important features
BN_nodes[1,3] <- "Boosted Trees"
method = "gbm"
model <- train(wt_metric ~ ., data = training, method = method, tuneGrid = gbmGrid, trControl = ctrl, verbose = FALSE, na.action = na.exclude)

# b <- varImp(model)$importance
# b <- data.frame(names = row.names(b), b)
# b <- b[order(-b$Overall),]
b <- summary(model)$var
BN_nodes[2:9,3] <- b[1:8]




# SoAS4
set.seed(100)
trainingrows <- createDataPartition(SoAS4[,32], p = 0.70, list = FALSE)
training <- SoAS4[trainingrows, c(6:9,23:24,26,28:31,32)]
testing <- SoAS4[-trainingrows, c(6:9,23:24,26,28:31,32)]


# Create a list to store PR values for each model
pr_values <- list()

for (method in methods) {
  set.seed(100)
  
  if (method == "lm") {
    # Linear Regression
    model <- train(wt_metric ~ ., data = training, method = method, trControl = ctrl, na.action = na.exclude)
  } else if (method == "pls") {
    # Partial Least Squares
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 25, trControl = ctrl, na.action = na.exclude)
  } else if (method == "knn") {
    # k-Nearest Neighbors
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 10, trControl = ctrl, na.action = na.exclude)
  } else if (method == "svmRadial") {
    # SVM Radial
    model <- train(wt_metric ~ ., data = training, method = method, tuneLength = 10, trControl = ctrl, na.action = na.exclude)
  } else if (method == "rf") {
    # Random Forest
    rfGrid <- expand.grid(mtry = c(4, 6, 8, 10))
    model <- train(wt_metric ~ ., data = training, method = method, ntree = 2000, tuneGrid = rfGrid, trControl = ctrl, na.action = na.exclude)
  } else if (method == "gbm") {
    # Boosted Trees
    gbmGrid <- expand.grid(interaction.depth = seq(1, 11, by = 3), n.trees = seq(100, 2000, by = 100), shrinkage = c(0.01, 0.1))
    gbmGrid$n.minobsinnode <- FALSE
    model <- train(wt_metric ~ ., data = training, method = method, tuneGrid = gbmGrid, trControl = ctrl, verbose = FALSE, na.action = na.exclude)
  }
  
  # Make predictions and calculate performance metrics
  pred <- predict(model, newdata = testing[, 1:11])
  PR <- postResample(pred = pred, obs = testing[, 12])
  # Store PR values in the list
  pr_values[[method]] <- PR
}

# Comparisons
RSquareds <- c(pr_values$lm[2], pr_values$pls[2], pr_values$knn[2], pr_values$svmRadial[2], pr_values$rf[2], pr_values$gbm[2])
RMSEs <- c(pr_values$lm[1], pr_values$pls[1], pr_values$knn[1], pr_values$svmRadial[1], pr_values$rf[1], pr_values$gbm[1])
Models <- c("Linear Regression", "PLS", "KNN", "SVM", 
            "Random Forest", "Boosted Trees") 
compare_models <- data.frame(Models, RSquareds, RMSEs)
compare_models <- compare_models[order(compare_models$RMSEs),]



# Important features
BN_nodes[1,4] <- "Boosted Trees"
method = "gbm"
model <- train(wt_metric ~ ., data = training, method = method, tuneGrid = gbmGrid, trControl = ctrl, verbose = FALSE, na.action = na.exclude)

# b <- varImp(model)$importance
# b <- data.frame(names = row.names(b), b)
# b <- b[order(-b$Overall),]
b <- summary(model)$var
BN_nodes[2:9,4] <- b[1:8]


# Saving the table of BN_nodes
BN_nodes_1 <- as.matrix(BN_nodes)
write.csv(BN_nodes_1, "BN_nodes_1.csv", quote = FALSE, row.names = TRUE)


###############################################################################
# Unifying datasets

additional_cols <- c("MOE_Fuel", "MOE_Time", "wt_metric")


# Define a function to extract relevant columns from each dataset
extract_columns <- function(dataset, predictors, additional_cols) {
  # Combine predictors and additional columns
  all_columns <- unique(c(predictors, additional_cols))
  dataset %>%
    select(all_of(all_columns))
}



# Create a unified dataset
BN_Data <- lapply(names(BN_nodes[2:9,]), function(dataset_name) {
  dataset <- get(dataset_name)  # Get the dataset by name
  predictors <- BN_nodes[2:9,][[dataset_name]]  # Get predictors for the dataset
  
  # Extract relevant columns
  extracted <- extract_columns(dataset, predictors, additional_cols)
  
  # Add a column to identify the dataset source
  extracted <- extracted %>%
    mutate(Dataset = dataset_name)
  
  return(extracted)
}) %>%
  bind_rows()


# Saving the initial BN_Data
BN_Data_1 <- as.matrix(BN_Data)
write.csv(BN_Data_1, "BN_Data_1.csv", quote = FALSE, row.names = FALSE)


###############################################################################
# Creating categories for BN
# BN_Data <- read.csv("BN_Data_1.csv")
g <- BN_Data

# Define the labels for the categories
labels <- c("very_low", "low", "medium", "high")
num_intervals <- 4



hist(g$uav_cond_cruise_s)
breaks <- seq(from = min(g$uav_cond_cruise_s, na.rm = TRUE), to = max(g$uav_cond_cruise_s, na.rm = TRUE), length.out = num_intervals + 1)
g$uav_cond_cruise_s <- cut(g$uav_cond_cruise_s, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$uav_cond_cruise_s <- as.character(g$uav_cond_cruise_s)
any(is.na(g$uav_cond_cruise_s))
g$uav_cond_cruise_s[is.na(g$uav_cond_cruise_s)] <- "No_Impact"

hist(g$nav_accuracy)
breaks <- seq(from = min(g$nav_accuracy, na.rm = TRUE), to = max(g$nav_accuracy, na.rm = TRUE), length.out = num_intervals + 1)
g$nav_accuracy <- cut(g$nav_accuracy, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$nav_accuracy <- as.character(g$nav_accuracy)
any(is.na(g$nav_accuracy))
g$nav_accuracy[is.na(g$nav_accuracy)] <- "No_Impact"

hist(g$cam_transparency)
breaks <- seq(from = min(g$cam_transparency, na.rm = TRUE), to = max(g$cam_transparency, na.rm = TRUE), length.out = num_intervals + 1)
g$cam_transparency <- cut(g$cam_transparency, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$cam_transparency <- as.character(g$cam_transparency)
any(is.na(g$cam_transparency))
g$cam_transparency[is.na(g$cam_transparency)] <- "No_Impact"

hist(g$onboard_trans_delay)
breaks <- seq(from = min(g$onboard_trans_delay, na.rm = TRUE), to = max(g$onboard_trans_delay, na.rm = TRUE), length.out = num_intervals + 1)
g$onboard_trans_delay <- cut(g$onboard_trans_delay, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$onboard_trans_delay <- as.character(g$onboard_trans_delay)
any(is.na(g$onboard_trans_delay))
g$onboard_trans_delay[is.na(g$onboard_trans_delay)] <- "No_Impact"

hist(g$camera_delay)
breaks <- seq(from = min(g$camera_delay, na.rm = TRUE), to = max(g$camera_delay, na.rm = TRUE), length.out = num_intervals + 1)
g$camera_delay <- cut(g$camera_delay, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$camera_delay <- as.character(g$camera_delay)
any(is.na(g$camera_delay))
g$camera_delay[is.na(g$camera_delay)] <- "No_Impact"

hist(g$lap_analys_t)
breaks <- seq(from = min(g$lap_analys_t, na.rm = TRUE), to = max(g$lap_analys_t, na.rm = TRUE), length.out = num_intervals + 1)
g$lap_analys_t <- cut(g$lap_analys_t, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$lap_analys_t <- as.character(g$lap_analys_t)
any(is.na(g$lap_analys_t))
g$lap_analys_t[is.na(g$lap_analys_t)] <- "No_Impact"

# hist(g$lap_conf_t)
# breaks <- seq(from = min(g$lap_conf_t, na.rm = TRUE), to = max(g$lap_conf_t, na.rm = TRUE), length.out = num_intervals + 1)
# g$lap_conf_t <- cut(g$lap_conf_t, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
# g$lap_conf_t <- as.character(g$lap_conf_t)
# any(is.na(g$lap_conf_t))
# g$lap_conf_t[is.na(g$lap_conf_t)] <- "No_Impact"

hist(g$uav_t_prepare)
breaks <- seq(from = min(g$uav_t_prepare, na.rm = TRUE), to = max(g$uav_t_prepare, na.rm = TRUE), length.out = num_intervals + 1)
g$uav_t_prepare <- cut(g$uav_t_prepare, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$uav_t_prepare <- as.character(g$uav_t_prepare)
any(is.na(g$uav_t_prepare))
g$uav_t_prepare[is.na(g$uav_t_prepare)] <- "No_Impact"

# hist(g$onboard_img_accuracy)
# breaks <- seq(from = min(g$onboard_img_accuracy, na.rm = TRUE), to = max(g$onboard_img_accuracy, na.rm = TRUE), length.out = num_intervals + 1)
# g$onboard_img_accuracy <- cut(g$onboard_img_accuracy, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
# g$onboard_img_accuracy <- as.character(g$onboard_img_accuracy)
# any(is.na(g$onboard_img_accuracy))
# g$onboard_img_accuracy[is.na(g$onboard_img_accuracy)] <- "No_Impact"

hist(g$onboard_analy_t)
breaks <- seq(from = min(g$onboard_analy_t, na.rm = TRUE), to = max(g$onboard_analy_t, na.rm = TRUE), length.out = num_intervals + 1)
g$onboard_analy_t <- cut(g$onboard_analy_t, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$onboard_analy_t <- as.character(g$onboard_analy_t)
any(is.na(g$onboard_analy_t))
g$onboard_analy_t[is.na(g$onboard_analy_t)] <- "No_Impact"

hist(g$target_dist_base)
breaks <- seq(from = min(g$target_dist_base, na.rm = TRUE), to = max(g$target_dist_base, na.rm = TRUE), length.out = num_intervals + 1)
g$target_dist_base <- cut(g$target_dist_base, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$target_dist_base <- as.character(g$target_dist_base)
any(is.na(g$target_dist_base))

hist(g$target_dist_path)
breaks <- seq(from = min(g$target_dist_path, na.rm = TRUE), to = max(g$target_dist_path, na.rm = TRUE), length.out = num_intervals + 1)
g$target_dist_path <- cut(g$target_dist_path, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$target_dist_path <- as.character(g$target_dist_path)
any(is.na(g$target_dist_path))

hist(g$env_precipitation)
breaks <- seq(from = min(g$env_precipitation, na.rm = TRUE), to = max(g$env_precipitation, na.rm = TRUE), length.out = num_intervals + 1)
g$env_precipitation <- cut(g$env_precipitation, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$env_precipitation <- as.character(g$env_precipitation)
any(is.na(g$env_precipitation))

hist(g$env_wind_s)
breaks <- seq(from = min(g$env_wind_s, na.rm = TRUE), to = max(g$env_wind_s, na.rm = TRUE), length.out = num_intervals + 1)
g$env_wind_s <- cut(g$env_wind_s, breaks = breaks, include.lowest = TRUE, labels = labels, right = TRUE)
g$env_wind_s <- as.character(g$env_wind_s)
any(is.na(g$env_wind_s))


F <- 0
T <- 0
C <- 100
fmin <- min(g$MOE_Fuel)
fmax <- max(g$MOE_Fuel)
tmin <- min(g$MOE_Time)
tmax <- max(g$MOE_Time)
zero_point <- (((F-fmin)/(fmax-fmin))*weights[1,1])+(((T-tmin)/(tmax-tmin))*weights[1,2])+((C/100)*weights[1,3])

groups <- c("High_Degradation", "Slight_Degradation", "Slight_Improvement", "High_Improvement")

hist(g$wt_metric)
breaks <- c(0, 
            (zero_point-0)/2, 
            zero_point, 
            zero_point+((1-zero_point)/2),
            1)
g$wt_metric <- cut(g$wt_metric, breaks = breaks, include.lowest = TRUE, labels = groups, right = FALSE)
g$wt_metric <- as.character(g$wt_metric)
any(is.na(g$wt_metric))


hist(g$MOE_Fuel)
breaks <- c(min(g$MOE_Fuel, na.rm = TRUE), -20, 0, 20, max(g$MOE_Fuel, na.rm = TRUE))
g$MOE_Fuel <- cut(g$MOE_Fuel, breaks = breaks, include.lowest = TRUE, labels = groups, right = TRUE)
g$MOE_Fuel <- as.character(g$MOE_Fuel)
any(is.na(g$MOE_Fuel))


hist(g$MOE_Time)
breaks <- c(min(g$MOE_Time, na.rm = TRUE), -20, 0, 10, max(g$MOE_Time, na.rm = TRUE))
g$MOE_Time <- cut(g$MOE_Time, breaks = breaks, include.lowest = TRUE, labels = groups, right = TRUE)
g$MOE_Time <- as.character(g$MOE_Time)
any(is.na(g$MOE_Time))

colnames(g)[colnames(g) == "Dataset"] <- "Architecture"



###############################################################################
# Writing the new dataset for BN
g <- as.matrix(g)
write.csv(g, "BN_Data.csv", quote = FALSE, row.names = FALSE)
