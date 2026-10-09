#Title: Practicum 1
#Author: Kerri Rose Riley
#Purpose: Replicate Figures 1 and 2 from Bartels (2000), “Partisanship and Voting Behavior”
#Requires: Cumulative CCES Dataset (2006-2024)
#Output: Figures as PDFs (ggplot only) 

# Packages
library(tidyverse) # Load the tidyverse package for data wrangling and analysis
library(haven) # Load haven so read_dta() can be used to import Stata data files
library(ggplot2) # Load ggplot2 for creating graphs and plots


# Set the working directory
setwd("~/Desktop/Current Classes/R/Practicum 1")

# Read the data file
ces <- read_dta("~/Desktop/Current Classes/R/Practicum 1/Data/ces.dta")
 
# Select the variables Bartel would use
ces_1 <- ces |> select(year, pid7, vv_turnout_gvm)
 
table(ces_1$year)
table(ces_1$pid7)
table(ces_1$vv_turnout_gvm)


# Recode party identification for Figure 1

ces_1 <- ces_1 |>
  mutate(
    # Exclude non-substantive responses
    pid = if_else(pid7 %in% 1:7, as.numeric(pid7), NA_real_),

    # Strong partisans
    strong = case_when(
      pid %in% c(1, 7) ~ 1,
      pid %in% 2:6 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Weak partisans
    weak = case_when(
      pid %in% c(2, 6) ~ 1,
      pid %in% c(1, 3, 4, 5, 7) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Party leaners
    leaners = case_when(
      pid %in% c(3, 5) ~ 1,
      pid %in% c(1, 2, 4, 6, 7) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Pure independents
    indeps = case_when(
      pid == 4 ~ 1,
      pid %in% c(1, 2, 3, 5, 6, 7) ~ 0,
      TRUE ~ NA_real_
    )
    
  )

# Figure 2 coding
ces_1 <- ces_1 |>
  mutate(
    # Convert turnout variable to readable labels
    turnout_label = as_factor(vv_turnout_gvm),
    
    # Identify party identifiers
    identifiers = case_when(
      pid %in% c(1, 2, 6, 7) ~ 1,
      pid %in% c(3, 4, 5) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Party identifiers among voters
    identvote = if_else(
      turnout_label == "Voted",
      identifiers,
      NA_real_
    ),
    
    # Party identifiers among those with no voting record
    identnovote = if_else(
      turnout_label == "No Record of Voting",
      identifiers,
      NA_real_
    ),
    
    # Recode turnout
    turnout = case_when(
      turnout_label == "Voted" ~ 1,
      turnout_label == "No Record of Voting" ~ 0,
      TRUE ~ NA_real_
    )
  )



# Collapse the data by year
figure1_data <- ces_1 |>
  group_by(year) |>
  summarise(
    strong = mean(strong, na.rm = TRUE),
    weak = mean(weak, na.rm = TRUE),
    leaners = mean(leaners, na.rm = TRUE),
    indeps = mean(indeps, na.rm = TRUE)
  )

# Plot strong and weak partisans
plot(x = 0, type = "n",
     xlim = range(figure1_data$year),
     ylim = c(0, 0.6),
     xlab = "Year",
     ylab = "Proportion",
     main = "Figure 1a: Party Identification")

lines(figure1_data$year, figure1_data$strong,
      type = "o", col = "black", lty = 1)

lines(figure1_data$year, figure1_data$weak,
      type = "o", col = "black", lty = 2)

legend("topright",
       legend = c("Strong Partisans", "Weak Partisans"),
       col = c("black", "black"),
       lty = c(1, 2),
       pch = 1)

# Collapse the data by year
figure1_data <- ces_1 |>
  group_by(year) |>
  summarise(
    strong = mean(strong, na.rm = TRUE),
    weak = mean(weak, na.rm = TRUE),
    leaners = mean(leaners, na.rm = TRUE),
    indeps = mean(indeps, na.rm = TRUE)
  )

# Plot strong and weak partisans
plot(x = 0, type = "n",
     xlim = c(2006, 2024), xaxt = "n",
     ylim = c(0, 0.6), yaxt = "n",
     xlab = "Year",
     ylab = "Proportion",
     main = "Figure 1a")

axis(1, at = seq(2008, 2024, by = 4))
axis(2)

lines(figure1_data$year, figure1_data$strong,
      type = "o", col = "black", lty = 1)

lines(figure1_data$year, figure1_data$weak,
      type = "o", col = "black", lty = 2)

legend("topright",
       legend = c("Strong", "Weak"),
       col = c("black", "black"),
       lty = c(1, 2), pch = 1)
# Indepents and Leaners

plot(x = 0, type = "n",
     xlim = c(2006, 2024), xaxt = "n",
     ylim = c(0, 0.6), yaxt = "n",
     xlab = "Year",
     ylab = "Proportion",
     main = "Figure 1b")

axis(1, at = seq(2008, 2024, by = 4))
axis(2)

lines(figure1_data$year, figure1_data$indeps,
      type = "o", col = "black", lty = 1)

lines(figure1_data$year, figure1_data$leaners,
      type = "o", col = "black", lty = 2)

legend("topright",
       legend = c("Independents", "Leaners"),
       col = c("black", "black"),
       lty = c(1, 2), pch = 1)


