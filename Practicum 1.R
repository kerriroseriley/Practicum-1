#Title: Practicum 1
#Author: Kerri Rose Riley
#Purpose: Replicate Figures 1 and 2 from Bartels (2000), “Partisanship and Voting Behavior”
#Requires: Cumulative CCES Dataset (2006-2024)
#Output: Figures and Quarto Document typeset as a .pdf
 
# Packages
library(tidyverse) # Load the tidyverse package for data wrangling and analysis
library(haven) # Load haven so read_dta() can be used to import Stata data files

# Set the working directory
setwd("~/Desktop/Current Classes/R/Practicum 1")

# Read the data file
ces <- read_dta("~/Desktop/Current Classes/R/Practicum 1/Data/ces.dta")
 
# Select the variables Bartel would use
ces_1 <- ces |> select(year, pid7, vv_turnout_gvm)

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
    
    # Party Leaners
    leaners = case_when(
      pid %in% c(3, 5) ~ 1,
      pid %in% c(1, 2, 4, 6, 7) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Pure Independents
    indeps = case_when(
      pid == 4 ~ 1,
      pid %in% c(1, 2, 3, 5, 6, 7) ~ 0,
      TRUE ~ NA_real_
    )
    
  )

# Calculate the mean for each year
figure1_data <- ces_1 |>
  group_by(year) |>
  summarise(
    strong = mean(strong, na.rm = TRUE),
    weak = mean(weak, na.rm = TRUE),
    leaners = mean(leaners, na.rm = TRUE),
    indeps = mean(indeps, na.rm = TRUE),
    .groups = "drop"
  )

# Graphing Section: Base R
# Figure 1A: Strong and weak partisans
plot(x = 0, type = "n",
     xlim = range(figure1_data$year),xaxt = "n",
     ylim = c(0, 0.5), yaxt = "n",
     xlab = " ",
     ylab = " ",
     main = "Distribution of Party Identification",)

# Add axis labels
axis(side = 1, at = seq(2006, 2024, by = 8))
axis(side = 2, at = seq(0, 0.5, by = 0.1))

# Add line for weal identifiers
lines(figure1_data$year, figure1_data$strong,
      type = "o", col = "black", lty = 1, pch=16)

# Add line for strong identifiers
lines(figure1_data$year, figure1_data$weak,
      type = "o", col = "black", lty = 2)

# Add legend
legend("bottom",
       legend = c("Strong Identifiers", "Weak Identifiers"),
       lty = c(2, 1), pch = c(1, 16),
       bty = "n")

# Figure 1B: Independents and leaners
plot(x = 0, type = "n",
     xlim = range(figure1_data$year), xaxt = "n",
     ylim = c(0, 0.5), yaxt = "n",
     xlab = " ",
     ylab = " ",
     main=" ")

# Add line for independent leaners
lines(figure1_data$year, figure1_data$indeps,
      type = "o", col = "black", lty = 1, pch=16)

# Add line for pure independents
lines(figure1_data$year, figure1_data$leaners,
      type = "o", col = "black", lty = 2)

# Add axis labels
axis(side = 1, at = seq(2006, 2024, by = 8))
axis(side = 2, at = seq(0, 0.5, by = 0.1))

#Add legend
legend("topleft",
       legend = c("Pure Independents", "Independent Leaners"),
       lty = c(2, 1), pch = c(1, 16),
       bty = "n")

# Figure 2: Recoding
ces_1 <- ces_1 |>
  mutate(
    turnout_label = trimws(as.character(as_factor(vv_turnout_gvm))),
    
    # Identify strong or weak partisans
    identifiers = case_when(
      pid %in% c(1, 2, 6, 7) ~ 1,
      pid %in% c(3, 4, 5) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Party identifiers among voters
    identvote = case_when(
      turnout_label == "Voted" ~ identifiers,
      TRUE ~ NA_real_
    ),
    
    # Party identifiers among nonvoters
    identnovote = case_when(
      turnout_label == "No Record of Voting" ~ identifiers,
      TRUE ~ NA_real_
    )
  )

# Calculate yearly means for Figure 2
figure2_data <- ces_1 |>
  group_by(year) |>
  summarise(
    voters = if (all(is.na(identvote))) NA_real_
    else mean(identvote, na.rm = TRUE),
    nonvoters = if (all(is.na(identnovote))) NA_real_
    else mean(identnovote, na.rm = TRUE),
    .groups = "drop"
  )

# Figure 2: Party Identification by Voting Status
plot(x = 0, type = "n",
     xlim = range(figure2_data$year), xaxt = "n",
     ylim = c(0, 1), yaxt = "n",
     xlab = "Year",
     ylab = "Proportion of Party Identifiers",
     main = "Party Identification by Voting Status")

# Add axis labels
axis(side = 1, at = seq(2006, 2024, by = 2))
axis(side = 2, at = seq(0, 1, by = 0.1), las = 1)

# Add line for voters
lines(figure2_data$year[!is.na(figure2_data$voters)],
      figure2_data$voters[!is.na(figure2_data$voters)],
      type = "o", col = "black", lty = 1, pch = 16)

# Add line for nonvoters
lines(figure2_data$year[!is.na(figure2_data$nonvoters)],
      figure2_data$nonvoters[!is.na(figure2_data$nonvoters)],
      type = "o", col = "black", lty = 2, pch = 1)

# Add legend
legend("bottomleft",
       legend = c("Voters", "Nonvoters"),
       lty = c(1, 2),
       pch = c(16, 1),
       bty = "n")