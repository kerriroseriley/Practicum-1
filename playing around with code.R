#ggplot

# Figure 1A: Strong and Weak Partisans
fig1a <- ggplot(ces_1, aes(x = year)) +
  geom_line(aes(y = strong, linetype = "Strong Identifiers"),
            linewidth = 1.2) +
  geom_line(aes(y = weak, linetype = "Weak Identifiers"),
            linewidth = 1.2) +
  geom_point(aes(y = strong), size = 2) +
  geom_point(aes(y = weak), size = 2) +
  scale_x_continuous(
    limits = c(2006, 2024),
    breaks = seq(2006, 2024, by = 2)
  ) +
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0, 0.5, by = 0.1)
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Strong Identifiers" = "solid",
               "Weak Identifiers" = "dashed")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Figure 1A. Strong and Weak Partisans"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Figure 1B: Pure Independents and Independent Leaners
fig1b <- ggplot(ces_1, aes(x = year)) +
  geom_line(aes(y = indeps, linetype = "Pure Independents"),
            linewidth = 1.2) +
  geom_line(aes(y = leaners, linetype = "Independent Leaners"),
            linewidth = 1.2) +
  geom_point(aes(y = indeps), size = 2) +
  geom_point(aes(y = leaners), size = 2) +
  scale_x_continuous(
    limits = c(2006, 2024),
    breaks = seq(2006, 2024, by = 2)
  ) +
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0, 0.5, by = 0.1)
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Pure Independents" = "solid",
               "Independent Leaners" = "dashed")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Figure 1B. Pure Independents and Independent Leaners"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Display the graphs
fig1a
fig1b

# Lattice:
# Figure 1A: Strong and Weak Partisans
library(lattice)
 
xyplot(
  strong + weak ~ year,
  data = figure1_data,
  type = "o",
  lwd = 1.2,
  lty = c(1, 2),
  pch = 16,
  xlab = "",
  ylab = "",
  main = "Figure 1A. Strong and Weak Partisans",
  scales = list(
    x = list(limits = c(2006, 2024), at = seq(2006, 2024, by = 2)),
    y = list(limits = c(0, 0.5), at = seq(0, 0.5, by = 0.1))
  ),
  auto.key = list(
    text = c("Strong Identifiers", "Weak Identifiers"),
    lines = TRUE,
    points = TRUE
  )
)

# Figure 1B: Pure Independents and Independent Leaners
xyplot(
  indeps + leaners ~ year,
  data = figure1_data,
  type = "o",
  lwd = 1.2,
  lty = c(1, 2),
  pch = 16,
  xlab = "",
  ylab = "",
  main = "Figure 1B. Pure Independents and Independent Leaners",
  scales = list(
    x = list(limits = c(2006, 2024), at = seq(2006, 2024, by = 2)),
    y = list(limits = c(0, 0.5), at = seq(0, 0.5, by = 0.1))
  ),
  auto.key = list(
    text = c("Pure Independents", "Independent Leaners"),
    lines = TRUE,
    points = TRUE
  )
)


# BASR R



# Graphing Section
# Figure 1A: Strong and weak partisans

# Plot
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

# Plot
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

# Figure 2: Party Identification by Voting Status
figure2_data <- ces_1 |>
  group_by(year) |>
  summarise(
    voters = mean(identvote, na.rm = TRUE),
    nonvoters = mean(identnovote, na.rm = TRUE)
  ) |>
  filter(!is.nan(voters), !is.nan(nonvoters))

# Create graph
# Plot
plot(x = 0, type = "n",
     xlim = range(figure2_data$year), xaxt = "n",
     ylim = c(0, 1), yaxt = "n",
     xlab = " ",
     ylab = " ",
     main = "Proportions of (Strong or Weak) Identifiers in Cooroperative Election Study Sample")

# Add axis labels
axis(side = 1, at = seq(2006, 2024, by = 8))
axis(side = 2, at = seq(0, 1, by = 0.1))

# Add Line for Voters
lines(figure2_data$year, figure2_data$voters,
      type = "o", col = "black", lty = 1, pch = 16)

# Add line for Nonvoters
lines(figure2_data$year, figure2_data$nonvoters,
      type = "o", col = "black", lty = 2, pch = 1)

# Customize legend
legend("bottomleft",
       legend = c("Voters", "Nonvoters"),
       lty = c(1, 2), pch = c(16, 1),
       bty = "n")


