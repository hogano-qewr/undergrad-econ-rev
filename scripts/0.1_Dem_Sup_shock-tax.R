library(tidyverse)
library(patchwork)

### DEMAND, SUPPLY and SHOCKS

## 1. The R Micro-Lab: Simulating the Market
#     We will write a script that finds the equilibrium price and quantity, then 
#       introduces a Tax to see how the market "breaks."

# STEPS
# 1. Define our Parameters
# Demand: Q = 100 - 5P  (a=100, b=5)
# Supply: Q = 10 + 4P   (c=10, d=4)

# 2. Create the Price Range
df_market <- tibble(price = seq(0, 20, by = 0.1)) %>%
  mutate(
    demand = 100 - 5 * price,
    supply = 10 + 4 * price
  )

# 3. Find Equilibrium (Where Demand - Supply = 0)
# Mathematically: 100 - 5P = 10 + 4P => 90 = 9P => P = 10
p_eq <- 10
q_eq <- 100 - 5 * p_eq # 50

# 4. Visualise the "Ideal" Undergrad Market
ggplot(df_market, aes(x = price)) +
  geom_line(aes(y = demand, color = "Demand"), size = 1.2) +
  geom_line(aes(y = supply, color = "Supply"), size = 1.2) +
  # Mark the Equilibrium
  geom_point(aes(x = p_eq, y = q_eq), color = "black", size = 4) +
  annotate("text", x = p_eq + 2, y = q_eq + 5, label = "Equilibrium (10, 50)") +
  labs(title = "Micro Lab: Market Equilibrium",
       x = "Price ($)", y = "Quantity (Q)", color = "Curve") +
  theme_minimal()


## 3. The "Postgrad" Move: Adding an Intervention (Tax)
#       Now, let's behave like a modern economist. If the government adds a Tax (T=3)
#         per unit, it creates a "wedge" between what the consumer pays (P_d) and what 
#         the producer receives (P_s)

tax <- 3

# New Equilibrium with Tax
# (100 - 5(Ps + 3)) = 10 + 4Ps
# 100 - 5Ps - 15 = 10 + 4Ps
# 75 = 9Ps  => Ps = 8.33, Pd = 11.33
ps_tax <- 75 / 9
pd_tax <- ps_tax + tax
q_tax  <- 10 + 4 * ps_tax # ~43.3

# Calculate Deadweight Loss (DWL) - The "Waste"
dwl <- 0.5 * (tax) * (q_eq - q_tax)

cat("--- TAX ANALYSIS ---\n")
cat("Original Quantity:", q_eq, "\n")
cat("Quantity with Tax:", round(q_tax, 2), "\n")
cat("Tax Revenue:", round(tax * q_tax, 2), "\n")
cat("Deadweight Loss:", round(dwl, 2), "\n")

# The R "Tax Wedge" Plot: We will use geom_ribbon() to shade the Deadweight Loss—this
#     is the "Modern R" way to show the economic cost of the intervention.

library(tidyverse)

# 1. Setup the basic market again
df_market <- tibble(price = seq(6, 14, by = 0.1)) %>%
  mutate(
    demand = 100 - 5 * price,
    supply = 10 + 4 * price
  )

# 2. Key Coordinates from our previous calculation
# Ps = 8.33, Pd = 11.33, Q_tax = 43.33
p_eq <- 10
q_eq <- 50
ps_tax <- 8.33
pd_tax <- 11.33
q_tax  <- 43.33

# 3. Create the Visualization
ggplot(df_market, aes(x = price)) +
  # Original Curves
  geom_line(aes(y = demand, color = "Demand"), size = 1) +
  geom_line(aes(y = supply, color = "Supply"), size = 1) +
  
  # The Tax Wedge (Vertical line at Q_tax)
  geom_segment(aes(x = ps_tax, xend = pd_tax, y = q_tax, yend = q_tax), 
               linetype = "dashed", color = "darkred", size = 1) +
  
  # Consumer Price (Pd) and Producer Price (Ps)
  geom_point(aes(x = pd_tax, y = q_tax), color = "darkred", size = 3) +
  geom_point(aes(x = ps_tax, y = q_tax), color = "darkred", size = 3) +
  
  # Shade the Deadweight Loss (DWL) Triangle
  # We create a small dataframe for the triangle coordinates
  geom_polygon(data = data.frame(
    x = c(ps_tax, pd_tax, p_eq), 
    y = c(q_tax, q_tax, q_eq)),
    fill = "red", alpha = 0.3) +
  
  # Annotations
  annotate("text", x = pd_tax + 0.5, y = q_tax + 2, label = "Price Consumer Pays", size = 3) +
  annotate("text", x = ps_tax - 0.5, y = q_tax - 2, label = "Price Producer Gets", size = 3) +
  annotate("text", x = 10.5, y = 47, label = "DWL", color = "red", fontface = "bold") +
  
  labs(title = "Micro Lab: Tax Intervention & Deadweight Loss",
       subtitle = paste("Tax = $3 | Reduction in Quantity =", q_eq - round(q_tax,1)),
       x = "Price ($)", y = "Quantity (Q)", color = "Curve") +
  theme_minimal()

# This is the same "mapping conflict" we saw with our DAGs. Because ggplot() is looking at 
#   your df_market (which has 81 rows of prices), it gets confused when you ask it to draw 
#   a single triangle or a single point using values that aren't in that dataframe.
# As the warning suggests, annotate() is the "cleanest" way to add these geometric shapes 
#   in R without confusing the data layers.

library(tidyverse)

# 1. Basic Market Setup
p_range <- seq(6, 14, by = 0.1)
df_market <- tibble(
  price = p_range,
  demand = 100 - 5 * price,
  supply = 10 + 4 * price
)

# 2. Key Coordinates (Calculated previously)
p_eq <- 10; q_eq <- 50
ps_tax <- 8.33; pd_tax <- 11.33; q_tax <- 43.33

# 3. Plotting using the 'Annotate' Method
ggplot(df_market, aes(x = price)) +
  # Draw the Curves
  geom_line(aes(y = demand, color = "Demand"), linewidth = 1.2) +
  geom_line(aes(y = supply, color = "Supply"), linewidth = 1.2) +
  
  # --- ADD THE TAX WEDGE ---
  # Vertical line at Q_tax connecting Ps and Pd
  annotate("segment", x = ps_tax, xend = pd_tax, y = q_tax, yend = q_tax, 
           color = "darkred", linewidth = 1.5) +
  
  # The Points at the Wedge
  annotate("point", x = pd_tax, y = q_tax, color = "darkred", size = 3) +
  annotate("point", x = ps_tax, y = q_tax, color = "darkred", size = 3) +
  
  # --- THE DEADWEIGHT LOSS TRIANGLE ---
  # We use annotate("polygon") to avoid the 'price not found' error
  annotate("polygon", 
           x = c(ps_tax, pd_tax, p_eq), 
           y = c(q_tax, q_tax, q_eq), 
           fill = "red", alpha = 0.3) +
  
  # --- LABELS ---
  annotate("text", x = p_eq, y = q_eq + 3, label = "Equilibrium", fontface = "bold") +
  annotate("text", x = pd_tax, y = q_tax + 3, label = "Pd (Consumer)", color = "darkred") +
  annotate("text", x = ps_tax, y = q_tax - 3, label = "Ps (Producer)", color = "darkred") +
  annotate("text", x = 10, y = 46, label = "DWL", color = "red") +
  
  # Final Styling
  scale_color_manual(values = c("Demand" = "blue", "Supply" = "green4")) +
  labs(title = "Undergrad Micro: The Tax Wedge & Welfare Loss",
       subtitle = "The red triangle represents the 'Deadweight Loss' caused by the tax.",
       x = "Price ($)", y = "Quantity (Q)", color = "Market Curve") +
  theme_minimal()

# The "Marshallian" Micro Lab: Price on Vertical

library(tidyverse)

# 1. Basic Market Setup
p_range <- seq(6, 14, by = 0.1)
df_market <- tibble(
  price = p_range,
  demand = 100 - 5 * price,
  supply = 10 + 4 * price
)

# 2. Key Coordinates
p_eq <- 10; q_eq <- 50
ps_tax <- 8.33; pd_tax <- 11.33; q_tax <- 43.33

# 3. Plotting with Price on Vertical (Y) and Quantity on Horizontal (X)
ggplot(df_market, aes(y = price)) +  # Notice y = price now
  # Draw the Curves (x is now the Quantity columns)
  geom_line(aes(x = demand, color = "Demand"), linewidth = 1.2) +
  geom_line(aes(x = supply, color = "Supply"), linewidth = 1.2) +
  
  # --- ADD THE TAX WEDGE ---
  # A vertical wedge at Q_tax connecting Ps and Pd
  annotate("segment", x = q_tax, xend = q_tax, y = ps_tax, yend = pd_tax, 
           color = "darkred", linewidth = 1.5) +
  
  # The Points at the Wedge
  annotate("point", x = q_tax, y = pd_tax, color = "darkred", size = 3) +
  annotate("point", x = q_tax, y = ps_tax, color = "darkred", size = 3) +
  
  # --- THE DEADWEIGHT LOSS TRIANGLE ---
  # X = Quantities, Y = Prices
  annotate("polygon", 
           x = c(q_tax, q_tax, q_eq), 
           y = c(ps_tax, pd_tax, p_eq), 
           fill = "red", alpha = 0.3) +
  
  # --- LABELS ---
  annotate("text", x = q_eq + 3, y = p_eq, label = "Equilibrium", fontface = "bold") +
  annotate("text", x = q_tax - 4, y = pd_tax, label = "Pd (Consumer)", color = "darkred") +
  annotate("text", x = q_tax - 4, y = ps_tax, label = "Ps (Producer)", color = "darkred") +
  annotate("text", x = 46, y = 10, label = "DWL", color = "red") +
  
  # Final Styling
  scale_color_manual(values = c("Demand" = "blue", "Supply" = "green4")) +
  labs(title = "Undergrad Micro: The Tax Wedge (Marshallian Axes)",
       subtitle = "Price on Vertical Axis | Red Triangle = Deadweight Loss",
       x = "Quantity (Q)", y = "Price ($)", color = "Market Curve") +
  theme_minimal()



## 2. THE ELASTICITY LAB: A Simulation Loop

library(patchwork) # For combining the 5 plots

# 1. Setup the Fixed Parameters
p_eq_base <- 10
q_eq_base <- 50
tax <- 3
supply_slope <- 4  # Constant supply slope

# 2. Define 5 different Demand Slopes (Elasticities)
# From nearly vertical (Inelastic) to very flat (Elastic)
slopes <- c(1, 3, 5, 10, 20)

# 3. Create a list to store our plots
plot_list <- list()

for(b in slopes) {
  # Calculate intercept 'a' so that Equilibrium is always at (10, 50)
  # 50 = a - b(10) => a = 50 + 10b
  a <- 50 + 10 * b
  
  # Calculate New Equilibrium with Tax (Ps)
  # a - b(Ps + tax) = 10 + 4Ps
  # a - b*Ps - b*tax = 10 + 4Ps
  # a - b*tax - 10 = (4 + b)Ps
  ps_tax <- (a - b * tax - 10) / (4 + b)
  pd_tax <- ps_tax + tax
  q_tax  <- 10 + 4 * ps_tax
  
  # Generate the Market Data for this specific slope
  df_sim <- tibble(price = seq(5, 15, by = 0.1)) %>%
    mutate(demand = a - b * price, supply = 10 + 4 * price)
  
  # 4. Create the Plot for this slope
  p <- ggplot(df_sim, aes(y = price)) +
    geom_line(aes(x = demand), color = "blue", linewidth = 1) +
    geom_line(aes(x = supply), color = "green4", linewidth = 1) +
    # The Tax Wedge
    annotate("segment", x = q_tax, xend = q_tax, y = ps_tax, yend = pd_tax, 
             color = "darkred", linewidth = 1.2) +
    # The DWL Triangle
    annotate("polygon", x = c(q_tax, q_tax, q_eq_base), 
             y = c(ps_tax, pd_tax, p_eq_base), 
             fill = "red", alpha = 0.4) +
    coord_cartesian(xlim = c(0, 100), ylim = c(5, 15)) +
    labs(title = paste("Slope =", b), x = "Q", y = "P") +
    theme_minimal()
  
  # Add to our list
  plot_list[[as.character(b)]] <- p
}

# 5. Display all 5 plots at once
# This is where the 'Elasticity' logic becomes visible
wrap_plots(plot_list, ncol = 5) + 
  plot_annotation(title = "Elasticity Lab: How Demand Slope impacts Deadweight Loss",
                  subtitle = "As Demand becomes flatter (more Elastic), the Tax destroys more trade (Larger Red Triangle)")


