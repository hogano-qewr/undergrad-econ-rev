library(tidyverse)

### THEORY: COST STRUCTURE (FIXED AND VARIABLE)
##    The R Producer-Lab: Visualising the "Cost Curves"

library(tidyverse)

# 1. Define the Cost Functions
q_range <- seq(1, 15, by = 0.5)

df_costs <- tibble(q = q_range) %>%
  mutate(
    TC  = 20 + 0.5 * q^2,
    ATC = TC / q,
    AVC = (0.5 * q^2) / q,
    MC  = q  # Derivative of TC
  )

# 2. Plotting with "Exact Matching" for Colors
ggplot(df_costs, aes(x = q)) +
  # We map color to simple strings that match our 'values' below
  geom_line(aes(y = ATC, color = "ATC"), linewidth = 1.2) +
  geom_line(aes(y = AVC, color = "AVC"), linewidth = 1.2) +
  geom_line(aes(y = MC, color = "MC"), linewidth = 1.2) +
  
  # The "Efficient Scale" (Where MC = ATC)
  # sqrt(40) is approx 6.32
  annotate("point", x = 6.32, y = 6.32, color = "black", size = 4) +
  
  # Manual Color Scale (Names here must match the strings in aes() exactly)
  scale_color_manual(values = c("ATC" = "blue", "AVC" = "green4", "MC" = "red"),
                     labels = c("Average Total Cost", "Average Variable Cost", "Marginal Cost")) +
  
  coord_cartesian(ylim = c(0, 15)) +
  labs(title = "Producer Lab: The Firm's Cost Structure",
       subtitle = "The Supply Curve is the Marginal Cost (MC) curve above AVC.",
       x = "Quantity Produced (Q)", y = "Cost/Price (£)", color = "Cost Metric") +
  theme_minimal()



## The "Postgrad" Move: Finding the Supply Curve
#     MC curve is firm's supply curve ... prove using optim()
#     If the Market Price is £10, how much will the firm produce to maximise Profit

# STEPS:
# Define Profit Function (We minimize negative profit)
calc_neg_profit <- function(Q, price) {
  total_cost <- 20 + 0.5 * Q^2
  revenue <- price * Q
  profit <- revenue - total_cost
  return(-profit)
}

# Solve for Price = 10
res_profit <- optim(par = 1, fn = calc_neg_profit, price = 10, 
                    method = "Brent", lower = 0, upper = 20)

cat("--- PROFIT MAXIMISATION ---\n")
cat("Market Price: £10\n")
cat("Optimal Quantity (Q*):", round(res_profit$par, 2), "\n")
cat("Max Profit: £", round(-res_profit$value, 2), "\n")


### MONOPOLY LAB: MARKET POWER VS. WELFARE
# 1. Setup the Monopoly Market
# Demand: P = 20 - Q  => Total Revenue = 20Q - Q^2
# Marginal Revenue (MR) = 20 - 2Q
# Marginal Cost (MC) = Q (from our previous lab)

df_mono <- tibble(q = seq(0, 15, by = 0.5)) %>%
  mutate(
    demand_p = 20 - q,
    mr = 20 - 2 * q,
    mc = q
  )

# 2. The Monopoly Choice (Where MR = MC)
# 20 - 2Q = Q  => 20 = 3Q => Q = 6.67
q_mono <- 6.67
p_mono <- 20 - q_mono # Price = 13.33

# 3. The Competitive Choice (Where Price = MC)
# 20 - Q = Q   => 20 = 2Q => Q = 10
q_comp <- 10
p_comp <- 10

# 4. Plot the Monopoly "Gap"
ggplot(df_mono, aes(x = q)) +
  geom_line(aes(y = demand_p, color = "Demand"), linewidth = 1.2) +
  geom_line(aes(y = mr, color = "Marginal Revenue"), linewidth = 1.2) +
  geom_line(aes(y = mc, color = "Marginal Cost"), linewidth = 1.2) +
  
  # Monopoly Point
  annotate("point", x = q_mono, y = p_mono, color = "darkred", size = 4) +
  annotate("segment", x = q_mono, xend = q_mono, y = 0, yend = p_mono, linetype = "dotted") +
  
  # The Deadweight Loss (DWL) Triangle
  # Between Q_mono, Q_comp, and the Demand/MC curves
  annotate("polygon", x = c(q_mono, q_comp, q_mono), 
           y = c(p_mono, p_comp, q_mono), fill = "red", alpha = 0.2) +
  
  scale_color_manual(values = c("Demand" = "blue", "Marginal Revenue" = "orange", "Marginal Cost" = "red")) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(title = "Micro Lab: Monopoly and Market Power",
       subtitle = "Monopolies produce less (Q=6.6) and charge more (P=13.3) than competitive markets.",
       x = "Quantity (Q)", y = "Price ($)", color = "Curve") +
  theme_minimal()


