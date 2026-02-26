library(tidyverse)

# SCARTH CHAPTER 2

# The R "Stability Lab": Simulating a Market Shock
#   We can use R to see if a market "heals" itself or "explodes" after a shock.

# 1. Define a Dynamic Price Adjustment Function
# Price tomorrow = Price today + lambda * (Excess Demand)
simulate_market <- TRUE
lambda <- 0.1 # Speed of adjustment
p_start <- 12 # Initial shock (Equilibrium is 10)
steps <- 1:20

prices <- numeric(length(steps))
prices[1] <- p_start

for (t in 1:(length(steps)-1)) {
  # Demand: 100 - 5P | Supply: 10 + 4P
  excess_demand <- (100 - 5 * prices[t]) - (10 + 4 * prices[t])
  prices[t+1] <- prices[t] + lambda * excess_demand
}

# 2. Plotting the Convergence
df_stability <- tibble(step = steps, price = prices)

ggplot(df_stability, aes(x = step, y = price)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  labs(title = "Scarth Lab: Market Stability & Convergence",
       subtitle = "If lambda is small and slopes are 'correct', price converges to 10.",
       x = "Time Step", y = "Price ($)") +
  theme_minimal()


# The R Lab: Simulating the AD-AS Convergence
#   We will simulate a Fiscal Expansion (Government spending rises). We'll watch how 
#     the "Excess Demand" created by the policy slowly pushes the Price Level up until 
#     the market "heals" at a new, higher equilibrium.

# 1. Setup Parameters
lambda <- 0.05    # Speed of Price Adjustment (The 'Scarth' Parameter)
steps <- 1:100    # Time steps to watch the convergence
P <- numeric(100)
P[1] <- 60        # Starting Price (Equilibrium was at 60)

# 2. Structural Equations (Linearized for simplicity)
# AD: Yd = 150 - P + G (Initially G=10, then we shock it to G=40)
G_shock <- 40 
# AS: Ys = 20 + 0.5P

# 3. The Dynamic Loop (Excess Demand Logic)
for (t in 1:(length(steps)-1)) {
  Yd <- 150 - P[t] + G_shock
  Ys <- 20 + 0.5 * P[t]
  
  excess_demand <- Yd - Ys
  
  # Scarth's Price Adjustment Rule: Change in P is proportional to ED
  P[t+1] <- P[t] + lambda * (excess_demand)
}

# 4. Visualising the Path to Stability
df_path <- tibble(time = steps, price = P) %>%
  mutate(gdp = 20 + 0.5 * price) # Calculate GDP along the path

# Plot Price Convergence
ggplot(df_path, aes(x = time, y = price)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_hline(yintercept = 113.3, linetype = "dashed") + # The new math equilibrium
  labs(title = "Scarth Lab: AD-AS Price Convergence",
       subtitle = "Price level rises over time to 'choke off' the Excess Demand from the Fiscal Shock.",
       x = "Time after Policy Shock", y = "Price Level (P)") +
  theme_minimal()




# A market that fails to heal itself

# 1. Setup an UNSTABLE market
# We make lambda (speed of adjustment) too high (0.3 instead of 0.05)
lambda_unstable <- 0.3 
steps <- 1:15
P_unstable <- numeric(15)
P_unstable[1] <- 11 # Start just slightly away from Equilibrium (10)

for (t in 1:14) {
  # Demand: 100 - 5P | Supply: 10 + 4P
  # Excess Demand = (100 - 5P) - (10 + 4P) = 90 - 9P
  ED <- 90 - 9 * P_unstable[t]
  P_unstable[t+1] <- P_unstable[t] + lambda_unstable * ED
}

# 2. Plotting the "Overshooting" / Explosion
df_unstable <- tibble(time = steps, price = P_unstable)

ggplot(df_unstable, aes(x = time, y = price)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red") +
  geom_hline(yintercept = 10, linetype = "dashed") +
  labs(title = "Scarth Lab: Unstable Market (Overshooting)",
       subtitle = "When adjustment is too fast (High Lambda), the market oscillates and explodes.",
       x = "Time", y = "Price ($)") +
  theme_minimal()
