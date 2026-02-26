library(tidyverse)
library(gert)


### The Macro Lab: Simulating 100 Years of Growth
#     We will simulate a country starting with very little capital and watch it 
#         "converge" to its steady state.

# 1. Setup Parameters
s <- 0.2         # Savings Rate (20%)
delta <- 0.05    # Depreciation (5%)
alpha <- 0.33    # Capital's share of income
years <- 1:100

# 2. Initialize Vectors
K <- numeric(length(years))
Y <- numeric(length(years))
K[1] <- 1        # Starting Capital (very low)

# 3. Run the Simulation (The 'Dynamic' Loop)
for (t in 1:(length(years) - 1)) {
  # Production Function
  Y[t] <- K[t]^alpha
  
  # Investment (s*Y) vs Depreciation (delta*K)
  investment <- s * Y[t]
  depreciation <- delta * K[t]
  
  # Next year's capital
  K[t+1] <- K[t] + investment - depreciation
}
# Final Year Production
Y[100] <- K[100]^alpha

df_growth <- tibble(year = years, capital = K, gdp = Y)

# 4. Visualise the Convergence (Base R for guaranteed display)
plot(df_growth$year, df_growth$gdp, type="l", col="darkgreen", lwd=2,
     xlab="Years", ylab="GDP (Y)", 
     main="Macro Lab: Solow-Swan Growth Simulation")
abline(h = (s/delta)^(alpha/(1-alpha)), col="red", lty=2)
text(80, 1.8, "Steady State", col="red")


git_add(".")
git_commit("Add Solow-Swan Growth Simulation")
git_push()
