library(tidyverse)
library(shiny)
library(bslib)

### CALCULATION ENGINE: SCARTH IS-LM


# 1. Define the Structural Parameters (Scarth's Sensitivity Coefficients)
Cy <- 0.8   # Marginal propensity to consume
t  <- 0.2   # Tax rate
Ir <- -200  # Interest sensitivity of investment (Negative)
Ly <- 0.5   # Income sensitivity of money demand
Lr <- -100  # Interest sensitivity of money demand (Negative)

# 2. Construct Scarth's Matrix A (LHS)
# Row 1: Goods Market (IS), Row 2: Money Market (LM)
A_matrix <- matrix(c(
  1 - Cy * (1 - t),   -Ir,  # IS Slope elements
  Ly,                  Lr   # LM Slope elements
), nrow = 2, byrow = TRUE)

# 3. Define the Policy Shocks B (RHS)
dG <- 10      # Increase Govt Spending by 10
dM <- 0       # No change in Money Supply
B_vector <- c(dG, dM)

# 4. Solve the System (returns dY and dR)
# This replaces the manual Cramer's Rule derivation
equilibrium_changes <- solve(A_matrix, B_vector)

# Output results
print(paste("Change in Output (dY):", round(equilibrium_changes[1], 2)))
print(paste("Change in Interest (dR):", round(equilibrium_changes[2], 4)))



