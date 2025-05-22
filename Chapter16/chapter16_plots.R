library(ggplot2)
library(dplyr)
library(purrr)

blue_chapter <- "#008ac4"  # Demand color
red_chapter <- "#af1a44"   # Supply color
yellow_chapter <- "#FCE205"
green_chapter <- "#7BB074"
orange_chapter <- "#FF954F"
turquoise_chapter <- "#26867d"
purple_chapter <- "#702963"

generate_monopoly_data <- function(
    demand_intercept, demand_slope,   # Demand parameters
    cost_a = NULL, cost_b = NULL, cost_c = NULL, cost_d = NULL, # Cost function: C(Q) = a + bQ + cQ² + dQ³
    q_range = 2
) {
  
  # Define demand function
  demand <- function(Q) { pmax(0, demand_intercept - demand_slope * Q) }
  
  # Define marginal revenue function (for linear demand)
  marginal_revenue <- function(Q) { pmax(0, demand_intercept - 2 * demand_slope * Q) }
  
  # Check if cost function parameters are provided
  if (!is.null(cost_a) & !is.null(cost_b) & !is.null(cost_c) & !is.null(cost_d)) {
    
    # Define total cost function: C(Q) = a + bQ + cQ² + dQ³
    total_cost <- function(Q) { cost_a + cost_b * Q + cost_c * Q^2 + cost_d * Q^3 }
    
    # Compute marginal cost: MC(Q) = dC/dQ = b + 2cQ + 3dQ²
    marginal_cost <- function(Q) { pmax(0, cost_b + 2 * cost_c * Q + 3 * cost_d * Q^2) }
    
    # Compute average cost: AC(Q) = C(Q) / Q (handling Q=0 separately)
    average_cost <- function(Q) { ifelse(Q == 0, NA, (cost_a / Q) + cost_b + cost_c * Q + cost_d * Q^2) }
    
  } else {
    # If no cost parameters, return NA for cost-related curves
    marginal_cost <- function(Q) { rep(NA, length(Q)) }
    average_cost <- function(Q) { rep(NA, length(Q)) }
  }
  
  # **Fix: Define `x_limit` before generating Q_vals**
  x_limit <- (demand_intercept / (2 * demand_slope)) * q_range  # Reasonable range for Q
  
  # Generate quantity values for plotting
  Q_vals <- seq(0, x_limit, length.out = 100)
  
  # Compute values for each curve
  Demand_vals <- demand(Q_vals)
  Marginal_Revenue_vals <- marginal_revenue(Q_vals)
  Marginal_Cost_vals <- marginal_cost(Q_vals)
  Average_Cost_vals <- average_cost(Q_vals)
  
  # Keep only non-negative price values
  valid_indices <- (Demand_vals >= 0)
  Q_vals <- Q_vals[valid_indices]
  Demand_vals <- Demand_vals[valid_indices]
  Marginal_Revenue_vals <- Marginal_Revenue_vals[valid_indices]
  Marginal_Cost_vals <- Marginal_Cost_vals[valid_indices]
  Average_Cost_vals <- Average_Cost_vals[valid_indices]
  
  # Compute monopoly equilibrium: **MR = MC** using numerical solver (for cubic costs)
  if (!all(is.na(Marginal_Cost_vals))) {
    eq_Q_monopoly <- tryCatch({
      uniroot(function(Q) marginal_revenue(Q) - marginal_cost(Q), lower = 0, upper = max(Q_vals))$root
    }, error = function(e) NA)  # If solver fails, return NA
    
    eq_P_monopoly <- demand(eq_Q_monopoly)
  } else {
    eq_Q_monopoly <- NA
    eq_P_monopoly <- NA
  }
  
  # Create structured tibble with computed values
  plot_data <- tibble(
    Q = Q_vals,
    Demand = Demand_vals,
    Marginal_Revenue = Marginal_Revenue_vals,
    Marginal_Cost = Marginal_Cost_vals,
    Average_Cost = Average_Cost_vals,
    eq_Q_monopoly = eq_Q_monopoly,
    eq_P_monopoly = eq_P_monopoly
  )
  
  # Return as a list with function parameters and data
  return(list(
    formulas = list(
      demand = tibble(intercept = demand_intercept, slope = demand_slope), 
      marginal_revenue = tibble(intercept = demand_intercept, slope = 2 * demand_slope),
      marginal_cost = if (!all(is.na(Marginal_Cost_vals))) tibble(intercept = cost_b, linear = 2 * cost_c, quadratic = 3 * cost_d) else NA,
      average_cost = if (!all(is.na(Average_Cost_vals))) tibble(intercept = cost_a, linear = cost_b, quadratic = cost_c, cubic = cost_d) else NA
    ),
    data = plot_data
  ))
}

plot_monopoly <- function(
    plot_data, 
    show_demand = TRUE, 
    show_marginal_revenue = TRUE, 
    show_marginal_cost = TRUE, 
    show_average_cost = TRUE, 
    show_eq_q = TRUE, show_eq_p = TRUE
) {
  
  # Extract equilibrium values
  eq_Q_monopoly <- plot_data$eq_Q_monopoly[1]
  eq_P_monopoly <- plot_data$eq_P_monopoly[1]
  eq_Q_rounded <- round(eq_Q_monopoly, 1)
  eq_P_rounded <- round(eq_P_monopoly, 1)
  
  # Extract values at max Q for labeling
  demand_label <- plot_data |> filter(Demand == min(Demand)) |> slice(1)
  
  q_max <- round(demand_label$Q)
  mr_max <- round(max((plot_data |> filter(Marginal_Revenue > 0))$Q), 1)
  p_max <- round((plot_data |> filter(Demand == max(Demand)))$Demand + 1)
  
  # Initialize ggplot
  p <- ggplot() +
    labs(x = "Quantity", y = "Price") +
    theme_chapter +
    scale_x_continuous(limits = c(0, q_max + 0.5)) +
    scale_y_continuous(limits = c(0, p_max + 0.5)) +
    coord_cartesian(clip = "off")
  
  # Add Demand Curve
  if (show_demand) {
    p <- p + 
      geom_line(data = plot_data, aes(x = Q, y = Demand), color = blue_chapter, size = 1.2) +
      annotate("text", x = demand_label$Q, y = demand_label$Demand, label = "D", 
               color = blue_chapter, fontface = "italic", size = 3.5, hjust = -1)
  }
  
  # Add Marginal Revenue Curve
  if (show_marginal_revenue) {
    p <- p + 
      geom_line(data = plot_data |> filter(Marginal_Revenue > 0), aes(x = Q, y = Marginal_Revenue), color = blue_chapter, size = 1.2, alpha = .5) +
      annotate("text", x = mr_max, y = min(plot_data$Marginal_Revenue, na.rm = TRUE), label = "MR", 
               color = blue_chapter, alpha = .5, fontface = "italic", size = 3.5, hjust = -0.75)
  }
  
  # Add Marginal Cost Curve (Only if Cost Parameters Exist)
  if (show_marginal_cost && !all(is.na(plot_data$Marginal_Cost))) {
    
    mc_lab_x <- (plot_data |> filter(round(Marginal_Cost) == p_max))$Q
    mc_lab_x <- mc_lab_x[length(mc_lab_x)]
    
    mc_lab_y <-(plot_data |> filter(round(Marginal_Cost) == p_max))$Marginal_Cost
    mc_lab_y <- mc_lab_y[length(mc_lab_y)]
    
    
    p <- p + 
      geom_line(data = plot_data, aes(x = Q, y = Marginal_Cost), color = red_chapter, size = 1.2) +
      annotate(
        "text", label = "MC",
        x = mc_lab_x, y = mc_lab_y, 
        color = red_chapter, fontface = "italic", size = 3.5, hjust = -0.5
      )
  }
  
  # Add Average Cost Curve (Only if Cost Parameters Exist)
  if (show_average_cost && !all(is.na(plot_data$Average_Cost))) {
    p <- p + 
      geom_line(data = plot_data, aes(x = Q, y = Average_Cost), color = "#FFA07A", size = 1.2, size = 1.2, alpha = .3) +
      annotate(
        x = (plot_data |> filter(Q == q_max))$Q, y = (plot_data |> filter(Q == q_max))$Average_Cost, 
        "text", label = "AC", 
        color = "#FFA07A", fontface = "italic", size = 3.5, hjust = -1
      )
  }
  
  if (show_eq_q && !is.na(eq_Q_monopoly)) {
    p <- p +
      geom_segment(aes(x = eq_Q_monopoly, xend = eq_Q_monopoly, y = 0, yend = eq_P_monopoly), linetype = "dashed", color = "gray50") +
      # Annotate with dynamic value of eq_Q_rounded
      annotate(
        "text",
        x = eq_Q_monopoly, y = 0,  
        label = bquote(Q^"*" == .(eq_Q_rounded)),  # Using bquote for dynamic value
        fontface = "italic", size = 3, hjust = -0.35
      )
  }
  
  if (show_eq_p && !is.na(eq_P_monopoly)) {
    p <- p +
      geom_segment(aes(x = 0, xend = eq_Q_monopoly, y = eq_P_monopoly, yend = eq_P_monopoly), linetype = "dashed", color = "gray50") +
      # Annotate with dynamic value of eq_P_rounded
      annotate(
        "text",  
        x = 0, y = eq_P_monopoly,  
        label = bquote(P^"*" == .(eq_P_rounded)),  # Using bquote for dynamic value
        fontface = "italic", size = 3, vjust = -.75  # Adjust size and alignment
      )
  }
  
  if (show_eq_q == T & show_eq_p == T & !is.na(eq_Q_monopoly) & !is.na(eq_P_monopoly)) {
    p <- p +
      geom_point(aes(x = eq_Q_monopoly, y = eq_P_monopoly), size = 2.5)
  } 
  
  return(p)
}
