library(ggplot2)
library(dplyr)
library(purrr)

# Function to generate supply and demand data with elasticity control ----
# Function to generate supply and demand data with independent elasticities
generate_supply_demand_data <- function(
    demand_intercept = NULL, demand_slope = NULL,
    supply_intercept = NULL, supply_slope = NULL,
    eq_P = NULL, eq_Q = NULL, 
    demand_elasticity = "inelastic", # Options: "elastic", "inelastic", "very_inelastic"
    supply_elasticity = "elastic",   # Options: "elastic", "inelastic", "very_inelastic"
    shift_demand = 0,  # Shift in demand curve (positive = right, negative = left)
    shift_supply = 0,  # Shift in supply curve (positive = right, negative = left)
    q_range = 2
) {
  
  # Define slope ranges based on elasticity type
  elasticity_ranges <- list(
    "elastic" = c(0.2, 0.5),       # Flatter curves (more responsive)
    "inelastic" = c(0.5, 1),       # Steeper curves (less responsive)
    "very_inelastic" = c(1.5, 5)   # Very steep curves
  )
  
  # Validate elasticity choices
  if (!(demand_elasticity %in% names(elasticity_ranges))) {
    stop("Invalid demand elasticity type. Use 'elastic', 'inelastic', or 'very_inelastic'.")
  }
  if (!(supply_elasticity %in% names(elasticity_ranges))) {
    stop("Invalid supply elasticity type. Use 'elastic', 'inelastic', or 'very_inelastic'.")
  }
  
  # Case 1: Parameters are provided manually
  if (!is.null(demand_intercept) & !is.null(demand_slope) &
      !is.null(supply_intercept) & !is.null(supply_slope)) {
    
    demand <- function(Q) { pmax(0, demand_intercept - demand_slope * Q) }
    supply <- function(Q) { pmax(0, supply_intercept + supply_slope * Q) }
    
    eq_Q <- (demand_intercept - supply_intercept) / (demand_slope + supply_slope)
    eq_P <- demand(eq_Q)
    
  } else if (!is.null(eq_P) & !is.null(eq_Q)) {
    # Case 2: Approximate demand & supply functions based on equilibrium
    
    demand_slope <- runif(1, elasticity_ranges[[demand_elasticity]][1], elasticity_ranges[[demand_elasticity]][2])  
    supply_slope <- runif(1, elasticity_ranges[[supply_elasticity]][1], elasticity_ranges[[supply_elasticity]][2])  
    
    demand_intercept <- eq_P + demand_slope * eq_Q
    supply_intercept <- eq_P - supply_slope * eq_Q
    
    demand <- function(Q) { pmax(0, demand_intercept - demand_slope * Q) }
    supply <- function(Q) { pmax(0, supply_intercept + supply_slope * Q) }
  } else {
    stop("Either provide (demand & supply parameters) OR (equilibrium price & quantity).")
  }
  
  # **Shifted curves (same slopes, different intercepts)**
  demand_intercept_shifted <- demand_intercept + shift_demand
  supply_intercept_shifted <- supply_intercept + shift_supply
  
  demand_shifted <- function(Q) { pmax(0, demand_intercept_shifted - demand_slope * Q) }
  supply_shifted <- function(Q) { pmax(0, supply_intercept_shifted + supply_slope * Q) }
  
  # **Fix: Define `x_limit` before generating Q_vals**
  x_limit <- eq_Q * q_range  # Ensure reasonable range for Q
  
  # Generate quantity values for plotting
  Q_vals <- seq(0, x_limit, length.out = 100)
  
  # Compute demand and supply ensuring non-negative values
  Demand_vals <- demand(Q_vals)
  Supply_vals <- supply(Q_vals)
  
  Demand_vals_shifted <- demand_shifted(Q_vals)
  Supply_vals_shifted <- supply_shifted(Q_vals)
  
  # Keep only non-negative price values
  valid_indices <- (Demand_vals >= 0 & Supply_vals >= 0)
  Q_vals <- Q_vals[valid_indices]
  Demand_vals <- Demand_vals[valid_indices]
  Supply_vals <- Supply_vals[valid_indices]
  Demand_vals_shifted <- Demand_vals_shifted[valid_indices]
  Supply_vals_shifted <- Supply_vals_shifted[valid_indices]
  
  # Compute new equilibrium after the shift
  eq_Q_ne <- (demand_intercept_shifted - supply_intercept_shifted) / (demand_slope + supply_slope)
  eq_P_ne <- demand_shifted(eq_Q_ne)
  
  # Create a structured tibble with computed values
  plot_data <- tibble(
    Q = Q_vals,
    Demand = Demand_vals,
    Supply = Supply_vals,
    Demand_Shifted = Demand_vals_shifted,
    Supply_Shifted = Supply_vals_shifted,
    eq_Q = eq_Q,
    eq_P = eq_P,
    eq_Q_ne = eq_Q_ne,
    eq_P_ne = eq_P_ne
  )
  
  # Filter out flat demand and supply
  plot_data <- plot_data |> 
    filter(Demand > 0) |> 
    filter(Supply > 0)
  
  # Return as a list with function parameters and data
  return(list(
    formulas = list(
      demand = tibble(intercept = demand_intercept, slope = demand_slope), 
      supply = tibble(intercept = supply_intercept, slope = supply_slope),
      demand_shifted = tibble(intercept = demand_intercept_shifted, slope = demand_slope),
      supply_shifted = tibble(intercept = supply_intercept_shifted, slope = supply_slope)
    ),
    data = plot_data
  ))
}


# Plot supply and demand ----
# Define chapter colors and theme
blue_chapter <- "#008ac4"  # Demand color
red_chapter <- "#af1a44"   # Supply color
yellow_chapter <- "#FCE205"
green_chapter <- "#7BB074"

theme_chapter <- theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 9)),
    axis.title.y = element_text(margin = margin(r = 9))
  ) 

# Function to plot supply and demand graphs with integer axes and curve labels after q_max
plot_supply_demand <- function(
    plot_data, 
    p_max = Inf, 
    show_demand = TRUE, 
    show_supply = TRUE, 
    show_equilibrium = TRUE, 
    show_shifted_demand = FALSE, 
    show_shifted_supply = FALSE
) {
  
  # Extract initial equilibrium values
  eq_Q <- plot_data$eq_Q[1]
  eq_P <- plot_data$eq_P[1]
  eq_Q_rounded <- round(eq_Q, 1)
  eq_P_rounded <- round(eq_P, 1)
  
  # Identify new equilibrium if shifts exist
  if (show_shifted_demand || show_shifted_supply) {
    new_eq_Q <- plot_data$eq_Q_ne[1]
    new_eq_P <- plot_data$eq_P_ne[1]
    
    new_eq_Q_rounded <- round(new_eq_Q, 1)
    new_eq_P_rounded <- round(new_eq_P, 1)
  } else {
    new_eq_Q <- eq_Q
    new_eq_P <- eq_P
  }
  
  # Extract values at q_max for labeling
  demand_label <- plot_data |> filter(Demand == min(Demand)) |> slice(1)
  supply_label <- plot_data |> filter(Supply == max(Supply)) |> slice(1)
  
  q_max <- round(demand_label$Q + 1)
  p_max <- round((plot_data |> filter(Demand == max(Demand)))$Demand + 1)
  
  # Initialize the ggplot
  p <- ggplot() +
    labs(x = "Quantity", y = "Price") +
    theme_chapter 
  
  # Add Original Demand Curve
  if (show_demand) {
    p <- p + 
      geom_line(data = plot_data, aes(x = Q, y = Demand), color = blue_chapter, size = 1.2) +
      annotate("text", x = min(demand_label$Q, supply_label$Q), y = demand_label$Demand, 
               label = "D", color = blue_chapter, fontface = "italic", size = 3.5, hjust = -1)
  }
  
  # Add Original Supply Curve
  if (show_supply) {
    p <- p + 
      geom_line(data = plot_data, aes(x = Q, y = Supply), color = red_chapter, size = 1.2) +
      annotate("text", x = min(demand_label$Q, supply_label$Q), y = supply_label$Supply, 
               label = "S", color = red_chapter, fontface = "italic", size = 3.5, hjust = -1)
  }
  
  # Add Shifted Demand Curve
  if (show_shifted_demand) {
    p <- p + 
      geom_line(data = plot_data, aes(x = Q, y = Demand_Shifted), color = blue_chapter, linetype = "dashed", size = 1.2) +
      annotate("text", x = min(demand_label$Q, supply_label$Q), y = demand_label$Demand_Shifted, 
               label = "D'", color = blue_chapter, fontface = "italic", size = 3.5, hjust = -0.1)
  }
  
  # Add Shifted Supply Curve
  if (show_shifted_supply) {
    p <- p + 
      geom_line(data = plot_data, aes(x = Q, y = Supply_Shifted), color = red_chapter, linetype = "dashed", size = 1.2) +
      annotate("text", x = min(demand_label$Q, supply_label$Q), y = supply_label$Supply_Shifted, 
               label = "S'", color = red_chapter, fontface = "italic", size = 3.5, hjust = -0.1)
  }
  
  # Add Original Equilibrium Dashed Lines and Labels
  if (show_equilibrium) {
    p <- p +
      geom_segment(aes(x = eq_Q, xend = eq_Q, y = 0, yend = eq_P), linetype = "dashed", color = "gray50") +
      geom_segment(aes(x = 0, xend = eq_Q, y = eq_P, yend = eq_P), linetype = "dashed", color = "gray50") +
      annotate("text", x = eq_Q, y = 0,  
               label = bquote(Q^"*" == .(eq_Q_rounded)), 
               size = 2.75, hjust = -.2, vjust = -.1) +
      annotate("text", x = 0, y = eq_P,  
               label = bquote(P^"*" == .(eq_P_rounded)), 
               size = 2.75, hjust = 0, vjust = -0.75)
  }
  
  # Add New Equilibrium Dashed Lines and Labels (if shifts exist)
  if ((show_shifted_demand || show_shifted_supply) && show_equilibrium) {
    p <- p +
      geom_segment(aes(x = new_eq_Q, xend = new_eq_Q, y = 0, yend = new_eq_P), linetype = "dashed", color = "black") +
      geom_segment(aes(x = 0, xend = new_eq_Q, y = new_eq_P, yend = new_eq_P), linetype = "dashed", color = "black") +
      annotate("text", x = new_eq_Q, y = 0,  
               label = bquote(Q^"*'" == .(new_eq_Q_rounded)), 
               size = 2.75, hjust = -.2, vjust = -.1) +
      annotate("text", x = 0, y = new_eq_P,  
               label = bquote(P^"*'" == .(new_eq_P_rounded)), 
               size = 2.75, hjust = 0, vjust = -0.75)
  }
  
  # Finalize plot with integer axis labels
  p <- p +
    scale_x_continuous(limits = c(0, q_max + 0.5), breaks = seq(0, q_max, by = 1)) +
    scale_y_continuous(limits = c(0, p_max)) +
    coord_cartesian(clip = "off")  # Ensure labels are not clipped
  
  return(p)
}

# Closest Q to a list of reference prices ----
# Function to filter the closest Q values to a list of reference quantities and break ties
filter_closest_Q_values <- function(plot_data, reference_Qs) {
  map_dfr(reference_Qs, function(ref_Q) {
    closest_points <- plot_data |> 
      mutate(Q_rounded = round(Q)) |>  # Round Q for tie resolution
      filter(abs(Q - ref_Q) == min(abs(Q - ref_Q))) |> 
      slice(1) |>  # Select the first occurrence to break ties
      mutate(reference_Q = ref_Q) |> 
      select(-Q_rounded)  # Remove helper column
    
    return(closest_points)
  })
}
