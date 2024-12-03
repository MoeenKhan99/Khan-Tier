#' Create a Tier List
#'
#' @param tiers A character vector of tier names.
#' @param colors A character vector of tier colors (optional).
#' @param items A character vector of items to rank (optional).
#'
#' @return A list object representing the tier list.
#' @examples
#' tier_list <- create_tier_list(tiers = c("S", "A"), items = c("Item1", "Item2"))
#' @export
create_tier_list <- function(tiers = c("S", "A", "B"),
                             colors = NULL,
                             items = NULL) {
  if (is.null(colors)) {
    colors <- grDevices::rainbow(length(tiers))  # Default to distinct colors for tiers
  }

  if (length(tiers) != length(colors)) {
    stop("The number of tiers and colors must match.")
  }

  if (is.null(items)) {
    items <- character()  # Initialize with an empty vector if no items are provided.
  }

  list(
    tiers = tiers,
    colors = colors,
    items = items,
    assignments = setNames(rep(NA, length(items)), items)  # Initialize assignments to NA
  )
}


#' Assign an Item to a Tier
#'
#' @param tier_list A tier list object created by `create_tier_list`.
#' @param item The item to assign.
#' @param tier The tier to assign the item to.
#'
#' @return Updated tier list object.
#' @examples
#' tier_list <- create_tier_list(items = c("Item1", "Item2"), tiers = c("S", "A"))
#' tier_list <- assign_to_tier(tier_list, "Item1", "S")
#' @export
assign_to_tier <- function(tier_list, item, tier) {
  if (!item %in% tier_list$items) {
    stop("Item is not in the tier list.")
  }
  if (!tier %in% tier_list$tiers) {
    stop("Tier does not exist.")
  }
  tier_list$assignments[item] <- tier
  tier_list
}


#' Add an Item to the Tier List
#'
#' @param tier_list A tier list object created by `create_tier_list`.
#' @param item The item to add.
#'
#' @return Updated tier list object.
#' @examples
#' tier_list <- add_item_to_tier_list(tier_list, "Item3")
#' @export
add_item_to_tier_list <- function(tier_list, item) {
  if (item %in% tier_list$items) {
    stop("Item already exists in the tier list.")
  }

  tier_list$items <- c(tier_list$items, item)
  tier_list$assignments[item] <- NA  # Initialize the item's tier assignment to NA
  tier_list
}


#' Move an Item Between Tiers
#'
#' @param tier_list A tier list object created by `create_tier_list`.
#' @param item The item to move.
#' @param new_tier The new tier to move the item to.
#'
#' @return Updated tier list object.
#' @examples
#' tier_list <- move_item_between_tiers(tier_list, "Item1", "A")
#' @export
move_item_between_tiers <- function(tier_list, item, new_tier) {
  if (!item %in% tier_list$items) {
    stop("Item is not in the tier list.")
  }
  if (!new_tier %in% tier_list$tiers) {
    stop("Tier does not exist.")
  }

  tier_list$assignments[item] <- new_tier
  tier_list
}


#' Plot tier list with advanced features
#'
#' This function creates an advanced plot for the tier list.
#' @name plot_tier_list_advanced
#' @param tier_list The tier list object to be plotted
#' @export

library(ggplot2)
library(dplyr)

# Plot a Tier List
plot_tier_list_advanced <- function(tier_list) {
  # Prepare data for tiers
  tier_data <- data.frame(
    tier = rev(tier_list$tiers),
    y_pos = seq_along(tier_list$tiers),   # Reversed so "S" is at the top
    colors = rev(tier_list$colors)
  )

  # Assign each item to its corresponding tier
  item_data <- data.frame(
    items = names(tier_list$assignments),
    tier = tier_list$assignments,
    stringsAsFactors = FALSE
  )

  # Assign y positions based on tier
  item_data$y_pos <- as.numeric(factor(item_data$tier, levels = rev(tier_list$tiers)))

  # Adjust x positions for each tier
  item_data <- item_data %>%
    group_by(y_pos) %>%
    mutate(x_pos = seq(1, length.out = n(), by = 1)) %>% # Ensure even spacing
    ungroup()

  # Debugging: Print item_data to verify correctness
  print("Item Data:")
  print(item_data)

  # Maximum x_pos for scaling plot width
  max_x_pos <- max(item_data$x_pos, na.rm = TRUE)

  # Create the plot
  ggplot() +
    # Draw colored rectangles for each tier
    geom_rect(data = tier_data,
              aes(xmin = 0, xmax = max_x_pos + 2,
                  ymin = y_pos - 0.5, ymax = y_pos + 0.5, fill = colors),
              inherit.aes = FALSE, alpha = 0.8) +

    # Add the items to the plot
    geom_text(data = item_data,
              aes(x = x_pos, y = y_pos, label = items),
              size = 5, color = "black", hjust = 0.5, vjust = 0.5) + # Center items

    # Adjust x-axis to hide labels and ticks
    scale_x_continuous(limits = c(0, max_x_pos + 2), labels = NULL) +

    # Adjust y-axis for tier labels
    scale_y_continuous(breaks = seq_along(tier_list$tiers), labels = rev(tier_list$tiers)) +

    # Set fill for each tier's rectangle
    scale_fill_identity() +

    # Theme adjustments
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.background = element_rect(fill = "#D3D3D3"),
      plot.margin = margin(10, 10, 10, 10)
    )
}



