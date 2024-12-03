# Testing package functionalities
library(testthat)
library(ggplot2)

# Test for creating a tier list
test_that("Tier list creation initializes tiers, colors, and assignments correctly", {
  tier_list <- create_tier_list(items = c("Item1", "Item2"), tiers = c("S", "A"))
  expect_equal(tier_list$tiers, c("S", "A"))
  expect_equal(length(tier_list$colors), 2)  # Ensure colors match tier length
  expect_equal(length(tier_list$assignments), 2)  # Check assignment initialization
})

# Test for assigning items to tiers
test_that("Assigning items to tiers works as expected", {
  tier_list <- create_tier_list(items = c("Item1", "Item2"), tiers = c("S", "A"))
  tier_list <- assign_to_tier(tier_list, "Item1", "S")
  tier_list <- assign_to_tier(tier_list, "Item2", "A")

  # Verify assignments
  expect_equal(unname(tier_list$assignments["Item1"]), "S")
  expect_equal(unname(tier_list$assignments["Item2"]), "A")

  # Test assigning a non-existing item
  expect_error(assign_to_tier(tier_list, "Item3", "B"), "Item is not in the tier list.")

  # Test assigning to a non-existing tier
  expect_error(assign_to_tier(tier_list, "Item1", "C"), "Tier does not exist.")
})

# Test for adding items to tier list
test_that("Adding items to tier list works correctly", {
  tier_list <- create_tier_list(tiers = c("S", "A"), colors = c("red", "yellow"))
  tier_list <- add_item_to_tier_list(tier_list, "Item1")

  # Check item addition
  expect_true("Item1" %in% tier_list$items)
  expect_true(is.na(tier_list$assignments["Item1"]))

  # Adding duplicate item should throw an error
  expect_error(add_item_to_tier_list(tier_list, "Item1"), "Item already exists in the tier list.")
})

# Test for moving items between tiers
test_that("Moving items between tiers updates assignments correctly", {
  tier_list <- create_tier_list(items = c("Item1", "Item2"), tiers = c("S", "A"))
  tier_list <- assign_to_tier(tier_list, "Item1", "S")
  tier_list <- move_item_between_tiers(tier_list, "Item1", "A")

  # Verify updated assignments
  expect_equal(unname(tier_list$assignments["Item1"]), "A")
})

# Test for plotting tier list
test_that("Plotting tier list produces a valid ggplot object", {
  tier_list <- create_tier_list(
    items = c("Item1", "Item2", "Item3"),
    tiers = c("S", "A"),
    colors = c("red", "blue")
  )
  tier_list <- assign_to_tier(tier_list, "Item1", "S")
  tier_list <- assign_to_tier(tier_list, "Item2", "A")

  plot <- plot_tier_list_advanced(tier_list)
  expect_s3_class(plot, "gg")
})

# Test edge cases
test_that("Edge cases for tier list creation and functionality", {
  # Create tier list with no tiers or colors
  tier_list <- create_tier_list(tiers = character(), colors = character())
  expect_equal(length(tier_list$tiers), 0)

  # Create tier list with one tier and one color
  tier_list <- create_tier_list(tiers = c("S"), colors = c("red"))
  expect_equal(length(tier_list$tiers), 1)

  # Create tier list with two tiers and two colors, but no items
  tier_list <- create_tier_list(tiers = c("S", "A"), colors = c("red", "blue"))
  expect_equal(length(tier_list$items), 0)
})

# Test for invalid input handling
test_that("Invalid inputs are handled gracefully", {
  # Mismatched tiers and colors
  expect_error(
    create_tier_list(tiers = c("S", "A"), colors = c("red")),
    "The number of tiers and colors must match."
  )

  # Adding an item to a non-existing tier
  tier_list <- create_tier_list(tiers = c("S", "A"), colors = c("red", "blue"))
  expect_error(
    assign_to_tier(tier_list, "NonExistentItem", "S"),
    "Item is not in the tier list."
  )
})


