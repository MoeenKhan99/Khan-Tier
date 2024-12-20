% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Khan_Tier.R
\name{move_item_between_tiers}
\alias{move_item_between_tiers}
\title{Move an Item Between Tiers}
\usage{
move_item_between_tiers(tier_list, item, new_tier)
}
\arguments{
\item{tier_list}{A tier list object created by \code{create_tier_list}.}

\item{item}{The item to move.}

\item{new_tier}{The new tier to move the item to.}
}
\value{
Updated tier list object.
}
\description{
Move an Item Between Tiers
}
\examples{
tier_list <- move_item_between_tiers(tier_list, "Item1", "A")
}
