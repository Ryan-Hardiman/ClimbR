% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_module.R
\name{filterModuleServer}
\alias{filterModuleServer}
\title{Server Logic for the Climbing Crag Filter Module}
\usage{
filterModuleServer(id, df)
}
\arguments{
\item{id}{A string identifier for the module.}

\item{df}{A reactive data frame (tibble) containing the climbing route data.}
}
\value{
A reactive filtered data frame that updates based on user-selected filters.
}
\description{
This server function handles the filtering logic for the climbing routes dataset based on
user input (grades and hardest moves). It updates the choices in the filter options and
generates a reactive filtered dataset.
}
