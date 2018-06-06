## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  warning = F, 
  message = F,
  collapse = TRUE,
  comment = "#>"
)

## ----sim-----------------------------------------------------------------
library(kendallRandomWalks)
library(dplyr)
library(ggplot2)
set.seed(17)
walks <- simulate_kendall_rw(1000, 1000, rnorm, 0.5, T)

## ----ex1-----------------------------------------------------------------
plot(walks, max_id = 1)

## ----unique--------------------------------------------------------------
ggplot(summarise_kendall_rw(walks, n_distinct), aes(x = aggregated)) +
  # geom_histogram() +
  theme_bw() +
  geom_density()

## ----jumps---------------------------------------------------------------
diffs <- mutate_kendall_rw(walks, function(x) x - lag(x))
diffs
diffs2 <- mutate_kendall_rw(walks, function(x) x - lag(x), F)
plot(diffs2, max_id = 10)

## ----skoki_dlugosc-------------------------------------------------------
diffs3 <- mutate_kendall_rw(diffs2, function(x) as.numeric(x != 0), F)
diffs <- diffs3$simulation
diffs <- group_by(diffs, sim_id) %>%
  mutate(id = 1:n())
lengths <- diffs %>%
  filter(sim != 0) %>%
  mutate(previous = ifelse(is.na(lag(id)), 0, lag(id))) %>%
  mutate(length = id - previous)
ggplot(subset(lengths, sim_id < 5), 
       aes(x = length, fill = as.factor(sim_id), group = as.factor(sim_id))) +
  geom_density() +
  theme_bw() +
  ggtitle("Distribution of time with no state-change (by simulation)")
ggplot(lengths, aes(x = length)) +
  geom_density() +
  theme_bw() +
  ggtitle("Distribution of time with no state-change (aggregated)")
ggplot(subset(lengths, sim_id < 10), aes(x = id, y = length, color = as.factor(sim_id))) +
  geom_point() +
  theme_bw() +
  geom_line() +
  ggtitle("Time with no state-change in time")

