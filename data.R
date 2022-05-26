## Preprocess data, write TAF data tables

## Before: catch.csv, effort.csv (bootstrap/data)
## After:  catch_effort.csv, catch_by_stock.png, catch_relative.png,
##         catch_total.png, driors_2.png, input.rds (data)

library(TAF)
library(SOFIA)
library(dplyr)   # filter, group_by, left_join, mutate, summarise, ungroup
library(ggplot2)
library(purrr)   # map2
library(sraplus) # format_driors, plot_driors
library(tidyr)   # nest, pivot_longer

mkdir("data")

## Read catch data, convert to tibble (long format)
catch <- read.taf("bootstrap/data/catch.csv")
catch$Total <- NULL  # not used, not a stock
catch <- pivot_longer(catch, !Year, "stock", values_to="capture")
names(catch) <- tolower(names(catch))

## Plot catches
catch %>%
  ggplot(aes(year, capture, color=stock)) +
  geom_line(show.legend=FALSE) +
  geom_point()
ggsave("data/catch_by_stock.png")
catch %>%
  group_by(year) %>%
  summarise(total_capture=sum(capture)) %>%
  ggplot(aes(year, total_capture)) +
  geom_line()
ggsave("data/catch_total.png")

## Select stocks with min 20 years of >10 tonne catches...
viable_stocks <- catch %>%
  group_by(stock) %>%
  summarise(n_pos_catch=sum(capture > 10)) %>%
  filter(n_pos_catch > 20)

## ...and discard zero-catch years at the beginning or end of series
catch <- catch %>%
  filter(stock %in% viable_stocks$stock) %>%
  group_by(stock) %>%
  filter(year > min(year[capture > 0]),
         year <= max(year[capture > 0]))

## Plot relative catch
catch %>%
  group_by(stock) %>%
  mutate(capture = capture / max(capture)) %>%
  ggplot(aes(year, capture, group=stock)) +
  geom_point()
ggsave("data/catch_relative.png")

## Add column 'taxa'
catch$taxa <- catch$stock

## Read effort data, add column to catch data
effort <- read.taf("bootstrap/data/effort.csv")
effort <- pivot_longer(effort, !Year, "stock", values_to="effort")
names(effort) <- tolower(names(effort))
catch_effort <- addEffort(catch, effort, stocks.combined=TRUE)

## Create nested tibble with 'data' column (catch and effort)
stocks <- catch_effort %>%
  group_by(stock, taxa) %>%
  nest() %>%
  ungroup()

## Read priors data, add as driors to stocks object
priors <- read.taf("bootstrap/data/priors.csv")
stocks <- addDriors(stocks, priors, stocks.combined, stocks.combined=TRUE)

## Plot driors for one stock
plot_driors(stocks$driors[[2]])
ggsave("data/driors_2.png")

## Export stocks and catch_effort
saveRDS(stocks, "data/input.rds")
write.taf(catch_effort, dir="data")
