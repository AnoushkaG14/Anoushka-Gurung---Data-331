

# Chapter 22 and 23 Executables.........

# Chapter 22  Arrow..............

# 22.1.1 Prerequisites..............

library(tidyverse)
library(arrow)

library(dbplyr, warn.conflicts = FALSE)
library(duckdb)

# 22.2 Getting the data.............

dir.create("data", showWarnings = FALSE)

curl::multi_download(
  "https://r4ds.s3.us-west-2.amazonaws.com/seattle-library-checkouts.csv",
  "data/seattle-library-checkouts.csv",
  resume = TRUE
)

# 22.3 Opening a dataset.............

seattle_csv <- open_dataset(
  sources = "data/seattle-library-checkouts.csv", 
  col_types = schema(ISBN = string()),
  format = "csv"
)

seattle_csv

seattle_csv |> glimpse()

seattle_csv |> 
  group_by(CheckoutYear) |> 
  summarise(Checkouts = sum(Checkouts)) |> 
  arrange(CheckoutYear) |> 
  collect()

# 22.4 The parquet format..........

# 22.4.3 Rewriting the Seattle library data.............

pq_path <- "data/seattle-library-checkouts"

seattle_csv |>
  group_by(CheckoutYear) |>
  write_dataset(path = pq_path, format = "parquet")

tibble(
  files = list.files(pq_path, recursive = TRUE),
  size_MB = file.size(file.path(pq_path, files)) / 1024^2
)

# 22.5 Using dplyr with arrow...........

seattle_pq <- open_dataset(pq_path)

query <- seattle_pq |> 
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear, CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(CheckoutYear, CheckoutMonth)

query

query |> collect()


# 22.5.1 Performance............

seattle_csv |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()

seattle_pq |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()

# 22.5.2 Using duckdb with arrow..............

seattle_pq |> 
  to_duckdb() |>
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutYear)) |>
  collect()

# Chapter 23  Hierarchical data..............

# 23.1.1 Prerequisites.............

library(tidyverse)
library(repurrrsive)
library(jsonlite)

# 23.2 Lists.............

x1 <- list(1:4, "a", TRUE)
x1

x2 <- list(a = 1:2, b = 1:3, c = 1:4)
x2

str(x1)

str(x2)

# 23.2.1 Hierarchy...........

x3 <- list(list(1, 2), list(3, 4))
str(x3)

c(c(1, 2), c(3, 4))

x4 <- c(list(1, 2), list(3, 4))
str(x4)

x5 <- list(1, list(2, list(3, list(4, list(5)))))
str(x5)

# 23.2.2 List-columns.............

df <- tibble(
  x = 1:2, 
  y = c("a", "b"),
  z = list(list(1, 2), list(3, 4, 5))
)
df


df |> 
  filter(x == 1)

# 23.3 Unnesting...........

df1 <- tribble(
df2 <- tribble(
  ~x, ~y,
  1, list(11, 12, 13),
  2, list(21),
  3, list(31, 32),
)
  ~x, ~y,
  1, list(a = 11, b = 12),
  2, list(a = 21, b = 22),
  3, list(a = 31, b = 32),
)


df2 <- tribble(
  ~x, ~y,
  1, list(11, 12, 13),
  2, list(21),
  3, list(31, 32),
)

# 23.3.1 unnest_wider() ...........

df1 |> 
  unnest_wider(y)

df1 |> 
  unnest_wider(y, names_sep = "_")

# 23.3.2 unnest_longer() ...........

df2 |> 
  unnest_longer(y)

df6 <- tribble(
  ~x, ~y,
  "a", list(1, 2),
  "b", list(3),
  "c", list()
)
df6 |> unnest_longer(y)

# 23.3.3 Inconsistent types...........

df4 <- tribble(
  ~x, ~y,
  "a", list(1),
  "b", list("a", TRUE, 5)
)

df4 |> 
  unnest_longer(y)

# 23.4 Case studies............

# 23.4.1 Very wide data.............

repos <- tibble(json = gh_repos)
repos

repos |> 
  unnest_longer(json)

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) 

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  names() |> 
  head(10)

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description)

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner)

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner, names_sep = "_")

# 23.4.2 Relational data.........

chars <- tibble(json = got_chars)
chars

chars |> 
  unnest_wider(json)

characters <- chars |> 
  unnest_wider(json) |> 
  select(id, name, gender, culture, born, died, alive)
characters

chars |> 
  unnest_wider(json) |> 
  select(id, where(is.list))

chars |> 
  unnest_wider(json) |> 
  select(id, titles) |> 
  unnest_longer(titles)

titles <- chars |> 
  unnest_wider(json) |> 
  select(id, titles) |> 
  unnest_longer(titles) |> 
  filter(titles != "") |> 
  rename(title = titles)
titles

# 23.4.3 Deeply nested..........

gmaps_cities

gmaps_cities |> 
  unnest_wider(json)

gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results)

locations <- gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results) |> 
  unnest_wider(results)
locations

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  unnest_wider(location)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  # focus on the variables of interest
  select(!location:viewport) |>
  unnest_wider(bounds)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  select(!location:viewport) |>
  unnest_wider(bounds) |> 
  rename(ne = northeast, sw = southwest) |> 
  unnest_wider(c(ne, sw), names_sep = "_") 

locations |> 
  select(city, formatted_address, geometry) |> 
  hoist(
    geometry,
    ne_lat = c("bounds", "northeast", "lat"),
    sw_lat = c("bounds", "southwest", "lat"),
    ne_lng = c("bounds", "northeast", "lng"),
    sw_lng = c("bounds", "southwest", "lng"),
  )

# 23.5.2 jsonlite............

gh_users_json()

gh_users2 <- read_json(gh_users_json())

identical(gh_users, gh_users2)

str(parse_json('1'))

str(parse_json('[1, 2, 3]'))

str(parse_json('{"x": [1, 2, 3]}'))

# 23.5.3 Starting the rectangling process...........

json <- '[
  {"name": "John", "age": 34},
  {"name": "Susan", "age": 27}
]'
df <- tibble(json = parse_json(json))
df

df |> 
  unnest_wider(json)

json <- '{
  "status": "OK", 
  "results": [
    {"name": "John", "age": 34},
    {"name": "Susan", "age": 27}
 ]
}
'
df <- tibble(json = list(parse_json(json)))
df

df |> 
  unnest_wider(json) |> 
  unnest_longer(results) |> 
  unnest_wider(results)

df <- tibble(results = parse_json(json)$results)
df |> 
  unnest_wider(results)



