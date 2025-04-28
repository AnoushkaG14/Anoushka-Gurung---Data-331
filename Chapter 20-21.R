

# Chapter 20 Spreadsheets..............

# 20.2 Excel...............

# 20.2.1 Prerequisites.............

library(readxl)
library(tidyverse)
library(writexl)

# 20.2.2 Getting started............

students <- read_excel("data/students.xlsx")

students

read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age")
)

read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1
)

read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A")
)

read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "numeric")
)

students <- read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "text")
)

students <- students |>
  mutate(
    age = if_else(age == "five", "5", age),
    age = parse_number(age)
  )

students


# 20.2.4 Reading worksheets..............

read_excel("data/penguins.xlsx", sheet = "Torgersen Island")

penguins_torgersen <- read_excel("data/penguins.xlsx", sheet = "Torgersen Island", na = "NA")

penguins_torgersen

excel_sheets("data/penguins.xlsx")

penguins_biscoe <- read_excel("data/penguins.xlsx", sheet = "Biscoe Island", na = "NA")
penguins_dream  <- read_excel("data/penguins.xlsx", sheet = "Dream Island", na = "NA")

dim(penguins_torgersen)
#> [1] 52  8
dim(penguins_biscoe)
#> [1] 168   8
dim(penguins_dream)
#> [1] 124   8

penguins <- bind_rows(penguins_torgersen, penguins_biscoe, penguins_dream)
penguins

# 20.2.5 Reading part of a sheet..........

deaths_path <- readxl_example("deaths.xlsx")
deaths <- read_excel(deaths_path)

deaths

read_excel(deaths_path, range = "A5:F15")

# 20.2.6 Data types..........

# 20.2.7 Writing to Excel............

bake_sale <- tibble(
  item     = factor(c("brownie", "cupcake", "cookie")),
  quantity = c(10, 5, 8)
)

bake_sale

write_xlsx(bake_sale, path = "data/bake-sale.xlsx")

read_excel("data/bake-sale.xlsx")


# 20.3.3 Reading Google Sheets.............

gs4_deauth()

students_sheet_id <- "1V1nPp1tzOuutXFLb3G9Eyxi3qxeEhnOXUzL5_BcCQ0w"
students <- read_sheet(students_sheet_id)

students <- read_sheet(
  students_sheet_id,
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = "dcccc"
)

students

penguins_sheet_id <- "1aFu8lnD_g0yjF5O-K6SFgSEWiHPpgvFCF0NY9D6LXnY"
read_sheet(penguins_sheet_id, sheet = "Torgersen Island")

sheet_names(penguins_sheet_id)

deaths_url <- gs4_example("deaths")
deaths <- read_sheet(deaths_url, range = "A5:F15")

deaths

# 20.3.4 Writing to Google Sheets...............

write_sheet(bake_sale, ss = "bake-sale")

write_sheet(bake_sale, ss = "bake-sale", sheet = "Sales")


# Chapter 21  Databases..................

# 21.1.1 Prerequisites.............

library(DBI)
library(dbplyr)
library(tidyverse)

# 21.3 Connecting to a database..........

con <- DBI::dbConnect(
  RMariaDB::MariaDB(), 
  username = "foo"
)
con <- DBI::dbConnect(
  RPostgres::Postgres(), 
  hostname = "databases.mycompany.com", 
  port = 1234
)

# 21.3.1 In this book..............

con <- DBI::dbConnect(duckdb::duckdb())

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

# 21.3.2 Load some data..........

dbWriteTable(con, "mpg", ggplot2::mpg)
dbWriteTable(con, "diamonds", ggplot2::diamonds)


# 21.3.3 DBI basics............

dbListTables(con)

con |> 
  dbReadTable("diamonds") |> 
  as_tibble()

sql <- "
  SELECT carat, cut, clarity, color, price 
  FROM diamonds 
  WHERE price > 15000
"
as_tibble(dbGetQuery(con, sql))


# 21.4 dbplyr basics...........

diamonds_db <- tbl(con, "diamonds")
diamonds_db

big_diamonds_db <- diamonds_db |> 
  filter(price > 15000) |> 
  select(carat:clarity, price)

big_diamonds_db

big_diamonds_db |>
  show_query()

big_diamonds <- big_diamonds_db |> 
  collect()
big_diamonds

# 21.5 SQL..........

dbplyr::copy_nycflights13(con)

flights <- tbl(con, "flights")
planes <- tbl(con, "planes")

# 21.5.1 SQL basics...........

flights |> show_query()

planes |> show_query()

flights |> 
  filter(dest == "IAH") |> 
  arrange(dep_delay) |>
  show_query()

flights |> 
  group_by(dest) |> 
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  show_query()

# 21.5.2 SELECT.............

planes |> 
  select(tailnum, type, manufacturer, model, year) |> 
  show_query()

planes |> 
  select(tailnum, type, manufacturer, model, year) |> 
  rename(year_built = year) |> 
  show_query()

planes |> 
  select(tailnum, type, manufacturer, model, year) |> 
  relocate(manufacturer, model, .before = type) |> 
  show_query()

flights |> 
  mutate(
    speed = distance / (air_time / 60)
  ) |> 
  show_query()

# 21.5.4 GROUP BY..........

diamonds_db |> 
  group_by(cut) |> 
  summarize(
    n = n(),
    avg_price = mean(price, na.rm = TRUE)
  ) |> 
  show_query()

# 21.5.5 WHERE..................

flights |> 
  filter(dest == "IAH" | dest == "HOU") |> 
  show_query()

flights |> 
  filter(arr_delay > 0 & arr_delay < 20) |> 
  show_query()

flights |> 
  filter(dest %in% c("IAH", "HOU")) |> 
  show_query()


flights |> 
  group_by(dest) |> 
  summarize(delay = mean(arr_delay))
#> Warning: Missing values are always r


flights |> 
  filter(!is.na(dep_delay)) |> 
  show_query()

diamonds_db |> 
  group_by(cut) |> 
  summarize(n = n()) |> 
  filter(n > 100) |> 
  show_query()
#> <SQL>


# 21.5.7 Subqueries,,,,,,,,,,,,,,,

flights |> 
  mutate(
    year1 = year + 1,
    year2 = year1 + 1
  ) |> 
  show_query()

flights |> 
  mutate(year1 = year + 1) |> 
  filter(year1 == 2014) |> 
  
# 21.5.8 Joins................
  
  flights |> 
  left_join(planes |> rename(year_built = year), by = "tailnum") |> 
  show_query()

SELECT flights.*, "type", manufacturer, model, engines, seats, speed
FROM flights
INNER JOIN planes ON (flights.tailnum = planes.tailnum)

SELECT flights.*, "type", manufacturer, model, engines, seats, speed
FROM flights
RIGHT JOIN planes ON (flights.tailnum = planes.tailnum)

SELECT flights.*, "type", manufacturer, model, engines, seats, speed
FROM flights
FULL JOIN planes ON (flights.tailnum = planes.tailnum)

# 21.6 Function translations.........

summarize_query <- function(df, ...) {
  df |> 
    summarize(...) |> 
    show_query()
}
mutate_query <- function(df, ...) {
  df |> 
    mutate(..., .keep = "none") |> 
    show_query()
}

flights |> 
  group_by(year, month, day) |>  
  summarize_query(
    mean = mean(arr_delay, na.rm = TRUE),
    median = median(arr_delay, na.rm = TRUE)
  )

flights |> 
  group_by(year, month, day) |>  
  mutate_query(
    mean = mean(arr_delay, na.rm = TRUE),
  )

flights |> 
  group_by(dest) |>  
  arrange(time_hour) |> 
  mutate_query(
    lead = lead(arr_delay),
    lag = lag(arr_delay)
  )

flights |> 
  mutate_query(
    description = if_else(arr_delay > 0, "delayed", "on-time")
  )

flights |> 
  mutate_query(
    description = 
      case_when(
        arr_delay < -5 ~ "early", 
        arr_delay < 5 ~ "on-time",
        arr_delay >= 5 ~ "late"
      )
  )

flights |> 
  mutate_query(
    description =  cut(
      arr_delay, 
      breaks = c(-Inf, -5, 5, Inf), 
      labels = c("early", "on-time", "late")
    )
  )

