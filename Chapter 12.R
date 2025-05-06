

#Chapter 12  Logical vectors..........

#12.1.1 Prerequisites

library(tidyverse)
library(nycflights13)

x <- c(1, 2, 3, 5, 7, 11, 13)
x * 2

df <- tibble(x)
df |> 
  mutate(y = x * 2)

#12.2 Comparisons..............

flights |> 
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used"
  )

flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
  ) |> 
  filter(daytime & approx_ontime)

#12.2.1 Floating point comparison........

x <- c(1 / 49 * 49, sqrt(2) ^ 2)
x

x == c(1, 2)

print(x, digits = 16)

near(x, c(1, 2))

#12.2.2 Missing values...............

NA > 5

10 == NA

NA == NA
#> [1] NA

# We don't know how old Mary is
age_mary <- NA

# We don't know how old John is
age_john <- NA

# Are Mary and John the same age?
age_mary == age_john
#> [1] NA
# We don't know!

flights |> 
  filter(dep_time == NA)

#12.2.3 is.na()............

is.na(c(TRUE, NA, FALSE))

is.na(c(1, NA, 3))

is.na(c("a", NA, "b"))

flights |> 
  filter(is.na(dep_time))

flights |> 
  filter(month == 1, day == 1) |> 
  arrange(dep_time)

flights |> 
  filter(month == 1, day == 1) |> 
  arrange(desc(is.na(dep_time)), dep_time)

#12.3.1 Missing values...........

df <- tibble(x = c(TRUE, FALSE, NA))

df |> 
  mutate(
    and = x & NA,
    or = x | NA
  )

#12.3.2 Order of operations...............

flights |> 
  filter(month == 11 | month == 12)

flights |> 
  filter(month == 11 | 12)

flights |> 
  mutate(
    nov = month == 11,
    final = nov | 12,
    .keep = "used"
  )

#12.3.3 %in% .......................

1:12 %in% c(1, 5, 11)

letters[1:10] %in% c("a", "e", "i", "o", "u")

flights |> 
  filter(month %in% c(11, 12))

c(1, 2, NA) == NA

c(1, 2, NA) %in% NA

flights |> 
  filter(dep_time %in% c(NA, 0800))

flights |> 
  group_by(year, month, day) |> 
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

#12.4.2 Numeric summaries of logical vectors.............

flights |> 
  group_by(year, month, day) |> 
  summarize(
    proportion_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    count_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

#12.4.3 Logical subsetting....................

flights |> 
  filter(arr_delay > 0) |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay),
    n = n(),
    .groups = "drop"
  )

flights |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

#12.5 Conditional transformations.................

#12.5.1 if_else()......

x <- c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")

if_else(x > 0, "+ve", "-ve", "???")

if_else(x < 0, -x, x)

x1 <- c(NA, 1, 2, NA)
y1 <- c(3, NA, 4, 6)
if_else(is.na(x1), y1, x1)

if_else(x == 0, "0", if_else(x < 0, "-ve", "+ve"), "???")

#12.5.2 case_when()...................

x <- c(-3:3, NA)
case_when(
  x == 0   ~ "0",
  x < 0    ~ "-ve", 
  x > 0    ~ "+ve",
  is.na(x) ~ "???"
)

case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve"
)

case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve",
  .default = "???"
)

case_when(
  x > 0 ~ "+ve",
  x > 2 ~ "big"
)

flights |> 
  mutate(
    status = case_when(
      is.na(arr_delay)      ~ "cancelled",
      arr_delay < -30       ~ "very early",
      arr_delay < -15       ~ "early",
      abs(arr_delay) <= 15  ~ "on time",
      arr_delay < 60        ~ "late",
      arr_delay < Inf       ~ "very late",
    ),
    .keep = "used"
  )

#12.5.3 Compatible types..............

if_else(TRUE, "a", 1)

case_when(
  x < -1 ~ TRUE,  
  x > 0  ~ now()
)








