#  
# #  Chapter 1 and 2
# 
# # Prerequisites: (Packages and libraries)
 # install.packages("tidyverse")
 # library(tidyverse)
 # tidyverse_update()
 # install.packages(
 #   c("arrow", "babynames", "curl", "duckdb", "gapminder",
 #     "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman",
 #     "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins",
 #     "repurrrsive", "tidymodels", "writexl")
 # )
 # 
 # library(palmerpenguins)
 # library(ggthemes)

library(ggplot2)

# First Steps
penguins
View(penguins)

ggplot(data = penguins)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()
  
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

#Section 1.3..............
#
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()


ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

ggplot(penguins, aes(x = species)) +
  geom_bar()

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

#Box Plot
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

#Density Plots
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)

#Color and Fill
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)


#1.5.2 Two categorical variables

#Bar Chart
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

#Stacked Bar
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

#1.5.2 Two numerical variables

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

#1.5.4 Three or more variables

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

#Including facets, subplots that each display one subset of the data

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

#1.6 Saving your plots

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

#.....
#    Chapter 2 workflow: basics
#Output in consolw and variables saved in rthe environment

x <-3 * 4

#Create vector for primes
primes <- c(2, 3, 5, 7, 11, 13)

#Multiply primes by 2?
primes * 2

#Subtracting primes by 1?
primes - 1

x
#x result in console

#long assignment
this_is_a_really_long_name <- 2.5

r_rocks <- 2^3

r_rock
R_rocks
#2.4 Calling functions
seq(from = 1, to = 10)

seq(1, 10)

x <- "hello world"
#Check environment for value of x

#quotation marks and parenthesis must always come in a pair.



