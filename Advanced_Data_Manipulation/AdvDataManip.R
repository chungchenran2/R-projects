# Title: Creative Data Manipulation: familiarising with the diversity of dplyr
# Your name
# Date

# Set working directory to where you saved the folder with tutorial materials on your computer
setwd("file-path")

# Libraries ----
library(dplyr)
# when we add new libraries throughout the tutorial, you can paste them here

# If you've never used a given package, install it with `install.packages("name")`

# Most of them are a part of `tidyverse` but we will be loading them separately so that you are aware of the contexts in which they are used

### PART I: OCEAN ANIMALS ----

# Load the datasets
animal_p1 <- read.csv("CC-data-manip-creative-dplyr-main/data/animal_p1.csv")
animal_p2 <- read.csv("CC-data-manip-creative-dplyr-main/data/animal_p2.csv")
animal_rp <- read.csv("CC-data-manip-creative-dplyr-main/data/animal_rp.csv")
animal_meal <- read.csv("CC-data-manip-creative-dplyr-main/data/animal_meal.csv")

# 2. Combining tables ----
# a) Combining rows ----

# Firstly view the p1 and p2 datasets, which will appear in your console
animal_p1
animal_p2

# Now, bind them on top of each other
# Adding brackets around the expression prints out the result
(animal <- bind_rows(animal_p1, animal_p2))  # 8 observations

# b) Set operations ----

setequal(animal_p1, animal_p2)  # FALSE is returned, so the tables are not identical

# Since they differ, let's see how many observations you and your rp have in common.
# You're not creating an object yet so the output will be shown in the console without having to add brackets

intersect(animal, animal_rp)  # 6 observations in common

# View which observations you have and your rp doesn't

setdiff(animal, animal_rp)  # id no. 2 and 5

# Now view which ones your rp has that you don't

setdiff(animal_rp, animal)  # id no. 6 and 10

# Let's now connect them with `union()` but removing any duplicated rows

(animal_weight <- union(animal, animal_rp) %>%
    arrange(id))  # puts the id in numerical order (useful function for rearranging cases!)

# c) Mutating joins ----

animal_meal  # there are 21 observations!

(animal_joined <- left_join(animal_weight, animal_meal,
                            by = c("id" = "IDs")))  # indicate which columns in two tables refer to the same `id` variable

# We can also write it differently by using the pipe operator
(animal_joined <- animal_weight %>%
    left_join(animal_meal, by = c("id" = "IDs")))

inner_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # only rows with id's appearing in both tables were left (turtle of id = 2 is now missing)

right_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # we have all meal id's but various NAs for `animal` and `weight` columns were introduced because there was no match for them in `animal_weight`

full_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # all possible id's from both tables are retained and various NAs are introduced

full_join(animal_p1, animal_p2, by = c("id", "animal", "weight"))

# d) Filtering joins ----

semi_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # returns `animal_weight` dataset apart from rows of which `id` is not present in `animal_meal`

anti_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # returns only one row from `animal_weight` of which `id` was not present in `animal_meal`

# e) Challenge ----

animal_new <- read.csv("CC-data-manip-creative-dplyr-main/data/animal_new.csv")
str(animal_new)  # check the dataset

#(final_animal <- bind_rows(animal_joined, animal_new))   # combines the two tables

(final_animal <- full_join(animal_joined, animal_new, by = c("id" = "ID", "animal" = "Animals", "weight", "meal" = "Meal")))

# solution
animal_final <- animal_joined %>%
  full_join(animal_new,
            by = c("id" = "ID", "animal" = "Animals", "weight", "meal" = "Meal"))

# Libraries
library(ggplot2)    # one of the tidyverse packages for beautiful graphs for the panel
install.packages("gridExtra")   # install gridExtra before loading
library(gridExtra)

# copy final_animal in to animal_final
animal_final <- final_animal

# Barplot of diet
(barplot <- ggplot(animal_final, aes(animal, fill = meal)) +
    geom_bar(alpha = 0.8) +
    labs(title = "Diversity of meals", x = NULL) +
    scale_fill_brewer(palette = "Set3", type = "seq", na.value = "grey") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

# Boxplot of weight
(boxplot <- ggplot(animal_final) +
    geom_boxplot(aes(animal, weight, fill = animal), alpha = 0.5, position = "dodge2") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(title = "Mean weights of animals", x = NULL, y = "Weight (kg)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

# Combine the plots into one panel
animal_panel <- grid.arrange(barplot, boxplot, ncol = 2)

# Save the panel if you want! You will have to create `images` folder where you've set your working directory (or change filename to "animal_panel.png")
ggsave(filename = "CC-data-manip-creative-dplyr-main/images/animal_panel.png", plot = animal_panel, width = 10, height = 5)

### PART II: OCEANIA LPI DATA ----

# Libraries
# We'll need some extra functions from `tidyverse` package for initial data wrangling, but we will then come back to `dplyr`!
library(tidyr)
library(readr)

# Load the LPI dataset
marine <- read.csv("CC-data-manip-creative-dplyr-main/data/LPI_marine.csv")  # you can also use `readr::read_csv()` - much faster for large files

# Now let's tidy the data a little
marine2 <- marine %>%
  # change to long format with `tidyr::gather()`
  gather(key = year, value = pop, c(25:69)) %>%
  # get rid of X's for year with `readr::parse_number()`
  mutate(year = parse_number(as.character(year)),
         # define `pop` to be numeric, missing data/character entries will become NAs
         pop = as.numeric(pop)) %>%
  # drop all missing values
  drop_na(pop)

# Ignore the warning message on NAs, that's because we kept only numeric values for `pop`

glimpse(marine2)  # have a look at the dataset
View(marine2)  # you can also scroll through the table

# self work to understand the data tidying
top_5_marine <- marine[1:5, ]
top_5_marine_subset_columns <- top_5_marine %>% subset(select = c(id, Common.Name, X1970, X1971, X1972, X1973, X1974, X1975, X1976, X1977))
top_5_marine2 <- top_5_marine_subset_columns %>%
  gather(key = year, value = pop, c(3:10)) %>%
  mutate(year = parse_number(as.character(year)), pop = as.numeric(pop)) %>%
  drop_na(pop)

# 3. Manipulating variables ----
# a) Extracting variables ----

marine2 %>%
  pull(Species) %>%
  glimpse()  # returns a vector of values

marine2 %>%
  select(Species) %>%
  glimpse()  # returns a new table with one column

# Select only columns you need, in the order you want
marine2 %>%
  select(id, pop, year, Country.list) %>%
  glimpse()

# Rename columns using the format `new_name = previous_name`
marine2 %>%
  select("Country list" = Country.list,  # you can use names than include spaces or other symbols that can create errors by putting them in `""`
         method = Sampling.method) %>%
  glimpse()

# Use `everything()` to return all unselected columns
marine2 %>%
  select(id, year, pop, everything()) %>%
  glimpse()  # notice that `id`, `year` & `pop` went to the front but the rest is unchanged

# Add ranges of columns with `star_col:end_col`
marine2 %>%
  select(Family:Species, 24:26) %>%
  glimpse()

# Delete columns with `-`
marine2 %>%
  select(-c(2:22, 24)) %>%
  glimpse()

# Call a vector of column names with `!!`
marine_cols <- c("Genus", "Species", "year", "pop", "id")

marine2 %>%
  select(!!marine_cols) %>%
  glimpse()

# Choose columns with names starting with "Decimal"
marine2 %>%
  select(starts_with("Decimal")) %>%
  glimpse()

marine2 %>%
  select_if(is.numeric) %>%
  glimpse()  # numeric data types only selected (here: integer or double)

marine2 %>% select(id,  # put id first
                   Class:Family,  # add columns between `Class` and `Family`
                   genus = Genus,  # rename `Genus` to lowercase
                   starts_with("Decimal"),  # add columns starting with "Decimal"
                   everything(),  # add all the other columns
                   -c(6:9, system:Data.transformed)) %>%  # delete columns in these ranges
  glimpse()
# OK, maybe it wasn't the most effective way of reaching the end state here but it proves the point of large flexibility!

# Save a new object `marine3` that we'll keep
marine3 <- marine2 %>%
  select(id, Class, Genus, Species, year, pop,
         location = Location.of.population,
         lat = Decimal.Latitude,
         lon = Decimal.Longitude) %>%
  glimpse()

# b) Renaming variables ----

# Renaming to lowercase
marine3 %>%
  rename(class = Class,
         genus = Genus,
         species = Species) %>%  # renames only chosen columns
  glimpse()

marine3 %>%
  rename_with(tolower) %>%
  glimpse()
# If you didn't want all of them renamed, you could specify columns with `.cols =`

marine4 <- marine3 %>%  # let's save this one!
  select_all(tolower) %>%
  glimpse()

# Select and rename a couple of columns only
marine3 %>%
  select_at(vars(Genus, Species), tolower) %>%
  glimpse()

# c) Making new variables ----

# New column appears at the end of the table
marine5 <- marine4 %>%
  mutate(genus_species = paste(genus, species, sep = "_")) %>%
  glimpse()

marine6 <- marine5 %>%
  mutate(region = case_when(lat > 0 & lon >= 0 ~ "NE",
                            lat <= 0 & lon >= 0 ~ "SE",
                            lat > 0 & lon < 0 ~ "NW",
                            lat <= 0 & lon < 0 ~ "SW")) %>%
  glimpse()

unique(marine6$region)  # we've got populations from both sides of equator and meridian 180!

# A table of only two variables: `genus_species` and `region`
marine4 %>%
  transmute(genus_species = paste(genus, species, sep = "_"),
            region = case_when(lat > 0 & lon >= 0 ~ "NE",
                               lat <= 0 & lon >= 0 ~ "SE",
                               lat > 0 & lon < 0 ~ "NW",
                               lat <= 0 & lon < 0 ~ "SW")) %>%
  glimpse()

# Change values to lowercase
marine6 %>%
  mutate_at(vars(class, genus, location), tolower) %>%
  glimpse()  # column names stayed the same, but individual values within them are lowercase

# Adding a column (`add_column()` is from a different package - `tibble`)
library(tibble)  # load `tibble` package

marine6 %>%
  add_column(observation_num = 1:4456) %>%  # we gave each row an observation number
  glimpse()

# Summarising functions
marine6 %>%
  select(genus_species, year) %>%
  group_by(genus_species) %>%
  add_tally(name = "observations_count") %>%
  glimpse()

marine6 %>%
  select(genus_species, year) %>%
  # `add_count()` includes the grouping variable (here `genus_species`) inside the function
  add_count(genus_species, name = "observations_count") %>%
  glimpse()

# 4. Manipulating cases ----
# a) Extracting cases ----

# Here we use comparison operator `==`
marine6 %>%
  filter(class == "Mammalia") %>%
  glimpse()  # mammals only

# Match one of the specified values with `%in%`
marine6 %>%
  filter(class %in% c("Mammalia", "Aves")) %>%
  glimpse()  # mammals and aves

# Same result as above but using `|`
marine6 %>%
  filter(class == "Mammalia" | class == "Aves") %>%
  glimpse()

# Omit a category
marine6 %>%
  filter(class != "Actinopteri") %>%
  glimpse()  # all classes but Actinopteri

marine6 %>%
  filter(!class %in% c("Mammalia", "Aves")) %>%
  glimpse()  # remove classes of mammals and aves

# Filter numeric variables
marine6 %>%
  filter(pop >= 10 & pop <= 100) %>%
  glimpse()  # observations for population size between 10 and 100

marine6 %>%
  filter(between(pop, 10, 100)) %>%
  glimpse()  # same as above but more aesthetically pleasing!

# Delete NAs from `pop`
marine6 %>%
  filter(!is.na(pop)) %>%
  glimpse()  # there's already none because we deleted them earlier with `mutate()`!

# Be careful with logical operators
marine6 %>%
  filter((class == "Mammalia" | pop > 100) & region != "SE") %>%
  glimpse()  # 38 rows

# Argument 1: class is Mammalia or population is larger than 100
# AND
# Argument 2: in each case the region cannot be SE


marine6 %>%
  filter(class == "Mammalia" | (pop > 100 & region != "SE")) %>%
  glimpse()  # 96 rows!

# Argument 1: class is Mammalia
# OR
# Argument 2: population is larger than 100 and region is not SE

marine6 %>%
  distinct() %>%
  glimpse()  # still 4456 rows so there were no duplicates!

# You can also check a number of distinct rows directly with `n_distinct()` - equivalent of length(unique(x))
marine6 %>%
  n_distinct()  # 4456 distinct rows

marine6 %>%
  select(id:species) %>%  # we'll keep less columns for clarity
  slice(2:4)  # keep only rows 2, 3 and 4! (they get new numbers though - 1, 2, 3)

marine6 %>%
  top_n(5, pop) %>%  # 5 highest population values
  glimpse()

# 4b challenge
marine7 <- marine6 %>%
  filter(genus_species == "Chrysophrys_auratus" & id == 2077) %>%
  select(id, genus_species, year, pop)

# b) Adding cases ----

# Create a table for id == "2077"
(marine7 <- marine6 %>%
   filter(id == "2077") %>%
   select(id, genus_species, year, pop))  # we can see 27 observations for 1970-1996

# Add a row at the end
(marine7 %>%
    add_row(id = 2077, genus_species = "Chrysophrys_auratus", year = 1997, pop = 39000))

# Add a row at a specified location
(marine7 %>%
    add_row(id = 2077, genus_species = "Chrysophrys_auratus", year = 1969, pop = 39000,
            .before = 1))

# 5. Extra: Green Sea Turtle population trend ----

marine_final <- marine6 %>%
  filter(genus_species == "Chelonia_mydas") %>%
  # change `id` to factor (otherwise it would display as a continuous variable on the plot)
  mutate(id = as.factor(id))

library(ggplot2)  # you don't have to load it again if you've done it in part 2e

(chelonia_trends <- ggplot(marine_final, aes(x = year, y = pop, colour = location)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", colour = "black", fill = "lightgrey") +
    scale_x_continuous(limits = c(1970, 2005), breaks = c(1970, 1980, 1990, 2000)) +
    labs(x = NULL, y = "Population count\n",
         title = "Positive trend of Green Sea Turtle population in Australia\n",
         colour = "Location") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

ggsave(chelonia_trends, filename = "CC-data-manip-creative-dplyr-main/images/chelonia_trends.png", width = 8, height = 6)
