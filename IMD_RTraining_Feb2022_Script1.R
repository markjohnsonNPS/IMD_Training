
install.packages("janitor")
library('janitor') #call janitor package
library ('tidyverse') #call tidyverse package

# create messy dataset
messy_data <- data.frame(TrailName = c('Bayside', 'Coastal', 'Tidepools', 
                                   'Monument', 'Tidepools', NA),
                     Visitor_count = c(200, 300, 400, 500, 400, NA),
                     `empty row` = NA)
#Use clean_names from the janitor package to standardize the existing row names so that they all use "_" also called "snake case"
clean_data <- clean_names(messy_data)
clean_data

#remove empty rows using janitor package
clean_data2 <- remove_empty(clean_data, which = c('rows'), quiet = FALSE)
clean_data2
# remove empty rows and columns at the same time
clean_data2 <- remove_empty(clean_data, which = c('rows', 'cols'), quiet = TRUE)
clean_data2

# a more streamlined version using the Pipe %>% feature; reads like "and then"
clean_data <- clean_names(messy_data) %>%
  remove_empty(which = c('rows', 'cols'), quiet = TRUE) %>%
  distinct() # a distinct in there as well

clean_data


# Create Messy data2   ############################
messy_data2 <- data.frame(`Park Code` = c(NA, NA, 'CABR', 'CHIS', 'SAMO', 'SAMO'),
                      visitor_count = c(NA, NA, 400, 500, 400, 400),
                      `empty row` = NA)


clean_data2 <- clean_names(messy_data2) %>% 
      remove_empty(which = c('rows', 'cols'), quiet= TRUE) %>% 
      distinct () 

clean_data2

#########  DPLYR   ########################
# load library
library('readr')

### import data from tidytuesday github page
park_visits <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

#select columns of interest from a data frame

# you can either select for the columns you want
park_visits1 <- park_visits %>% select(year, parkname, unit_code, state, visitors)

park_visits1 <- select(park_visits, year, parkname, unit_code, state, visitors)

# or against the columns you don't want, by using the minus sign before the column name
park_visits2 <- park_visits %>% select(-gnis_id, -geometry, -metadata, -number_of_records, -region, -unit_name, -unit_type)


# rename parkname column to make it snake case; The two lines below do the same thing
park_visits <- park_visits1 %>% rename(park_name = parkname) #uses pipe
# same as above but written differently
park_visits <- rename(park_visits, park_name = parkname)  #names the data frame in the rename




##############Challenge 1 - get totals for state of your choice; you are not calculating a total here the totals exist in the data set as seperate rows

pa_park_visits <- park_visits %>%
  # filter for parks in PA
  filter(state == 'PA'&
           # filter for total visitation
           year == 'Total')

# remove result from environment
remove(pa_park_visits)



wa_park_visits <- park_visits %>%
  # filter for parks in WA
  filter(state == 'WA' &  # == is not math, it is doing a true false evaluation
           # filter for total visitation
           year == 'Total' & 
           # filter for over 5 million visitors
           visitors > 5000000)%>%
  # arrange by visitation - default is ascending order
  arrange(visitors)

#### Arrang edescending order
wa_park_visits2 <- wa_park_visits %>%
  arrange(desc(visitors))

### mutate fucntion to create new columns; good for converting units etc
wa_park_visits_millions <- wa_park_visits %>%
  mutate(visitors_mil = visitors/1000000)

#### Group By Example

park_visits2 <- park_visits %>%
  # remove the rows with Totals per park
  filter(year != "Total")  %>% 
  # do calculations by park
  group_by(park_name) %>%
  # add visit_percent column
  # visits for each year divided by the sum of total visits for each park
  # the na.rm = TRUE means that NA values are not included in calculations
  # round(2) is rounds result to 2 decimal places for easier reading
  mutate(visit_percent = (100*visitors/sum(visitors, na.rm = TRUE)) %>%
           round(2)) 

#### summarize example


state_summary <- park_visits %>%
  # filter for total visitation numbers (to avoid double counts)
  filter(year == 'Total') %>%
  # do calculations by state
  group_by(state) %>%
  # calculate summaries
  summarize(
    # mean number of total visitors across parks in each state
    mean_visit = mean(visitors, na.rm = TRUE),
    # sd of total visitors across parks in each state
    sd_visit = sd(visitors, na.rm = TRUE),
    # min total visitors across parks in each state
    min_visit = min(visitors, na.rm = TRUE),
    # max total visitors across parks in each state
    max_visit = max(visitors, na.rm = TRUE),
    # get number of park units w data in each state
    n_parks = n()
  )

### Challenge 2

challenge2 <- park_visits %>%
    filter(year == 'Total') %>%  # filter for total visitation numbers (to avoid double counts)
       group_by(state) %>%  # do calculations by state
   summarize(mean_visit = mean(visitors, na.rm = TRUE), n_parks = n() ) %>%
    ungroup() %>%    # ungroup to do another calculation
    mutate(visit_per_park = mean_visit/n_parks) %>%  # calculate visit/n
  
  select(state, visit_per_park) %>%   # select relevant columns
      arrange(desc(visit_per_park))  # arrange in descending order

######  PIVOT SECTION  ###################################################


# we'll first take a subset of the park visits data
medn_visits <- park_visits %>%
  # get parks in MEDN and no totals, years 2010-2015
  filter(unit_code %in% c("CABR", "CHIS", "SAMO") & year != "Total") %>%
  # make year a number, since no more text
  mutate(year = as.numeric(year)) %>%
  # get years 2010:2015
  filter(year >= 2010 & year <= 2015) %>%
  # arrange in ascending order
  arrange(year) %>%
  # select relevant columns
  select(unit_code, year, visitors)

#Pivot Wider so you more easily make comparison across years
medn_visits_wide <- pivot_wider(medn_visits, names_from = year, values_from = visitors)

##wide example 2
medn_visits_wide <- medn_visits %>%
  pivot_wider(names_from = unit_code, values_from = visitors)
medn_visits_wide

## pivot it back to long
medn_visits_long <- medn_visits_wide %>%
  pivot_longer(
    # first argument is the columns you'd like to transform
    SAMO:CHIS,
    # next is what you'd like to name the former name cols
    names_to = "unit_code",
    # last is what you'd like to name the former values cols
    values_to = "visitors"
  )

### Pivot Challenge 


# get 3 states of choice (FL, CA, AK) for years 2010-2015
solution_original <- state_pop %>%
  filter(state %in% c("FL", "AK", "CA") &
           year >= 2010 & year <= 2015)
# pivot wider
solution_wide <- solution_original %>%
  pivot_wider(
    names_from = "year",
    values_from = "pop"
  )

# pivot longer to return to format of solution_original
solution_long <- solution_wide %>%
  pivot_longer(`2010`:`2015`, names_to = "year", values_to = "pop")

#####  Joining Tables  ###############################################

# we'll first make two subsets of the park_visits data
SHEN_visits <- park_visits %>% 
  filter(unit_code == "SHEN")

ACAD_visits <- park_visits %>%
    filter(unit_code == "ACAD")

# now we will combine the 2 data.frames into one.
SHEN_ACAD_visits <- bind_rows(SHEN_visits, ACAD_visits)

# check that data from both parks is there
SHEN_ACAD_visits %>%
  pull(unit_code) %>%
  table()  # makes a table in the output below

state_pop2 <- state_pop %>% filter(year < 2000)


# left_join example
state_pop_gas1 <- left_join(
  x = state_pop2, # first data.frame
  y = gas_price, # second data.frame
  by = "year" # key column
)

# Right_join example...Missing years in the gas price data frame will end up in less rows
state_pop_gas2 <- right_join(
  x = state_pop2, # first data.frame
  y = gas_price, # second data.frame
  by = "year" # key column
)

## Day 3 Visualization #############################

# Packages used in this section
library(tidyverse)
library(viridis) # for colorblind-friendly palettes
library(terra)

## code to install useful visualization packages 
packages <- c("tidyverse", "ggthemes", "GGally", "RColorBrewer", 
              "viridis", "scales", "plotly", "patchwork", 
              "sf", "tmap", "leaflet", "spsurvey")

install.packages(setdiff(packages, rownames(installed.packages())))

lapply(packages, library, character.only = TRUE)  #call all packages using library function


park_visits <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv") # visits to US National Parks

gas_price <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv") # gas prices over space and time

summary(park_visits)  # summary of data frame attributes
glimpse(park_visits) #abbreviated look at data frame
str(park_visits)   #Structure
dim(park_visits)  # dimension = number of records and variables
class(packages)  # tells you whether something is text or not
unique(park_visits$region)  ## gives you a distinct list of the region column

# NOTES: 
# 1. When we preface function names with the package name, we avoid potential issues with duplicate function names among packages
# 2. The dplyr::select() function can simultaneously select columns, rename them, and order them as specified. See `?select`
# 3. When we `dplyr::filter()` by multiple conditions, we can use a comma (,) or and-sign (&) to combine the multiple conditions
# 4. `gas_price$gas_constant` is gas price in constant 2015 dollars/gallon


joined_dat <- park_visits %>%
  dplyr::filter( # filter by multiple conditions
    region %in% c("IM", "PW", "SE"), 
    unit_type %in% c("National Monument", "National Historic Site", "National Park"),
    year %in% "2000":"2015") %>%
  dplyr::select(region, unit_type, unit_name, unit_code, year, visitors) %>% # keep these columns only, and arrange them in this order
  dplyr::mutate(year = as.numeric(year)) %>% # convert `park_visits$year` to numeric, to match the data type of `gas_price$year`
  dplyr::left_join(gas_price[c("year", "gas_constant")], by = "year")

## FACTOR DATA TYPES - categorical variable not continuous

## example
reg<- as.character (c("SE","IM","PW"))  # create vector
reg_F<-factor(c("SE", "IM", "PW") , levels = c("PW","IM", "SE")) # convert to vectors and reorder the levels

levels(reg_F)  #check the levels

# NOTES:
# 1. The factor data type is similar to (and often interchangeable with) the character data type, but limits entries to pre-specified values and allows us to specify an order to those values (other than the default alphabetical order)
# 2. Factors are especially useful for ordered categories (e.g., low < medium < high) with a small number of possible values
# 3. Under the hood, `R` store factors as integer vectors representing the order of the value (with character string labels)
# 4. I usually classify categorical variables as character data types, unless I want to limit entries to a few pre-specified values, or if I want to enforce a particular order to the category levels

joined_dat$region <- 
  factor(joined_dat$region, 
         levels = c("PW", "IM", "SE"), # specifies the factor levels and an order for them. If we omit this argument, the factor levels will be the unique set of values in the column, ordered alphabetically
         labels = c("Pacific West", "Intermountain", "Southeast")) # relabels the levels with more descriptive names, keeping the same order as the `levels` argument

# Check the data
glimpse(joined_dat) # `region` is now a factor data type


##### Step 5. Make sure every park unit has one and only one data record for each year

# NOTES: 
# 1. Check out `?expand_grid` for an alternative approach
# 2. The `table` data class describes

# Run the code in pieces to see what the different lines do. For example, copy `table(uc = park_sub$unit_code, yr =park_sub$year)` and run it in the console to see the output. Then copy `table(uc = park_sub$unit_code, yr =park_sub$year) %>% as.data.frame()` and run it in the console to see the output. Repeat until the end of the code chunk.
remove_units <- 
  table(unit = joined_dat$unit_code, yr = joined_dat$year) %>% # build a contingency table of counts for every combination of `unit_code` and `year`
  as.data.frame() %>% # convert from `table` to `data frame` class
  dplyr::filter(Freq != 1) %>% # filter for unit-year combinations that are less than or greater than 1
  distinct(unit) # identify the corresponding park units

# Remove those park units from the data
viz_dat <- joined_dat %>%
  dplyr::filter(!unit_code %in% remove_units$unit)

# View(viz_dat) to make sure the park units have been removed. The final data frame should have 1856 rows and 7 columns.


#save locally if you want
saveRDS(viz_dat, "viz_dat.RDS")
# To read the RDS file from your working directory and assign it to `viz_dat`, use the code below. You can assign it to any name you want. If you're reading it from a location other than your working current directory, provide the file path in the function argument.
viz_dat <- readRDS("viz_dat.RDS")


#### Plotting ################################################################

# Subset the data
subdat <- viz_dat %>%
  dplyr::filter(
    year %in% c(2000, 2005, 2010, 2015),
    unit_type == "National Monument",
    region == "Intermountain")

boxplot(visitors ~ year, data = subdat) # the basic boxplot, with data provided as a formula

# Save this boxplot to `p_box` so we can look at the underlying structure
p_box <- boxplot(visitors/1000 ~ year, # so now, 200,000 will be shown as 200 on the y-axis
                 data = subdat,
                 main = "National Monuments in the Intermountain Region", # plot title
                 xlab = "Year", # x-axis title
                 ylab = "Number of visitors, in 1000's") # y-axis title

# Subset for just Arches NM
arch_dat <- viz_dat %>% 
    dplyr::filter (unit_code == "ARCH")

# scatter plot 
plot(visitors/1000 ~ gas_constant, 
      data = arch_dat,
      pch=19,
     cex=1.5)

# line plot 
plot(visitors/1000 ~ year,
     type="b", # b=both line and points; p= just points
     data = arch_dat,
     pch=19,
     cex=1.5)

# line plot different version
plot(visitors/1000 ~ year, type = "b", pch = 19, data=arch_dat,
     col = "red", xlab = "x", ylab = "y")

### ggplot  ###########################################################

library(ggthemes)


# NOTES:
# 1. The first argument provided to the `ggplot()` function is assumed to be the data frame, so it's not necessary to include the argument name `data =`.
# 2. We are assigning the plot to the name `p` so we can build on this `ggplot()` master template in the next step.
# 3. The parentheses around the line of code is a shortcut for `print`. Without it, the plot would assign to `p` but not print in the plots pane.

(p <- ggplot(data = arch_dat, aes(x = year, y = visitors/1000)))

summary(p) # summary of the information contained in the plot object
p$data # the plot object is self-contained. The underlying data frame is saved a list element in the plot object. 


# NOTES:
# 1. Our plot code is getting long, so we will separate out the components to improve readability
# 2. We will assign this plot object to the name `p2` so we can just add to `p2` in the next step instead of re-typing all these lines
# 3. `ggplot2` also provides shortcuts for some scaling arguments. For example, if we just want to set y-axis limits we can add the layer `ylim (600, 1500)` instead of setting limits via `scale_y_continuous()`

# Check out `?scale_y_continuous` for ways to modify the y-axis

ggplot() +
  geom_line(data = arch_dat, aes(x = year, y = visitors/1000)) +
  geom_point()
p + layer(geom = "line", stat = "identity", position = "identity") # this creates the same line graph we had created using `geom_line()`

(p2 <- p + 
    geom_line() + 
    geom_point() +
    scale_y_continuous(
      name = "Number of visitors, in 1000's", # y-axis title
      limits = c(600, 1500), # minimum and maximum values for y-axis
      breaks = seq(600, 1500, by = 100)) # label the axis at 600, 700, 800... up to 1500
)

(p3 <- p2 + 
    labs(
      title = "Visitor Counts at Arches National Park, 2000 - 2015", # plot title
      subtitle = "Visits to Arches National Park have increased each year since 2004", # plot subtitle
      x = "Year") # x-axis title. We had already set the y-axis title with `scale_y_continous()`.
)

### p3 theme
ggplot() +
  geom_line(data = arch_dat, aes(x = year, y = visitors/1000)) +
  geom_point()
p3 + layer(geom = "line", stat = "identity", position = "identity") # this creates the same line graph we had created using `geom_line()`

#### FACET wrap

### create a dataset of three parks
subdat <- subset(viz_dat, unit_code %in% c("ARCH", "GRCA", "YOSE")) # plot data for Arches National Park (ARCH), Grand Canyo

### create a faceted (multiple graphs in 1)

ggplot(subdat, aes(x = year, y = visitors/1000)) + 
  geom_line() + 
  geom_point() +
  labs(
    title = "Annual Visitor Counts at Three National Parks, 2000 - 2015",
    x = "Year", 
    y = "Number of visitors, in 1000's") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) + # center the plot title
  facet_wrap(vars(unit_name), ncol = 1, scales = "free_y")


### Matrix Heat Map ##########################################################


# Subset the data, only keeping data for National Parks
# Instead of showing visitors in 1000's we will show visitors in millions because annual National Park visitor counts are generally higher
tab_visits <- viz_dat %>%
  dplyr::filter(unit_type == "National Park") %>%
  dplyr::mutate(visitors_m = visitors/1000000,
                year_f = as.factor(year)) 

# Starting plot
ggplot(tab_visits, aes(y = unit_name, x = year_f, fill = visitors_m)) + 
  geom_tile() # geom_tile is the heat map type

#tweak 1
ggplot(tab_visits, aes(y = unit_name, x = year_f, fill = visitors_m)) + 
  geom_tile(colour = "white", size = 0.3) + # <<< white border with thicker border than default
  scale_fill_viridis_c() # <color-blind friendly and larger range of colors

#tweak 2
ggplot(tab_visits, aes(y = unit_name, x = year_f, fill = visitors_m)) +
  geom_tile(colour = "white", size = 0.3) +
  scale_fill_viridis_c(direction = -1) + # <<< Reverse color order
  geom_text(aes(label=round(visitors_m, 1), color= visitors_m >7), size=3, )+
  # add cell labels, rounded to one digit. make color based on visitors, adjust font size.
scale_color_manual (values =c("black", "white")) #the values turn white when scale is dark!


## Tmap  #############################################################


#  install.packages(c("sf", "spsurvey"))
library(dplyr) # for filter, select, mutate and %>%  
library(sf)  # simple features - working with GIS, define or change spatial features
library(tmap) # pretty GIS maps
library(spsurvey)

# Read in shapefiles from teams data folder
sara_bound1 <- st_read('D:/AAAA_MJJ/AAAA Monitoring/Breeding Bird/R Analysis/CODE/NCRNbirds/Data/IMD2022/data/SARA_boundary_4269.shp')
sara_veg1 <- st_read('D:/AAAA_MJJ/AAAA Monitoring/Breeding Bird/R Analysis/CODE/NCRNbirds/Data/IMD2022/data/SARA_Veg.shp')


# Generate a random number for the seed
set.seed(2407)
sample(1:100000, 1) #62051

# Check that the projections match; fix the one that doesn't match
st_crs(sara_bound1) == st_crs(sara_veg1) # FALSE!! - projections don't match.

# sara_bound1 needs to be re-projected to UTM Zone 18N NAD83. 
sara_bound <- st_transform(sara_bound1, crs = 26918)

st_crs(sara_bound) == st_crs(sara_veg1) # Now Tests as TRUE!!! booya

# Quick plot of first column in attribute table
plot(sara_bound[1])
plot(sara_veg1[1]) # bigger extent than boundary

# CLIP / Intersect boundary and veg to be same extend
sara_veg <- st_intersection(sara_veg1, sara_bound)  #clip to boundary

plot(sara_veg[1]) # View: now the veg is clipped to boundary! FAST


# View attribute table of layers
head(sara_bound) # 1 feature with 95 fields

str(sara_veg)
head(sara_veg)
names(sara_veg)
table(sara_veg$ANDERSONII)

# Simplify vegetation types for easier plotting
dev <- c('1. Urban or Built-up Land', '11. Residential', 
         '12. Commercial and Services', '13. Industrial',
         '14. Transportation, Communications, and Utilities', 
         '17. Other Urban or Built-up Land')
crop <- c('21. Cropland and Pasture', 
          '22. Orchards, Groves, Vineyards, and Nurseries', 
          '31. Herbaceous Rangeland')
shrubland <- c('32. Shrub and Brush Rangeland')
forest_up <- c('41. Deciduous Forest Land', '42. Evergreen Forest Land', 
               '43. Mixed Forest Land')
forest_wet <- c('61. Forested Wetland')
open_wet <- c('62. Nonforested wetland', '62. Nonforested Wetland')
water <- c('5. Water', '51. Streams and Canals', '53. Reservoirs')
unk <- 'Unclassified'

# Create 2 fields in the veg attribute table: simp_veg, and fills
sara_veg <- sara_veg %>% 
  mutate(simp_veg = case_when(ANDERSONII %in% dev ~ 'Developed',
                              ANDERSONII %in% crop ~ 'Open field',
                              ANDERSONII %in% shrubland ~ 'Shrublands',
                              ANDERSONII %in% forest_up ~ 'Forest',
                              ANDERSONII %in% forest_wet ~ 'Forested wetland',
                              ANDERSONII %in% open_wet ~ 'Open wetland',
                              ANDERSONII %in% water ~ 'Open water',
                              ANDERSONII %in% unk ~ 'Unclassified',
                              TRUE ~ 'Unknown'),
         fill_col = case_when(simp_veg == 'Developed' ~ '#D8D8D8',
                              simp_veg == 'Open field' ~ '#f5f0b0',
                              simp_veg == 'Shrublands' ~ '#F29839',
                              simp_veg == 'Powerline ROW' ~ '#F9421D',
                              simp_veg == 'Forest' ~ '#55785e',
                              simp_veg == 'Forested wetland' ~ '#9577a6',
                              simp_veg == 'Open wetland' ~ '#c497d4',
                              simp_veg == 'Open water' ~ '#AFD0F2',
                              simp_veg == 'Unclassified' ~ '#ffffff'))

#table(sara_veg$simp_veg)  #shows you if you have anything unclassified

#Generate GRTS sample
  

sara_grts <- grts(sara_bound, n_base = 100) # generate 100 points within SARA boundary

sara_grts$sites_base$priority <- as.numeric(row.names(sara_grts$sites_base)) # add priority number (same as row.name)

#  sara_grts$sites_base  - this is the important column


# Spatial join
grts_veg <- st_join(sara_grts$sites_base, sara_veg %>% select(simp_veg)) # st_join 

#names(grts_veg)

# Create list of forest habitats
sort(unique(sara_veg$simp_veg))
forest_veg <- c('Forest', 'Forested wetland')

# Filter out non-forest points
grts_forest <- grts_veg %>% filter(simp_veg %in% forest_veg)

table(grts_forest$simp_veg) # Shows how many fell into forest VS For wetland

grts_50<-grts_veg %>% filter(priority <=50)

nrow(grts_veg) # 100 points
nrow(grts_50) # fewer points


### CREATE MAP   #################################


# Creating list of simp_veg types and their fill colors for easier legend
for_legend <- unique(data.frame(simp_veg = sara_veg$simp_veg, fill_col = sara_veg$fill_col)) 

sara_map <- 
  # Vegetation map
  tm_shape(sara_veg, projection = 26918, bbox = sara_bound) +
  tm_fill('fill_col') +
  tm_add_legend(type = 'fill', labels = for_legend$simp_veg, 
                col = for_legend$fill_col, z = 3)  +
  
  # Park boundary
  tm_shape(sara_bound) +
  tm_borders('black', lwd = 2) +
  tm_add_legend(type = 'line', labels = 'Park Boundary', col = 'black',
                z = 2)+
  
  # GRTS points  
  tm_shape(grts_forest) +
  tm_symbols(shapes = 21, col = '#EAFF16', border.lwd = 0.5, size = 0.3) + 
  tm_text('priority', size = 0.9, xmod = -0.4, ymod = -0.4) +
  tm_add_legend(type = 'symbol', labels = 'GRTS points', shape = 21, 
                col = '#EAFF16', border.lwd = 1, size = 0.5, 
                z = 1) +
  
  # Other map features
  tm_compass(size = 2, type = 'arrow', 
             text.size = 1, position = c('left', 'bottom')) +
  tm_scale_bar(text.size = 1.25, position = c('center', 'bottom')) + 
  tm_layout(inner.margins = c(0.2, 0.02, 0.02, 0.02), # make room for legend
            outer.margins = 0,
            legend.text.size = 1.25,
            legend.just = 'right',
            legend.position = c('right', 'bottom'),
            title = 'Saratoga NHP GRTS points',
            title.position = c('center', 'top'))

#sara_map   # View Map

tmap_save(sara_map, 'SARA_GRTS.png', height = 10.5, width = 8, 
          units = 'in', dpi = 600, outer.margins = 0)

### Interactive Map  ########################################################

# Load park tiles
NPSbasic = 'https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg'

NPSimagery = 'https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg'

NPSslate = 'https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg'

NPSlight = 'https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg'

# Set parktiles as default basemaps for all interactive maps
tmap_options(basemaps = c(Map = NPSbasic,
                          Imagery = NPSimagery,
                          Light = NPSlight,
                          Slate = NPSslate))

# Switch to interactive mode, and plot the sara_map
tmap_mode('view') # turn interactive mode on
sara_map


### If Then and Itteration  ########################################


d <- c(1:4, 1:4 * 100, NA)
# checks for NA valies
if (all(is.finite(d))) { # checks for NA values
  "no NA, NaN, or Inf present"
} else {
  "NA present"
}
d2 <- c(1:4, 100*1:4)

# Case when example
d3 <- c(1:4, 100*1:4, NA)  # create vector
dplyr::case_when(
  is.na(d) ~ "Missing",   # if any values in the vector are NA output "missing"
  d3 < mean(d, na.rm = T) / 10 ~ "Verify", #if d3 is less than the mean divided by 10
  d3 > mean(d, na.rm = T) / 10 ~ "OK" # if d3 is greater than...
)  # na.rm  removes NAs before doing mean calculation



### For Loops ##################################################

# Example of basic for loop
vec<-1:9 #vector over which we will iterate
for (i in vec) { # for the number 1 through 9
  print(i+1)  # add one to the value in the vector and prints to console
}   

#alternatively predefine a container to receive the data
OutDat <- NULL # define an empty variable. 
for (i in 1:9) {
  OutDat[i] <- i + 1 # store that in the ith position of the variable OutDat
}
OutDat

library(magrittr)
library(ggplot2)

#create a list of file names that we will read in
fNames <- paste0(
  "https://raw.githubusercontent.com/KateMMiller/IMD_R_Training_Intro/master/Data/",
  c(
    "APIS01_20548905_2021_temp.csv",
    "APIS02_20549198_2021_temp.csv",
    "APIS03_20557246_2021_temp.csv",
    "APIS04_20597702_2021_temp.csv",
    "APIS05_20597703_2021_temp.csv"
  )
)
fNames

temp<-read.csv(fNames[1], skip = 2, header = F) #Set header to F so read.csv doesn't treat data as a header.
names(temp)[1:3]<-c("idx", "DateTime", "T_F") #let's just assign some column names really quick
head(temp)


#### For Loop that downloads multiple datasets and create plots of them

OutPath <- paste0(getwd(), "/Output/IMD2022/hobo_ouputs/") # output path

for (i in fNames) {    #fnames is a vector html paths to csv files
  
  #1 Read in/generate data
  fName <- basename(i) #basename is filename part of path
  d <- read.csv(i, skip = 1, header = T)[1:3] %>%  #d is a dataframe
    setNames(., c("idx", "DateTime", "T_F")) %>%  #  the "." is where the data goes
    dplyr::mutate(., DateTime2 = as.POSIXct(DateTime, "%m/%d/%y %H:%M:%S", tz = "UCT"))
 
   #2 Do the thing you want to do, in this case make and save plots
  p <- ggplot(d, aes(x = DateTime2, y = T_F)) +  #p is a list of plots
    geom_point(size = 0.5) +
    ggtitle(fName)
 
   #3 Output the results outside of the for loop
  dir.create(OutPath, showWarnings = F)
  ggsave(
    filename = gsub(".csv", "_plot.pdf", fName), # gsub removes ".csv" and replaces it
    path = OutPath, device = "pdf", # saves as a PDF
    plot = p, width = 5, height = 5, units = "in"
  )
}

