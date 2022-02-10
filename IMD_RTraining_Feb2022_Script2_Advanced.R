### SETUP  ##################################################################
# Create vector list of packages required for Advanced R training
packages <- c("tidyverse", "DBI", "odbc", "readr", "dbplyr", # Reading Databases
              "GADMTools", "sf", "tmap", # GIS in R
              "readxl", "rvest", "htmltab", "stringr", "jsonlite", "httr", "geojsonsf", # Web services
              "dataRetrieval", "lubridate", "jsonlite", "httr", # Aquarius
              "rFIA" # for USFS FIA data
)

install.packages(setdiff(packages, rownames(installed.packages()))) #installs above list


installed_packages <- packages %in% installed.packages() # check which packages are installed
if (length(packages[!installed_packages]) > 0){
  install.packages(packages[!installed_packages], dep = TRUE)} # if some are missing, install them

# load all packages into library
lapply(packages, library, character.only = TRUE) 

sara_bound1 <- st_read('D:/AAAA_MJJ/AAAA Monitoring/Breeding Bird/R Analysis/CODE/NCRNbirds/Data/IMD2022/data/SARA_boundary_4269.shp')

#Day 1:

library(DBI)
library(odbc)
library(readr)
library(magrittr)
library(dplyr)
library(dbplyr)  #works with data in databases



#----------Connect to Access------------#

db_path <- "/IMD2022/data/Trees.accdb"
conn_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", db_path)

conn <- dbConnect(odbc(), .connection_string = conn_string)


# data from the web ###################



# Day 2 - Fnctional Programming  ###########################

packages <- c("tidyverse", "parallel", "microbenchmark", "profvis", 
              "modelr", "lubridate", "tidyselect")

installed_packages <- packages %in% installed.packages() # check which packages are installed
if (length(packages[!installed_packages]) > 0){
  install.packages(packages[!installed_packages], dep = TRUE)} # if some are missing, install them

lapply(packages, library, character.only = TRUE) 


# example for loop
nIter=10 #number of iterations
for (i in 1:nIter) {
  print(data.frame(x=i + 1, y=i))
}

##

#  "growing" a dataframe with rbind
nIter=10000
OutDat <- NULL # define an empty variable. 
system.time(for (i in 1:nIter) {
  d <- data.frame(x=i + 1,y=i)
  OutDat<-rbind(OutDat,d)# append the previous steps' outputs with this step's output
}) #4.47 seconds on my system

#growing a dataframe with append()  
OutDat <- NULL # define an empty variable. 
system.time(for (i in 1:nIter) {
  d <- data.frame(x=i + 1,y=i)
  OutDat<-append(OutDat,d)# append the previous steps' outputs with this step's output
}) #4.93 seconds on my system

### Important to know that eventually it will get so big it wont even process, there are better ways

### define size of your output ahead of time, will take ,uch less time to execute
nIter=10000 # number of iterations
OutDat <- data.frame(x=rep(NA,nIter),y=rep(NA,nIter)) # define an preallocated dataframe
system.time(for (i in 1:nIter) {
  OutDat[i,] <- data.frame(x=i + 1, y=i)
}) #2.5 seconds on my system
rm(OutDat)   #removes outdat since he refines this later in his examples

# Water Example
#create a few variables that will be used for file path manipulation later on
inPath <- "https://raw.githubusercontent.com/KateMMiller/IMD_R_Training_Intro/master/Data/" #location of the data
fNames <- c(
  "APIS01_20548905_2021_temp.csv",
  "APIS02_20549198_2021_temp.csv",
  "APIS03_20557246_2021_temp.csv",
  "APIS04_20597702_2021_temp.csv",
  "APIS05_20597703_2021_temp.csv"
) #names of the files

#preallocate the output object
HoboData<-vector(mode = "list", length = length(fNames))%>%
  setNames(.,fNames)
#read in the data
for(i in fNames){
  HoboData[[i]]<-read.csv(file.path(inPath,i), skip = 1, header = T)[1:3]%>%
    setNames(.,c("idx", "DateTime", "T_F"))
}

#str(HoboData) #uncomment to inspect
format(object.size(HoboData), units="Mb") #how many Mb of memory is this object now occupying


### Flagging example  ###############

#Set Working Directory
setwd("D:/AAAA_MJJ/AAAA Monitoring/Breeding Bird/R Analysis/CODE/NCRNbirds/Data/IMD2022/data")

OutPath <- paste0(getwd(), "/hobo_outputs/") # or put your preferred path here (we will use this later)
# set up some arbitrary QA/QC thresholds
TempUpper <- 40
TempLower <- (-40)
#not outputting anything so we aren't going to preallocate.
for (i in fNames) {
  # 1 Read in the data
  OutPut <- HoboData[[i]] %>% #read in the data and define a output
    # 2 Generate your output in this case, some QA/QC flagging
    dplyr::mutate(
      # Create a variable that collects temperature change flags. This can be thought of as the 'first derivative' of the data.
      TChangeFlag = ifelse(
        c(abs(diff(T_F, lag = 1)) > 10, FALSE), #logical vector 
        yes = "D10", #TRUE case
        no = NA),    #FALSE case
      # Create a variable that captures flags for high and low temp data.
      Flag = dplyr::case_when(
        is.na(T_F) ~ "MISSING",
        T_F > TempUpper ~ "High",
        T_F < TempLower ~ "Low"
      )
    ) %>%
    # unite is like a 'paste mutate' that combines columns and replaces them with 1 new column
    tidyr::unite("Flag", c(TChangeFlag, Flag), sep = "; ", remove = TRUE, na.rm = TRUE)
  # 3 output the data
  dir.create(OutPath, showWarnings = F)
  write.csv(OutPut,
            paste0(OutPath, gsub(".csv", "_QC.csv", i)),
            row.names = F, quote = F
  )
  rm(list=c("OutPut","i"))
}

### Issues with this code is that some things are hard coded and would be hard to reuse, so lets move on!


mean2<-  #Tell [R] that I want this new function to be named "mean2"
  function(x,na.rm=T){  #the function consists of 1 parameter named x (aka the data) The { begins the function source code / expressions. 
    mean(x,na.rm=na.rm) #in the mean function change the default for na.rm=T
  }#close function
mean2(c(1:9,NA))  #will remove the NA and be able to perform the calculation
#VS
mean(c(1:9,NA)) # will not do the mean becuase of the NA, returns NA

#####  Turn previous code into a function

#OutPath <- paste0(getwd(), "/hobo_outputs/") 
#TempUpper <- 40
#TempLower <- (-40)

HoboQAQC <- function(data, #The data of course
                     fNames = names(data), #extract the file names that are supplied to data as the default filenames
                     OutPath = paste0(getwd(), "/hobo_outputs/"), #set up an output path character string.
                     TempUpper = 40, #Make the temperature flag thresholds into one of  the function's arguments. 
                     TempLower = -40) {
  for (i in fNames) {
    # 1 Read in the data
    OutPut <- data[[i]] %>% # Note, this is now called 'data'
      # 2 Generate your output in this case, some QA/QC flagging
      dplyr::mutate(
        TChangeFlag = ifelse(
          c(abs(diff(T_F, lag = 1)) > 10, FALSE), 
          yes = "D10", 
          no = NA
        ), 
        Flag = dplyr::case_when(
          is.na(T_F) ~ "MISSING",
          T_F > TempUpper ~ "High",
          T_F < TempLower ~ "Low"
        )
      ) %>%
      tidyr::unite("Flag", c(TChangeFlag, Flag), sep = "; ", remove = TRUE, na.rm = TRUE)
    # 3 output the data
    dir.create(OutPath, showWarnings = F)
    write.csv(OutPut,
              paste0(OutPath, gsub(".csv", "_QC.csv", i)),
              row.names = F, quote = F
    )
    rm(list=c("OutPut", "i"))
  }
}
#uncomment to call function - will generate 5 files same as above
HoboQAQC(data=HoboData) 

##### Now we continue to evolve the function to a "pure" function - 
# Here we split out the part that Flags the data

HoboFlag <- function(data, #argument that accepts the name of the input data, assumed to be a dataframe. 
                     TempName="T_F", #Gives us some flexibility if we want to supply a different column name
                     TChange=10, #Allows to provide a value for the temperature change flagging threshold 
                     TempUpper=40, #provide a value for the upper temperature limit flag 
                     TempLower=-40) { #provide a value for the lower temperature limit flag
  
  #OutPut <- HoboData[[i]] %>%
  OutPut<-dplyr::mutate(data,  
                        TChangeFlag = ifelse(
                          c(abs(diff(get(TempName), lag = 1)) > TChange, FALSE),
                          yes = paste0("D", TChange),
                          no = NA
                        ),
                        Flag = dplyr::case_when(
                          is.na(get(TempName)) ~ "MISSING",
                          get(TempName) > TempUpper ~ "High",
                          get(TempName) < TempLower ~ "Low"
                        )
  ) %>%
    tidyr::unite("Flag", c(TChangeFlag, Flag), sep = "; ", remove = TRUE, na.rm = TRUE)
  return(OutPut) # because this is a function, we have to tell it what it is returning. In this case it is the output variable we defined above. 
  rm(list=c("OutPut","i")) #optional cleanup
}

## Here is the create CSV portion put into a new function - not a pure function!
#Writes the data out and creates directories. 

HoboWrite.csv <- function(i, #an index variable, needs to be the filename.
                          data, #this is the input data, it could be a list or a dataframe, but ultimately we will need to output a dataframe from this function.
                          OutPath=paste0(getwd(), "/hobo_outputs/"), #same as before
                          ...) { # ellipsis will let us pass in some optional arguments on the columns we want down below.
  dir.create(OutPath, showWarnings = F, recursive = T) # recursive=T will create all sub-directories as well
  
  # selects the data that may have been passed from an lapply pipe or a for loop.
  if (is.data.frame(data)) {
    df <- data[...] # data passed by a for loop  #[...] represents the columns
  } else {
    df <- data[[i]][...] #data passed by an lapply pipe 
  }
  # write out the data
  write.csv(df,
            paste0(OutPath, gsub(".csv", "_QC.csv", i)),  # rename file
            row.names = F, quote = F
  )
  rm(list=c("df","i")) #this removes the df and i from 
}


#Create a for loop to use both functions we created above

#OutPath <- paste0(getwd(), "/hobo_outputs/") #change this if you want it going elsewhere 

for (i in fNames) {
  HoboFlag(HoboData[[i]])%>%
    HoboWrite.csv(i, data= . , OutPath = OutPath) #I want the data to pass into that second position so I am using '.'
  rm(i) #optional housekeeping
}

### WE will now move away from for loops to Functionals!!!

#  "anonymous" function that calculates the coefficient of variation 
out<-apply(mtcars, MARGIN = 2, FUN=function(x) sd(x)/mean(x)) #margin=2 means use columns of mtcars
# is.vector(out)  #Is this a vector? Yes

lapply(mtcars, FUN=function(x) sd(x)/mean2(x))  #this creates a list instead of a vector!

##example
data_list<-list(A=1:10, B=11:20, C=c(21:29,NA))   # create some data in a list
lapply(data_list, mean2) # returns a list

sapply(data_list, mean2)  #will return a vector, instead of a list

## Using lapply to call our existing functions - this replaces the forloop example above
lapply(HoboData, FUN=HoboFlag)%>%
  lapply(names(.), FUN=HoboWrite.csv, data=., OutPath=OutPath)

# move data into global environment
d<-lapply(HoboData, FUN=HoboFlag)
#str(d) #uncomment to inspect

Sites<-names(d)[-3] #remove a problem site with no data
lapply(Sites, FUN=function(x) lm(T_F~idx, data=d[[x]]))%>% #anonymous function
  lapply(coef)%>% #extract slope and intercept
  bind_rows()%>% #glue all the list items together into 1
  dplyr::mutate(Sites=Sites,.before="(Intercept)")  #  .before adds new column before column "Intercept"



##############################################################  Start of JP section ######

library(tidyverse)
library(lubridate)
library(modelr)

#file names
fNames <- c(
  "APIS01_20548905_2021_temp.csv",
  "APIS02_20549198_2021_temp.csv",
  "APIS03_20557246_2021_temp.csv",
  "APIS04_20597702_2021_temp.csv",
  "APIS05_20597703_2021_temp.csv"
)

# Make path to files
fPaths <- paste0("https://raw.githubusercontent.com/KateMMiller/IMD_R_Training_Intro/master/Data/", fNames)

fPaths<- set_names(fPaths, c("APIS01", "APIS02", "APIS03", "APIS04", "APIS05"))
fPaths  # This thing is now a vector where each element has a name

## import the site data into a list where each element comes from a different file
intensity_data_raw <- fPaths %>% map( ~ read_csv(file = .x,  skip = 1))  #the tilda ~ says the next thing is the function

# check its class
class(intensity_data_raw) # its a list
# check it length
length(intensity_data_raw) # 5 elements

# check class of element in list
class(intensity_data_raw[[1]]) # each element is a data.frame (well, tibble actually)


### Use map again to give you list of rows in each DF
intensity_data_raw %>% map_dbl(~ nrow(.x))  #.x is the input   # map_dbl tells it to return it as a vector instead of list, map_chr would be a character vector, the package has lots of options

# this gives you all the colnames for each DF... they are formatted awfully!!!... so....
intensity_data_raw %>% map(~ colnames(.x)) 

#Fix the data!!!

library(lubridate)

#removes any data frames with 0 records
intensity_data_raw <- intensity_data_raw %>% discard(~ nrow(.x) == 0)  

#create function to clean up the column names
Fix_Data <- function(data) { #ingests a data frame
  data %>%   #starts_with - you can use this to ignore things that don't meet the criteria
    select(starts_with("Date") | starts_with("Temp") | starts_with("Intensity")) %>%
    rename("Date_Time" = starts_with("Date"), "Temp_F" = starts_with("Temp"), "Intensity" = starts_with("Intensity")) %>%
    mutate(Temp_C = 5 * (Temp_F - 32) / 9, Date_Time = mdy_hms(Date_Time), Date = date(Date_Time))
}

#create a new list of the cleaned data
intensity_data <- intensity_data_raw %>% map(~ Fix_Data(.x))

## Create a function to summarise the data by Day
Summarise_Data <- function(data) {
  data %>%
    group_by(Date) %>%
    summarise(across(where(is.numeric), mean)) #summarize where columns are numeric 
}

summary_data <- intensity_data %>% map(~ Summarise_Data(.x)) 


### Oops - some of the intensity data is 0, fix this for later use in gamma glm().
# create new function
Zero_Intensity_Fix <- function(data) {
  data %>%
    filter(across(any_of("Intensity"), ~ .x != 0)) # remove any zeroes
}

summary_data <- summary_data %>% map(~ Zero_Intensity_Fix(.x))  #rewrites the data 

## Let's make graphs!!!

Summary_Graph <- function(data) {  #data is the placeholder for the data you input
  graph_data <- data %>%
    pivot_longer(cols = where(is.numeric), names_to = "Measure", values_to = "Value") %>% #pivot to long format
    filter(Measure != "Temp_F")
  # create ggplot  # dont need pipe because graph_data is specified in ggplot below
  Graph <- ggplot(graph_data, aes(x = Date, y = Value, group = Measure, color = Measure)) +
    geom_point() +
    facet_grid(Measure ~ ., scales = "free_y")
  
  return(Graph)
}
#create list of graphical output
Graphs1 <- summary_data %>% map(~ Summary_Graph(.x))

#display
Graphs1[[5]]

## different version where you use pipes for everything
Summary_Graph <- function(data) {
  graph <- data %>%
    pivot_longer(cols = where(is.numeric), names_to = "Measure", values_to = "Value")
  %>%
    filter(Measure != "Temp_F") %>%
    ggplot(., aes(x = Date, y = Value, group = Measure, color = Measure)) +
    geom_point() +
    facet_grid(Measure ~ ., scales = "free_y")
  return(graph)
}

## Do a regression on the data

Intensity_Regression <- function(data, ...) {  #using the "..." allows you to capture whatever else a user specifies and passes it to the function, CAN ONLY USE THIS TRICK in one place
  glm(Intensity ~ Temp_C, data = data, ...)
}

Gauss_Reg <- summary_data %>%
  map_if(~ all(c("Intensity", "Temp_C") %in% colnames(.x)), #map_if: use only DFs that have these col names
         ~ Intensity_Regression(.x, family = gaussian), # family statement gets pulled in place of the "..."
         .else = ~ NA
  )

# View
Gauss_Reg [[2]]

Gamma_Reg <- summary_data %>%
  map_if(~ all(c("Intensity", "Temp_C") %in% colnames(.x)),
         ~ Intensity_Regression(.x, family = Gamma(link="log")), #change to gamma
         .else = ~NA
  )

## Working with more than one list at a time

library(modelr)

summary_data <- map2(.x = summary_data, .y = Gauss_Reg, .f = ~ {
  if ("Intensity" %in% colnames(.x)) {
    add_predictions(data = .x, model = .y, var = "Pred") #adds prediction column
  } else {
    .x
  }
})

#view
summary_data [[2]]

## combine lists together - turns it into a data frame
Pred_Graph_Data <- bind_rows(summary_data, .id = "Site") %>% filter(!is.na(Pred))

ggplot(Pred_Graph_Data, aes(x = Date, y = Intensity)) +
  geom_point() +
  geom_line(aes(x = Date, y = Pred, color = "red")) +
  facet_grid(Site ~ ., scales = "free_y")







###############################################

# Performance section - important when running multiple analyses that each run 30 minutes! or very large data files like rasters

library(tidyverse)
library(lubridate)
library(microbenchmark)
library(profvis)

## Vectorizatoin
#lots of things in R are vectorized like numbers, letters, etc
# date example
x <- mdy("02/07/2022", "02/08/2022", "02/09/2022", "02/10/2022")
x+1  #adds a day to a date

# Shows what is running and how long it took so you can try replace code with something more efficient
profvis::profvis(Fix_Data(intensity_data_raw[[2]]), interval = 0.005)


library(microbenchmark)

# Original function
Fix_Data_Lubridate <- function(data) {
  data %>%
    select(starts_with("Date") | starts_with("Temp") | starts_with("Intensity")) %>%
    rename("Date_Time" = starts_with("Date"), "Temp_F" = starts_with("Temp"), "Intensity" = starts_with("Intensity")) %>%
    mutate(Temp_C = 5 * (Temp_F - 32) / 9, Date_Time = mdy_hms(Date_Time), Date = date(Date_Time))
}


# New version using as.POSIXct()  
Fix_Data_POSIXct <- function(data) {
  data %>%
    select(starts_with("Date") | starts_with("Temp") | starts_with("Intensity")) %>%
    rename("Date_Time" = starts_with("Date"), "Temp_F" = starts_with("Temp"), "Intensity" = starts_with("Intensity")) %>%
    mutate(
      Temp_C = 5 * (Temp_F - 32) / 9, Date_Time = as.POSIXct(Date_Time, "%m/%d/%y %H:%M:%S", tz = "UCT"),
      Date = date(Date_Time)
    )
}

# new version using strptime
Fix_Data_strptime <- function(data) {
  data %>%
    select(starts_with("Date") | starts_with("Temp") | starts_with("Intensity")) %>%
    rename("Date_Time" = starts_with("Date"), "Temp_F" = starts_with("Temp"), "Intensity" = starts_with("Intensity")) %>%
    mutate(
      Temp_C = 5 * (Temp_F - 32) / 9, Date_Time = strptime(Date_Time, "%m/%d/%y %H:%M:%S", tz = "UCT"),
      Date = date(Date_Time)
    )
}

# Do Comparison
mb<-microbenchmark(
  Lubridate = Fix_Data_Lubridate(intensity_data_raw[[2]]),
  POSIXct =  Fix_Data_POSIXct(intensity_data_raw[[2]]),
  Strptime = Fix_Data_strptime(intensity_data_raw[[2]]),
  times = 100L, unit = "ms"
)

mb # shows which ran faster - lubridate was fastest







