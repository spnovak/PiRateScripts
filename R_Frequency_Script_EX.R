#############################################################################
#3D Frequency Plot 
#by Scott Novak, PhD
#of Kingfish Statistics + Data Analytics, Inc. 
# GITHUB: spnovak 
# Stackoverflow #scott@kingfish-stat, American1!
# Example data sets: https://vincentarelbundock.github.io/Rdatasets/datasets.html 
# Source on Intro Table 1: https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html 
#############################################################################


## Helpful hints commands and keystrokes 
# "Run" a specific command:  CTRL + ENTER 
# Refer to package in code to make sure it is referenced: dply::mutate() or nycflights23::flights 
# Got questions? Try STACKOVERFLOW and produce a reprex, aka a minimally reproducable example to help others answer your questions
# More online help: http://github.com/trending/r 
# https://github.com/qinwf/awesome-R#bioinformatics-and-biostatistics

# Online book: http://r4ds.had.co.nz.  
# How to package data for easy import: dput(DATASETNAME)
# Online datasets---http://kaggle.com/datasets
# More online federal datasets-- http:catalog.data.gov/dataset 

## installing data packages 
install.packages(c("nycflights13", "gapminder", "Lahman"))


# Find working directory on Windows machine  
getWD() 

# Change working directory
cd /Users/SN/Desktop/ 
  
# See what files in a directory 
dir()

# how to download a file, not point and click, for reproducability 
> download.file("http://archive.ics.uci.edu/ml/machine-le\arning-databases/00275/Bike-Sharing-Dataset.zip",
                  + "ProjectData/Bike-Sharing-Dataset.zip")

# For each final run, include the following, and save to final syntax file.   
sessionInfo() 

# Saving randomly generated numbers for reproducability 
set.seed(firstnum)   eg set.seed(10)
rnorm(secondnum)    eg norm(5)

# Import R file in current working directory
read.csv("mydata.csv")  ##included both CSV in two places  

# R Markdown example 
1. Open FILE--> RMarkdown tab 
  
#example 1: Import Excel data file 
library(readxl)
covid_testing <- read_excel("C:/Users/spnov/Downloads/covid_testing.xlsx")
View(covid_testing)


#example 2: Import R data file (RDS)
workingfilename <- import("C:/Users/spnov/Downloads/covid_testing.rds")
data(mtcars)



#example 3: Import Multiple Packages for Analysis 
packages <- c("rio", "here", "skimr", "tidyverse", "gtsummary", "rstatix", "janitor", "scales", "flextable", "rayshader", "ggplot2")
lapply(packages, library, character.only = TRUE)
  

#example 4: Create a 3D plot of a 3-way interaction  
## 3 quant variables from dataset: age, ct_result, col_rec_tat  

## ggplot (2D visualization)
g1 <- mtcars %>%  
    ggplot(aes(disp,mpg,color=cyl)) + 
    geom_point(size=2) + 
    scale_color_continuous(limits=c(0,8)) + 
    ggtitle("mtcars: Dis vs mpg vs num cylinders") + 
    theme(title = element_text(size=8),
            text = element_text(size=12))
          
  
# rayshader  
g1 %>% 
    plot_gg(
          height  =3, 
          width =3.5, 
          multicore =TRUE, 
          pointcontract =0.7, 
          soliddepth =  -200
    )

render_camera(zoom = 0.5, theta = -30, phi = 30)
render_snapshot(clear=FALSE) 
  
### 

library(ggplot2)
library(viridis)
library(rayshader)
library(tidyverse)
library(magick)

mtplot <- ggplot(mtcars) + 
  geom_point(aes(x=mpg,y=disp,color=cyl)) + 
  scale_color_continuous(limits=c(0,8)) 
mtplot

plot1 <- plot_gg(mtplot, width=3.5, sunangle=225, preview = TRUE)


plot2 <- plot_gg(mtplot, width=3.5, multicore = TRUE, windowsize = c(1400,866), sunangle=225,
                 zoom = 0.60, phi = 30, theta = 45)
render_snapshot(clear = TRUE)


# 3D EVEVATION MATRIX 
vocano 

volcano %>% class() 

 insta 
  
  
dput(mtcars)  
        
    )

standard frequency table 

#load package
library("ipw")

####### TEST 
install.packages("pxR")
install.packages("rgeos")
install.packages("RColorBrewer")


library(pxR)
library(RColorBrewer)
library(rgeos)
#install.packages("rgdal", repos = "http://cran.us.r-project.org") 
library(rgdal)
library(rayshader)
library(knitr)
library(magrittr)
library(tidyverse)

as.numeric.factor <- function(x) {    # Custom function to convert fctr to num factor value
  return(suppressWarnings(as.numeric(levels(x))[x]))
}

if(!dir.exists("data")) dir.create("data")  # Create the download directory

utils::download.file(url = "http://www.ine.es/pcaxisdl/t20/e245/p05/a2018/l0/00000006.px",
                     destfile = "data/census_2018.px")


tbl_census_2018 <- read.px("data/census_2018.px") %>%              # Load & format
  as_tibble()

tbl_census_2018 %<>% 
  set_names(c("age", "city", "sex", "population")) %>%             # Cambiamos los nombre
  na.omit() %>%                                                    # Na rmv
  filter((city!="Total")&(age!="Total")&(sex=="Ambos sexos")) %>%  # Duplicate info rmv
  separate(city, c('postal_code', 'city_name'), sep="-") %>%       # Sep City column
  mutate(age = as.numeric.factor(age)) %>%                         # Conv to numeric
  group_by(city_name, postal_code) %>%                             # Group to operate
  summarise(avg_age = sum(population*age,na.rm = T)/sum(population,na.rm=T)) %>%  # Avg age
  select(city_name, postal_code, avg_age)                          # Discard columns

temp <- tempfile()                              # Create the tempfile
u="http://www.arcgis.com/sharing/rest/content/items/8e31c4c1a0b348f79058f212d0d807a1/data"
utils::download.file(url = u, destfile = temp,
                     mode="wb")                 # Binary mode for correct download

unzip(temp, exdir = "data/cities_gis")          # Unzip in data/cities_gis
unlink(temp)                                    # Delete temp file 

tlb_cities_gis <- readOGR(dsn = "./data/cities_gis/Municipios_ETRS89_30N.shp",
                          verbose=FALSE) # Spatial data reading
tlb_cities_gis %<>% 
  fortify(region = "Codigo") # %>%             # Conv "spatial object" to data.frame
# broom::tidy()

plot_canarias <- F                              # Control param, initial app config

if(plot_canarias==F){                           # Should be moduled in a funct
  tlb_cities_gis %<>%
    filter((long>0) & (lat>4000000))              # Filter peninsular data
} 

tbl_cities_avg_age <- tlb_cities_gis %>% 
  left_join(tbl_census_2018, by = c("id" = "postal_code")) 

tbl_cities_avg_age %>%
  group_by(id) %>%
  summarise(na = sum(is.na(avg_age))) %>%                 # NAs by city
  summarise(missing_perc = sum(na>0)/length(na)*100) %>%  # Perc cities with at least 1 na 
  select(missing_perc)


tbl_cities_avg_age %<>%       
  arrange(id) %>% 
  fill(avg_age, .direction = "down")            # Fill with the previous pc data.

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))      # Create reverse Spectral palette

plot_cities <- ggplot() +
  geom_polygon(data = tbl_cities_avg_age, aes(fill = avg_age,   
                                              x = long, 
                                              y = lat, 
                                              group = id)) +      # Dummy variable to correct fill by PCode.
  scale_fill_gradientn(colours=myPalette(4)) +                 # Choose palette colours.
  labs(fill="Avg age")
plot(plot_cities)

plot_gg(plot_cities,multicore=TRUE,width=5,height=3,scale=310)    # Plot_gg de rayshader
render_snapshot(filename = "3D_spain")

render_movie("img/movie_spain.mp4",frames = 720, fps=30,zoom=0.6,fov = 30)
##############################################################################################

install.packages("pheatmap")
install.packages("lme4")
install.packages("nlme")

########################################
summarize data with tbl_summary  

library(gtsummary)
library(tidyverse)
# below we create a smaller subset of the data
table1 <-
  trial %>%
  tbl_summary(include = c(age, grade, response))
view(table1)

  
table1 <- 
  sm_td6trf cd   rial %>% 
  tbl_summary(by = trt)

# default stats are median(IQR) for continuous, n(%) for categorical/dichotomous
# by TRT stratifies by different treatments 

# Generate Fake Data 
library(wakefield)
library(dplyr)
library(gtsummary)

df <- data.frame(Name    =wakefield::name(100)
                . Education =wakefield::education(100)
                . HairColor =wakefield::hair(100)
                . Sex          =wakefieldsex(100))
view(df)
#####################################################
# summarize the data with our package
library(gtsummary)
library(tidyverse)
table1 <- 
  trial %>%
  tbl_summary(include = c(age, grade, response))
table1

mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)

t1 <- tbl_regression(mod1, exponentiate = TRUE) 
t1



#############################################################
Save file as HTML, PDF, et ceter. 
library(gt)

t1 %>%
  as_gt() %>%
  gt::gtsave(filename = "test1fin.docx") # use extensions .png, .html, .docx, .rtf, .tex, .ltx 

############################################################
stratifygroup by.data.frame()

library(dplyr) 
head(mtcars)

mpg_cyl = mtcars %>% group_by(cyl,var2,var3, etc) %>% 
  summarize(meanmpg = mean(mpg)) 
mpg_cyl   


###########################################################
Display first few rows on a data set or last (head/tails)
head(DATAFILE)  ##displays first 4-6 rows.  
head(DATAFILE, N=2)  ##to display first two rows 
