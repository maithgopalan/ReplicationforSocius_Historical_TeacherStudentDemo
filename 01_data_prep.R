#--------------------------------------------# 
# Title: Data Prep Code for Socius paper 
# Date: 01-24-2025
# Author: Jilli Jung; Maithreyi Gopalan made minor changes to ensure all libraries are installed
#--------------------------------------------# 

#--------------------------------------------# 
#### 0. Prepare packages ####
#--------------------------------------------# 
#install.packages("pacman")
library(pacman)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)
library(extrafont)


p_load(
  here, 
  haven,
  arrow,
  writexl, expss, openxlsx,

  tidyverse, 
  gridExtra, cowplot, 
  ggpubr, grid,
  
  gt, gtsummary,

  sf, 
  tigris, 
  rmapshaper 
)


#--------------------------------------------# 
#--------------------------------------------# 
#### 1. Import data ####
#--------------------------------------------# 
#--------------------------------------------# 

acs_original <- read_dta("data/1980-2021/Data_to_be_shared/usa_00030.dta") 
acs_original <- acs_original |> select(year, perwt, region, statefip, puma, conspuma, cpuma0010, sex, age, race, hispan, hispand, labforce, occ2010)
write_parquet(acs_original, here("data/1980-2021/Data_to_be_shared", "usa_00030.parquet"))

acs_original <- read_parquet(here("data/1980-2021/Data_to_be_shared", "usa_00030.parquet"))
 
acs80 <- filter(acs_original, year == 1980)
write_parquet(acs80, here("data/1980-2021/Data_to_be_shared", "acs80.parquet")); rm(acs80)
acs90 <- filter(acs_original, year == 1990)
write_parquet(acs90, here("data/1980-2021/Data_to_be_shared", "acs90.parquet")); rm(acs90)
acs00 <- filter(acs_original, year == 2000)
write_parquet(acs00, here("data/1980-2021/Data_to_be_shared", "acs00.parquet")); rm(acs00)
acs10 <- filter(acs_original, year == 2010)
write_parquet(acs10, here("data/1980-2021/Data_to_be_shared", "acs10.parquet")); rm(acs10)
acs21 <- filter(acs_original, year == 2021)
write_parquet(acs21, here("data/1980-2021/Data_to_be_shared", "acs21.parquet")); rm(acs21)


acs80 <- read_parquet(here("data/1980-2021/Data_to_be_shared", "acs80.parquet"))
acs90 <- read_parquet(here("data/1980-2021/Data_to_be_shared", "acs90.parquet"))
acs00 <- read_parquet(here("data/1980-2021/Data_to_be_shared", "acs00.parquet"))
acs10 <- read_parquet(here("data/1980-2021/Data_to_be_shared", "acs10.parquet"))
acs21 <- read_parquet(here("data/1980-2021/Data_to_be_shared", "acs21.parquet"))


#--------------------------------------------# 
#--------------------------------------------# 
#### 2. Clean  ####
#--------------------------------------------# 
#--------------------------------------------# 

# save dataframe names to be used in loop
acs.set <- c("acs80", "acs90", "acs00", "acs10", "acs21")

#--------------------------------------------# 
##### 1. Clean individual-level variables ####
#--------------------------------------------# 

for (data in acs.set) {
  df <- get(data)
  df <- df |> 
    mutate(
      # race/ethnicity
      hispanic = if_else(hispan == 0, 0, 1),
      white = if_else(race == 1 & hispanic == 0, 1, 0),
      black = if_else(race == 2 & hispanic == 0, 1, 0),
      asian = if_else(race %in% c(4, 5, 6) & hispanic == 0, 1, 0),
      allothers = if_else(race %in% c(3, 7, 8, 9) & hispanic == 0, 1, 0),
      
      native = if_else(race == 3 & hispanic == 0, 1, 0),
      other = if_else(race == 7 & hispanic == 0, 1, 0),
      multi = if_else(race %in% c(8, 9) & hispanic == 0, 1, 0),
      nonwhite = if_else(white == 0, 1, 0),
      allrace = if_else(nonwhite %in% c(0,1), 1, 0),
      
      # school-age vs. working-age
      school.age = if_else(age %in% c(5:18), 1, 0),
      work.age = if_else(age %in% c(15:64), 1, 0),
      
      # teacher: should consider whether labor force participation and employment status are used for exclusion. 
      teacher = if_else(occ2010 %in% c(2300, 2310, 2320, 2330) & age %in% c(25:64) & labforce == 2, 1, 0),
      
      # sex
      female = if_else(sex == 2, 1, 0),
      allsex = if_else(female %in% c(0,1), 1, 0),
      
      puma = statefip*1000000 + puma, #Unlike CPUMA, puma is state-dependent
      geo = if_else(year == 2021, cpuma0010, conspuma) # use old consistent puma as a main. and for 2021 which doesnt have old consistent puma, use the new one. 
    )
  assign(data, df)
}
rm(df)

#--------------------------------------------# 
##### 2. Create geo level data ####
#--------------------------------------------# 

aggfunc <- function(data, unit, yearnum){

  df <- data |> 
    group_by({{unit}}) |> 
    summarise(across(c(white, black, hispanic, asian, allothers, other, multi, nonwhite, allrace, female, allsex), # repeat following calculation for all variables listed here. 
                     list(
                       # calculate population of each race in school-age population
                       s.sum = ~ sum(.x[school.age==1] * perwt[school.age==1], na.rm = T), 
                       
                       # calculate population of each race in teacher population
                       t.sum = ~ sum(.x[teacher==1] * perwt[teacher==1], na.rm = T)), 

                     .names = "{col}.{fn}")) # define column names
  
  ## calculate ratio of each race in state-by-puma cell
  df <- df |>
    mutate(across(c(white.s.sum, black.s.sum, hispanic.s.sum, nonwhite.s.sum, asian.s.sum, allothers.s.sum), # across all race categories in school-age population
                  ~ .x / allrace.s.sum, .names = "{col}.ratio"),
           across(c(white.t.sum, black.t.sum, hispanic.t.sum, nonwhite.t.sum, asian.t.sum, allothers.t.sum), # across all race categories in teacher population
                  ~ .x / allrace.t.sum, .names = "{col}.ratio"),
           female.s.ratio = female.s.sum / allsex.s.sum,
           female.t.ratio = female.t.sum / allsex.t.sum,
           year = yearnum)
  
  names(df) <- gsub(x = names(df), pattern = "sum.ratio", replacement = "ratio") # simplify column names
  print(df)
  
}

acs80_geo <- aggfunc(acs80, geo, 1980)
acs90_geo <- aggfunc(acs90, geo, 1990)
acs00_geo <- aggfunc(acs00, geo, 2000)
acs10_geo <- aggfunc(acs10, geo, 2010)
acs21_geo <- aggfunc(acs21, geo, 2021)

acs_geo <- rbind(acs80_geo, acs90_geo, acs00_geo, acs10_geo, acs21_geo)


# for bar plot
acs80_all <- aggfunc(acs80, NULL, 1980)
acs90_all <- aggfunc(acs90, NULL, 1990)
acs00_all <- aggfunc(acs00, NULL, 2000)
acs10_all <- aggfunc(acs10, NULL, 2010)
acs21_all <- aggfunc(acs21, NULL, 2021)

acs_all<- rbind(acs80_all, acs90_all, acs00_all, acs10_all, acs21_all)

saveRDS(acs_geo, "data/1980-2021/Data_to_be_shared/acs_geo.rds")
saveRDS(acs_all, "data/1980-2021/Data_to_be_shared/acs_all.rds")
