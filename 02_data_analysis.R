#--------------------------------------------# 
# Title: Code for Socius paper 
# Date: 01-24-2025
# Author: Jilli Jung
# Replicated by: Maithreyi Gopalan - who made minor changes to ensure all libraries are installed
#--------------------------------------------# 

#--------------------------------------------# 
#--------------------------------------------# 
#### Replication Starts here
#--------------------------------------------# 
#--------------------------------------------# 

#--------------------------------------------# 
#### 1. Prepare packages ####
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
#### 1 and 2. Load the cleaned and summarized data ####
#--------------------------------------------# 

acs_all <- readRDS("<path>/data/1980-2021/Data_to_be_shared/acs_all.rds")
acs_geo <- readRDS("<path>/data/1980-2021/Data_to_be_shared/acs_geo.rds")

#--------------------------------------------# 
#--------------------------------------------# 
#### 3. Bubble plots ####
#--------------------------------------------# 
#--------------------------------------------# 

## Set ggplot theme
theme_set(
  theme_classic(base_family = "Times New Roman", base_size = 16) + 
    theme(plot.title = element_text(hjust = 0.5))  
)



#--------------------------------------------# 
##### 1. Define a plot function  ####
#--------------------------------------------# 

bubble_single <- function(data, yearnum, xvar, yvar, racegroup, colorchoice) {

  ggplot(filter(data, year %in% c(yearnum)), 
         aes(x = {{xvar}}, y = {{yvar}}, size = allrace.s.sum)) + # add `size` argument for bubble plot. 
    geom_point(color = colorchoice, alpha = 1, fill = NA, shape = 21, stroke = 1) + 
    scale_size(range = c(1, 10), # bubble size range
               name= "Population", # Legend title
               breaks = c(250000, 500000, 750000), labels = c("25K", "50K", "75K")) + # Legend categories and labels
    
    geom_abline(intercept = 0, slope = 1, size = 0.5) + # add 45 degree line
    geom_smooth(method = "loess", se = FALSE, size = 0.5, color = "red", linetype = "dashed") + # Local Polynomial Regression Fitting
    
    labs(title = "",
         subtitle = "",
         x = paste0("Proportion of ", racegroup, " Students"),
         y = paste0("Proportion of ", racegroup, " Teachers"),
         caption = "") +
    scale_x_continuous(limits = c(0, 1.02), expand = c(0, 0)) + # remove space before 0
    scale_y_continuous(limits = c(0, 1.02), expand = c(0, 0)) + # remove space before 0
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),  # set plot margins (t, r, b, l)
          axis.title.y = element_text(face = "bold"),
          axis.text.x = element_text(size = 16, face = "bold"), # increase x-axis label font size
          axis.text.y = element_text(size = 16, face = "bold")) + # increase y-axis label font size
    coord_fixed() # fixed ratio of x-axis and y-axis
  
}


#--------------------------------------------# 
##### 2. Define a group plot function  ####
#--------------------------------------------# 

create_plots <- function(data, race, s_ratio, t_ratio, color, labels) {
  years <- c(1980, 1990, 2000, 2010, 2021)
  
  # Generate individual bubble plots
  plots <- map(years, ~bubble_single(data, .x, !!sym(s_ratio), !!sym(t_ratio), race, color))
  names(plots) <- paste0("p_", tolower(gsub("\\s+", "", race)), "_", years)
  
  # Combine individual plots
  combined <- plot_grid(
    plotlist = map(years, ~plots[[paste0("p_", tolower(race), "_", .x)]] + 
                     theme(legend.position = "none") + xlab("") + 
                     if(.x != 1980) ylab("") else ylab(plots[[1]]$labels$y)),
    labels = labels,
    label_size = 12,
    ncol = 5, align = "hv", vjust = 3, hjust = -3.5
  )
  
  # Add x-axis label
  x_label <- textGrob(paste("Proportion of", race, "Students"),
                      gp = gpar(fontfamily = "Times New Roman", fontsize = 16, fontface = "bold"),
                      vjust = -4)
  
  # Return the combined plot with x-axis label
  grid.arrange(arrangeGrob(combined, bottom = x_label))
  
}



#--------------------------------------------# 
##### 3. Create plots for each race/ethnicity   ####
#--------------------------------------------# 


p_black <- create_plots(acs_geo, "Black", "black.s.ratio", "black.t.ratio", "#39568CFF", c("1980", "1990", "2000", "2010", "2021"))
p_hisp <- create_plots(acs_geo, "Hispanic", "hispanic.s.ratio", "hispanic.t.ratio", "orange", NA)
p_white <- create_plots(acs_geo, "White", "white.s.ratio", "white.t.ratio", "pink", NA)
p_asian <- create_plots(acs_geo, "Asian", "asian.s.ratio", "asian.t.ratio", "lightgreen", NA)
p_allothers <- create_plots(acs_geo, "Other", "allothers.s.ratio", "allothers.t.ratio", "purple", NA)


#--------------------------------------------# 
##### 4. Black + hispanic ####
#--------------------------------------------# 
acs_long <- acs_geo |> 
  select(geo, year,  allrace.s.sum, ends_with("ratio")) |> 
  pivot_longer(
    cols = -c(geo, year, allrace.s.sum),
    names_to = c("race", ".value"),
    names_pattern = "(.*)\\.(s\\.ratio|t\\.ratio)") |> 
  filter(race %in% c("black", "hispanic")) |> 
  mutate(race = factor(race, levels = c("black", "hispanic")))

bubble <- function(data, yearnum, xvar, yvar) {
  
  ggplot(filter(data, year %in% c(yearnum)), 
         aes(x = {{xvar}}, y = {{yvar}}, size = allrace.s.sum, color = race, alpha = race)) + # add `size` argument for bubble plot. 
    geom_point(alpha = 1, fill = NA, shape = 21, stroke = 1) + 
    scale_color_manual(values = c("#39568CFF", "orange"), 
                       name = "Race", 
                       labels = c("Hispanic", "Black")) + # define bubble color and transparency
    scale_alpha_manual(values = c(0.5, 0.8),
                       name = "Race",
                       labels = c("Hispanic", "Black")) +
    scale_size(range = c(1, 10), # bubble size range
               name= "Population", # Legend title
               breaks = c(250000, 500000, 750000), labels = c("25K", "50K", "75K")) + # Legend categories and labels
    geom_abline(intercept = 0, slope = 1, size = 0.5) + # add 45 degree line
    geom_smooth(data = filter(data, year %in% c(yearnum) & race == "hispanic"), method = "loess", se = FALSE, size = 1, color = "darkorange", linetype = "dashed") + # Local Polynomial Regression Fitting
    geom_smooth(data = filter(data, year %in% c(yearnum) & race == "black"), method = "loess", se = FALSE, size = 1, color = "#39568CFF", linetype = "dashed") +
   
    labs(title = "",
         subtitle = "",
         x = "Proportion of Students",
         y = "Proportion of Teachers",
         caption = "") +
    
    scale_x_continuous(limits = c(0, 1.02), expand = c(0, 0)) + # remove space before 0
    scale_y_continuous(limits = c(0, 1.02), expand = c(0, 0)) + # remove space before 0
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm')) + # set plot margins (t, r, b, l)
    guides(size = "none") + # remove size legend
    coord_fixed() # fixed ratio of x-axis and y-axis
  
}


p_80 <- bubble(acs_long, 1980, s.ratio, t.ratio)
p_90 <- bubble(acs_long, 1990, s.ratio, t.ratio)
p_00 <- bubble(acs_long, 2000, s.ratio, t.ratio)
p_10 <- bubble(acs_long, 2010, s.ratio, t.ratio)
p_21 <- bubble(acs_long, 2021, s.ratio, t.ratio)


combined_plot <- plot_grid(p_80 + theme(legend.position = "none") + xlab("") ,
                           p_90 + theme(legend.position = "none") + xlab("") + ylab(""),
                           p_00 + theme(legend.position = "bottom") + xlab("") + ylab(""),
                           p_10 + theme(legend.position = "none") + xlab("") + ylab(""),
                           p_21 + theme(legend.position = "none") + xlab("") + ylab(""),
                           labels = c("(A) 1980", "(B) 1990", "(C) 2000", "(D) 2010", "(E) 2021"),
                           ncol = 5, align = "hv", vjust = 3)
# Create shared x-axis label
x.grob.student <- textGrob("Proportion of Students", 
                             gp=gpar(fontfamily = "Times New Roman"),
                             vjust = -3)  # Adjust this value to move the label closer to or further from the x-axis

# Add x-axis
combined_plot <- grid.arrange(arrangeGrob(combined_plot,  bottom = x.grob.student))

#--------------------------------------------# 
##### 9. cross graph ####
#--------------------------------------------# 

acs_cross <- acs_geo |> 
  select(geo, year,  allrace.s.sum, ends_with("ratio")) |> 
  pivot_longer(
    cols = -c(geo, year, allrace.s.sum, ends_with("s.ratio")),
    names_to = c("race", ".value"),
    names_pattern = "(.*)\\.(t\\.ratio)") |> 
  filter(race %in% c("black", "hispanic", "asian", "allothers", "white")) |> 
  mutate(race = factor(race, levels = c("black", "hispanic", "asian", "allothers", "white")))

cross <- function(yearnum, xvar, racegroup) {
  
  ggplot(filter(acs_cross, year %in% c(yearnum)), 
         aes(x = {{xvar}}, y = t.ratio, size = allrace.s.sum, color = race, alpha = race)) + # add `size` argument for bubble plot. 
    # geom_point() + 
    scale_color_manual(values = c("#39568CFF", "orange", "lightgreen","purple", "pink"),
                       name = "Race of Teachers",
                       labels = c("Black", "Hispanic", "Asian", "Other", "White")) +
    scale_alpha_manual(values = c(0.2, 0.2, 0.2, 0.2, 0.2),
                       name = "Race of Teachers",
                       labels = c("Black", "Hispanic", "Asian", "Other", "White")) +
    # scale_size(range = c(1, 10), # bubble size range
    #            breaks = c(250000, 500000, 750000),
    #            labels = NULL) + # Legend categories and labels
    
    # scale_size(range = c(1, 10), # bubble size range
    #            name= "Population", # Legend title
    #            breaks = c(250000, 500000, 750000), labels = c("25K", "50K", "75K")) + # Legend categories and labels
    geom_abline(intercept = 0, slope = 1, size = 0.5) + # add 45 degree line
    geom_smooth(data = filter(acs_cross, year %in% c(yearnum) & race == "hispanic"), method = "loess", se = FALSE, size = 1, color = "darkorange", linetype = "solid") + # Local Polynomial Regression Fitting
    geom_smooth(data = filter(acs_cross, year %in% c(yearnum) & race == "black"), method = "loess", se = FALSE, size = 1, color = "#39568CFF", linetype = "dashed") +
    geom_smooth(data = filter(acs_cross, year %in% c(yearnum) & race == "asian"), method = "loess", se = FALSE, size = 1, color = "lightgreen", linetype = "dotted") +
    geom_smooth(data = filter(acs_cross, year %in% c(yearnum) & race == "allothers"), method = "loess", se = FALSE, size = 1, color = "purple", linetype = "dotdash") +
    geom_smooth(data = filter(acs_cross, year %in% c(yearnum) & race == "white"), method = "loess", se = FALSE, size = 1, color = "pink", linetype = "twodash") +
    
    labs(title = "",
         subtitle = "",
         x = paste0("Proportion of ", racegroup, " Students"),
         y = "Proportion of Teachers",
         caption = "") +
    
    scale_x_continuous(limits = c(0, 1.02), expand = c(0, 0)) + # remove space before 0
    scale_y_continuous(limits = c(0, 1.02), expand = c(0, 0)) + # remove space before 0
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm')) + # set plot margins (t, r, b, l)
    guides(size = "none") + # remove size legend
    coord_fixed() # fixed ratio of x-axis and y-axis
    
}

create_race_plots <- function(race, s_ratio, labels) {
  years <- c(1980, 1990, 2000, 2010, 2021)
  plots <- map(years, ~cross(.x, !!sym(s_ratio), race))
  
  # Extract legend from the third plot
  legend <- get_legend(plots[[3]] + theme(legend.position = "bottom"))
  
  # Remove legend from all plots
  plots_no_legend <- map(plots, ~. + theme(legend.position = "none"))
  
  combined <- plot_grid(
    plots_no_legend[[1]] + xlab(""),
    plots_no_legend[[2]] + xlab("") + ylab(""),
    plots_no_legend[[3]] + xlab("") + ylab(""),
    plots_no_legend[[4]] + xlab("") + ylab(""),
    plots_no_legend[[5]] + xlab("") + ylab(""),
    labels = labels,
    label_size = 12,
    ncol = 5, align = "hv", vjust = 3
  )
  
  # Add legend to the bottom
  combined_with_legend <- plot_grid(combined, legend, ncol = 1, rel_heights = c(1, 0.1))
  
  x_label <- textGrob(paste("Proportion of", race, "Students"), 
                      gp = gpar(fontfamily = "Times New Roman"), vjust = -5)
  
  grid.arrange(arrangeGrob(combined_with_legend, bottom = x_label))
}


black_plots <- create_race_plots("Black", "black.s.ratio", 
                                 paste("(", 1:5, ") Black,", c(1980, 1990, 2000, 2010, 2021)))
hispanic_plots <- create_race_plots("Hispanic", "hispanic.s.ratio", 
                                    paste("(", 6:10, ") Hispanic,", c(1980, 1990, 2000, 2010, 2021)))
asian_plots <- create_race_plots("Asian", "asian.s.ratio", 
                                 paste("(", 11:15, ") Asian,", c(1980, 1990, 2000, 2010, 2021)))
other_plots <- create_race_plots("Other", "allothers.s.ratio", 
                                 paste("(", 16:20, ") Other,", c(1980, 1990, 2000, 2010, 2021)))
white_plots <- create_race_plots("White", "white.s.ratio", 
                                 paste("(", 21:25, ") White,", c(1980, 1990, 2000, 2010, 2021)))


#--------------------------------------------# 
#--------------------------------------------# 
#### 4. Histograms ####
#--------------------------------------------# 
#--------------------------------------------# 

#--------------------------------------------# 
##### 1. Define histogram function ####
#--------------------------------------------# 

histogram <- function(data, yearnum, xvar, racegroup, ymax, colorchoice) {
  
  ggplot() + 
    geom_histogram(data = filter(data, year %in% c(yearnum)),
                   aes(x = {{xvar}}, y=after_stat(count) / sum(after_stat(count))), 
                   bins = 50,
                   color=colorchoice, fill=colorchoice) +
    scale_x_continuous(limits = c(-0.02, 1.01), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, ymax), expand = c(0, 0)) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm')) + # set plot margins (t, r, b, l)

    labs(title = "",
         subtitle = "",
         x = paste0("Proportion of ", racegroup, " Students"),
         y = "Probability",
         caption = "") + 
    # coord_fixed() # fixed ratio of x-axis and y-axis
    theme(aspect.ratio = 1) # fixed ratio of x-axis and y-axis
  
}

#--------------------------------------------# 
##### 2. Define a group plot function ####
#--------------------------------------------# 

create_race_histograms <- function(data, race, ratio, color, ymax, is_teacher = FALSE, years = c(1980, 1990, 2000, 2010, 2021)) {
  
  # Create histograms
  histograms <- map(years, ~histogram(data, .x, !!sym(ratio), race, ymax, color))
  names(histograms) <- paste0("h_", tolower(race), "_", substr(years, 3, 4))
  
  # Combine histograms
  combined <- plot_grid(plotlist = map(histograms, ~. + theme(legend.position = "none") + xlab("") + ylab("")),
                        labels = paste0("(", seq_along(years), ") ", years),
                        label_size = 12, ncol = 5, align = "hv", vjust = 3)
  
  x_label <- textGrob(paste("Proportion of", race, if(is_teacher) "Teachers" else "Students"), 
                      gp = gpar(fontfamily = "Times New Roman"), vjust = -3)
  
  ph <- grid.arrange(arrangeGrob(combined, bottom = x_label))
  
  return(list(histograms = histograms, combined_plot = ph))
}

# For students
create_student_histograms <- function(data, race, ratio, color, ymax) {
  create_race_histograms(data, race, ratio, color, ymax, is_teacher = FALSE)
}

# For teachers
create_teacher_histograms <- function(data, race, ratio, color, ymax) {
  create_race_histograms(data, race, ratio, color, ymax, is_teacher = TRUE)
}

#--------------------------------------------# 
##### 3. create histograms ####
#--------------------------------------------# 

# Usage for students:
black_student_plots <- create_student_histograms(acs_geo, "Black", "black.s.ratio", "#39568CFF", 0.31)
hispanic_student_plots <- create_student_histograms(acs_geo, "Hispanic", "hispanic.s.ratio", "orange", 0.41)
asian_student_plots <- create_student_histograms(acs_geo, "Asian", "asian.s.ratio", "lightgreen", 0.81)
other_student_plots <- create_student_histograms(acs_geo, "Other", "allothers.s.ratio", "purple", 1.01)
white_student_plots <- create_student_histograms(acs_geo, "White", "white.s.ratio", "pink", 0.21)

# Usage for teachers:
black_teacher_plots <- create_teacher_histograms(acs_geo, "Black", "black.t.ratio", "#39568CFF", 0.41)
hispanic_teacher_plots <- create_teacher_histograms(acs_geo, "Hispanic", "hispanic.t.ratio", "orange", 0.61)
asian_teacher_plots <- create_teacher_histograms(acs_geo, "Asian", "asian.t.ratio", "lightgreen", 1.01)
other_teacher_plots <- create_teacher_histograms(acs_geo, "Other", "allothers.t.ratio", "purple", 1.01)
white_teacher_plots <- create_teacher_histograms(acs_geo, "White", "white.t.ratio", "pink", 0.21)


#--------------------------------------------# 
#--------------------------------------------# 
#### 5. Bar plots ####
#--------------------------------------------# 
#--------------------------------------------# 

#--------------------------------------------# 
##### 1. data for bar plots ####
#--------------------------------------------# 
acs_all_long <- acs_all |> 
  select(year, ends_with("ratio")) |> 
  pivot_longer(
    cols = -c(year),
    names_to = c("race", ".value"),
    names_pattern = "(.*)\\.(s\\.ratio|t\\.ratio)") |> 
  filter(race %in% c("black", "hispanic", "asian", "allothers", "white")) |> 
  mutate(s.percent = s.ratio * 100,
         t.percent = t.ratio * 100,
         race = factor(race, levels = c("white", "allothers", "asian", "hispanic", "black")),
         vjust = case_when(
           race %in% c("white", "hispanic", "black") ~ 0.5,
           race == "asian" ~ 0.5,
           race == "allothers" ~ 2))


ggplot(acs_all_long, 
       aes(x = as.factor(year), y = s.percent, fill = race)) +
  #geom_bar(stat="identity") + 
  geom_col(position = "stack") +
  geom_text(aes(label = round(s.percent, 1), vjust = vjust), position = position_stack(vjust = 0.5), family = "Times New Roman", size = 3) + # color = ifelse(acs_bar$race %in% c("American Indian and/or Alaska Native", "Other race", "Multiple-race"), "black", "white")) + # adds the text labels to the plot. The position_stack(vjust = 0.5) argument centers the text labels vertically within each bar. 
  # scale_fill_grey() + ## grey palette
  # scale_fill_viridis(discrete = TRUE , begin = 0.8, end=0.2, option = "E") + # vivid palette
  scale_fill_manual(values = c("pink", "purple",  "lightgreen", "orange", "#39568CFF"),
                    labels = c("White", "Other", "Asian", "Hispanic", "Black")) +
  scale_y_continuous(expand = c(0,0)) + ## remove space between 0 and x-axis
  coord_flip() + ## change x-axis and y-axis
 
  labs(title = "", # omit title
       x = "Year",
       y = "Percentage of Students",
       fill = "",
       caption = "") + 
  guides(fill=guide_legend(reverse=TRUE))


ggplot(acs_all_long, 
       aes(x = as.factor(year), y = t.percent, fill = race)) +
  #geom_bar(stat="identity") + 
  geom_col(position = "stack") +
  geom_text(aes(label = round(t.percent, 1), vjust = vjust), position = position_stack(vjust = 0.5), family = "Times New Roman", size = 3) + # color = ifelse(acs_bar$race %in% c("American Indian and/or Alaska Native", "Other race", "Multiple-race"), "black", "white")) + # adds the text labels to the plot. The position_stack(vjust = 0.5) argument centers the text labels vertically within each bar. 
  # scale_fill_grey() + ## grey palette
  # scale_fill_viridis(discrete = TRUE , begin = 0.8, end=0.2, option = "E") + # vivid palette
  scale_fill_manual(values = c("pink", "purple",  "lightgreen", "orange", "#39568CFF"),
                    labels = c("White", "Other", "Asian", "Hispanic", "Black")) +
  scale_y_continuous(expand = c(0,0)) + ## remove space between 0 and x-axis
  coord_flip() + ## change x-axis and y-axis
  
  labs(title = "", # omit title
       x = "Year",
       y = "Percentage of Teachers",
       fill = "",
       caption = "") + 
  guides(fill=guide_legend(reverse=TRUE))


#--------------------------------------------# 
#--------------------------------------------# 
#### END  ####
#--------------------------------------------# 
#--------------------------------------------# 