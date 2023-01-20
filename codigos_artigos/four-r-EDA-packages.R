# Four R packages for Automated Exploratory Data Analysis you might have missed
# Author: Nicolo Cosimo Albanese
# Date: Jan 12, 2022
# Source: https://towardsdatascience.com/four-r-packages-for-automated-exploratory-data-analysis-you-might-have-missed-c38b03d4ee16

# --- INSTALLING PACKAGES
if (!("tidyverse") %in% installed.packages()) install.packages("tidyverse")
if (!("dplyr") %in% installed.packages()) install.packages("dplyr")
if (!("readr") %in% installed.packages()) install.packages("readr")

library(dplyr)
library(readr)


# read csv file
df <- 
  read_delim(
    file = "./dados/cardio_train.csv",
    col_types = "iifidiiffffff",
    delim=";")


# pre-processing
df <- 
  # remove the id
  select(df, -id) %>%
  # age: days -> years
  mutate(age = round(age / 365))


# observe first rows
head(df)

summary(df)

if (!("DataExplorer") %in% installed.packages()) install.packages("DataExplorer")
library(DataExplorer)


df %>%
  create_report(
    output_file = paste("Report", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep=" - "),
    report_title = "EDA Report - Cardiovascular Disease Dataset",
    y = "cardio"
  )


if (!("GGally") %in% installed.packages()) install.packages("GGally")
library(GGally)

# change plot size (optional)
options(repr.plot.width = 20, repr.plot.height = 10)

df %>% 
  select("age", "cholesterol", "height", "weight") %>%
  ggpairs(mapping = aes(color = df$cardio, alpha = 0.5))


if (!("SmartEDA") %in% installed.packages()) install.packages("SmartEDA")
library(SmartEDA)

# similarly, with dplyr syntax: df %>% ExpReport(...)
ExpReport(
  df,
  Target="cardio",
  label=NULL,
  op_file="Report.html",
  op_dir=getwd())

head(df)

if (!("tableone") %in% installed.packages()) install.packages("tableone")
library(tableone)

# we perform a stratification based on the presence of cardiovascular disease
tableOne <- CreateTableOne(vars = colnames(select(df, -"cardiovascular disease")), 
                           strata = c("cardiovascular disease"), 
                           data = df)


# we pass a list of continuous variables not normally distributed in the "nonnormal" argument
print(
  tableOne,
  nonnormal = c("age", "weight", "height", "systolic blood pressure", "diastolic blood pressure"),
  showAllLevels = TRUE)

# categorical part only
tableOne$CatTable


# ----

if (!("dlstats") %in% installed.packages()) install.packages("dlstats")
library("dlstats")

stats <- cran_stats(c("SmartEDA", "DataExplorer", "tableone", "GGally", "Hmisc", 
                      "exploreR", "dlookr", "desctable", "summarytools"))

stats %>%
  filter(start >= "2022-01-01" & end < "2023-01-01") %>%
  select(package, downloads) %>%
  group_by(package) %>% 
  summarize(downloads = sum(downloads)) %>%
  arrange(desc(downloads))
