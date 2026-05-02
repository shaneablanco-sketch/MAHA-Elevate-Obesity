# Set up ------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(readxl)
library(skimr)
library(survey)
library(srvyr)
library(janitor)
library(gtsummary)
library(styler)
# Loading Data ------------------------------------------------------------

conditions_data <- c(
  "data/conditions_data/2021/conditions_2021.csv",
  "data/conditions_data/2022/conditions_2022.csv",
  "data/conditions_data/2023/conditions_2023.csv"
)
cond_21 <- read_csv("data/conditions_data/2021/conditions_2021.csv")
cond_22 <- read_csv("data/conditions_data/2022/conditions_2022.csv")
cond_23 <- read_csv("data/conditions_data/2023/conditions_2023.csv")

# Tidying --------------------------------------------------------------------
cond_21 |>
  janitor::clean_names()
cond_22 |>
  janitor::clean_names()
cond_23 |>
  janitor::clean_names()
