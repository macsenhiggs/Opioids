library(tidyverse)

library(data.table)

tedsd_puf_2023 <- fread("TEDSD 2017 to 2023/tedsd_puf_2023.csv")

tedsd_puf_2022 <- fread("TEDSD 2017 to 2023/tedsd_puf_2022.csv") |>
  rename(SEX = GENDER) 



tedsd_puf_join <- bind_rows(
  tedsd_puf_2023, tedsd_puf_2022
)


# tedsd_puf_2020 <- fread("TEDSD 2017 to 2023/tedsd_puf_2020.csv") |>
#   rename(
#     SEX = GENDER,
#     CBSA2020 = CBSA2010
#   ) 
# 
# tedsd_puf_join <- bind_rows(
#   tedsd_puf_join, tedsd_puf_2020
# )