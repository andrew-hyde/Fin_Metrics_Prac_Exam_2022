
Q5.2_vol_table_func <- function(df_data){

    library(tidyverse)
    library(knitr)
    library(lubridate)

cncyIV_5year <- cncyIV %>% filter(date >= as.Date("2016-10-29"))

df_q5.2_cncyIV_data <- cncyIV_5year %>%
                    select(date, Name, Price) %>%
                    mutate(Name = gsub("_IV","", cncyIV_5year$Name)) %>% # remove characters
                    group_by(Name) %>%
                    summarise(across(everything(), mean)) %>%
                    select(-date) %>%
                    arrange(desc(Price)) %>% head(10)


df_q5.2_cncyIV_data %>%
     knitr::kable(x = ., digits = 2,
        col.names = c("Name", "Value"), caption = "Implied Volatility",)

}


