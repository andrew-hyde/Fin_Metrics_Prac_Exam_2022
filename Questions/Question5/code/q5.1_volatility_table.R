
Q5.1_vol_table_func <- function(df_data){

    library(tidyverse)
    library(knitr)
    library(lubridate)


df_q5.1_cncyIV_data <-
        cncyIV %>%
        select(date, Name, Price) %>%
        mutate(Name = gsub("_IV","", cncyIV$Name)) %>% # remove characters
        group_by(Name) %>%
        summarise(across(everything(), mean)) %>%
        select(-date) %>%
        arrange(desc(Price)) %>% head(10)


df_q5.1_cncyIV_data %>%
     knitr::kable(x = ., digits = 2,
        col.names = c("Name", "Value"), caption = "Implied Volatility",)

}

Q5.1_vol_table_func(df_data = df_q5.1_cncyIV_data)

