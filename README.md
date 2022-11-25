# Purpose

Purpose of this work folder.

Ideally store a minimum working example data set in data folder.

Add binary files in bin, and closed R functions in code. Human Readable
settings files (e.g. csv) should be placed in settings/

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 453478 24.3     973548   52   644205 34.5
    ## Vcells 815038  6.3    8388608   64  1635617 12.5

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# How to get started

``` r
# eval=F and echo=T, as i want to see the code but I don't want it to run again

# create a project
fmxdat::make_project(Open = T)
# location: C:\Masters Economics\Fin Metrics\Fin_Metrics_Prac_Exam_2022\Fin_Metrics_Prac_Exam_2022
# from the clipboard

# create folders for each Question
# create folders for each Question
Texevier::create_template(
    directory = "C:/Masters Economics/Fin Metrics/Fin_Metrics_Prac_Exam_2022/Fin_Metrics_Prac_Exam_2022",
            template_name = "Question1", build_project = TRUE, open_project = FALSE)
# create folders for each Question
Texevier::create_template(
    directory = "C:/Masters Economics/Fin Metrics/Fin_Metrics_Prac_Exam_2022/Fin_Metrics_Prac_Exam_2022",
            template_name = "Question2", build_project = TRUE, open_project = FALSE)# create folders for each Question
Texevier::create_template(
    directory = "C:/Masters Economics/Fin Metrics/Fin_Metrics_Prac_Exam_2022/Fin_Metrics_Prac_Exam_2022",
            template_name = "Question3", build_project = TRUE, open_project = FALSE)# create folders for each Question
Texevier::create_template(
    directory = "C:/Masters Economics/Fin Metrics/Fin_Metrics_Prac_Exam_2022/Fin_Metrics_Prac_Exam_2022",
            template_name = "Question4", build_project = TRUE, open_project = FALSE)# create folders for each Question
Texevier::create_template(
    directory = "C:/Masters Economics/Fin Metrics/Fin_Metrics_Prac_Exam_2022/Fin_Metrics_Prac_Exam_2022",
            template_name = "Question5", build_project = TRUE, open_project = FALSE)# create folders for each Question
Texevier::create_template(
    directory = "C:/Masters Economics/Fin Metrics/Fin_Metrics_Prac_Exam_2022/Fin_Metrics_Prac_Exam_2022",
            template_name = "Question6", build_project = TRUE, open_project = FALSE)# create folders for each Question
Texevier::create_template(
    directory = "C:/Masters Economics/Fin Metrics/Fin_Metrics_Prac_Exam_2022/Fin_Metrics_Prac_Exam_2022",
            template_name = "Question7", build_project = TRUE, open_project = FALSE)
```

# Question 2: Yield Spread 

Economists have recently pointed out that the current yield spreads in
local mid to longer dated bond yields have since 2020 been the highest
in decades.

## Load the data

``` r
SA_bonds <- read_rds("data/SA_Bonds.rds")
BE_Infl <- read_rds("data/BE_Infl.rds")
bonds_2y <- read_rds("data/bonds_2y.rds")
bonds_10y <- read_rds("data/bonds_10y.rds")
usdzar <- read_rds("data/usdzar.rds")
ZA_Infl <- read_rds("data/ZA_Infl.rds")
IV <- read_rds("data/IV.rds")
```

## South African Bond Yields

I begin by viewing the data and determine if there are any missing
values. I then plot the yields for bonds of differing maturities to
visualize the spread between the 3 Month, 2 year and 10 Year South
African bonds.

From the graph below, it does appear that the yield spreads in local mid
to longer dated bond yields have have increased since 2020 when compare
to historical spreads. The yield on the 3 year bond has decreased since
2015, this may be a reflection of anticipated interest rate cuts in the
near future. An increase in interest rates would imply an increase in
bond prices and thus yields would decrease.

``` r
# colSums(is.na(sa_bonds_tidy))
# there are no missing values or NA's

graph_q2.1_func(df_data = df_q2.1_data,
                title = "South African Bond Yields",
                subtitle = "From 2000 to 2022",
                caption = "",
                xlabel = "Date",
                ylabel = "Yield")
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

To further investigate this topic I combine the available data for this
question using ‘left_join’. I make use of this combined data set to
perform the rest analysis.

``` r
q2_data_combined <- left_join(
        # JOIN us DATA
        bonds_10y %>% group_by(Name) %>% filter(Name == "US_10Yr") %>% ungroup() %>% select(-Name),
        bonds_2y %>% group_by(Name) %>% filter(Name == "US_2yr") %>% ungroup() %>% select(-Name),
        by = "date") %>%
    
        # JOIN SA DATA
        left_join(.,
                  SA_bonds %>% select(-SA_3M),
                  by = "date") %>%
        # JOIN THE VIX
        left_join(.,
                  IV %>% mutate(VIX = Price) %>% group_by(Name) %>% filter(Name == "VIX")
                  %>% ungroup() %>% select(-Name, -Price),
                  by = "date") %>%

        # JOIN US ZAR Exchnage Rate
        left_join(.,
                  usdzar %>% mutate(USDZAR = Price) %>% ungroup() %>% select(-Name, -Price),
                  by = "date") %>%

        # JOIN THE inflation data
        left_join(.,
                  BE_Infl %>% mutate(Infl_est = Price) %>% ungroup() %>% select(-Name, -Price),
                  by = "date")
```

Using the combined data set, I plot out the the yields on mid and long
term South African Bonds from 2019 to 2022 to get an idea how the
spreads have changed since the pandemic.

The graph below illustrates how bond how mid and long term bond yields
have adjusted after the COVID-19 pandemic. It is not surprising that 2
year bond yields are down considerably since the pandemic first hit,
this may be the result of investors anticipating monetary easing through
lowered interest rates to combat the economic downturn of the pandemic.
This can be seen with sharp dive on the 2 year bond late in 2020. Since
then, the monetary authorities have been concerned with rising inflation
and so interest rate hikes have been taking place, which explains the
increases in the 2 year yields since late 2020.

``` r
graph_q2.3_func(df_data = df_q2.3_data,
                title = "South African Mid and Long Bonds Yields",
                subtitle = "With 10 Year Break-even inflation estimate, from 2019 to 2022",
                caption = "",
                xlabel = "Date",
                ylabel = "Percentage (%)")
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

## Domestic and Foreign Yields

I construct line graph and use ‘facet_wrap’ to display each variable on
its own axis. The graph shows the the mid to long term yield spread for
both South Africa and the United States over time, as well as the
prevailing interest rate at the time.

Over the past 5 years in both the local South African Bonds market and
the US Bonds Market, the mid to long term yield spread has widen for
both countries. However, as the RSA bond spread is larger it appears
that capital is flowing into South Africa to invest

``` r
graph_q2.2_func(df_data = df_q2.2_data,
                title = "US and ZAR Bond Yield Spreads",
                subtitle = "10 Year (Long) and 2 year (Mid) spreads, from 2018 to 2022",
                caption = "The US Dollar RSA Rand exchange rate is plotted alongside yield spreads.",
                xlabel = "Date",
                ylabel = "")
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
# to deetermine which countries are included in the data.
#bonds_10y %>% select(Name) %>% unique()
#bonds_2y %>% select(Name) %>% unique()
```

I then plot the the 10 and 2 year yield spread of BRICS Countries along
with the VIX. I do this by calculating spreads for each country and then
select for these spread for the combined data set. The CBOE Volatility
Index has been added to the plot to provide of how yield spreads between
mid and long term bonds for the BRICS countries are increasing as
volatility expectations increase.

From the graph it appears that that there is a positive relationship
between volatility in the equities market implied by the VIX (when
looking at the VIX spike in early 2020) and the bond yield spreads.
Additionally, it appears that the yield spread between the more
developing BRICS countries (Brazil, India, South Africa) experiences a
widening of the yield spread since the beginning of the pandemic.

``` r
graph_q2.4_func(df_data = df_q2.4_data,
                title = "BRICS Bond Yield Spreads",
                subtitle = "10 Year (Long) and 2 year (Mid) spreads, from 2018 to 2022",
                caption = "The CBOE Volatily Index (VIX) is plotted alongside yield spreads.",
                xlabel = "Date",
                ylabel = "")
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)
