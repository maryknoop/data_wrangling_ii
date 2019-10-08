data\_wrangling\_ii\_10082019
================
Mary Knoop
10/8/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)
```

``` r
#read in url and name it drug_use_xml
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_xml = read_html(url)

drug_use_xml
```

    ## {html_document}
    ## <html lang="en">
    ## [1] <head>\n<link rel="P3Pv1" href="http://www.samhsa.gov/w3c/p3p.xml">\ ...
    ## [2] <body>\r\n\r\n<noscript>\r\n<p>Your browser's Javascript is off. Hyp ...

``` r
#scrape the first table from the dataset gathered from the url
drug_use_xml %>%
  html_nodes(css = "table")
```

    ## {xml_nodeset (15)}
    ##  [1] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [2] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [3] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [4] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [5] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [6] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [7] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [8] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [9] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [10] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [11] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [12] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [13] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [14] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [15] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...

``` r
#view table data
table_marj = 
  (drug_use_xml %>% html_nodes(css = "table")) %>% 
  .[[1]] %>%
  html_table() 
```

``` r
#view table from url and remove the first column since it was a repeating note in each column
table_marj = 
  (drug_use_xml %>% html_nodes(css = "table")) %>% 
  .[[1]] %>%
  html_table() %>%
  slice(-1) %>% 
  as_tibble()
table_marj
```

    ## # A tibble: 56 x 16
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-201…
    ##    <chr> <chr>            <chr>            <chr>          <chr>           
    ##  1 Tota… 12.90a           13.36            0.002          13.28b          
    ##  2 Nort… 13.88a           14.66            0.005          13.98           
    ##  3 Midw… 12.40b           12.76            0.082          12.45           
    ##  4 South 11.24a           11.64            0.029          12.02           
    ##  5 West  15.27            15.62            0.262          15.53a          
    ##  6 Alab… 9.98             9.60             0.426          9.90            
    ##  7 Alas… 19.60a           21.92            0.010          17.30           
    ##  8 Ariz… 13.69            13.12            0.364          15.12           
    ##  9 Arka… 11.37            11.59            0.678          12.79           
    ## 10 Cali… 14.49            15.25            0.103          15.03           
    ## # … with 46 more rows, and 11 more variables: `12-17(2014-2015)` <chr>,
    ## #   `12-17(P Value)` <chr>, `18-25(2013-2014)` <chr>,
    ## #   `18-25(2014-2015)` <chr>, `18-25(P Value)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `26+(P Value)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>, `18+(P Value)` <chr>

``` r
#scrape nyc cost of living from url 
nyc_cost = 
  read_html("https://www.bestplaces.net/cost_of_living/city/new_york/new_york") %>%
  html_nodes(css = "table") %>%
  .[[1]] %>%
  html_table(header = TRUE)
nyc_cost
```

    ##     COST OF LIVING New York New York      USA
    ## 1          Overall    209.3    129.4      100
    ## 2          Grocery    114.7    101.7      100
    ## 3           Health      101    100.8      100
    ## 4          Housing      354    150.8      100
    ## 5 Median Home Cost $680,500 $305,400 $231,200
    ## 6        Utilities    150.5    115.9      100
    ## 7   Transportation    211.5    161.5      100
    ## 8    Miscellaneous    121.2    101.6      100

``` r
#scrape from IMDB
hpsaga_html = 
  read_html("https://www.imdb.com/list/ls000630791/")
hpsaga_html
```

    ## {html_document}
    ## <html xmlns:og="http://ogp.me/ns#" xmlns:fb="http://www.facebook.com/2008/fbml">
    ## [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset= ...
    ## [2] <body id="styleguide-v2" class="fixed">\n            <img height="1" ...

``` r
title_vec = 
  hpsaga_html %>%
  html_nodes(".lister-item-header a") %>%
  html_text()
title_vec
```

    ## [1] "Harry Potter and the Sorcerer's Stone"       
    ## [2] "Harry Potter and the Chamber of Secrets"     
    ## [3] "Harry Potter and the Prisoner of Azkaban"    
    ## [4] "Harry Potter and the Goblet of Fire"         
    ## [5] "Harry Potter and the Order of the Phoenix"   
    ## [6] "Harry Potter and the Half-Blood Prince"      
    ## [7] "Harry Potter and the Deathly Hallows: Part 1"
    ## [8] "Harry Potter and the Deathly Hallows: Part 2"

``` r
gross_rev_vec = 
  hpsaga_html %>%
  html_nodes(".text-small:nth-child(7) span:nth-child(5)") %>%
  html_text()

runtime_vec = 
  hpsaga_html %>%
  html_nodes(".runtime") %>%
  html_text()

hpsaga_df = 
  tibble(
    title = title_vec,
    rev = gross_rev_vec,
    runtime = runtime_vec)
```
