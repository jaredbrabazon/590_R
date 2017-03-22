Write a Function
================

-   Complete this task by creating an R notebook (.Rmd file) and accompanying .md file \#\# Functions and error handling: Write a function that fulfills the following criteria:

-   It should be tidyverse compatible (i.e., the first argument must be a data frame)

-   It should add two arbitrary columns of the data frame (specified by the user) and put them in a new column of that data frame, with the name of the new column specified by the user

``` r
# load libraries
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(ggplot2)

column_addition <- function(df, col1, col2, name = "summation"){
  if (class(df) != "data.frame"){
    stop("Your first argument is not a data frame!")
  }
  
  if (class(col1) != "numeric" || class(col2) != "numeric"){
    #warning("your columns are non-numeric")
    #why does the function change the col1 and col2 to character
  }

  df[[name]] = df[[col1]] + df[[col2]]
  df
}

#source('~/University/Classes/590_R/R/task_8_function_2.R')
```

``` r
#test the function
column_addition(mtcars, "mpg", "cyl", name = "add")
```

    ##                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    ## Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    ## Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    ## Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    ## Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    ## Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    ## Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    ## Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    ## Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    ## Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    ## Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    ## Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    ## Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    ## Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    ## Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    ## Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    ## Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    ## AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    ## Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    ## Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    ## Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    ## Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    ## Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    ## Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    ## Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    ## Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    ## Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
    ##                      add
    ## Mazda RX4           27.0
    ## Mazda RX4 Wag       27.0
    ## Datsun 710          26.8
    ## Hornet 4 Drive      27.4
    ## Hornet Sportabout   26.7
    ## Valiant             24.1
    ## Duster 360          22.3
    ## Merc 240D           28.4
    ## Merc 230            26.8
    ## Merc 280            25.2
    ## Merc 280C           23.8
    ## Merc 450SE          24.4
    ## Merc 450SL          25.3
    ## Merc 450SLC         23.2
    ## Cadillac Fleetwood  18.4
    ## Lincoln Continental 18.4
    ## Chrysler Imperial   22.7
    ## Fiat 128            36.4
    ## Honda Civic         34.4
    ## Toyota Corolla      37.9
    ## Toyota Corona       25.5
    ## Dodge Challenger    23.5
    ## AMC Javelin         23.2
    ## Camaro Z28          21.3
    ## Pontiac Firebird    27.2
    ## Fiat X1-9           31.3
    ## Porsche 914-2       30.0
    ## Lotus Europa        34.4
    ## Ford Pantera L      23.8
    ## Ferrari Dino        25.7
    ## Maserati Bora       23.0
    ## Volvo 142E          25.4

-   It should throw an informative warning if any invalid arguments are provided. Invalid arguments might include:

    -   The first argument is not a data frame

    -   Less than two valid columns are specified to add (e.g., one or both of the column names isn't in the supplied data frame)

    -   The columns specified are not numeric, and therefore can't be added - use tryCatch() for this

-   If the columns to add aren't valid but the new column name is, the function should create a column of NA values

Loop and performance metric task
================================

-   Write a function named that uses a for loop to calculate the sum of the elements of a vector, which is passed as an argument (i.e., it should do the same thing that sum() does with vectors). your\_fun(1:10^4) should return 50005000.

``` r
#### write a for loop
my_sum <- function(x){
  r <- c(x)
  s <- 0
  
  for (i in r) {
    s <- s + i
  }
  s
}
```

``` r
# test my loop
my_sum(1:10^4)
```

    ## [1] 50005000

``` r
# test the sum function
sum(1:10^4)
```

    ## [1] 50005000

-   Use the microbenchmark::microbenchmark function to compare the performace of your function to that of sum in adding up the elements of the vector 1:10^4. The benchmarking code should look something like:

    test.vec &lt;- 1:10^4 microbenchmark( my\_sum(test.vec), sum(test.vec) )

``` r
#use the microbenchmark function to compare performance
library(microbenchmark)
```

    ## Warning: package 'microbenchmark' was built under R version 3.3.3

``` r
test.vec <- 1:10^4
microbenchmark(
  my_sum(test.vec),
  sum(test.vec)
  )
```

    ## Unit: microseconds
    ##              expr      min       lq       mean   median       uq       max
    ##  my_sum(test.vec) 3817.921 3936.975 4743.37672 4053.975 4220.445 56210.874
    ##     sum(test.vec)   12.727   13.137   15.28866   14.369   17.243    23.401
    ##  neval
    ##    100
    ##    100

Is there a difference? Why? \* the for loop takes by far longer to run
