
# mtvc

As widely discussed in biostatistics, and particularly in the field of
Survival Analysis, time varying covariates should be taken into account
with the counting process structure. However, in some cases, there might
be models where more than one variable changes its value during the
follow-up. The function `mtvc` takes as input one more more time varying
variable, with the respective date in which that change was found, and
restructures the data frame into the counting process strucure, where
each patient has a time window which reflects the comorbidity status.

## Installation

You can load the package as follows:

``` r
library(mtvc)
```

## Example

Now use `mtvc` function in order to restructure the data frame:

``` r
data("simwide")
#
cp.dataframe=mtvc(data=simwide,
                  origin='1970-01-01',
                  dates=c(FIRST_CHRONIC,FIRST_ACUTE,FIRST_RELAPSE),
                  complications=c(CHRONIC,ACUTE,RELAPSE),
                  start=DATETRAN,
                  stop=DLASTSE,
                  event=EVENT) 
#
head(cp.dataframe[,c('id','tdep_acute','tdep_chronic','tdep_relapse','start','stop')])
#> # A tibble: 6 × 6
#> # Groups:   id [3]
#>      id tdep_acute tdep_chronic tdep_relapse start  stop
#>   <int>      <dbl>        <dbl>        <dbl> <dbl> <dbl>
#> 1     1          0            0            0     0    26
#> 2     1          1            0            0    26    56
#> 3     1          1            1            0    56    88
#> 4     2          0            0            0     0    20
#> 5     2          0            1            0    20   533
#> 6     3          0            0            0     0     6
```
