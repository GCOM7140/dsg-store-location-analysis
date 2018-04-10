Creating a Database of Dick's Sporting Goods Stores
================
jpb2f
Tue Apr 10 12:35:22 2018

Let's begin by loading the libraries we'll need for this exercise. Uncomment and run the `install.packages()` functions if any of the libraries are not installed on your computer.

``` r
# install.packages("qdapRegex")
library(qdapRegex) # for ex_between()
library(tidyverse)
```

    ## ── Attaching packages ───────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────── tidyverse_conflicts() ──
    ## ✖ ggplot2::%+%()   masks qdapRegex::%+%()
    ## ✖ dplyr::explain() masks qdapRegex::explain()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()

``` r
# install.packages("pbapply")
library(pbapply) # for progress bars
library(here)
```

    ## here() starts at /Users/jpb2f/Box Sync/teaching/Customer Analytics/2018 Customer Analytics/github/dsg-store-location-analysis

``` r
# Now let's create a function called `get_urls` that we can use to scrape State
# URLs from http://stores.dickssportinggoods.com/.

get_urls <- function(x){
  
  tmp <- readLines(x, warn = FALSE)
  
  tmp <- tmp[grep("State index", tmp)]
  
  unlist(ex_between(tmp, left = "href=\"", right = "\""))
  
}

state_urls <- get_urls("http://stores.dickssportinggoods.com/")

head(state_urls)
```

    ## [1] "https://stores.dickssportinggoods.com/al/"
    ## [2] "https://stores.dickssportinggoods.com/ar/"
    ## [3] "https://stores.dickssportinggoods.com/az/"
    ## [4] "https://stores.dickssportinggoods.com/ca/"
    ## [5] "https://stores.dickssportinggoods.com/co/"
    ## [6] "https://stores.dickssportinggoods.com/ct/"

With the State URLs in `state_urls`, let's use `get_urls()` to scrape the City URLs that can be found at each State URL (e.g., <https://stores.dickssportinggoods.com/al/>). The `lapply()` function allows us to loop `get_urls()` through the State URLs in `state_urls`. Scraping City URLs should take about a minute to run.

``` r
city_urls <- lapply(state_urls, get_urls) %>% unlist()

head(city_urls)
```

    ## [1] "https://stores.dickssportinggoods.com/al/alabaster/" 
    ## [2] "https://stores.dickssportinggoods.com/al/birmingham/"
    ## [3] "https://stores.dickssportinggoods.com/al/cullman/"   
    ## [4] "https://stores.dickssportinggoods.com/al/daphne/"    
    ## [5] "https://stores.dickssportinggoods.com/al/dothan/"    
    ## [6] "https://stores.dickssportinggoods.com/al/florence/"

Now let's create a second `get_urls()` function called `get_urls2()`. We'll use this function to scrape Store URLs from the City URLs in city\_urls. An example Store URL is <http://stores.dickssportinggoods.com/al/alabaster/1078/>.

``` r
get_urls2 <- function(x){
  
  tmp <- readLines(x)
  
  tmp <- tmp[grep("itemprop=\"streetAddress\"><a linktrack=\"Landing page\"", 
                  tmp)]
  
  unlist(ex_between(tmp, left = "href=\"", right = "\""))
}
```

Let's use the `pblapply()` function to loop the `get_urls2()` function through the City URLs in city\_urls, because it will give us a progress bar. Scraping the Store URLs should take about 15 minutes in total.

``` r
store_urls <- pblapply(city_urls, get_urls2) %>% unlist()

head(store_urls)
```

    ## [1] "https://stores.dickssportinggoods.com/al/alabaster/1078/"
    ## [2] "https://stores.dickssportinggoods.com/al/birmingham/330/"
    ## [3] "https://stores.dickssportinggoods.com/al/cullman/1161/"  
    ## [4] "https://stores.dickssportinggoods.com/al/daphne/617/"    
    ## [5] "https://stores.dickssportinggoods.com/al/dothan/337/"    
    ## [6] "https://stores.dickssportinggoods.com/al/florence/4626/"

Now we have a vector of URLs for every Dick's Sporting Goods store. From these websites, we can scrape address and geo-location information. How about we scrape each store's id, name, address, city, state, zip, country, phone, latitude, and longitude? Here are the lines of code we're interested in from the Stony Point Pkwy store on Richmond's City URL website: "@id":"447", "name": "Dick's Sporting Goods", "address":{ "@type":"PostalAddress", "streetAddress":"9204 STONY POINT PKWY", "addressLocality":"RICHMOND", "addressRegion":"VA", "postalCode":"23235", "addressCountry":"US" }, "containedIn":"stony point fashion park", "geo":{ "@type":"GeoCoordinates", "latitude":37.5484, "longitude":-77.5726 }, "branchOf": { "name":"Dicks Sporting Goods", "url": "<http://www.dickssportinggoods.com/home/index.jsp>" }, "url":"<http://stores.dickssportinggoods.com/va/richmond/447>", "telephone":"804-253-0800", Based on this information structure, let's create a function called `get_store_info()` to scrape id, name, address, city, state, zip, country, phone, latitude, and longitude from each store URL.

``` r
get_store_info <- function(x){
  
  tmp <- readLines(x, warn = FALSE)
  
  id <- tmp[grep("\"@id\":", tmp)] %>% 
    ex_between(left = ":\"", right = "\"") %>% unlist()
  
  store <- tmp[grep("\"containedIn\":", tmp)] %>% 
    ex_between(left = ":\"", right = "\"") %>% unlist() %>% toupper()
  
  address <- tmp[grep("\"streetAddress\":", tmp)] %>% 
    ex_between(left = ":\"", right = "\"") %>% unlist()
  
  city <- tmp[grep("\"addressLocality\":", tmp)] %>% 
    ex_between(left = ":\"", right = "\"") %>% unlist()
  
  state <- tmp[grep("\"addressRegion\":", tmp)] %>% 
    ex_between(left = ":\"", right = "\"") %>% unlist()
  
  zip <- tmp[grep("\"postalCode\":", tmp)] %>% 
    ex_between(left = ":\"", right = "\"") %>% unlist()
  
  country <- tmp[grep("\"addressCountry\":", tmp)] %>% 
    ex_between(left = ":\"", right = "\"") %>% unlist()
  
  phone <- tmp[grep("\"telephone\":", tmp)] %>% 
    ex_between(left = ":\"", right = "\"") %>% unlist()
  
  latitude <- tmp[grep("\"latitude\":", tmp)] %>% 
    ex_between(left = "\":", right = ",") %>% unlist()
  
  longitude <- tmp[grep("\"longitude\":", tmp)] %>% 
    str_extract(pattern = "-[0-9]{2,3}\\.[0-9]{1,}")
  
  cbind(id, store, address, city, state, zip, country, phone, latitude, 
        longitude)
  
}
```

With `get_store_info()` in our global environments, let's use it to scrape the store info we're interested in from `store_info`. The pblapply function will loop `get_store_info()` through the Store URLs in `store_urls` and give us a progress bar so we can gauge how long the scraping will take (should be ~35 minutes in total).

``` r
store_info <- pblapply(store_urls, get_store_info)

# Almost done. Now let's combine the rows of `store_info` into a tibble named
# `dsg_stores`. Some of the stores got scraped multiple times, but that's okay
# because `distinct()` will clean that up.

dsg_stores <- do.call(rbind, store_info) %>% 
    as_tibble() %>% 
    distinct() %>% 
    filter(!is.na(id))
```

Finally, let's write the data to a .csv file called "dsg\_stores.csv" that can later be wrangled, transformed, visualized, and modeled.

``` r
write_csv(dsg_stores, here("data", "dsg_stores.csv"))
```
