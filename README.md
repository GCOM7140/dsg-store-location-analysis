Where in the US are Dick’s Sporting Goods Stores?
================

[Clay Ford](https://github.com/clayford) and [Jeff Boichuk](https://github.com/jeffboichuk)
April 10, 2018

The following script can be used to scrape [this website](https://stores.dickssportinggoods.com/) and create a database of addresses for Dick's Sporting Goods stores in the United States. Let's begin by loading the libraries we're going to use.

``` r
library(qdapRegex) # for ex_between()
library(pbapply) # for progress bars
library(tidyverse) # because, tidyverse
library(here) # for file-path management
```

Next let's create a function called `get_urls` that we can use to scrape State URLs from [this website](https://stores.dickssportinggoods.com/).

``` r
get_urls <- function(x){
  
  tmp <- readLines(x, warn = FALSE)
  
  tmp <- tmp[grep("State index", tmp)]
  
  ex_between(tmp, left = "href=\"", right = "\"") %>% unlist()
  
}

state_urls <- get_urls("http://stores.dickssportinggoods.com/")

head(state_urls)
```

Let's now use `get_urls()` to scrape the city URLs that can be found at each State URL of `state_urls`. The `lapply()` function allows us to loop `get_urls()` through the State URLs in `state_urls`. Scraping the store URLs will likely take a minute or so to run.

``` r
# scraping city URLs should take about a minute to run
city_urls <- lapply(state_urls, get_urls) %>% unlist()

head(city_urls)
```

Now let's create a second function called `get_urls2()`. We'll use this function to scrape store URLs from the city URLs in `city_urls`.

``` r
get_urls2 <- function(x){
  
  tmp <- readLines(x)
  
  tmp <- tmp[grep("itemprop=\"streetAddress\"><a linktrack=\"Landing page\"", 
                  tmp)]
  
  unlist(ex_between(tmp, left = "href=\"", right = "\""))
}
```

Let's use `pblapply()` to loop `get_urls2()` through `city_urls`, because it will give us a progress bar. Scraping the store URLs will likely take in the neighborhood of 15 minutes.

``` r
store_urls <- pblapply(city_urls, get_urls2) %>% unlist()

head(store_urls)
```

Now we have a vector of URLs for every Dick's Sporting Goods store in the US. From these websites, we can scrape address and geo-location information. How about we scrape each store's id, name, address, city, state, zip, country, phone, latitude, and longitude? Here's the code we're interested in scraping for one of [Richmond's locations](http://stores.dickssportinggoods.com/va/richmond/):

------------------------------------------------------------------------

"@id":"447", <br /> "name": "Dick's Sporting Goods", <br /> "address":{ <br /> "@type":"PostalAddress", <br /> "streetAddress":"9204 STONY POINT PKWY", <br /> "addressLocality":"RICHMOND", <br /> "addressRegion":"VA", <br /> "postalCode":"23235", <br /> "addressCountry":"US" <br /> }, <br /> "containedIn":"stony point fashion park", <br /> "geo":{ <br /> "@type":"GeoCoordinates", <br /> "latitude":37.5484, <br /> "longitude":-77.5726 <br /> }, <br /> "branchOf": { <br /> "name":"Dicks Sporting Goods", <br /> "url": "<http://www.dickssportinggoods.com/home/index.jsp>" <br /> }, <br /> "url":"<http://stores.dickssportinggoods.com/va/richmond/447>", <br /> "telephone":"804-253-0800", <br />

------------------------------------------------------------------------

Given this structure, we can create a function called `get_store_info()` to scrape the variables of interest (i.e., `id`, `name`, `address`, `city`, `state`, `zip`, `country`, `phone`, `latitude`, and `longitude`) from each store URL.

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

With `get_store_info()` defined, we can use it to scrape the store info we're interested in using `store_info`. The `pblapply()` function will loop `get_store_info()` through the store URLs in `store_urls` and give us a progress bar so we can gauge how long the scraping will take (should be ~35 minutes).

``` r
store_info <- pblapply(store_urls, get_store_info)
```

Once that finishes, we're almost done! What's left is combining the rows of `store_info` into a tibble and doing some clean up. Some of the stores got scraped multiple times, which `distinct()` can handle.

``` r
dsg_stores <- do.call(rbind, store_info) %>% 
    as_tibble() %>% 
    distinct()
```

Finally, let's write the data to a .csv file called "dsg\_stores.csv" that can later be wrangled, transformed, visualized, and modeled.

``` r
write_csv(dsg_stores, here("data", "dsg_stores.csv"))
```
