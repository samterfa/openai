
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openai

<!-- badges: start -->
<!-- badges: end -->

openai provides R scripts to use the [Open AI
API](https://beta.openai.com/docs/api-reference/)

## Installation

You can install the development version of openai from
[GitHub](https://github.com/samterfa/openai) with:

``` r
# install.packages("devtools")
devtools::install_github("samterfa/openai")
```

## Set Up

API calls are authenticated using your [API
keys](https://beta.openai.com/account/api-keys). The environment
variables set below are consulted while making each call.

``` r
Sys.setenv(openai_organization_id = {your_organization_id})
Sys.setenv(openai_secret_key = {your_secret_key})
```

More details on authentication can be found
[here](https://platform.openai.com/docs/api-reference/authentication).

## Examples

Retrieve the currently available models used to generate text.

``` r
library(openai)
library(purrr)

list_models() %>% 
  pluck('data') %>% 
  map_dfr(compact)
#> # A tibble: 70 × 7
#>    id                          object created owned_by permission   root  parent
#>    <chr>                       <chr>    <int> <chr>    <list>       <chr> <chr> 
#>  1 babbage                     model   1.65e9 openai   <named list> babb… <NA>  
#>  2 davinci                     model   1.65e9 openai   <named list> davi… <NA>  
#>  3 babbage-code-search-code    model   1.65e9 openai-… <named list> babb… <NA>  
#>  4 text-similarity-babbage-001 model   1.65e9 openai-… <named list> text… <NA>  
#>  5 text-davinci-001            model   1.65e9 openai   <named list> text… <NA>  
#>  6 ada                         model   1.65e9 openai   <named list> ada   <NA>  
#>  7 curie-instruct-beta         model   1.65e9 openai   <named list> curi… <NA>  
#>  8 babbage-code-search-text    model   1.65e9 openai-… <named list> babb… <NA>  
#>  9 babbage-similarity          model   1.65e9 openai-… <named list> babb… <NA>  
#> 10 gpt-3.5-turbo               model   1.68e9 openai   <named list> gpt-… <NA>  
#> # … with 60 more rows
```

Create a completion request using the davinci engine.

``` r
create_completion(
  model = 'davinci', 
  max_tokens = 5,
  temperature = 1,
  top_p = 1,
  n = 1,
  stream = F, 
  prompt = 'Once upon a time') %>% 
  pluck('choices') %>% 
  map_chr(~ .x$text)
#> [1] ", a million years ago"
```

Generate an image based on a prompt.

``` r
create_image(
  prompt = "A rollerskating zebra", 
  n = 1, 
  response_format = "url")
#> $created
#> [1] 1679237401
#> 
#> $data
#> $data[[1]]
#> $data[[1]]$url
#> [1] "https://oaidalleapiprodscus.blob.core.windows.net/private/org-nKKiUxRVJQl2MhzgM9gtTsko/user-uQ6jdzskUi7KqutVEN82ZpLB/img-OrB7bqvviivitTuuF2oM1KMO.png?st=2023-03-19T13%3A50%3A01Z&se=2023-03-19T15%3A50%3A01Z&sp=r&sv=2021-08-06&sr=b&rscd=inline&rsct=image/png&skoid=6aaadede-4fb3-4698-a8f6-684d7786b067&sktid=a48cca56-e6da-484e-a814-9c849652bcb3&skt=2023-03-19T14%3A34%3A41Z&ske=2023-03-20T14%3A34%3A41Z&sks=b&skv=2021-08-06&sig=CORwrEynGrkfYA2WK9PLoNAbI8%2BBkRSoXaBiGt8frRk%3D"
```

Use the included addin to code collaboratively with a model.

![View Demo](https://youtu.be/8jGkLRZ7KDw)
