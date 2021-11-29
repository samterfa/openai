
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

Access to the API is still limited and must be
[requested](https://openai.com/join/). Once granted, API calls are
authenticated using your [API
keys](https://beta.openai.com/account/api-keys). The environment
variables set below are consulted while making each call.

``` r
Sys.setenv(openai_organization_id = {your_organization_id})
Sys.setenv(openai_secret_key = {your_secret_key})
```

More details on authentication can be found
[here](https://beta.openai.com/docs/api-reference/authentication).

## Examples

Retrieve the currently available engines used to generate text.

``` r
library(openai)
library(purrr)

list_engines() %>% 
  pluck('data') %>% 
  map_dfr(compact)
#> # A tibble: 7 × 4
#>   id                       object owner  ready
#>   <chr>                    <chr>  <chr>  <lgl>
#> 1 ada                      engine openai TRUE 
#> 2 babbage                  engine openai TRUE 
#> 3 curie                    engine openai TRUE 
#> 4 curie-instruct-beta      engine openai TRUE 
#> 5 davinci                  engine openai TRUE 
#> 6 davinci-instruct-beta    engine openai TRUE 
#> 7 davinci-instruct-beta-v3 engine openai TRUE
```

Create a completion request using the ada engine.

``` r
create_completion(
  engine_id = 'davinci', 
  max_tokens = 5,
  temperature = 1,
  top_p = 1,
  n = 1,
  stream = F, 
  prompt = 'Once upon a time') %>% 
  pluck('choices') %>% 
  map_chr(~ .x$text)
#> [1] " we turned $200 into"
```

Classify a query based on provided examples.

``` r
create_classification(
  query = "It is a rainy day :(", 
  examples = 
    list(
      list("A happy moment", "Positive"), 
      list("I am sad.", "Negative"), 
      list("I am feeling awesome", "Positive")), 
  labels = c('Positive', 'Negative', 'Neutral'), 
  search_model = 'ada', 
  model = 'curie') %>%
  pluck('label')
#> [1] "Negative"
```

Answer a question based on context and provided documents.

``` r
create_answer(
  documents = list("Puppy A is happy.", "Puppy B is sad."),
  question = "which puppy is happy?",
  examples_context = "In 2017, U.S. life expectancy was 78.6 years.",
  examples = list(list("What is human life expectancy in the United States?","78 years.")),
  search_model = 'ada', 
  model = 'curie',
  max_tokens = 5
) %>%
  pluck('answers') %>%
  pluck(1)
#> [1] "puppy A."
```
