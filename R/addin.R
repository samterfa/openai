
autocomplete_r_code <- function(prompt = rstudioapi::getConsoleEditorContext()$contents, debug = FALSE, reset = FALSE){
  
  suppressPackageStartupMessages(
    library(tidyverse)
  )
  
  if(debug) message(paste("Creating completion for", prompt))
  
  if(!exists('openai_completions') | reset){
    
    bonus_prompt <- paste(prompt, 'For the rest of this conversation, return R code only. Do not include anything else including extra characters or comments.')
    
    openai_completions <<-
      list(
        list(
          role = 'user', 
          content = bonus_prompt
        )
      )
    
    openai_completions_usage <<- nchar(bonus_prompt)
    
  }else{
    
    openai_completions <<- 
      append(
        openai_completions,
        list(
          list(
            role = 'user', 
            content = prompt
          )
        )
      )
    
    openai_completions_usage <<- 
      openai_completions_usage + 
      nchar(prompt)
  }
  
  total_request_chars <-
    openai_completions %>% purrr::keep(~ .x$role == 'user') %>% purrr::map_chr(~ .x$content) %>% paste(collapse = '') %>% nchar()
  
  completion <- 
    openai::create_chat_completion(
      messages = openai_completions, 
      model = 'gpt-3.5-turbo', 
      n = 1,
      max_tokens = 4096 - openai_completions_usage - 1, 
      stream = F)
  
  openai_completions <<-
    append(
      openai_completions,
      list(
        completion$choices[[1]]$message
      )
    )
  
  openai_completions_usage <<- completion$usage$total_tokens # Could be completion_tokens or prompt_tokens also.
  
  # completion <- 
  #   openai::create_completion(prompt = prompt, 
  #                             model = 'gpt-3.5-turbo', 
  #                             best_of = 1, 
  #                             top_p = 1,
  #                             frequency_penalty = 0,
  #                             presence_penalty = 0,
  #                             temperature = 1,
  #                             echo = T, 
  #                             max_tokens = 4096 - nchar(prompt), 
  #                             stream = F)$choices[[1]]$text

  cat("\014")
  rstudioapi::sendToConsole(code = paste0(prompt, completion$choices[[1]]$message$content) %>% 
                              stringr::str_remove('^\n') %>% stringr::str_remove_all('\\`\\`\\`\\{r\\}') %>% str_remove_all('\\`\\`\\`') %>% str_trim(side = 'left'))
  rstudioapi::sendToConsole("", execute = FALSE, echo = TRUE)
}
