
autocomplete_r_code <- function(prompt = rstudioapi::getConsoleEditorContext()$contents, debug = FALSE, reset = FALSE){
  
  gpt_model <- ifelse(Sys.getenv('openai_addin_model') == '', 'gpt-3.5-turbo', Sys.getenv('openai_addin_model'))
  gpt_max_tokens <- ifelse(Sys.getenv('openai_addin_model') == '' || Sys.getenv('openai_addin_model_max_tokens') == '', 4096, as.numeric(Sys.getenv('openai_addin_model_max_tokens')))
  
  if(debug) message('Started...')
  
  suppressPackageStartupMessages(
    library(tidyverse)
  )
  
  if(debug) message(paste("Creating completion for", prompt))
  
  if(!exists('openai_completions') | reset){
    
    bonus_prompt <- paste('You are an R coding assistant.', prompt, 'For the rest of this conversation, return R code only. Do not include anything else such as extra characters or comments.')
    
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
  
  if(debug) message('Making request...')
  
  resp <- 
    openai::create_chat_completion(
      messages = openai_completions, 
      model = gpt_model, 
      n = 1,
      max_tokens = gpt_max_tokens - openai_completions_usage - 1, 
      stream = F, 
      return_response = TRUE)
  
  if(debug) message('Finished request...')
  
  if(resp$status_code < 400){
    
    completion <- httr::content(resp)
    
    openai_completions <<-
      append(
        openai_completions,
        list(
          completion$choices[[1]]$message
        )
      )
    
    openai_completions_usage <<- completion$usage$total_tokens # Could be completion_tokens or prompt_tokens also.
    
    cat("\014")
    
    # code = prompt, ...
    rstudioapi::sendToConsole(code = paste0(completion$choices[[1]]$message$content) %>% 
                                stringr::str_remove('^\n') %>% stringr::str_remove('^R') %>% stringr::str_remove_all('\\`\\`\\`\\{r\\}') %>% str_remove_all('\\`\\`\\`') %>% str_trim(side = 'left'))
    rstudioapi::sendToConsole("", execute = FALSE, echo = TRUE)
  }else{
    
    openai_completions <<-
      openai_completions %>% 
      head(-1)
    
    openai_completions_usage <<- 
      openai_completions_usage - 
      nchar(prompt)
    
    stop(httr::content(resp))
  }
  
  invisible()
}


stream_autocompletion_testing <- function(prompt, max_tokens = 8000, stream_buffer = .2){
  
  chunk_txt <- ''
  processed_rows <- integer()
  
  callback <- function(x){
    
    chunk_txt <<- paste0(chunk_txt, rawToChar(x))
    
    chunk_df <<- chunk_txt %>% stringr::str_extract_all('data: .*]\\}\n\n') %>% purrr::pluck(1) %>% stringr::str_remove('^data: ') %>% purrr::map(jsonlite::fromJSON) %>% dplyr::bind_rows() %>% dplyr::mutate(row = dplyr::row_number())
    
    to_process <- chunk_df %>% dplyr::filter(!row %in% processed_rows)
    
    processed_rows <<- chunk_df %>% dplyr::pull(row)
    
    if('choices' %in% names(to_process)) to_process <- to_process %>% tidyr::unnest(choices)
    if('delta' %in% names(to_process)) to_process <- to_process %>% tidyr::unnest(delta)
    if('content' %in% names(to_process)) cat(to_process$content %>% na.omit() %>% paste(collapse = ''))
    
    TRUE
  }
  
  cat("\014")
  
  results <- 
    httr2::request('https://api.openai.com/v1/chat/completions') %>% 
    httr2::req_body_json(list(messages = 
                                list(
                                  list(role = 'user', 
                                       content = prompt
                                  )
                                ), 
                              max_tokens = max_tokens, 
                              model = gpt_model, 
                              stream = TRUE)) %>% 
    httr2::req_auth_bearer_token(token = Sys.getenv('openai_secret_key')) %>% 
    httr2::req_stream(callback, buffer_kb = stream_buffer)
  
  invisible()
}

gpt_voice_command <- function(time_out = 10, sample_rate = 8000, file_path = tempfile(fileext = '.wav'), auto_execute = FALSE, debug = FALSE){
  
  if(!require('audio', quietly = TRUE)) stop('You must install the "audio" package to use this addin.')
  
  file_path <- 
    path.expand(file_path)
  
  x <- rep(NA_real_, sample_rate * time_out)
  
  cat("\014")
  
  message("Listening. Press return when finished.")
  
  audio::record(x, sample_rate, 1)
  
  R.utils::withTimeout(readline(''), timeout = time_out, onTimeout = "silent")
  
  cat("\014")
  
  audio::save.wave(what = x %>% na.omit(), 
                   where = file_path)
  
  voice_text <-
    openai::create_transcription(file_path, 'whisper-1')$text
  
  ###### stream_autocompletion_testing(voice_text)
  
  autocomplete_r_code(prompt = voice_text, debug = FALSE)
  
  invisible()
}
