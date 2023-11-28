
extract_r_code <- function(txt){
  
  r_code <-
    txt  %>% 
    stringr::str_replace_all("\n", "asdfdareasrads") %>% 
    stringr::str_extract("(?<=```).*(?=```)") %>% 
    stringr::str_remove("^[rR]") %>% 
    stringr::str_replace_all(fixed("asdfdareasrads"), "\n")
  
  comments <-
    txt  %>% 
    stringr::str_replace_all("\n", "asdfdareasrads") %>% 
    stringr::str_extract(".*(?=`*)") %>%
    stringr::str_replace_all(fixed("asdfdareasrads"), "\n")
  
  return(list(r_code = r_code, comments = comments))
}

extract_r_code2 <- function(txt){
  
  parsed_txt <- 
    tibble::tibble(txt = txt %>% stringr::str_split("```") %>% purrr::pluck(1), 
                   is_code = stringr::str_detect(txt, '^[rR]')) %>% 
    dplyr::mutate(txt = txt %>% stringr::str_remove_all("^[rR]") %>% stringr::str_remove_all("^\\n*") %>% stringr::str_remove_all("\\n*$"),
                  txt = ifelse(!is_code, paste0("# ", txt %>% stringr::str_replace_all("\n", "jfjfjfjfjfjfjfj")), txt)) %>%
    dplyr::filter(txt != '') %>%
    dplyr::pull(txt) %>%
    stringr::str_replace_all("jfjfjfjfjfjfjfj", "\n# ") %>%
    paste(collapse = '\n\n')
  
  parsed_txt
}


autocomplete_r_code <- function(prompt = rstudioapi::getConsoleEditorContext()$contents, debug = FALSE, reset = FALSE){
  
  library(dplyr)
  
  gpt_model <- ifelse(Sys.getenv('openai_addin_model') == '', 'gpt-3.5-turbo', Sys.getenv('openai_addin_model'))
  gpt_max_tokens <- ifelse(Sys.getenv('openai_addin_model') == '' || Sys.getenv('openai_addin_model_max_tokens') == '', 4096, as.numeric(Sys.getenv('openai_addin_model_max_tokens')))
  
  if(debug) message('Started...')
  
  if(debug) message(paste("Creating completion for", prompt))
  
  if(!exists('openai_completions') | reset || length(openai_completions) == 0){
    
    bonus_prompt <- paste('You are an RStudio coding assistant. All code you return will be run in RStudio. For the rest of this conversation, return R code chunks only. DO NOT install any packages but assume I already have them installed.', prompt)
    
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
    
    openai_completions_usage <<- openai_completions_usage + nchar(prompt)
  }
  
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
  
  if(resp$status_code < 300){
    
    completion <- httr::content(resp)
    
    openai_completions <<-
      append(
        openai_completions,
        list(
          completion$choices[[1]]$message
        )
      )
    
    openai_completions_usage <<- completion$usage$total_tokens
    
    cat("\014")
    
    #  rstudioapi::sendToConsole(prompt, execute = TRUE)
    
    # if(stringr::str_detect(completion$choices[[1]]$message$content %>% stringr::str_remove_all("[rR]\n"), "(?<=```[rR]).*(?=```)")){
    #   # to_run <-
    #   #   paste0("#", prompt, "\n",
    #   #          completion$choices[[1]]$message$content %>% 
    #   #            stringr::str_remove("[rR]\n") %>%
    #   #            stringr::str_extract("(?<=```).*(?=```)"))
    #  
    # }else{
    #   
    #   # to_run <-
    #   #   paste0("#", prompt, "\n",
    #   #          completion$choices[[1]]$message$content %>%
    #   #            stringr::str_remove('R\n') %>%
    #   #            stringr::str_remove_all('\\`\\`\\`\\{r\\}') %>%
    #   #            stringr::str_remove_all('\\`\\`\\`') %>%
    #   #            stringr::str_trim(side = 'left')
    #   #   )
    # }
    
    to_run <-
      paste0("# ", prompt, "\n",
             extract_r_code2(completion$choices[[1]]$message$content))
    
    rstudioapi::sendToConsole(code = to_run, 
                              execute = TRUE, 
                              echo = TRUE)
    # rstudioapi::sendToConsole("", execute = FALSE, echo = TRUE)
  }else{
    
    # If an error is received, remove the last prompt from the completions chain.
    openai_completions <<-
      openai_completions %>% 
      utils::head(-1)
    
    # If an error is received, subtract the last estimate of usage added.
    openai_completions_usage <<- 
      openai_completions_usage - 
      nchar(prompt)
    
    stop(httr::content(resp))
  }
  
  invisible()
}


stream_autocompletion_testing <- function(prompt, stream_buffer = .2){
  
  if(!require('httr2', quietly = TRUE)) stop('You must install the "httr2" package to use this addin.')
  
  gpt_model <- ifelse(Sys.getenv('openai_addin_model') == '', 'gpt-3.5-turbo', Sys.getenv('openai_addin_model'))
  gpt_max_tokens <- ifelse(Sys.getenv('openai_addin_model') == '' || Sys.getenv('openai_addin_model_max_tokens') == '', 4096, as.numeric(Sys.getenv('openai_addin_model_max_tokens')))
  
  chunk_txt <- ''
  processed_rows <- integer()
  
  callback <- function(x){
    
    chunk_txt <<- paste0(chunk_txt, rawToChar(x))
    
    chunk_df <<- chunk_txt %>% stringr::str_extract_all('data: .*]\\}\n\n') %>% purrr::pluck(1) %>% stringr::str_remove('^data: ') %>% purrr::map(jsonlite::fromJSON) %>% dplyr::bind_rows() %>% dplyr::mutate(row = dplyr::row_number())
    
    to_process <- chunk_df %>% dplyr::filter(!row %in% processed_rows)
    
    processed_rows <<- chunk_df %>% dplyr::pull(row)
    
    if('choices' %in% names(to_process)) to_process <- to_process %>% tidyr::unnest(choices)
    if('delta' %in% names(to_process)) to_process <- to_process %>% tidyr::unnest(delta)
    if('content' %in% names(to_process)) cat(to_process$content %>% stats::na.omit() %>% paste(collapse = ''))
    
    TRUE
  }
  
  cat("\014")
  
  messages <- list(list(role = 'user', content = prompt))
  messages_char_count <- messages %>% jsonlite::toJSON() %>% nchar()
  
  results <- 
    httr2::request('https://api.openai.com/v1/chat/completions') %>% 
    httr2::req_body_json(list(messages = messages, 
                              max_tokens = gpt_max_tokens - messages_char_count, 
                              model = gpt_model, 
                              stream = TRUE)) %>% 
    httr2::req_auth_bearer_token(token = Sys.getenv('openai_secret_key')) %>% 
    httr2::req_stream(callback, buffer_kb = stream_buffer)
  
  suppressWarnings({
    rm(chunk_txt)
    rm(chunk_df)
    rm(processed_rows)
  })
  
  invisible()
}

gpt_voice_command <- function(time_out = 20, sample_rate = 8000, file_path = tempfile(fileext = '.wav'), auto_execute = FALSE, debug = FALSE, use_voice_detection = TRUE){
  
  gpt_model <- ifelse(Sys.getenv('openai_addin_model') == '', 'gpt-3.5-turbo', Sys.getenv('openai_addin_model'))
  gpt_max_tokens <- ifelse(Sys.getenv('openai_addin_model') == '' || Sys.getenv('openai_addin_model_max_tokens') == '', 4096, as.numeric(Sys.getenv('openai_addin_model_max_tokens')))
  
  if(!require('audio', quietly = TRUE)) stop('You must install the "audio" package to use this addin.')
  
  file_path <- 
    path.expand(file_path)
  
  x <- rep(NA_real_, sample_rate * time_out)
  
  cat("\014")
  
  if(use_voice_detection){
    message("Listening...")
    x <- detect_talking(play_back = FALSE)
  }else{
    
    message("Listening. Press return when finished.")
    
    audio::record(x, sample_rate, 1)
    
    readline('')
  }
  
  cat("\014")
  
  message("Waiting for model...")
  
  audio::save.wave(what = x %>% stats::na.omit(), 
                   where = file_path)
  
  voice_text <-
    openai::create_transcription(file_path, 'whisper-1')$text
  
  if(auto_execute){
    autocomplete_r_code(prompt = voice_text, debug = FALSE)
  }else{
    stream_autocompletion_testing(voice_text)
  }
  
  invisible()
}

gpt_voice_command_exec <- function(time_out = 20, sample_rate = 8000, file_path = tempfile(fileext = '.wav'), auto_execute = TRUE, debug = FALSE, use_voice_detection = TRUE){
  
  if(!require('audio', quietly = TRUE)) stop('You must install the "audio" package to use this addin.')
  
  file_path <- 
    path.expand(file_path)
  
  x <- rep(NA_real_, sample_rate * time_out)
  
  cat("\014")
  
  if(use_voice_detection){
    message("Listening...")
    x <- detect_talking(play_back = FALSE)
  }else{
    
    message("Listening. Press return when finished.")
    
    recording <- 
      audio::record(x, sample_rate, 1)
    
    on.exit({
      audio::pause(recording)
      audio::close.audioInstance(recording)
    })
    
    readline('')
    
    audio::pause(recording)
    audio::close.audioInstance(recording)
  }
  
  cat("\014")
  
  message("Waiting for model...")
  
  audio::save.wave(what = x %>% stats::na.omit(), 
                   where = file_path)
  
  voice_text <-
    openai::create_transcription(file_path, 'whisper-1')$text
  
  if(auto_execute){
    autocomplete_r_code(prompt = voice_text, debug = FALSE)
  }else{
    stream_autocompletion_testing(voice_text)
  }
  
  invisible()
}

detect_talking <- function(play_back = FALSE){
  
  rt <- 8000
  dt <- .1 
  timeout <- 20
  ambient_time <- .3
  compare_time <- 1 # seconds
  compare_offset <- 1 # seconds -- currently not used
  wait_time <- 1
  sound_decrease <- .5
  
  x <- rep(NA_real_, timeout * rt)
  # start recording into x
  recording <- audio::record(x, rt, 1)
  
  on.exit({
    audio::pause(recording)
    audio::close.audioInstance(recording)
  })
  
  # Compare previous compare_time * rt entries of audio to the most recent compare_time * rt entries of audio data.
  # End recording automatically if most recent compare_time * rt entries of audio is 10% of previous compare_time * rt entries of audio "volume".
  while(is.na(x[length(x)])){
    if((x %>% stats::na.omit() %>% length()) > (compare_time * rt)){
      
      ambient_sound <-
        mean((abs(x) %>% stats::na.omit())[1:(ambient_time * rt)])
      
      if(sound_decrease * (mean(abs(x) %>% stats::na.omit() %>% utils::tail(2 * compare_time * rt) %>% utils::head(compare_time * rt)) - ambient_sound) > (mean(abs(x) %>% stats::na.omit() %>% utils::tail(compare_time * rt)) - ambient_sound)){
        
        Sys.sleep(wait_time)
        
        # If it's still quiet...
        if(sound_decrease * (mean(abs(x) %>% stats::na.omit() %>% utils::tail(2 * (compare_time + wait_time) * rt) %>% utils::head(compare_time * rt)) - ambient_sound) > (mean(abs(x) %>% stats::na.omit() %>% utils::tail((compare_time + wait_time) * rt) %>% utils::head(compare_time * rt)) - ambient_sound)){
          break;
        }
      }
    }
    Sys.sleep(dt)
  }
  
  # If ending early
  x <- x %>% stats::na.omit()
  
  # play the recorded audio for testing.
  if(play_back) audio::play(x)
  
  x
}
