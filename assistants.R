
library(tidyverse)
library(openai)

list_tools <- function(){
  
  tibble::tibble(
    type = c('code_interpreter', 'function', 'retrieval')
  )
}

create_function_parameters <- function(parameter, type)

create_function <- function(name, description, parameters){
  
  
  
}

make_session <- function(name = 'My Assistant', instructions = NULL, description = NULL, model = 'gpt-4-1106-preview', tools = list(list(type = 'retrieval'), list(type = 'code_interpreter')), initial_messages = NULL){
  
  assistant <- 
    openai::create_assistant(model = model, name = name, description = description, instructions = instructions, tools = tools)
  
  thread <- 
    openai::create_thread(messages = initial_messages)
  
  tibble::tibble(name = assistant$name, assistant_id = assistant$id, thread_id = thread$id)
}

use_assistant <- function(session = make_session(), new_message = NULL, additional_instructions = NULL, assistant = make_assistant(), thread = make_thread(), role = 'user', timeout_mins = 1){
  
  library(tidyverse)
  library(openai)
  
  prev_runs <-
    session %>% 
    select(any_of(c('role', 'content', 'created_at')))
  
  session <-
    session %>%
    select(-any_of(c('role', 'content', 'created_at'))) %>%
    distinct()
  
  assistant_id <- session$assistant_id[[1]]
  thread_id <- session$thread_id[[1]]
  
  if(!is.null(new_message)){
    
    msg <-
      create_message(content = new_message, thread_id = session$thread_id[[1]], role = role)
    
    run <- 
      create_run(assistant_id = assistant_id, thread_id = thread_id, instructions = additional_instructions)
    
    is_finished_or_timed_out <- FALSE
    tm <- Sys.time();
    while(!is_finished_or_timed_out){
      
      run_progress <- 
        retrieve_run(run_id = run$id, thread_id = thread_id)
      
      elapsed_mins <- (Sys.time() - tm)/lubridate::dminutes(1)
      
      is_finished_or_timed_out <- 
        elapsed_mins > timeout_mins |
        run_progress$status == 'completed'
      
      cat('.')
      Sys.sleep(1)
    }
  }else{
    run <- list(status = 'retrieving messages only')
  }
  
  msgs <- 
    list_messages(thread_id = thread_id)$data %>% 
    map_dfr(~ tibble(created_at = .x$created_at %>% lubridate::as_datetime(tz = 'America/Chicago'), 
                     role = .x$role, 
                     content = .x$content %>% keep(~ .x$type == 'text') %>% compact() %>% pluck(1) %>% pluck('text') %>% pluck('value'))) %>% 
    arrange(created_at)
  
  session <- 
    bind_cols(session, msgs)
  
  cat(session %>% slice_max(created_at) %>% pull(content))
  
  session
}



