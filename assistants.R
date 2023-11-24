
library(tidyverse)
library(openai)

list_tools <- function(){
  
  tibble::tibble(
    type = c('code_interpreter', 'function', 'retrieval')
  )
}

run_code_screen_capture <- function(code_to_run){
  
  tmpfile <- tempfile(fileext = '.png')
  # 
  # p <-
  #   promises::future_promise(
  #     expr = {
  #       Sys.sleep(4); 
  #       screenshot::screenshot(file = tmpfile)
  #     })
  
  # Clear Screen and run the code
  rstudioapi::sendToConsole('cat("\\014")', execute = TRUE, focus = TRUE)
  rstudioapi::sendToConsole(code_to_run, execute = TRUE, focus = TRUE)
  Sys.sleep(2)
  screenshot::screenshot(file = tmpfile)
  
  #$ promises::promise_all(p)
  
  tmpfile
}


create_function_parameters <- function(parameter, type){
  
  
}

create_run_code_function <- function(){
  
  '{
      "type": "function",
      "function": {
        "name": "run_r_code",
        "description": "Run R code in RStudio",
        "parameters": {
          "type": "object",
          "properties": {
            "code": {
              "type": "string",
              "description": "The code to run in RStudio"
            }
          },
          "required": [
            "code"
          ]
        }
      }
    }' %>% 
    stringr::str_remove_all("\n") %>% 
    stringr::str_squish() %>% 
    jsonlite::fromJSON(F, F, F, F)
  
}

make_session <- function(name = 'My Assistant', instructions = NULL, description = NULL, model = 'gpt-4-1106-preview', tools = list(list(type = 'retrieval'), list(type = 'code_interpreter'), create_run_code_function()), initial_messages = NULL, file_ids = NULL){
  
  assistant <- 
    openai::create_assistant(model = model, name = name, description = description, instructions = instructions, tools = tools, file_ids = file_ids)
  
  if(!is.null(file_ids)){
    for(file_id in file_ids){
      create_assistant_file(assistant_id = assistant$id, file_id = file_id)
    }
  }
  
  thread <- 
    openai::create_thread(messages = initial_messages)
  
  tibble::tibble(name = assistant$name, assistant_id = assistant$id, thread_id = thread$id)
}

use_assistant <- function(session = make_session(), new_message = NULL, additional_instructions = NULL, assistant = make_assistant(), thread = make_thread(), file_ids = NULL, role = 'user', timeout_mins = 3){
  
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
    
    potential_error <-
      tryCatch({
        
        msg <-
          create_message(content = new_message, thread_id = session$thread_id[[1]], role = role, file_ids = file_ids)
        
        print(msg)
        
        run <- 
          create_run(assistant_id = assistant_id, thread_id = thread_id, instructions = additional_instructions)
        
        run
      }, error = function(e) e)
    
    if('error' %in% class(potential_error)){
      
      stop("There was an error!")
      
      runs <-
        list_runs(thread_id = session$thread_id)
      
      # Pick up where we left off.
      run <-
        runs$data %>% discard(~ .x$status %in% 'expired') %>% list_flatten()
      
    }else{
      
      run <- potential_error
      
    }
    
    is_finished_or_timed_out <- run$status == 'completed'
    tm <- Sys.time();
    while(!is_finished_or_timed_out){
      
      #  cat('.')
      Sys.sleep(1)
      
      run_progress <- 
        retrieve_run(run_id = run$id, thread_id = thread_id)
      
      elapsed_mins <- (Sys.time() - tm)/lubridate::dminutes(1)
      
      if(run_progress$status == 'requires_action'){
        
        required_action_details <-
          run_progress$required_action$submit_tool_outputs %>% 
          map_dfr(~ .x %>% list_flatten() %>% as.data.frame() %>% tibble() %>% janitor::clean_names())
        
        code_to_run <-
          required_action_details %>% pull(function_arguments) %>% jsonlite::fromJSON() %>% pluck('code')
        
        if(nrow(required_action_details) > 1) stop("Cannot handle multiple required actions")
        
        submit_tool_outputs_to_run(run_id = run_progress$id, thread_id = session$thread_id, tool_outputs = list(list(tool_call_id = required_action_details$id, output = run_r_code(code_to_run))))
      }
      
      is_finished_or_timed_out <- 
        elapsed_mins > timeout_mins |
        run_progress$status == 'completed'
    }  
  }else{
    run_progress <- list(status = 'retrieving messages only')
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

use_assistant_screenshot <- function(session = make_session(), new_message = NULL, additional_instructions = NULL, assistant = make_assistant(), thread = make_thread(), file_ids = NULL, role = 'user', timeout_mins = 3){
  
  stop("GPT Vision does not work with assistants. Models cannot see screenshots.")
  
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
    
    potential_error <-
      tryCatch({
        
        msg <-
          create_message(content = new_message, thread_id = session$thread_id[[1]], role = role, file_ids = file_ids)
        
        print(msg)
        
        run <- 
          create_run(assistant_id = assistant_id, thread_id = thread_id, instructions = additional_instructions)
        
        run
      }, error = function(e) e)
    
    if('error' %in% class(potential_error)){
      
      print(potential_error)
      
      runs <-
        list_runs(thread_id = session$thread_id)
      
      # Pick up where we left off.
      run <-
        runs$data %>% discard(~ .x$status %in% 'expired') %>% list_flatten()
      
    }else{
      
      run <- potential_error
      
    }
    
    is_finished_or_timed_out <- run$status == 'completed'
    tm <- Sys.time();
    while(!is_finished_or_timed_out){
      
      #  cat('.')
      Sys.sleep(1)
      
      run_progress <- 
        retrieve_run(run_id = run$id, thread_id = thread_id)
      
      elapsed_mins <- (Sys.time() - tm)/lubridate::dminutes(1)
      
      if(run_progress$status == 'requires_action'){
        
        required_action_details <-
          run_progress$required_action$submit_tool_outputs %>% 
          map_dfr(~ .x %>% list_flatten() %>% as.data.frame() %>% tibble() %>% janitor::clean_names())
        
        code_to_run <-
          required_action_details %>% pull(function_arguments) %>% jsonlite::fromJSON() %>% pluck('code')
        
        if(nrow(required_action_details) > 1) stop("Cannot handle multiple required actions")
        
        tmpfile <- run_code_screen_capture(code_to_run)
        
        file <- upload_file(tmpfile, purpose = 'assistants')
        
        submit_tool_outputs_to_run(run_id = run_progress$id, thread_id = session$thread_id, tool_outputs = list(list(tool_call_id = required_action_details$id, output = glue::glue("The following file contains a screenshot for the output of the code you ran. file_id: {file$id}"))))
      }
      
      is_finished_or_timed_out <- 
        elapsed_mins > timeout_mins |
        run_progress$status == 'completed'
    }  
  }else{
    run_progress <- list(status = 'retrieving messages only')
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


vision_prompting <- function(iter = 5){
  
  model <- 'gpt-4-vision-preview'
  
  starting_image <-
    '~/Desktop/Screen Shot 2023-11-21 at 8.03.57 PM.png' %>%
    base64enc::base64encode()
  
  messages <-
    list(
      list(
        role = 'system',
        content = 'You are an R code generator. The user will run your code in an RStudio session and return to you a screenshot showing you the results of running your code. Only return R code.'
      ),
      list(
        role = 'user',
        content = list(
          list(
            type = 'text',
            text = 'Recreate and iteratively improve this plot until it is publication ready.'
          ),
          list(
            type = 'image_url',
            image_url = list(
              url = glue::glue("data:image/jpeg;base64,{starting_image}")
            )
          )
        )
      )
    )
  
  for(i in 1:iter){
    
    completion <-
      create_chat_completion(messages = messages, model = model, max_tokens = 300)
    
    return_message <- 
      completion$choices[[1]]$message
    
    messages <-
      messages %>%
      append(list(return_message))
    
    code_to_run <-
      return_message$content %>%
      extract_r_code()
    
    if(is.na(code_to_run)) break()
    
    tmpfile <-
      run_code_screen_capture(code_to_run)
    
    new_message <-
      list(
        role = 'user',
        content = list(
          list(
            type = 'text',
            text = 'Here is a screenshot of your RStudio session.'
          ),
          list(
            type = 'image_url',
            image_url = list(
              url = glue::glue("data:image/jpeg;base64,{base64enc::base64encode(tmpfile)}")
            )
          )
        )
      )
    
    messages <-
      messages %>%
      append(list(new_message))
  }
  
  messages
}





extract_r_code <- function(txt){
  txt  %>% 
    stringr::str_replace_all("\n", "asdfdareasrads") %>% 
    stringr::str_extract("(?<=```).*(?=```)") %>% 
    str_remove("^[rR]") %>% 
    stringr::str_replace_all(fixed("asdfdareasrads"), "\n")
}









run_r_code <- function(code_to_run){
  
  suppressMessages(
    suppressWarnings(
      reprex::reprex(input = paste0("suppressMessages({x <- readr::read_csv('~/Desktop/data.csv')}); ", code_to_run %>% str_replace_all("\n", "; "), "\n"), 
                     session_info = FALSE, 
                     advertise = FALSE, 
                     tidyverse_quiet = TRUE,
                     html_preview = FALSE) %>% 
        paste(collapse = '\n') %>%
        str_remove(fixed("suppressMessages({x <- readr::read_csv('~/Desktop/data.csv')}); "))
    )
  )
}



