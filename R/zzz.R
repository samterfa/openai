
check_authentication <- function(){
  if(Sys.getenv("openai_secret_key") == '') stop('Must set env var "openai_secret_key"')
}

parse_response <- function(response){
  
  response_content <- httr::content(response)
  
  if('error' %in% names(response_content)) stop(response_content$error$message)
  
  if(response$status_code >= 300) stop(response_content)
  
  response_content
}

# Download html manually from https://beta.openai.com/docs/api-reference/introduction
parse_endpoints <- function(documentation_path = '~/API Reference - OpenAI API.html'){
  
  endpoints <-
    rvest::read_html(documentation_path) %>% 
    rvest::html_elements('.endpoint')
  
  endpoints_df <- tibble::tibble()
  
  for(endpoint in endpoints){
    
    endpoint_df <- tibble::tibble()
    
    endpoint_documentation <-
      endpoint %>%
      rvest::html_elements('a') %>%
      rvest::html_attr('href') %>%
      purrr::pluck(1)
    
    endpoint_name <- 
      endpoint %>%
      rvest::html_elements('a h2') %>%
      rvest::html_text() %>%
      stringr::str_replace_all('Beta$', ' - Beta') %>%
      stringr::str_to_title()
    
    endpoint_method <-
      endpoint %>%
      rvest::html_elements('.endpoint-method') %>%
      rvest::html_text() %>%
      toupper()
    
    endpoint_url <- 
      endpoint %>%
      rvest::html_elements('.endpoint-path') %>%
      rvest::html_text()
    
    endpoint_description <-
      endpoint %>%
      rvest::html_elements('p') %>%
      rvest::html_text() %>%
      purrr::pluck(2) %>%
      glue::glue_collapse('\n')
    
    endpoint_df <-
      endpoint_df %>%
      dplyr::bind_rows(
        tibble::tibble(endpoint_name = endpoint_name,
                       endpoint_method = endpoint_method,
                       endpoint_url = endpoint_url,
                       endpoint_description = endpoint_description,
                       endpoint_documentation = endpoint_documentation)
      )
    
    endpoint_params_sections <-
      endpoint %>%
      rvest::html_elements('.param-section')
    
    params_section_df <- tibble::tibble()
    
    for(params_section in endpoint_params_sections){
      
      params_section_title <-
        params_section %>%
        rvest::html_elements('h3') %>%
        rvest::html_text()
      
      params_section_params <-
        params_section %>%
        rvest::html_elements(".param-row")
      ####     rvest::html_elements('.api-ref-anchor-link-hover')
      
      params_df <- tibble::tibble()
      
      for(param in params_section_params){
        
        param_name <-
          param %>% 
          rvest::html_element('.param-name') %>%
          rvest::html_text()
        
        param_data_type <-
          param %>% 
          rvest::html_element('.param-type') %>%
          rvest::html_text()
        
        param_required <-
          param %>%
          rvest::html_element('.param-reqd') %>%
          rvest::html_text()
        
        if(is.na(param_required)){
          param_required <- FALSE
        }else{
          if(param_required == 'Required'){
            param_required <- TRUE
          }else{
            param_required <- FALSE
          }
        }
        
        param_description <-
          param %>%
          rvest::html_elements('p') %>%
          rvest::html_text() %>%
          glue::glue_collapse(" ")
        
        if(params_section_title %>% tolower() %>% stringr::str_detect('path')){
          param_type = 'path'
        }
        
        if(params_section_title %>% tolower() %>% stringr::str_detect('body')){
          param_type = 'body'
        }
        
        if(params_section_title %>% tolower() %>% stringr::str_detect('query')){
          param_type = 'query'
        }
        
        params_df <-
          dplyr::bind_rows(
            params_df,
            tibble::tibble(
              param_name = param_name,
              param_type = param_type,
              param_data_type = param_data_type,
              param_required = param_required,
              param_description = param_description
            )
          )
      }
      
      params_section_df <-
        dplyr::bind_rows(
          params_section_df,
          params_df
        )
    }
    
    
    if(nrow(params_section_df) > 0){
      endpoint_df <-
        endpoint_df %>%
        dplyr::bind_cols(
          params_section_df
        )
    }
    
    endpoints_df <-
      endpoints_df %>%
      dplyr::bind_rows(
        endpoint_df
      )
  }
  
  endpoints_df <-
    endpoints_df %>%
    dplyr::mutate(dplyr::across(param_required, ~ ifelse(is.na(param_required), F, param_required)))
}

generate_functions <- function(doc_path = '~/API Reference - OpenAI API.html', endpoints_df = parse_endpoints(doc_path), output_path = 'R/functions.R'){
  
  base_url <- 'https://api.openai.com/v1'
  
  functions_df <-
    endpoints_df %>%
    dplyr::mutate(function_name = 
                    endpoint_name %>% 
                    snakecase::to_snake_case() %>% 
                    stringr::str_remove_all('_beta')) %>%
    dplyr::relocate(function_name, .before = 1) 
  
  functions_df_sub <-
    functions_df %>% 
    dplyr::select(tidyselect::starts_with(c('function', 'endpoint'))) %>%
    dplyr::distinct()
  
  function_text <- "\n"
  
  # For each unique endpoint
  for(i in 1:nrow(functions_df_sub)){
    
    function_name <- functions_df_sub$function_name[[i]]
    endpoint_name <- functions_df_sub$endpoint_name[[i]]
    endpoint_method <- functions_df_sub$endpoint_method[[i]]
    endpoint_url <- functions_df_sub$endpoint_url[[i]]
    endpoint_description <- functions_df_sub$endpoint_description[[i]]
    endpoint_documentation <- functions_df_sub$endpoint_documentation[[i]]
    
    # Add Function Title
    function_text <-
      paste0(function_text, 
             glue::glue("\n#' {endpoint_name}", .trim = F))
    
    function_text <-
      paste0(function_text, 
             glue::glue("\n#'", .trim = F))
    
    # Add Function Description
    function_text <-
      paste0(function_text, 
             glue::glue("\n#' {endpoint_description}", .trim = F))
    
    function_text <-
      paste0(function_text, 
             glue::glue("\n#'\n", .trim = F))
    
    functions_df_params <- 
      functions_df %>% 
      dplyr::filter(function_name == !!function_name, !is.na(param_name)) %>% 
      dplyr::arrange(function_name, -param_required, param_name)
    
    # Add param info to documentation
    if(nrow(functions_df_params) > 0){
      
      for(j in 1:nrow(functions_df_params %>% dplyr::filter(!is.na(param_name)))){
        
        param_name <- functions_df_params$param_name[[j]]
        param_type <- functions_df_params$param_type[[j]]
        param_data_type <- functions_df_params$param_data_type[[j]]
        param_required <- functions_df_params$param_required[[j]]
        param_description <- functions_df_params$param_description[[j]] %>% stringr::str_replace_all('\n', ' ')
        
        # Add parameters to documentation text
        function_text <-
          paste0(function_text,
                 glue::glue("#' @param {param_name} ({param_data_type}) {param_description} {ifelse(param_required, 'Required', '')}\n", .trim = F))
        
      }
    }
    
    # Wrap up documentation text
    function_text <-
      paste0(function_text,
             "#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).\n")
    
    function_text <-
      paste0(function_text,
             glue::glue("#' @seealso \\href{>>>endpoint_documentation<<<}{Open AI Documentation}\n", .trim = F, .open = '>>>', .close = '<<<'))
    
    function_text <-
      paste0(function_text,
             "#' @export\n")
    
    # Define function
    function_text <-
      paste0(function_text, 
             glue::glue("{function_name} <- function(", .trim = F))
    
    # Add param info to function definition
    if(nrow(functions_df_params) > 0){
      
      for(j in 1:nrow(functions_df_params %>% dplyr::filter(!is.na(param_name)))){
        
        param_name <- functions_df_params$param_name[[j]]
        param_type <- functions_df_params$param_type[[j]]
        param_data_type <- functions_df_params$param_data_type[[j]]
        param_required <- functions_df_params$param_required[[j]]
        param_description <- functions_df_params$param_description[[j]]
        
        # Add parameters to function definition
        function_text <-
          paste0(function_text, 
                 ifelse(param_required,
                        glue::glue("{param_name}, "),
                        glue::glue("{param_name} = NULL, ")
                 )
          )
      }
    }
    
    function_text <-
      paste0(function_text, 
             'return_response = F){\n\n'
      )
    
    # Fill out function definition
    function_text <- 
      paste0(function_text, 
             "\tcheck_authentication()\n\n")
    
    function_text <- 
      paste0(function_text, 
             glue::glue("\tendpoint_url <- glue::glue('{endpoint_url}')\n\n", .trim = F))
    
    # Grab params by type
    query_params <- 
      functions_df_params %>%
      dplyr::filter(param_type == 'query') %>% 
      dplyr::select(tidyselect::starts_with('param'))
    
    body_params <- 
      functions_df_params %>%
      dplyr::filter(param_type == 'body') %>% 
      dplyr::select(tidyselect::starts_with('param'))
    
    path_params <- 
      functions_df_params %>%
      dplyr::filter(param_type == 'path') %>% 
      dplyr::select(tidyselect::starts_with('param'))
    
    # Update function_text with body param info
    if(nrow(body_params) > 0){
      function_text <-
        paste0(function_text,
               glue::glue("\tbody_params <- c({paste0('\"', paste(body_params$param_name, collapse = '\",\"'), '\"')})\n", .trim = F),
               glue::glue("\tbody <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()\n\n", .trim = F))
    }else{
      function_text <-
        paste0(function_text,
               glue::glue("\tbody <- NULL\n\n", .trim = F))
    }
    
    # Facilitate uploading files
    if('file' %in% body_params$param_name){ #####function_name == 'upload_file'){
      function_text <-
        paste0(function_text,
               "\tbody$file <- httr::upload_file(body$file)\n\n")
    }
    
    # Update function_text with query param info
    if(nrow(query_params) > 0){
      function_text <-
        paste0(function_text,
               glue::glue("\tquery_params <- c({paste0('\"', paste(query_params$param_name, collapse = '\",\"'), '\"')})\n", .trim = F),
               glue::glue("\tquery <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()\n\n", .trim = F))
    }else{
      function_text <-
        paste0(function_text,
               glue::glue("\tquery <- NULL\n\n", .trim = F))
    }
    
    if(stringr::str_detect(endpoint_name, " - Beta$") & stringr::str_detect(function_name, '_assistant|_thread|_message|_run')){
      beta_header_val <- '"assistants=v1"'
    }else{
      beta_header_val <- "NULL"
    }
    
    # Facilitate uploading files
    if('file' %in% body_params$param_name){ ###if(function_name == 'upload_file'){
      function_text <-
        paste0(function_text,
               glue::glue("\tresponse <- httr::{endpoint_method}(url = endpoint_url, body = body, encode = 'multipart', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv(\"openai_organization_id\"), `OpenAI-Beta` = {beta_header_val}, Authorization = glue::glue('Bearer {{Sys.getenv(\"openai_secret_key\")}}')))"))
    }else{
      function_text <-
        paste0(function_text,
               glue::glue("\tresponse <- httr::{endpoint_method}(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv(\"openai_organization_id\"), `OpenAI-Beta` = {beta_header_val}, Authorization = glue::glue('Bearer {{Sys.getenv(\"openai_secret_key\")}}')))"))
    }
    
    function_text <-
      paste0(function_text,
             "\n\n\tif(return_response) return(response)")
    
    function_text <-
      paste0(function_text,
             "\n\n\tparse_response(response)"
      )
    
    function_text <-
      paste0(function_text,
             "\n\n}\n\n\n")
  }
  
  readr::write_lines(function_text, output_path, append = F)
  
  utils::file.edit(output_path)
  
  invisible()
}


