
autocomplete_r_code <- function(prompt = rstudioapi::selectionGet()$value, debug = F){
  
  if(debug) message(paste("Creating completion for", prompt))
  
  to_complete <- paste('# R Language', prompt, sep = '\n\n')
  
  completion <- 
    openai::create_completion(prompt = prompt, 
                              engine_id = 'code-davinci-001', 
                              best_of = 1, 
                              top_p = 1,
                              frequency_penalty = 0,
                              presence_penalty = 0,
                              temperature = 1,
                              echo = T, 
                              max_tokens = 20, 
                              stream = F)$choices[[1]]$text
  
  rstudioapi::insertText(text = completion)
}
