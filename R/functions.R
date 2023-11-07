

#' Create Speech
#'
#' Generates audio from the input text.
#'
#' @param input (string) The text to generate audio for. The maximum length is 4096 characters. Required
#' @param model (string) One of the available TTS models: tts-1 or tts-1-hd Required
#' @param voice (string) The voice to use when generating the audio. Supported voices are alloy, echo, fable, onyx, nova, and shimmer. Required
#' @param response_format (string) The format to audio in. Supported formats are mp3, opus, aac, and flac. 
#' @param speed (number) The speed of the generated audio. Select a value from 0.25 to 4.0. 1.0 is the default. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/audio/createSpeech}{Open AI Documentation}
#' @export
create_speech <- function(input, model, voice, response_format = NULL, speed = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/audio/speech')

	body_params <- c("input","model","voice","response_format","speed")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Transcription
#'
#' Transcribes audio into the input language.
#'
#' @param file (file) The audio file object (not file name) to transcribe, in one of these formats: flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm. Required
#' @param model (string) ID of the model to use. Only whisper-1 is currently available. Required
#' @param language (string) The language of the input audio. Supplying the input language in ISO-639-1 format will improve accuracy and latency. 
#' @param prompt (string) An optional text to guide the model's style or continue a previous audio segment. The prompt should match the audio language. 
#' @param response_format (string) The format of the transcript output, in one of these options: json, text, srt, verbose_json, or vtt. 
#' @param temperature (number) The sampling temperature, between 0 and 1. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. If set to 0, the model will use log probability to automatically increase the temperature until certain thresholds are hit. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/audio/createTranscription}{Open AI Documentation}
#' @export
create_transcription <- function(file, model, language = NULL, prompt = NULL, response_format = NULL, temperature = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/audio/transcriptions')

	body_params <- c("file","model","language","prompt","response_format","temperature")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	body$file <- httr::upload_file(body$file)

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'multipart', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Translation
#'
#' Translates audio into English.
#'
#' @param file (file) The audio file object (not file name) translate, in one of these formats: flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm. Required
#' @param model (string) ID of the model to use. Only whisper-1 is currently available. Required
#' @param prompt (string) An optional text to guide the model's style or continue a previous audio segment. The prompt should be in English. 
#' @param response_format (string) The format of the transcript output, in one of these options: json, text, srt, verbose_json, or vtt. 
#' @param temperature (number) The sampling temperature, between 0 and 1. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. If set to 0, the model will use log probability to automatically increase the temperature until certain thresholds are hit. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/audio/createTranslation}{Open AI Documentation}
#' @export
create_translation <- function(file, model, prompt = NULL, response_format = NULL, temperature = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/audio/translations')

	body_params <- c("file","model","prompt","response_format","temperature")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	body$file <- httr::upload_file(body$file)

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'multipart', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Chat Completion
#'
#' Creates a model response for the given chat conversation.
#'
#' @param messages (array) A list of messages comprising the conversation so far. Example Python code. Required
#' @param model (string) ID of the model to use. See the model endpoint compatibility table for details on which models work with the Chat API. Required
#' @param frequency_penalty (number or null) Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim. See more information about frequency and presence penalties. 
#' @param function_call (string or object) Deprecated in favor of tool_choice. Controls which (if any) function is called by the model. none means the model will not call a function and instead generates a message. auto means the model can pick between generating a message or calling a function. Specifying a particular function via {"name": "my_function"} forces the model to call that function. none is the default when no functions are present. `auto`` is the default if functions are present. 
#' @param functions (array) Deprecated in favor of tools. A list of functions the model may generate JSON inputs for. 
#' @param logit_bias (map) Modify the likelihood of specified tokens appearing in the completion. Accepts a JSON object that maps tokens (specified by their token ID in the tokenizer) to an associated bias value from -100 to 100. Mathematically, the bias is added to the logits generated by the model prior to sampling. The exact effect will vary per model, but values between -1 and 1 should decrease or increase likelihood of selection; values like -100 or 100 should result in a ban or exclusive selection of the relevant token. 
#' @param max_tokens (integer or null) The maximum number of tokens to generate in the chat completion. The total length of input tokens and generated tokens is limited by the model's context length. Example Python code for counting tokens. 
#' @param n (integer or null) How many chat completion choices to generate for each input message. 
#' @param presence_penalty (number or null) Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics. See more information about frequency and presence penalties. 
#' @param response_format (object) An object specifying the format that the model must output. Setting to { type: "json_object" } enables JSON mode, which guarantees the message the model generates is valid JSON. Important: when using JSON mode you must still instruct the model to produce JSON yourself via some conversation message, for example via your system message. If you don't do this, the model may generate an unending stream of whitespace until the generation reaches the token limit, which may take a lot of time and give the appearance of a "stuck" request. Also note that the message content may be partial (i.e. cut off) if finish_reason="length", which indicates the generation exceeded max_tokens or the conversation exceeded the max context length. 
#' @param seed (integer or null) This feature is in Beta. If specified, our system will make a best effort to sample deterministically, such that repeated requests with the same seed and parameters should return the same result. Determinism is not guaranteed, and you should refer to the system_fingerprint response parameter to monitor changes in the backend. 
#' @param stop (string / array / null) Up to 4 sequences where the API will stop generating further tokens. 
#' @param stream (boolean or null) If set, partial message deltas will be sent, like in ChatGPT. Tokens will be sent as data-only server-sent events as they become available, with the stream terminated by a data: [DONE] message. Example Python code. 
#' @param temperature (number or null) What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. We generally recommend altering this or top_p but not both. 
#' @param tool_choice (string or object) Controls which (if any) function is called by the model. none means the model will not call a function and instead generates a message. auto means the model can pick between generating a message or calling a function. Specifying a particular function via {"type: "function", "function": {"name": "my_function"}} forces the model to call that function. none is the default when no functions are present. auto is the default if functions are present. 
#' @param tools (array) A list of tools the model may call. Currently, only functions are supported as a tool. Use this to provide a list of functions the model may generate JSON inputs for. 
#' @param top_p (number or null) An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered. We generally recommend altering this or temperature but not both. 
#' @param user (string) A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/chat/create}{Open AI Documentation}
#' @export
create_chat_completion <- function(messages, model, frequency_penalty = NULL, function_call = NULL, functions = NULL, logit_bias = NULL, max_tokens = NULL, n = NULL, presence_penalty = NULL, response_format = NULL, seed = NULL, stop = NULL, stream = NULL, temperature = NULL, tool_choice = NULL, tools = NULL, top_p = NULL, user = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/chat/completions')

	body_params <- c("messages","model","frequency_penalty","function_call","functions","logit_bias","max_tokens","n","presence_penalty","response_format","seed","stop","stream","temperature","tool_choice","tools","top_p","user")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Completionlegacy
#'
#' Creates a completion for the provided prompt and parameters.
#'
#' @param model (string) ID of the model to use. You can use the List models API to see all of your available models, or see our Model overview for descriptions of them. Required
#' @param prompt (string or array) The prompt(s) to generate completions for, encoded as a string, array of strings, array of tokens, or array of token arrays. Note that <|endoftext|> is the document separator that the model sees during training, so if a prompt is not specified the model will generate as if from the beginning of a new document. Required
#' @param best_of (integer or null) Generates best_of completions server-side and returns the "best" (the one with the highest log probability per token). Results cannot be streamed. When used with n, best_of controls the number of candidate completions and n specifies how many to return – best_of must be greater than n. Note: Because this parameter generates many completions, it can quickly consume your token quota. Use carefully and ensure that you have reasonable settings for max_tokens and stop. 
#' @param echo (boolean or null) Echo back the prompt in addition to the completion 
#' @param frequency_penalty (number or null) Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim. See more information about frequency and presence penalties. 
#' @param logit_bias (map) Modify the likelihood of specified tokens appearing in the completion. Accepts a JSON object that maps tokens (specified by their token ID in the GPT tokenizer) to an associated bias value from -100 to 100. You can use this tokenizer tool (which works for both GPT-2 and GPT-3) to convert text to token IDs. Mathematically, the bias is added to the logits generated by the model prior to sampling. The exact effect will vary per model, but values between -1 and 1 should decrease or increase likelihood of selection; values like -100 or 100 should result in a ban or exclusive selection of the relevant token. As an example, you can pass {"50256": -100} to prevent the <|endoftext|> token from being generated. 
#' @param logprobs (integer or null) Include the log probabilities on the logprobs most likely tokens, as well the chosen tokens. For example, if logprobs is 5, the API will return a list of the 5 most likely tokens. The API will always return the logprob of the sampled token, so there may be up to logprobs+1 elements in the response. The maximum value for logprobs is 5. 
#' @param max_tokens (integer or null) The maximum number of tokens to generate in the completion. The token count of your prompt plus max_tokens cannot exceed the model's context length. Example Python code for counting tokens. 
#' @param n (integer or null) How many completions to generate for each prompt. Note: Because this parameter generates many completions, it can quickly consume your token quota. Use carefully and ensure that you have reasonable settings for max_tokens and stop. 
#' @param presence_penalty (number or null) Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics. See more information about frequency and presence penalties. 
#' @param seed (integer or null) If specified, our system will make a best effort to sample deterministically, such that repeated requests with the same seed and parameters should return the same result. Determinism is not guaranteed, and you should refer to the system_fingerprint response parameter to monitor changes in the backend. 
#' @param stop (string / array / null) Up to 4 sequences where the API will stop generating further tokens. The returned text will not contain the stop sequence. 
#' @param stream (boolean or null) Whether to stream back partial progress. If set, tokens will be sent as data-only server-sent events as they become available, with the stream terminated by a data: [DONE] message. Example Python code. 
#' @param suffix (string or null) The suffix that comes after a completion of inserted text. 
#' @param temperature (number or null) What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. We generally recommend altering this or top_p but not both. 
#' @param top_p (number or null) An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered. We generally recommend altering this or temperature but not both. 
#' @param user (string) A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/completions/create}{Open AI Documentation}
#' @export
create_completionlegacy <- function(model, prompt, best_of = NULL, echo = NULL, frequency_penalty = NULL, logit_bias = NULL, logprobs = NULL, max_tokens = NULL, n = NULL, presence_penalty = NULL, seed = NULL, stop = NULL, stream = NULL, suffix = NULL, temperature = NULL, top_p = NULL, user = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/completions')

	body_params <- c("model","prompt","best_of","echo","frequency_penalty","logit_bias","logprobs","max_tokens","n","presence_penalty","seed","stop","stream","suffix","temperature","top_p","user")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Embeddings
#'
#' Creates an embedding vector representing the input text.
#'
#' @param input (string or array) Input text to embed, encoded as a string or array of tokens. To embed multiple inputs in a single request, pass an array of strings or array of token arrays. The input must not exceed the max input tokens for the model (8192 tokens for text-embedding-ada-002) and cannot be an empty string. Example Python code for counting tokens. Required
#' @param model (string) ID of the model to use. You can use the List models API to see all of your available models, or see our Model overview for descriptions of them. Required
#' @param encoding_format (string) The format to return the embeddings in. Can be either float or base64. 
#' @param user (string) A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/embeddings/create}{Open AI Documentation}
#' @export
create_embeddings <- function(input, model, encoding_format = NULL, user = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/embeddings')

	body_params <- c("input","model","encoding_format","user")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Fine-Tuning Job
#'
#' Creates a job that fine-tunes a specified model from a given dataset.
#'
#' @param model (string) The name of the model to fine-tune. You can select one of the supported models. Required
#' @param training_file (string) The ID of an uploaded file that contains training data. See upload file for how to upload a file. Your dataset must be formatted as a JSONL file. Additionally, you must upload your file with the purpose fine-tune. See the fine-tuning guide for more details. Required
#' @param hyperparameters (object) The hyperparameters used for the fine-tuning job. 
#' @param suffix (string or null) A string of up to 18 characters that will be added to your fine-tuned model name. For example, a suffix of "custom-model-name" would produce a model name like ft:gpt-3.5-turbo:openai:custom-model-name:7p4lURel. 
#' @param validation_file (string or null) The ID of an uploaded file that contains validation data. If you provide this file, the data is used to generate validation metrics periodically during fine-tuning. These metrics can be viewed in the fine-tuning results file. The same data should not be present in both train and validation files. Your dataset must be formatted as a JSONL file. You must upload your file with the purpose fine-tune. See the fine-tuning guide for more details. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tuning/create}{Open AI Documentation}
#' @export
create_fine_tuning_job <- function(model, training_file, hyperparameters = NULL, suffix = NULL, validation_file = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine_tuning/jobs')

	body_params <- c("model","training_file","hyperparameters","suffix","validation_file")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Fine-Tuning Jobs
#'
#' List your organization's fine-tuning jobs
#'
#' @param after (string) Identifier for the last job from the previous pagination request. 
#' @param limit (integer) Number of fine-tuning jobs to retrieve. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tuning/list}{Open AI Documentation}
#' @export
list_fine_tuning_jobs <- function(after = NULL, limit = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine_tuning/jobs')

	body <- NULL

	query_params <- c("after","limit")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Fine-Tuning Job
#'
#' Get info about a fine-tuning job.
#'
#' @param fine_tuning_job_id (string) The ID of the fine-tuning job. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tuning/retrieve}{Open AI Documentation}
#' @export
retrieve_fine_tuning_job <- function(fine_tuning_job_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine_tuning/jobs/{fine_tuning_job_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Cancel Fine-Tuning
#'
#' Immediately cancel a fine-tune job.
#'
#' @param fine_tuning_job_id (string) The ID of the fine-tuning job to cancel. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tuning/cancel}{Open AI Documentation}
#' @export
cancel_fine_tuning <- function(fine_tuning_job_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine_tuning/jobs/{fine_tuning_job_id}/cancel')

	body <- NULL

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Fine-Tuning Events
#'
#' Get status updates for a fine-tuning job.
#'
#' @param fine_tuning_job_id (string) The ID of the fine-tuning job to get events for. Required
#' @param after (string) Identifier for the last event from the previous pagination request. 
#' @param limit (integer) Number of events to retrieve. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tuning/list-events}{Open AI Documentation}
#' @export
list_fine_tuning_events <- function(fine_tuning_job_id, after = NULL, limit = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine_tuning/jobs/{fine_tuning_job_id}/events')

	body <- NULL

	query_params <- c("after","limit")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Files
#'
#' Returns a list of files that belong to the user's organization.
#'
#' @param purpose (string) Only return files with the given purpose. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/files/list}{Open AI Documentation}
#' @export
list_files <- function(purpose = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/files')

	body <- NULL

	query_params <- c("purpose")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Upload File
#'
#' Upload a file that can be used across various endpoints/features. The size of all the files uploaded by one organization can be up to 100 GB.
#'
#' @param file (string) The File object (not file name) to be uploaded. Required
#' @param purpose (string) The intended purpose of the uploaded file. Use "fine-tune" for Fine-tuning and "assistants" for Assistants and Messages. This allows us to validate the format of the uploaded file is correct for fine-tuning. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/files/create}{Open AI Documentation}
#' @export
upload_file <- function(file, purpose, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/files')

	body_params <- c("file","purpose")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	body$file <- httr::upload_file(body$file)

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'multipart', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Delete File
#'
#' Delete a file.
#'
#' @param file_id (string) The ID of the file to use for this request. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/files/delete}{Open AI Documentation}
#' @export
delete_file <- function(file_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/files/{file_id}')

	body <- NULL

	query <- NULL

	response <- httr::DELETE(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve File
#'
#' Returns information about a specific file.
#'
#' @param file_id (string) The ID of the file to use for this request. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/files/retrieve}{Open AI Documentation}
#' @export
retrieve_file <- function(file_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/files/{file_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve File Content
#'
#' Returns the contents of the specified file.
#'
#' @param file_id (string) The ID of the file to use for this request. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/files/retrieve-contents}{Open AI Documentation}
#' @export
retrieve_file_content <- function(file_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/files/{file_id}/content')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Image
#'
#' Creates an image given a prompt.
#'
#' @param prompt (string) A text description of the desired image(s). The maximum length is 1000 characters for dall-e-2 and 4000 characters for dall-e-3. Required
#' @param model (string) The model to use for image generation. 
#' @param n (integer or null) The number of images to generate. Must be between 1 and 10. For dall-e-3, only n=1 is supported. 
#' @param quality (string) The quality of the image that will be generated. hd creates images with finer details and greater consistency across the image. This param is only supported for dall-e-3. 
#' @param response_format (string or null) The format in which the generated images are returned. Must be one of url or b64_json. 
#' @param size (string or null) The size of the generated images. Must be one of 256x256, 512x512, or 1024x1024 for dall-e-2. Must be one of 1024x1024, 1792x1024, or 1024x1792 for dall-e-3 models. 
#' @param style (string or null) The style of the generated images. Must be one of vivid or natural. Vivid causes the model to lean towards generating hyper-real and dramatic images. Natural causes the model to produce more natural, less hyper-real looking images. This param is only supported for dall-e-3. 
#' @param user (string) A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/images/create}{Open AI Documentation}
#' @export
create_image <- function(prompt, model = NULL, n = NULL, quality = NULL, response_format = NULL, size = NULL, style = NULL, user = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/images/generations')

	body_params <- c("prompt","model","n","quality","response_format","size","style","user")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Image Edit
#'
#' Creates an edited or extended image given an original image and a prompt.
#'
#' @param image (string) The image to edit. Must be a valid PNG file, less than 4MB, and square. If mask is not provided, image must have transparency, which will be used as the mask. Required
#' @param prompt (string) A text description of the desired image(s). The maximum length is 1000 characters. Required
#' @param mask (string) An additional image whose fully transparent areas (e.g. where alpha is zero) indicate where image should be edited. Must be a valid PNG file, less than 4MB, and have the same dimensions as image. 
#' @param model (string) The model to use for image generation. Only dall-e-2 is supported at this time. 
#' @param n (integer or null) The number of images to generate. Must be between 1 and 10. 
#' @param response_format (string or null) The format in which the generated images are returned. Must be one of url or b64_json. 
#' @param size (string or null) The size of the generated images. Must be one of 256x256, 512x512, or 1024x1024. 
#' @param user (string) A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/images/createEdit}{Open AI Documentation}
#' @export
create_image_edit <- function(image, prompt, mask = NULL, model = NULL, n = NULL, response_format = NULL, size = NULL, user = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/images/edits')

	body_params <- c("image","prompt","mask","model","n","response_format","size","user")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Image Variation
#'
#' Creates a variation of a given image.
#'
#' @param image (string) The image to use as the basis for the variation(s). Must be a valid PNG file, less than 4MB, and square. Required
#' @param model (string) The model to use for image generation. Only dall-e-2 is supported at this time. 
#' @param n (integer or null) The number of images to generate. Must be between 1 and 10. For dall-e-3, only n=1 is supported. 
#' @param response_format (string or null) The format in which the generated images are returned. Must be one of url or b64_json. 
#' @param size (string or null) The size of the generated images. Must be one of 256x256, 512x512, or 1024x1024. 
#' @param user (string) A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/images/createVariation}{Open AI Documentation}
#' @export
create_image_variation <- function(image, model = NULL, n = NULL, response_format = NULL, size = NULL, user = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/images/variations')

	body_params <- c("image","model","n","response_format","size","user")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Models
#'
#' Lists the currently available models, and provides basic information about each one such as the owner and availability.
#'
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/models/list}{Open AI Documentation}
#' @export
list_models <- function(return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/models')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Model
#'
#' Retrieves a model instance, providing basic information about the model such as the owner and permissioning.
#'
#' @param model (string) The ID of the model to use for this request Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/models/retrieve}{Open AI Documentation}
#' @export
retrieve_model <- function(model, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/models/{model}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Delete Fine-Tune Model
#'
#' Delete a fine-tuned model. You must have the Owner role in your organization to delete a model.
#'
#' @param model (string) The model to delete Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/models/delete}{Open AI Documentation}
#' @export
delete_fine_tune_model <- function(model, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/models/{model}')

	body <- NULL

	query <- NULL

	response <- httr::DELETE(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Moderation
#'
#' Classifies if text violates OpenAI's Content Policy
#'
#' @param input (string or array) The input text to classify Required
#' @param model (string) Two content moderations models are available: text-moderation-stable and text-moderation-latest. The default is text-moderation-latest which will be automatically upgraded over time. This ensures you are always using our most accurate model. If you use text-moderation-stable, we will provide advanced notice before updating the model. Accuracy of text-moderation-stable may be slightly lower than for text-moderation-latest. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/moderations/create}{Open AI Documentation}
#' @export
create_moderation <- function(input, model = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/moderations')

	body_params <- c("input","model")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Assistant - Beta
#'
#' Create an assistant with a model and instructions.
#'
#' @param model () ID of the model to use. You can use the List models API to see all of your available models, or see our Model overview for descriptions of them. Required
#' @param description (string or null) The description of the assistant. The maximum length is 512 characters. 
#' @param file_ids (array) A list of file IDs attached to this assistant. There can be a maximum of 20 files attached to the assistant. Files are ordered by their creation date in ascending order. 
#' @param instructions (string or null) The system instructions that the assistant uses. The maximum length is 32768 characters. 
#' @param metadata (map) Set of 16 key-value pairs that can be attached to an object. This can be useful for storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long and values can be a maxium of 512 characters long. 
#' @param name (string or null) The name of the assistant. The maximum length is 256 characters. 
#' @param tools (array) A list of tool enabled on the assistant. There can be a maximum of 128 tools per assistant. Tools can be of types code_interpreter, retrieval, or function. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/assistants/createAssistant}{Open AI Documentation}
#' @export
create_assistant <- function(model, description = NULL, file_ids = NULL, instructions = NULL, metadata = NULL, name = NULL, tools = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/assistants')

	body_params <- c("model","description","file_ids","instructions","metadata","name","tools")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Assistant - Beta
#'
#' Retrieves an assistant.
#'
#' @param assistant_id (string) The ID of the assistant to retrieve. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/assistants/getAssistant}{Open AI Documentation}
#' @export
retrieve_assistant <- function(assistant_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/assistants/{assistant_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Modify Assistant - Beta
#'
#' Modifies an assistant.
#'
#' @param assistant_id (string) The ID of the assistant to modify. Required
#' @param description (string or null) The description of the assistant. The maximum length is 512 characters. 
#' @param file_ids (array) A list of File IDs attached to this assistant. There can be a maximum of 20 files attached to the assistant. Files are ordered by their creation date in ascending order. If a file was previosuly attached to the list but does not show up in the list, it will be deleted from the assistant. 
#' @param instructions (string or null) The system instructions that the assistant uses. The maximum length is 32768 characters. 
#' @param metadata (map) Set of 16 key-value pairs that can be attached to an object. This can be useful for storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long and values can be a maxium of 512 characters long. 
#' @param model () ID of the model to use. You can use the List models API to see all of your available models, or see our Model overview for descriptions of them. 
#' @param name (string or null) The name of the assistant. The maximum length is 256 characters. 
#' @param tools (array) A list of tool enabled on the assistant. There can be a maximum of 128 tools per assistant. Tools can be of types code_interpreter, retrieval, or function. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/assistants/modifyAssistant}{Open AI Documentation}
#' @export
modify_assistant <- function(assistant_id, description = NULL, file_ids = NULL, instructions = NULL, metadata = NULL, model = NULL, name = NULL, tools = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/assistants/{assistant_id}')

	body_params <- c("description","file_ids","instructions","metadata","model","name","tools")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Delete Assistant - Beta
#'
#' Delete an assistant.
#'
#' @param assistant_id (string) The ID of the assistant to delete. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/assistants/deleteAssistant}{Open AI Documentation}
#' @export
delete_assistant <- function(assistant_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/assistants/{assistant_id}')

	body <- NULL

	query <- NULL

	response <- httr::DELETE(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Assistants - Beta
#'
#' Returns a list of assistants.
#'
#' @param after (string) A cursor for use in pagination. after is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include after=obj_foo in order to fetch the next page of the list. 
#' @param before (string) A cursor for use in pagination. before is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include before=obj_foo in order to fetch the previous page of the list. 
#' @param limit (integer) A limit on the number of objects to be returned. Limit can range between 1 and 100, and the default is 20. 
#' @param order (string) Sort order by the created_at timestamp of the objects. asc for ascending order and desc for descending order. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/assistants/listAssistants}{Open AI Documentation}
#' @export
list_assistants <- function(after = NULL, before = NULL, limit = NULL, order = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/assistants')

	body <- NULL

	query_params <- c("after","before","limit","order")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Assistant File - Beta
#'
#' Create an assistant file by attaching a File to an assistant.
#'
#' @param assistant_id (string) The ID of the assistant for which to create a File. Required
#' @param file_id (string) A File ID (with purpose="assistants") that the assistant should use. Useful for tools like retrieval and code_interpreter that can access files. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/assistants/createAssistantFile}{Open AI Documentation}
#' @export
create_assistant_file <- function(assistant_id, file_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/assistants/{assistant_id}/files')

	body_params <- c("file_id")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Assistant File - Beta
#'
#' Retrieves an AssistantFile.
#'
#' @param assistant_id (string) The ID of the assistant who the file belongs to. Required
#' @param file_id (string) The ID of the file we're getting. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/assistants/getAssistantFile}{Open AI Documentation}
#' @export
retrieve_assistant_file <- function(assistant_id, file_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/assistants/{assistant_id}/files/{file_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Delete Assistant File - Beta
#'
#' Delete an assistant file.
#'
#' @param assistant_id (string) The ID of the assistant that the file belongs to. Required
#' @param file_id (string) The ID of the file to delete. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/assistants/deleteAssistantFile}{Open AI Documentation}
#' @export
delete_assistant_file <- function(assistant_id, file_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/assistants/{assistant_id}/files/{file_id}')

	body <- NULL

	query <- NULL

	response <- httr::DELETE(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Assistant Files - Beta
#'
#' Returns a list of assistant files.
#'
#' @param assistant_id (string) The ID of the assistant the file belongs to. Required
#' @param after (string) A cursor for use in pagination. after is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include after=obj_foo in order to fetch the next page of the list. 
#' @param before (string) A cursor for use in pagination. before is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include before=obj_foo in order to fetch the previous page of the list. 
#' @param limit (integer) A limit on the number of objects to be returned. Limit can range between 1 and 100, and the default is 20. 
#' @param order (string) Sort order by the created_at timestamp of the objects. asc for ascending order and desc for descending order. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/assistants/listAssistantFiles}{Open AI Documentation}
#' @export
list_assistant_files <- function(assistant_id, after = NULL, before = NULL, limit = NULL, order = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/assistants/{assistant_id}/files')

	body <- NULL

	query_params <- c("after","before","limit","order")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Thread - Beta
#'
#' Create a thread.
#'
#' @param messages (array) A list of messages to start the thread with. 
#' @param metadata (map) Set of 16 key-value pairs that can be attached to an object. This can be useful for storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long and values can be a maxium of 512 characters long. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/threads/createThread}{Open AI Documentation}
#' @export
create_thread <- function(messages = NULL, metadata = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads')

	body_params <- c("messages","metadata")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Thread - Beta
#'
#' Retrieves a thread.
#'
#' @param thread_id (string) The ID of the thread to retrieve. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/threads/getThread}{Open AI Documentation}
#' @export
retrieve_thread <- function(thread_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Modify Thread - Beta
#'
#' Modifies a thread.
#'
#' @param thread_id (string) The ID of the thread to modify. Only the metadata can be modified. Required
#' @param metadata (map) Set of 16 key-value pairs that can be attached to an object. This can be useful for storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long and values can be a maxium of 512 characters long. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/threads/modifyThread}{Open AI Documentation}
#' @export
modify_thread <- function(thread_id, metadata = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}')

	body_params <- c("metadata")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Delete Thread - Beta
#'
#' Delete a thread.
#'
#' @param thread_id (string) The ID of the thread to delete. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/threads/deleteThread}{Open AI Documentation}
#' @export
delete_thread <- function(thread_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}')

	body <- NULL

	query <- NULL

	response <- httr::DELETE(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Message - Beta
#'
#' Create a message.
#'
#' @param content (string) The content of the message. Required
#' @param role (string) The role of the entity that is creating the message. Currently only user is supported. Required
#' @param thread_id (string) The ID of the thread to create a message for. Required
#' @param file_ids (array) A list of File IDs that the message should use. There can be a maximum of 10 files attached to a message. Useful for tools like retrieval and code_interpreter that can access and use files. 
#' @param metadata (map) Set of 16 key-value pairs that can be attached to an object. This can be useful for storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long and values can be a maxium of 512 characters long. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/messages/createMessage}{Open AI Documentation}
#' @export
create_message <- function(content, role, thread_id, file_ids = NULL, metadata = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/messages')

	body_params <- c("content","role","file_ids","metadata")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Message - Beta
#'
#' Retrieve a message.
#'
#' @param message_id (string) The ID of the message to retrieve. Required
#' @param thread_id (string) The ID of the thread to which this message belongs. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/messages/getMessage}{Open AI Documentation}
#' @export
retrieve_message <- function(message_id, thread_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/messages/{message_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Modify Message - Beta
#'
#' Modifies a message.
#'
#' @param message_id (string) The ID of the message to modify. Required
#' @param thread_id (string) The ID of the thread to which this message belongs. Required
#' @param metadata (map) Set of 16 key-value pairs that can be attached to an object. This can be useful for storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long and values can be a maxium of 512 characters long. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/messages/modifyMessage}{Open AI Documentation}
#' @export
modify_message <- function(message_id, thread_id, metadata = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/messages/{message_id}')

	body_params <- c("metadata")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Messages - Beta
#'
#' Returns a list of messages for a given thread.
#'
#' @param thread_id (string) The ID of the thread the messages belong to. Required
#' @param after (string) A cursor for use in pagination. after is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include after=obj_foo in order to fetch the next page of the list. 
#' @param before (string) A cursor for use in pagination. before is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include before=obj_foo in order to fetch the previous page of the list. 
#' @param limit (integer) A limit on the number of objects to be returned. Limit can range between 1 and 100, and the default is 20. 
#' @param order (string) Sort order by the created_at timestamp of the objects. asc for ascending order and desc for descending order. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/messages/listMessages}{Open AI Documentation}
#' @export
list_messages <- function(thread_id, after = NULL, before = NULL, limit = NULL, order = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/messages')

	body <- NULL

	query_params <- c("after","before","limit","order")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Message File - Beta
#'
#' Retrieves a message file.
#'
#' @param file_id (string) The ID of the file being retrieved. Required
#' @param message_id (string) The ID of the message the file belongs to. Required
#' @param thread_id (string) The ID of the thread to which the message and File belong. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/messages/getMessageFile}{Open AI Documentation}
#' @export
retrieve_message_file <- function(file_id, message_id, thread_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/messages/{message_id}/files/{file_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Message Files - Beta
#'
#' Returns a list of message files.
#'
#' @param message_id (string) The ID of the message that the files belongs to. Required
#' @param thread_id (string) The ID of the thread that the message and files belong to. Required
#' @param after (string) A cursor for use in pagination. after is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include after=obj_foo in order to fetch the next page of the list. 
#' @param before (string) A cursor for use in pagination. before is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include before=obj_foo in order to fetch the previous page of the list. 
#' @param limit (integer) A limit on the number of objects to be returned. Limit can range between 1 and 100, and the default is 20. 
#' @param order (string) Sort order by the created_at timestamp of the objects. asc for ascending order and desc for descending order. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/messages/listMessageFiles}{Open AI Documentation}
#' @export
list_message_files <- function(message_id, thread_id, after = NULL, before = NULL, limit = NULL, order = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/messages/{message_id}/files')

	body <- NULL

	query_params <- c("after","before","limit","order")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Run - Beta
#'
#' Create a run.
#'
#' @param assistant_id (string) The ID of the assistant to use to execute this run. Required
#' @param thread_id (string) The ID of the thread to run. Required
#' @param instructions (string or null) Override the default system message of the assistant. This is useful for modifying the behavior on a per-run basis. 
#' @param metadata (map) Set of 16 key-value pairs that can be attached to an object. This can be useful for storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long and values can be a maxium of 512 characters long. 
#' @param model (string or null) The ID of the Model to be used to execute this run. If a value is provided here, it will override the model associated with the assistant. If not, the model associated with the assistant will be used. 
#' @param tools (array or null) Override the tools the assistant can use for this run. This is useful for modifying the behavior on a per-run basis. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/runs/createRun}{Open AI Documentation}
#' @export
create_run <- function(assistant_id, thread_id, instructions = NULL, metadata = NULL, model = NULL, tools = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/runs')

	body_params <- c("assistant_id","instructions","metadata","model","tools")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Run - Beta
#'
#' Retrieves a run.
#'
#' @param run_id (string) The ID of the run to retrieve. Required
#' @param thread_id (string) The ID of the thread that was run. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/runs/getRun}{Open AI Documentation}
#' @export
retrieve_run <- function(run_id, thread_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Modify Run - Beta
#'
#' Modifies a run.
#'
#' @param run_id (string) The ID of the run to modify. Required
#' @param thread_id (string) The ID of the thread that was run. Required
#' @param metadata (map) Set of 16 key-value pairs that can be attached to an object. This can be useful for storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long and values can be a maxium of 512 characters long. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/runs/modifyRun}{Open AI Documentation}
#' @export
modify_run <- function(run_id, thread_id, metadata = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}')

	body_params <- c("metadata")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Runs - Beta
#'
#' Returns a list of runs belonging to a thread.
#'
#' @param thread_id (string) The ID of the thread the run belongs to. Required
#' @param after (string) A cursor for use in pagination. after is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include after=obj_foo in order to fetch the next page of the list. 
#' @param before (string) A cursor for use in pagination. before is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include before=obj_foo in order to fetch the previous page of the list. 
#' @param limit (integer) A limit on the number of objects to be returned. Limit can range between 1 and 100, and the default is 20. 
#' @param order (string) Sort order by the created_at timestamp of the objects. asc for ascending order and desc for descending order. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/runs/listRuns}{Open AI Documentation}
#' @export
list_runs <- function(thread_id, after = NULL, before = NULL, limit = NULL, order = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/runs')

	body <- NULL

	query_params <- c("after","before","limit","order")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Submit Tool Outputs To Run - Beta
#'
#' When a run has the status: "requires_action" and required_action.type is submit_tool_outputs, this endpoint can be used to submit the outputs from the tool calls once they're all completed. All outputs must be submitted in a single request.
#'
#' @param run_id (string) The ID of the run that requires the tool output submission. Required
#' @param thread_id (string) The ID of the thread to which this run belongs. Required
#' @param tool_outputs (array) A list of tools for which the outputs are being submitted. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/runs/submitToolOutputs}{Open AI Documentation}
#' @export
submit_tool_outputs_to_run <- function(run_id, thread_id, tool_outputs, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}/submit_tool_outputs')

	body_params <- c("tool_outputs")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Cancel A Run - Beta
#'
#' Cancels a run that is in_progress.
#'
#' @param run_id (string) The ID of the run to cancel. Required
#' @param thread_id (string) The ID of the thread to which this run belongs. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/runs/cancelRun}{Open AI Documentation}
#' @export
cancel_a_run <- function(run_id, thread_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}/cancel')

	body <- NULL

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Thread And Run - Beta
#'
#' Create a thread and run it in one request.
#'
#' @param assistant_id (string) The ID of the assistant to use to execute this run. Required
#' @param instructions (string or null) Override the default system message of the assistant. This is useful for modifying the behavior on a per-run basis. 
#' @param metadata (map) Set of 16 key-value pairs that can be attached to an object. This can be useful for storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long and values can be a maxium of 512 characters long. 
#' @param model (string or null) The ID of the Model to be used to execute this run. If a value is provided here, it will override the model associated with the assistant. If not, the model associated with the assistant will be used. 
#' @param tools (array or null) Override the tools the assistant can use for this run. This is useful for modifying the behavior on a per-run basis. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/runs/createThreadAndRun}{Open AI Documentation}
#' @export
create_thread_and_run <- function(assistant_id, instructions = NULL, metadata = NULL, model = NULL, tools = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/runs')

	body_params <- c("assistant_id","instructions","metadata","model","tools")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Run Step - Beta
#'
#' Retrieves a run step.
#'
#' @param run_id (string) The ID of the run to which the run step belongs. Required
#' @param step_id (string) The ID of the run step to retrieve. Required
#' @param thread_id (string) The ID of the thread to which the run and run step belongs. Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/runs/getRunStep}{Open AI Documentation}
#' @export
retrieve_run_step <- function(run_id, step_id, thread_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}/steps/{step_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Run Steps - Beta
#'
#' Returns a list of run steps belonging to a run.
#'
#' @param run_id (string) The ID of the run the run steps belong to. Required
#' @param thread_id (string) The ID of the thread the run and run steps belong to. Required
#' @param after (string) A cursor for use in pagination. after is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include after=obj_foo in order to fetch the next page of the list. 
#' @param before (string) A cursor for use in pagination. before is an object ID that defines your place in the list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can include before=obj_foo in order to fetch the previous page of the list. 
#' @param limit (integer) A limit on the number of objects to be returned. Limit can range between 1 and 100, and the default is 20. 
#' @param order (string) Sort order by the created_at timestamp of the objects. asc for ascending order and desc for descending order. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/runs/listRunSteps}{Open AI Documentation}
#' @export
list_run_steps <- function(run_id, thread_id, after = NULL, before = NULL, limit = NULL, order = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}/steps')

	body <- NULL

	query_params <- c("after","before","limit","order")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = "assistants=v1", Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Fine-Tunedeprecated
#'
#' Creates a job that fine-tunes a specified model from a given dataset.
#'
#' @param training_file (string) The ID of an uploaded file that contains training data. See upload file for how to upload a file. Your dataset must be formatted as a JSONL file, where each training example is a JSON object with the keys "prompt" and "completion". Additionally, you must upload your file with the purpose fine-tune. See the fine-tuning guide for more details. Required
#' @param batch_size (integer or null) The batch size to use for training. The batch size is the number of training examples used to train a single forward and backward pass. By default, the batch size will be dynamically configured to be ~0.2% of the number of examples in the training set, capped at 256 - in general, we've found that larger batch sizes tend to work better for larger datasets. 
#' @param classification_betas (array or null) If this is provided, we calculate F-beta scores at the specified beta values. The F-beta score is a generalization of F-1 score. This is only used for binary classification. With a beta of 1 (i.e. the F-1 score), precision and recall are given the same weight. A larger beta score puts more weight on recall and less on precision. A smaller beta score puts more weight on precision and less on recall. 
#' @param classification_n_classes (integer or null) The number of classes in a classification task. This parameter is required for multiclass classification. 
#' @param classification_positive_class (string or null) The positive class in binary classification. This parameter is needed to generate precision, recall, and F1 metrics when doing binary classification. 
#' @param compute_classification_metrics (boolean or null) If set, we calculate classification-specific metrics such as accuracy and F-1 score using the validation set at the end of every epoch. These metrics can be viewed in the results file. In order to compute classification metrics, you must provide a validation_file. Additionally, you must specify classification_n_classes for multiclass classification or classification_positive_class for binary classification. 
#' @param hyperparameters (object) The hyperparameters used for the fine-tuning job. 
#' @param learning_rate_multiplier (number or null) The learning rate multiplier to use for training. The fine-tuning learning rate is the original learning rate used for pretraining multiplied by this value. By default, the learning rate multiplier is the 0.05, 0.1, or 0.2 depending on final batch_size (larger learning rates tend to perform better with larger batch sizes). We recommend experimenting with values in the range 0.02 to 0.2 to see what produces the best results. 
#' @param model (string) The name of the base model to fine-tune. You can select one of "ada", "babbage", "curie", "davinci", or a fine-tuned model created after 2022-04-21 and before 2023-08-22. To learn more about these models, see the Models documentation. 
#' @param prompt_loss_weight (number or null) The weight to use for loss on the prompt tokens. This controls how much the model tries to learn to generate the prompt (as compared to the completion which always has a weight of 1.0), and can add a stabilizing effect to training when completions are short. If prompts are extremely long (relative to completions), it may make sense to reduce this weight so as to avoid over-prioritizing learning the prompt. 
#' @param suffix (string or null) A string of up to 40 characters that will be added to your fine-tuned model name. For example, a suffix of "custom-model-name" would produce a model name like ada:ft-your-org:custom-model-name-2022-02-15-04-21-04. 
#' @param validation_file (string or null) The ID of an uploaded file that contains validation data. If you provide this file, the data is used to generate validation metrics periodically during fine-tuning. These metrics can be viewed in the fine-tuning results file. Your train and validation data should be mutually exclusive. Your dataset must be formatted as a JSONL file, where each validation example is a JSON object with the keys "prompt" and "completion". Additionally, you must upload your file with the purpose fine-tune. See the fine-tuning guide for more details. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tunes/create}{Open AI Documentation}
#' @export
create_fine_tunedeprecated <- function(training_file, batch_size = NULL, classification_betas = NULL, classification_n_classes = NULL, classification_positive_class = NULL, compute_classification_metrics = NULL, hyperparameters = NULL, learning_rate_multiplier = NULL, model = NULL, prompt_loss_weight = NULL, suffix = NULL, validation_file = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine-tunes')

	body_params <- c("training_file","batch_size","classification_betas","classification_n_classes","classification_positive_class","compute_classification_metrics","hyperparameters","learning_rate_multiplier","model","prompt_loss_weight","suffix","validation_file")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Fine-Tunesdeprecated
#'
#' List your organization's fine-tuning jobs
#'
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tunes/list}{Open AI Documentation}
#' @export
list_fine_tunesdeprecated <- function(return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine-tunes')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Retrieve Fine-Tunedeprecated
#'
#' Gets info about the fine-tune job.
#'
#' @param fine_tune_id (string) The ID of the fine-tune job Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tunes/retrieve}{Open AI Documentation}
#' @export
retrieve_fine_tunedeprecated <- function(fine_tune_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine-tunes/{fine_tune_id}')

	body <- NULL

	query <- NULL

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Cancel Fine-Tunedeprecated
#'
#' Immediately cancel a fine-tune job.
#'
#' @param fine_tune_id (string) The ID of the fine-tune job to cancel Required
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tunes/cancel}{Open AI Documentation}
#' @export
cancel_fine_tunedeprecated <- function(fine_tune_id, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine-tunes/{fine_tune_id}/cancel')

	body <- NULL

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' List Fine-Tune Eventsdeprecated
#'
#' Get fine-grained status updates for a fine-tune job.
#'
#' @param fine_tune_id (string) The ID of the fine-tune job to get events for. Required
#' @param stream (boolean) Whether to stream events for the fine-tune job. If set to true, events will be sent as data-only server-sent events as they become available. The stream will terminate with a data: [DONE] message when the job is finished (succeeded, cancelled, or failed). If set to false, only events generated so far will be returned. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/fine-tunes/list-events}{Open AI Documentation}
#' @export
list_fine_tune_eventsdeprecated <- function(fine_tune_id, stream = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/fine-tunes/{fine_tune_id}/events')

	body <- NULL

	query_params <- c("stream")
	query <- query_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(query_params) %>% purrr::compact()

	response <- httr::GET(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}



#' Create Editdeprecated
#'
#' Creates a new edit for the provided input, instruction, and parameters.
#'
#' @param instruction (string) The instruction that tells the model how to edit the prompt. Required
#' @param model (string) ID of the model to use. You can use the text-davinci-edit-001 or code-davinci-edit-001 model with this endpoint. Required
#' @param input (string or null) The input text to use as a starting point for the edit. 
#' @param n (integer or null) How many edits to generate for the input and instruction. 
#' @param temperature (number or null) What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. We generally recommend altering this or top_p but not both. 
#' @param top_p (number or null) An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered. We generally recommend altering this or temperature but not both. 
#' @param return_response (boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).
#' @seealso \href{https://platform.openai.com/docs/api-reference/edits/create}{Open AI Documentation}
#' @export
create_editdeprecated <- function(instruction, model, input = NULL, n = NULL, temperature = NULL, top_p = NULL, return_response = F){

	check_authentication()

	endpoint_url <- glue::glue('https://api.openai.com/v1/edits')

	body_params <- c("instruction","model","input","n","temperature","top_p")
	body <- body_params %>% purrr::map(~ eval(parse(text = .x))) %>% stats::setNames(body_params) %>% purrr::compact()

	query <- NULL

	response <- httr::POST(url = endpoint_url, body = body, encode = 'json', query = query, httr::add_headers(`OpenAI-Organization` = Sys.getenv("openai_organization_id"), `OpenAI-Beta` = NULL, Authorization = glue::glue('Bearer {Sys.getenv("openai_secret_key")}')))

	if(return_response) return(response)

	parse_response(response)

}




