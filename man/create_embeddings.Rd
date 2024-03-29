% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{create_embeddings}
\alias{create_embeddings}
\title{Create Embeddings}
\usage{
create_embeddings(
  input,
  model,
  encoding_format = NULL,
  user = NULL,
  return_response = F
)
}
\arguments{
\item{input}{(string or array) Input text to embed, encoded as a string or array of tokens. To embed multiple inputs in a single request, pass an array of strings or array of token arrays. The input must not exceed the max input tokens for the model (8192 tokens for text-embedding-ada-002) and cannot be an empty string. Example Python code for counting tokens. Required}

\item{model}{(string) ID of the model to use. You can use the List models API to see all of your available models, or see our Model overview for descriptions of them. Required}

\item{encoding_format}{(string) The format to return the embeddings in. Can be either float or base64.}

\item{user}{(string) A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more.}

\item{return_response}{(boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).}
}
\description{
Creates an embedding vector representing the input text.
}
\seealso{
\href{https://platform.openai.com/docs/api-reference/embeddings/create}{Open AI Documentation}
}
