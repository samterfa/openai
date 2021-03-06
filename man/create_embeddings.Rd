% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{create_embeddings}
\alias{create_embeddings}
\title{Create Embeddings}
\usage{
create_embeddings(engine_id, input, user = NULL, return_response = F)
}
\arguments{
\item{engine_id}{(string) The ID of the engine to use for this request Required}

\item{input}{(string or array) Input text to get embeddings for, encoded as a string or array of tokens. To get embeddings for multiple inputs in a single request, pass an array of strings or array of token arrays. Each input must not exceed 2048 tokens in length. Unless your are embedding code, we suggest replacing newlines (\n) in your input with a single space, as we have observed inferior results when newlines are present. Required}

\item{user}{(string) A unique identifier representing your end-user, which will help OpenAI to monitor and detect abuse.}

\item{return_response}{(boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).}
}
\description{
Creates an embedding vector representing the input text.
}
\seealso{
\href{https://beta.openai.com/docs/api-reference/embeddings/create}{Open AI Documentation}
}
