% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{retrieve_model}
\alias{retrieve_model}
\title{Retrieve Model}
\usage{
retrieve_model(model, return_response = F)
}
\arguments{
\item{model}{(string) The ID of the model to use for this request Required}

\item{return_response}{(boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).}
}
\description{
Retrieves a model instance, providing basic information about the model such as the owner and permissioning.
}
\seealso{
\href{https://platform.openai.com/docs/api-reference/models/retrieve}{Open AI Documentation}
}
