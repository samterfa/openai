% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{retrieve_engine}
\alias{retrieve_engine}
\title{Retrieve Engine}
\usage{
retrieve_engine(engine_id, return_response = F)
}
\arguments{
\item{engine_id}{(string) The ID of the engine to use for this request Required}

\item{return_response}{(boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).}
}
\description{
Retrieves an engine instance, providing basic information about the engine such as the owner and availability.
}
\seealso{
\href{https://beta.openai.com/docs/api-reference/engines/retrieve}{Open AI Documentation}
}
