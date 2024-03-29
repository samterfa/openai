% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{create_moderation}
\alias{create_moderation}
\title{Create Moderation}
\usage{
create_moderation(input, model = NULL, return_response = F)
}
\arguments{
\item{input}{(string or array) The input text to classify Required}

\item{model}{(string) Two content moderations models are available: text-moderation-stable and text-moderation-latest. The default is text-moderation-latest which will be automatically upgraded over time. This ensures you are always using our most accurate model. If you use text-moderation-stable, we will provide advanced notice before updating the model. Accuracy of text-moderation-stable may be slightly lower than for text-moderation-latest.}

\item{return_response}{(boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).}
}
\description{
Classifies if text violates OpenAI's Content Policy
}
\seealso{
\href{https://platform.openai.com/docs/api-reference/moderations/create}{Open AI Documentation}
}
