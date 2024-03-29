% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{create_fine_tuning_job}
\alias{create_fine_tuning_job}
\title{Create Fine-Tuning Job}
\usage{
create_fine_tuning_job(
  model,
  training_file,
  hyperparameters = NULL,
  suffix = NULL,
  validation_file = NULL,
  return_response = F
)
}
\arguments{
\item{model}{(string) The name of the model to fine-tune. You can select one of the supported models. Required}

\item{training_file}{(string) The ID of an uploaded file that contains training data. See upload file for how to upload a file. Your dataset must be formatted as a JSONL file. Additionally, you must upload your file with the purpose fine-tune. See the fine-tuning guide for more details. Required}

\item{hyperparameters}{(object) The hyperparameters used for the fine-tuning job.}

\item{suffix}{(string or null) A string of up to 18 characters that will be added to your fine-tuned model name. For example, a suffix of "custom-model-name" would produce a model name like ft:gpt-3.5-turbo:openai:custom-model-name:7p4lURel.}

\item{validation_file}{(string or null) The ID of an uploaded file that contains validation data. If you provide this file, the data is used to generate validation metrics periodically during fine-tuning. These metrics can be viewed in the fine-tuning results file. The same data should not be present in both train and validation files. Your dataset must be formatted as a JSONL file. You must upload your file with the purpose fine-tune. See the fine-tuning guide for more details.}

\item{return_response}{(boolean) Whether to return the API response or parse the contents of the response. Defaults to FALSE (parse the response).}
}
\description{
Creates a job that fine-tunes a specified model from a given dataset.
}
\seealso{
\href{https://platform.openai.com/docs/api-reference/fine-tuning/create}{Open AI Documentation}
}
