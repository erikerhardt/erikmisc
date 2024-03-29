% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_message_capture.R
\name{e_message_capture}
\alias{e_message_capture}
\title{Capture a message, warning, or error to take actions}
\usage{
e_message_capture(f)
}
\arguments{
\item{f}{a function that might return a message, warning, or error}
}
\value{
if there was an exception, return its content
}
\description{
Capture a message, warning, or error to take actions
}
\examples{
e_message_capture(test_message)(1)
}
