% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_transform.R
\name{df_transform}
\alias{df_transform}
\title{Transform or pivot FLS tables}
\usage{
df_transform(d_frame, names_col = "metric")
}
\arguments{
\item{d_frame}{A data.frame or table from FLS model run, e.g \code{summary.second.fx.t60.csv}}

\item{names_col}{A character to be used in pivot_longer as column header}
}
\value{
A data.frame, which is a presentation ready table
}
\description{
Transform or pivot FLS tables
}
