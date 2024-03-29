% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DefineAndUpdateVariables.R
\name{DefineTranslation}
\alias{DefineTranslation}
\title{Define translation}
\usage{
DefineTranslation(
  processData,
  UseProcessData = FALSE,
  DefinitionMethod = c("ResourceFile", "Table"),
  FileName = character(),
  VariableName = character(),
  Conditional = FALSE,
  ConditionalVariableNames = character(),
  TranslationTable = data.table::data.table(),
  ValueColumn = character(),
  NewValueColumn = character(),
  ConditionalValueColumns = character()
)
}
\arguments{
\item{processData}{The current data produced by a previous instance of the function.}

\item{UseProcessData}{Logical: If TRUE use the existing function output in the process.}

\item{DefinitionMethod}{Character: A string naming the method to use, one of "ResourceFile", for reading the table from the file given by \code{FileName}; and "Table", for defining the \code{TranslationTable} directly as an input.}

\item{FileName}{The csv file holding a table with the \code{TranslationTable}. Required columns are given by \code{ValueColumn} and \code{NewValueColumn}, and, in the case that Conditional == TRUE, \code{ConditionalValueColumns}.}

\item{VariableName}{The name of the variable to translate. This will be the name of the first column of the TranslationTable when generated from the StoX GUI.}

\item{Conditional}{Logical: If TRUE condition the translation on values of other variables.}

\item{ConditionalVariableNames}{The names of the variables to condition the translation on. Must be given if \code{Conditional == TRUE}. This will be the name(s) of the third column and onwards of the TranslationTable when generated from the StoX GUI.}

\item{TranslationTable}{A table holding the following columns: The first column holds the values to translate FROM, the second column holds the values to translate TO, and the remaining zero or more columns holds the values for the conditional variables specified in \code{ConditionalVariableNames}. I.e., if \code{VariableName} = "IndividualAge" and \code{ConditionalVariableNames} = "IndividualSex", and the \code{TranslationTable} has values 3, 4 and "F" in the first row, female fish at age 3 are translated to age 4. Use NA to translate missing values (shown as "-" in Preview in the StoX GUI, and usually as empty cell in excel). Values in the \code{TranslationTable} can be given either as single values or as expressions of functions of the variable specified by the column name. See details of \code{\link{DefineTranslation}}.}

\item{ValueColumn, NewValueColumn}{The name of the columns of \code{FileName} representing the current values and the values to translate to, respectively.}

\item{ConditionalValueColumns}{The names of the columns of \code{FileName} representing the conditional values.}
}
\value{
A \code{\link{Translation}} object.
}
\description{
This function defines the translation table used as input to \code{\link{TranslateStoxBiotic}} and similar functions to translate values of one or more columns to new values given by a table or read from a CSV file.
}
\details{
The columns of the \code{TranslationTable} (except the NewValue column) can be given in one of two ways: (1) A single value or a string to be evaluated and matched using the "\%in\%" operator, such as "HER" or "c(\"HER\", \"CLU\")"; or (2) a string expressing a function of the variable given by the column name, such as "function(IndividualTotalLength) IndividualTotalLength > 10". When the \code{TranslationnTable} is given in the StoX GUI the strings need not be escaped ((1) HER or c("HER", "CLU"); or (2) function(IndividualTotalLength) IndividualTotalLength > 10). 

E.g., to set all individuals with missing IndividualMaturity as "Adult" if longer than 10 cm, use \code{function(IndividualMaturity) is.na(IndividualMaturity)} in the first column named "IndividualMaturity", \code{Adult} in the "NewValue" column, and \code{function(IndividualTotalLength) IndividualTotalLength > 10} in the third (conditional) column named "IndividualTotalLength". To translate all IndividualMaturity to a e.g. NA, use \code{function(IndividualMaturity) TRUE} in the "IndividualMaturity" column and \code{NA} in the "NewValue" column.
}
\seealso{
\code{\link{TranslateStoxBiotic}}, \code{\link{TranslateStoxAcoustic}}, \code{\link{TranslateBiotic}}, \code{\link{TranslateAcoustic}}, \code{\link{TranslateStoxLanding}}, \code{\link{TranslateLanding}}, \code{\link{TranslateICESBiotic}} and \code{\link{TranslateICESAcoustic}} for applying the translation.
}
