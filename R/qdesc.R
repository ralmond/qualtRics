

#' Constructor for Qualtrics Question Descriptions
#'
#' This is a constructor for qualtrics question object.  It is not
#' normally called by users, but rather is generated from the survey description
#' in Qualtrics.
#'
#' @param QuestionID -- Character value giving the Qualtrics ID of the question.
#' Starts with "QID.."  This is normally assigned by Qualtrics.
#' @param DataExportTag -- Label assigned by user and by default used to label
#' columns in response file.  NOTE:  Qualtrics does not ensure that these are unique.
#' @param QuestionText -- Text of the question, may contain HTML tags.
#' @param QuestionType -- Short String identifying Qualtrics question type:
#' "MC" -- Multiple Choice, "Matrix" -- Matrix, "TE" -- Text Entry, and
#' "Slider" -- slider input, are the most commonly used.
#' @param Selector -- Another Qualtrics code indicating how selection is done.
#' The values "MACOL","MAHR","MAVR", and "MSB" indicate that there might be multiple
#' selections.
#' @param SubSelector -- More Qualtrics codes.
#' @param ChoiceOrder -- A character vector giving the order of the choices.
#' @param Choices -- A named list of the choices where each component is a list
#' with an element named "Display" with the text of the choice.
#' @param AnswerOrder -- A character vector giving the order of the answers in a matrix question.
#' @param Answers -- A named list, similar to Choices.
#' @param Configuration -- A list of Qualtrics data.  There is an "Autoscale" element
#' which seems to contain information to distinguish ordered from unordered responses.
#' @param Language -- Probably what you think it is.
#' @param NextAnswerId -- Seems to be a number, no idea how it is used.
#' @param NextChoiceId -- Seems to be a number, no idea how it is used.
#' @param QuestionDescription -- Seems to be a mirror of QuestionText, possibly with
#' aHTML tags stripped
#' @param Validation -- Probably related to any error checking is done.
#' @param Randomization -- Probably related to randomizing choices.
#' @param AnswerRandomization -- Related to randomizing answers in matrix questions.
#' @param RecodeValues -- Probably a mapping of choices to values.
#' @param DefaultChoices -- Probably what you think it is.
#' @param DataVisibility -- List with two logical values.
#' @param QuestionText_Unsafe -- Question text with HTML codes.
#' @param SearchSource -- More sketchily documented stuff from Qualtrics
#' @param GradingData -- Probably a way to indicate the key in a Multiple Choice
#' Question.
#' @param ChoiceDataExportTags -- A logical value that means something
#' @param AnswerColumns -- Seems to be an interger, undocumented.
#' @param ClarifyingSymbolType -- Undocumented.
#' @param DynamicChoicesData -- Undocumented.
#'
#' @details
#' The normal way to create these objects is to read them from the API using [fetch_description]
#' method with the "questions" option.  This will produce a list lists containing the
#' question metadata.  This can be passed to `as_Qdesc` to tag the lists as question
#' descriptions.
#'
#' An alternative method is to export a `qsf` file from Qualtric.  The QSF file
#' is a JSON file which can be read using [jsonlite::fromJSON] with the `simplify=FALSE`
#'
#' @references <https://api.qualtrics.com/1ebd0bb7008f8-get-question> Is the official
#' documentation which contains partial information about what is in these fields.
#'
#' @return An object of type "Qdesc"
#' @export
#'
#' @examples
#' \dontrun{
#' lapply(fetch_description(surveyID,"questions")$questions,
#'        as_Qdesc)
#' lapply(jsonlite::fromJSON("SurveyDescription.qsf",FALSE),
#'        as_Qdesc)
#'
#' }
new_Qdesc <- function(...,QuestionID="QID...",DataExportTag="Q...",
                      QuestionText="Riddle me this",
                      QuestionType=c("MC","Matrix","Captcha",
                                     "CS","DB","DD","Draw",
                                     "DynamicMatrix","FileUpload",
                                     "GAP","HeatMap","HL","HotSpot",
                                     "Meta","PGR","RO","SBS","Slider",
                                     "SS","TE","Timing","TreeSelect"),
                      Selector=c("DL","GRB","MACOL","MAHR","MAVR","MSB",
                                 "NPS","SACOL","SAHR","SAVR","SB","TB","TXOT",
                                 "PTB","SL","ML","ESTB","FORM","PW",
                                 "Bipolar","Likert","TE","Profile","CS",
                                 "RO","MaxDiff","HBAR","HSLIDER","STAR"),
                      SubSelector=c("GR","TX","TXOT","WOTB","WTXB",
                                    "DL","DND","Long","Medium","MultipleAnswer",
                                    "Short","SingleAnswer","WOTB","WVTB",
                                    "WTXB","Essay"),
                      ChoiceOrder=character(),
                      Choices=list(),
                      AnswerOrder=character(),
                      Answers=list(),
                      Configuration=list(),
                      Language=NA_character_,
                      NextAnswerId=NA_integer_,
                      NextChoiceId=NA_integer_,
                      QuestionDescription=cleanTags(QuestionText),
                      Validation=list(),
                      Randomization=FALSE,
                      AnswerRandomization=FALSE,
                      RecodeValues=list(),
                      DefaultChoices=FALSE,
                      DataVisibility=list(),
                      QuestionText_Unsafe=QuestionText,
                      SearchSource=list(),
                      GradingData=list(),
                      ChoiceDataExportTags=FALSE,
                      AnswerColumns=NA_integer_,
                      ClarifyingSymbolType=list(),
                      DynamicChoicesData=list()) {
  res <- list(...)
  class(res) <- "Qdesc"
  res
}


#' @describeIn new_Qdesc Marks a list as containing Qdesc information
#' @param record -- A list (from parsed JSON).
#' @export
as_Qdesc <- function (record) {
  if (is.null(record$QuestionID)) {
    stop("Record does not have a QuestionID, is it really a Qualtrics Question Description?")
  }
  class(record) <- "Qdesc"
  record
}


#' Strips HTML tags from strings
#'
#' @param htmlstring -- A character vector which may contain HTML tags
#'
#' @return String with HTML tags returned.
#' @export
#'
#' @examples
#' cleanTags("<span font="Ariel">Yes</span>")
cleanTags <- function(htmlstring) {gsub("<[^<]+>","",htmlstring)}

#' Extract information from Qdesc objects
#'
#' These functions return interesting information from [Qdesc] objects.
#' @param qdesc A [Qdesc] object (or list with the correct fields).
#' @details
#'
#' The function `qid()` returns the Qualtrics ID of the question.  This is
#' unique within a survey, and usually starts with "QID".
#'
#' The function `qname()` returns the DataExportTag, which is set by the
#' user when editing the question in Qualtrics.  The names of the corresponding
#' columns should start with this string.  For questions of type "Matrix" and
#' ones which allow multiple selections, generally the column names will be the
#' `qname()_n` where `n` is an option number.  Extra text (e.g., "Other, specify...")
#' answers will have the form `qname()_n_TEXT`.
#'
#'
#' The function `qtext()` returns the text of the question (with HTML tags
#' stripped out).
#'
#' @return A character string describing the text.
#' @export
#'
#' @examples
qname <- function (qdesc) {qdesc$DataExportTag}
#' @describeIn qname Qualtrics ID
#' @export
qid <- function (qdesc) qdesc$QuestionID
#' @describeIn qname Question text
#' @export
qtext <- function (qdesc) cleanTags(qdesc$QuestionText)

#' Gets the type of the question.
#'
#' These functions extract the type of the type of the question, with the
#' most common types being "MC" -- Multiple Choice, "Matrix" -- Matrix,
#' "TE" -- Text Entry, "Slider" -- Slider.  Additionally, if multiple
#' selection can be made, "_Multiselect" is added to the type.
#'
#' @param qdesc
#'
#' @details
#'
#' The primary question type is determined by the QuestionType field.
#' Multiselect questions are determined by the value of the Selector field.
#'
#' @return `question_type()` returns a string identifying the type of question.
#' `is_multi_select()` returns a logical value.
#' @export
#'
#' @examples
is_multi_select <- function (qdesc) {
  qdesc$Selector %in% c("MACOL","MAHR","MAVR","MSB")
}
question_type <- function(qdesc) {
  if (is_multi_select(qdesc))
    return(paste0(qdesc$QuestionType,"_Multiselect"))
  return(qdesc$QuestionType)
}



#' Extract information about multiple-choice and matrix options.
#'
#' In multiple-choice questions, the options are the possible answers
#' the respondant can select.  In matrix questions, they are the stems
#' labeling the rows of the matrix.
#'
#' @param qdesc
#'
#' @return `option_names()` returns a character vector giving the text
#' associated with the choices in the order they appear.
#' `option_order()` gives a numeric vector
#' showing the order in which the options appear.  The other two functions return
#' a logical value.
#' @export
#'
#' @details
#'
#' Internal to Qualtrics, the numbering of options seems to be related to when
#' the option was created rather than its order on the question.  This can
#' result in a mismatch between the numeric code in the results and the
#' expectation, and can be especially problematic with ordinal variables.
#'
#' Using `match(col,option_order(qdesc))` will fix any glitches in the ordering.
#'
#'  Thus to covert the scale to a factor, use
#'  ```
#'  responses$col <- factor(match(col,option_order(qdesc)),
#'      1L:length(option_names(qdesc)),option_names(qdesc), ...)
#'  ```
#'
#' ## Detecting order and reverse ordered scales.
#'
#'  There is an undocumented field "Autoscale" in the "Configuration"
#'  field.  With some questions having `Configuration$Autoscale$Yscale$Type`
#'  equal to "likert" (which would indicate an ordered scale).  There is also
#'  a logical value `$Yscale$Rev`.  This is undocumented.  In particular, I have
#'  no idea what values other than "likert" are possible for the Type field,
#'  and which of them might indicate ordered outputs.
#'
#' @examples
option_names <- function(qdesc) {
  sapply(qdesc$Choices[as.character(qdesc$ChoiceOrder)],
         \(a) cleanTags(a$Display))
}
#' @describeIn option_names Order options appear in.
#' @export
option_order <- function(qdesc) {
  sapply(qdesc$ChoiceOrder, \(o) as.numeric(o))
}
#' @describeIn option_names Are the options ordered?
#' @export
options_orderd <- function(qdesc) {
  if (is.null(qdesc$Configuration$Autoscale$Yscale)) return(FALSE)
  qdesc$Configuration$Autoscale$Yscale$Type=="likert"
}
#' @describeIn option_names Are the options ordered?
#' @export
options_rev_orderd <- function(qdesc) {
  if (is.null(qdesc$Configuration$Autoscale$Yscale)) return(FALSE)
  qdesc$Configuration$Autoscale$Yscale$Reverse
}

#' Recodes an integer variable as a factor using the options metadata
#'
#'  This recodes an integer variable as a factor using the state names
#'  in [option_names()] and codes the levels in the order of [option_order()].
#'
#' @param col -- A variable (column in the response data) to be recoded.
#' @param qdesc -- A question description object.
#' @param ordered -- A logical value, if true an ordered factor should be
#' produced.
#' @param reversed -- A logical value, if true the values will be coded
#' with levels `rev(option_names(qdesc))`.
#'
#' @details
#'
#' Note that this generally returns a column which needs to replace
#' the value in the original data frame or tibble.
#'
#' @return A factor variable
#' @export
#' @seealso [answer2factor()] -- For matrix questions,
#' [option_names()] -- For extracting answer metadata
#'
#' @examples
option2factor <- function (col,qdesc, ordered=options_orderd(qdesc),
                          reversed=options_rev_orderd(qdesc)) {
  lab <- sjlabelled::get_label(col)
  onames <- option_names(qdesc)
  oorder <- option_order(qdesc)
  if (reversed) {
    onames <- rev(onames)
    oorder <- rev(oorder)
  }
  out <- factor(match(col, oorder), 1L:length(onames),onames,ordered=ordered)
  sjlabelled::set_label(out) <- lab
  out
}



#' Extract information about matrix answers.
#'
#' In matrix questions, answers are the possible answers
#' the respondant can select.  The stems are the [option_names][options].
#'
#' @param qdesc
#'
#' @return `answer_names()` returns a character vector giving the text
#' associated with the choices in the order they appear.
#' `answer_order()` gives a numeric vector
#' showing the order in which the options appear.  The other two functions return
#' a logical value.
#' @export
#'
#' @details
#'
#' Internal to Qualtrics, the numbering of options seems to be related to when
#' the option was created rather than its order on the question.  This can
#' result in a mismatch between the numeric code in the results and the
#' expectation, and can be especially problematic with ordinal variables.
#'
#' Using `match(col,answer_order(qdesc))` will fix any glitches in the ordering.
#'
#'  Thus to covert the scale to a factor, use
#'  ```
#'  responses$col <- factor(match(col,answer_order(qdesc)),
#'      1L:length(answer_names(qdesc)),answer_names(qdesc), ...)
#'  ```
#'
#' ## Detecting order and reverse ordered scales.
#'
#'  There is an undocumented field "Autoscale" in the "Configuration"
#'  field.  With some questions having `Configuration$Autoscale$Xscale$Type`
#'  equal to "likert" (which would indicate an ordered scale).  There is also
#'  a logical value `$Xscale$Rev`.  This is undocumented.  In particular, I have
#'  no idea what values other than "likert" are possible for the Type field,
#'  and which of them might indicate ordered outputs.
#'
#' @examples
answer_names <- function(qdesc) {
  sapply(qdesc$Answers[as.character(qdesc$AnswerOrder)],
         \(a) cleanTags(a$Display))
}
#' @describeIn answer_names Order answers appear in.
#' @export
answer_order <- function(qdesc) {
  sapply(qdesc$AnswerOrder, \(o) as.numeric(o))
}
#' @describeIn answer_names Are answers ordered
#' @export
answers_orderd <- function(qdesc) {
  if (is.null(qdesc$Configuration$Autoscale$Xscale)) return(FALSE)
  qdesc$Configuration$Autoscale$Xscale$Type=="likert"
}
#' @describeIn answer_names Are answers reverse coded.
#' @export
answers_rev_orderd <- function(qdesc) {
  if (is.null(qdesc$Configuration$Autoscale$Xscale)) return(FALSE)
  qdesc$Configuration$Autoscale$Xscale$Reverse
}

#' Recodes an integer variable as a factor using the answers metadata
#'
#'  This recodes an integer variable as a factor using the state names
#'  in [answer_names()] and codes the levels in the order of [answer_order()].
#'  This is most useful for Matrix questions.
#'
#' @param col -- A variable (column in the response data) to be recoded.
#' @param qdesc -- A question description object.
#' @param ordered -- A logical value, if true an ordered factor should be
#' produced.
#' @param reversed -- A logical value, if true the values will be coded
#' with levels `rev(option_names(qdesc))`.
#'
#' @details
#'
#' Note that this generally returns a column which needs to replace
#' the value in the original data frame or tibble.
#'
#' @return A factor variable
#' @export
#' @seealso [option_factor()] -- For multiple choice questions,
#' [answer_names()] -- For extracting answer metadata
#'
#'
#' @examples
answer2factor <- function (col,qdesc,ordered=answers_orderd(qdesc),
                           reversed=answers_rev_orderd(qdesc)) {
  lab <- sjlabelled::get_label(col)
  anames <- answer_names(qdesc)
  aorder <- answer_order(qdesc)
  if (reversed) {
    anames <- rev(anames)
    aorder <- rev(aorder)
  }
  out <- factor(match(col, aorder), 1L:length(anames),anames, ordered=ordered)
  sjlabelled::set_label(out) <- lab
  out
}



#' Identify options with extra text
#'
#' This is for identifying which option(s) in a multiple choice question
#' have an extra text entry field (e.g., "Other, specify...").
#'
#' @param qdesc
#'
#' @return An integer vector containing the indexes of the options with text entry
#' fields.  If there are none, returns a vector of length 0.
#' @export
#'
#' @examples
whichTextEntry <- function (qdesc) {
  which(sapply(qdesc$Choices,function (c) !is.null(c$TextEntry)))
}


#' Check for questions with duplicate names
#'
#' The column names in the response tibble come from the "DataExportTag"
#' field of the item set in Qualtrics.  Qualtrics does not check that this
#' value is unique, so multiple question descriptions with the same
#' [qname()] will likely cause confusion.
#'
#' @param qlist -- A list of [new_Qdesc][Qdesc] objects.
#'
#' ## Fixing the problem in Qualtrics
#'
#' There are two sources of this problem.  One is that the Qualtrics API exports
#' both active questions, and ones in the trash.  So simply emptying the trash
#' in Quatrics (and redoing the export) may fix the problem.
#'
#' The second cause is that Qualtrics makes no provision that DataExportTags
#' are unique.  So there might be more than one active item with the same tag.
#' This is especially true if items were copied from one survey to another,
#' or more than one person worked on the survey at more than one time.
#' Editing the names in Qualtrics is likely the best way to fix this.
#'
#' ### Naming Conventions
#'
#' The DataExportTag is the first line of the item (looks like a title) which
#' has a default value of "Q<n>".  These are not visible to respondants taking
#' the survey, so pick a convention that will be useful for analysis.  One
#' possible suggestion is to give a short variable name that reflects the
#' question content.  This is especially valuable for Matrix and multi-select
#' questions where the default label is the question stem plus the option, so
#' can be quite long.
#'
#'
#' @return This returns a named list whose names are the duplicated qnames, and
#' whose values are the QIDs of the items with that qname.  If there are no
#' duplicates then an empty list is returned.
#' @export
#' @seealso [qname()], [qid()]
#' Both [fetch_question_descriptions()] and
#' [read_question_descriptions()]  optionally call this after loading
#' a new list of question descriptions to provide a warning about
#' potential problems.
#'
#' @examples
duplicate_qnames <- function(qlist) {
  qnames <- sapply(qlist,qname)
  qids <- sapply(qlist,qid)
  dups <- list()
  for (dup in unique(qnames[duplicated(qnames)])) {
    dups[[dup]] <- qids[dup==qnames]
  }
  dups
}


#' Reads the question description metadata for a survey
#'
#' This downloads a list of descriptions for questions in a survey
#' as a list of [new_Qdesc][Qdesc] objects.
#'
#' @param surveyID -- The Qualtrics ID of the survey.
#' @param check_qnames -- A logical value.  If true [duplicate_qnames()]
#' is run and a warning is issued if there are questions with duplicate
#' [qnames()].
#'
#' ## Cautions
#'
#' The export from Qualtrics contains both questions show to the respondants
#' and inactive questions in the trash.  Consider emptying the trash in
#' Qualtrics before exporting the questions.
#'
#' Note that if multiple questions have the same [qname()] (or DataExportTag)
#' this can cause confustion.  The function [duplicate_qnames()]
#' checks for duplicates.  If `check_qnames=TRUE` then [duplicate_qnames()]
#' is run an a warning is issued if duplicates are found.  Running it again
#' will provide more information to debug the problem.
#'
#' @return A list of [new_Qdesc][Qdesc] objects.
#' @export
#' @seealso [read_question_descriptions()] will work from a saved `qsf` file.
#'
#' @examples
#' \dontrun{
#' qlist <- fetch_question_descriptions("SV0000001")
#' }
fetch_question_descriptions <- function(surveyID, check_qnames=TRUE) {
  qlist <- lapply(fetch_description(surveyID,"questions")$questions,
                  as_Qdesc)
  if (check_qnames) {
    dups <- duplicate_qnames(qlist)
    if (length(dups) > 0L)
      warning("Questions with duplicate names: ",paste(names(dups),collapse=", "))
  }
  qlist
}

#' Reads the question description metadata from a Qualtrics qsf file.
#'
#' A survey can be exported from Qualtrics as a `qsf` file.  This
#' extracts the qustion information as a list of [new_Qdesc][Qdesc] objects.
#'
#' @param qsffile -- A [base::file()] or something that can be converted to
#' one (e.g., a pathname) for the `qsf` file exported from Qualtrics
#' @param check_qnames -- A logical value.  If true [duplicate_qnames()]
#' is run and a warning is issued if there are questions with duplicate
#' [qnames()].
#'
#' ## Cautions
#'
#' The export from Qualtrics contains both questions show to the respondants
#' and inactive questions in the trash.  Consider emptying the trash in
#' Qualtrics before exporting the questions.
#'
#' Note that if multiple questions have the same [qname()] (or DataExportTag)
#' this can cause confustion.  The function [duplicate_qnames()]
#' checks for duplicates.  If `check_qnames=TRUE` then [duplicate_qnames()]
#' is run an a warning is issued if duplicates are found.  Running it again
#' will provide more information to debug the problem.
#'
#' @return A list of [new_Qdesc][Qdesc] objects.
#' @export
#'
#' @examples
read_question_descriptions <- function(qsffile, check_qnames=TRUE) {
  qsf <- jsonlite::fromJSON(qsffile,FALSE)$SurveyElements
  whichSQ <- which(sapply(qsf, \(el) el$Element=="SQ"))
  qlist <- lapply(qsf[whichSQ], \(sq) as_Qdesc(sq$Payload))
  names(qlist) <- sapply(qlist, \(qd) qid(qd))
  if (check_qnames) {
    dups <- duplicate_qnames(qlist)
    if (length(dups) > 0L)
      warning("Questions with duplicate names: ",paste(names(dups),collapse=", "))
  }
  qlist
}

findCols <- function(qdesc,responses)
  grep(paste0("^",qname(qdesc),"(_.+)?$"),names(responses),value=TRUE)


