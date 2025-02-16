block_items <- function (block) {
  if (length(block$BlockElements)==0L) return (character())
  unlist(sapply(block$BlockElements, \(el) el$QuestionID))
}

trashed_items <- function (blockset) {
  do.call("c",
          sapply(blockset, function (bl) {
            if (bl$Type=="Trash") {
              block_items(bl)
            } else character()
          }))
}

qsf2blocks <- function(qsf) {
  blem <- which(sapply(qsf$SurveyElements,\(el) el$Type=="BL"))
  qsf$SurveyElements[[blem]]$Payload
}

block_table <- function(blockset) {
  blocks <- lapply(blockset, \(bl) block_items(bl))
  names(blocks) <- sapply(blockset, \(bl) bl$Description)
  blocks
}
