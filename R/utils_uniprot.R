# uniprot_get.R
# ::rtemis::
# 2022 E.D. Gennatas lambdamd.org

#' Get protein sequence from UniProt
#'
#' @param accession Character: UniProt Accession number - e.g. "Q9UMX9"
#' @param baseURL Character: UniProt rest API base URL.
#' Default = "https://rest.uniprot.org/uniprotkb"
#' @param verbosity Integer: Verbosity level.
#'
#' @return List with three elements: Identifier, Annotation, and Sequence.
#' @author E.D. Gennatas
#' @export
#'
#' @examples
#' \dontrun{
#' mapt <- uniprot_get("Q9UMX9")
#' }
uniprot_get <- function(
  accession,
  baseURL = "https://rest.uniprot.org/uniprotkb",
  verbosity = 1
) {
  # Check types
  check_inherits(accession, "character")

  path <- paste0(baseURL, "/", accession, ".fasta")
  dat <- seqinr::read.fasta(path, seqtype = "AA")
  Annotation <- attr(dat[[1]], "Annot")
  Identifier <- gsub(" .*", "", Annotation)
  if (verbosity > 0L) {
    msg("Got:", highlight(Annotation))
  }

  list(
    Identifier = Identifier,
    Annotation = Annotation,
    Sequence = as.character(dat[[1]])
  )
} # /rtemis::uniprot_get
