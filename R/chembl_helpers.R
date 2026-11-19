#' Internal function that returns all possible chembl resources
#' @keywords internal
#' @noRd
.chembl_resources <- function() {
  c(
    "activity",
    "assay",
    "atc_class",
    "binding_site",
    "biotherapeutic",
    "cell_line",
    "chembl_id_lookup",
    "compound_record",
    "compound_structural_alert",
    "document",
    "document_similarity",
    "document_term",
    "drug",
    "drug_indication",
    "drug_warning",
    "go_slim",
    "image",
    "mechanism",
    "metabolism",
    "molecule",
    "molecule_form",
    "organism",
    "protein_classification",
    "similarity",
    "source",
    "status",
    "substructure",
    "target",
    "target_component",
    "target_relation",
    "tissue",
    "xref_source"
  )
}

#' Internal function that returns all possible chembl filter types
#' @keywords internal
#' @noRd
.chembl_filter_types <- function() {
  c(
    "exact",
    "iexact",
    "contains",
    "icontains",
    "startswith",
    "istartswith",
    "endswith",
    "iendswith",
    "regex",
    "iregex",
    "gt",
    "gte",
    "lt",
    "lte",
    "range",
    "in",
    "isnull",
    "search",
    "only"
  )
}

#' Internal function that returns all possible chembl mechanism columns
#' @keywords internal
#' @noRd
.chembl_mechanism_cols <- function() {
  c(
    "action_type",
    "binding_site_comment",
    "direct_interaction",
    "disease_efficacy",
    "max_phase",
    "mec_id",
    "mechanism_comment",
    "mechanism_of_action",
    "mechanism_refs",
    "molecular_mechanism",
    "molecule_chembl_id",
    "parent_molecule_chembl_id",
    "record_id",
    "selectivity_comment",
    "site_id",
    "target_chembl_id",
    "variant_sequence"
  )
}

#' Internal function that returns the complete schema for a Chembl resource
#' @keywords internal
#' @noRd
.chembl_resource_schema <- function(resource) {
  .cache_fetch(
    namespace = "chembl/schema",
    params = list(resource = resource),
    FUN = function() {
      .build_chembl_request(paste0(resource, "/schema")) |>
        .perform_request() |>
        .parse_chembl_json()
    }
  )
}

#' Parse a ChEMBL response without treating service failures as data
#' @keywords internal
#' @noRd
.parse_chembl_json <- function(resp) {
  status <- httr2::resp_status(resp)
  content_type <- httr2::resp_content_type(resp)
  unavailable <- status == 429L || status >= 500L
  html <- status >= 200L && status < 300L &&
    content_type %in% c("text/html", "application/xhtml+xml")
  if (unavailable || html) {
    detail <- if (html) {
      paste0('Unexpected content type "', content_type, '" instead of JSON')
    } else {
      paste("HTTP", status)
    }
    stop(errorCondition(
      paste0("ChEMBL service unavailable: ", detail, "."),
      class = "annotationgx_chembl_unavailable"
    ))
  }
  # Authentication, invalid requests, malformed JSON and other errors still fail.
  httr2::resp_check_status(resp)
  .parse_resp_json(resp)
}
