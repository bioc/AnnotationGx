# Changes in version 0.99.12

- Identify CC BY 4.0 explicitly for every bundled metadata object and the
  Cellosaurus field table, with source license evidence and attribution.
- Clarify that the generated annotated treatment TSV and its development
  article are excluded from the source package and unused at runtime.
- Exclude recipe.yaml and recipes.yaml from the source package.

# Changes in version 0.99.11

- Remove bundled GDSC metadata and dedicated source files; direct users to
  official downloads subject to the upstream usage policy.
- Use CCLE metadata in introductory examples.
- Keep ChEMBL vignette examples evaluated while reporting service failures
  without aborting the build.
- Test ChEMBL response handling with deterministic fixtures, including HTML
  responses, and retain a guarded live endpoint test.
- Support the updated UniChem source schema while preserving the documented
  output columns and representing unavailable release metadata as `NA`.
- Apply the existing request timeout and retry policy to PubChem annotation
  requests so temporary HTTP 503 responses can recover.

# Changes in version 0.99.8

- Address Bioconductor submission review feedback.

# Changes in version 0.99.7

- Add persistent caching with `BiocFileCache` for remote metadata and parsed API query results.
