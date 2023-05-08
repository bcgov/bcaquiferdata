# Code Design

This file contains notes about code design and conventions with the aim of
making collaboration and future modifications easier.

## Naming
- Snake case is used wherever possible
- Test files are named `test-XX_DESCRIPTION.R`, where `XX` is the order they
should be run (try to test lower order functions first).

## Documentation
- Descriptions for parameter arguments should follow:
  - `@param arg_name Type. Description`
  - e.g., `@param lidar_dir Character. File path of where LiDAR tiles should be stored.`
  - e.g., `@param region sf simple features object. Shape file of the region of interest.`

- Document repetitive arguments (ones found in more than one function) in the
`R/aa_common_docs.R` file (named to sort to the top of the folder), and use
`@inheritParams common_docs` in the function documentation. This way
duplicate documentation stays consistent.

- Use `@noRd` to document internal functions (documentation for developers that
  isn't compiled into the docs)
