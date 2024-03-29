# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

# common_docs ------------------
#' Common arguments and documentation for various functions
#'
#' @param region sf simple features object. Shape file of the region of
#'   interest.
#' @param update Logical. Force update of the data?
#' @param wells_sub sf spatial data frame. Subset of wells data output by
#'   `wells_subset()`
#' @param permission Logical. Permission to create the cache folder. If `FALSE`,
#'   user is asked for permission, if `TRUE`, permission is implied.
#'
#' @details
#' Use `@inheritParams common_docs` to include the above in any function
#' documentation with a matching argument (will only include matching args)
#'
#' @keywords internal
#' @name common_docs
NULL
