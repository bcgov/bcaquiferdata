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

#' @importFrom rlang .data .env
NULL


# Dealing with CRAN Notes due to Non-standard evaluation
.onLoad <- function(
  libname = find.package("bcaquiferdata"),
  pkgname = "bcaquiferdata"
) {
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      # Vars used in Non-Standard Evaluations, declare here to
      # avoid CRAN warnings
      c(
        # Expaneded lists in lithology_cleaning.R
        "terms_good_main",
        "terms_good_org",
        "terms_good_main_y",
        "terms_good_sgtill",
        "terms_good_bedrock_desc",
        "terms_good_bedrock",
        "terms_good_other",
        "terms_good_first",
        "terms_good_extra",
        "terms_good_yield",
        "terms_good_joins",
        "dem", # expanded list of reactives in mod_export_data.R
        "wells"
        # "." # piping requires '.' at times
      )
    )
  }
}
