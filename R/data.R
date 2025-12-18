# Copyright 2025 DARWIN EU®
#
# This file is part of CodelistGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Table showing the route category associated with each dose form.
#' @format A data frame
#' \describe{
#'  \item{dose_form_concept_id}{Concept ID of each dose form}
#'  \item{dose_form_concept_name}{Concept name of each dose form}
#'  \item{route_category}{Route category associated to the dose form}
#' }
#' @keywords data
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' doseFormToRoute
#' }
"doseFormToRoute"
