# Copyright 2022 DARWIN EU®
#
# This file is part of IncidencePrevalence
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

#' Check the database type.
#'
#' @param cdm cdm reference
#' @param type type of the database, default cdm_reference
#' @param messageStore checkmate collection
#'
checkDbType <- function(cdm, type = "cdm_reference", messageStore) {
  dbInheritsCheck <- inherits(cdm, type)
  checkmate::assertTRUE(dbInheritsCheck,
                        add = messageStore)
  if (!isTRUE(dbInheritsCheck)) {
    messageStore$push(glue::glue("- cdm must be a CDMConnector {type} object"))
  }
}


#' Check if given table exists in cdm.
#'
#' @param cdm the db reference
#' @param tableName checkmate collection
#' @param messageStore the message store
#'
checkTableExists <- function(cdm, tableName, messageStore) {
  table_exists <- inherits(cdm[[tableName]], c("tbl_dbi",
                                               "ArrowObject", "ArrowTabular",
                                               "tbl" ,  "data.frame"))
  checkmate::assertTRUE(table_exists, add = messageStore)
  if (!isTRUE(table_exists)) {
    messageStore$push(glue::glue("- {tableName} is not found in the cdm reference"))
  }
}
