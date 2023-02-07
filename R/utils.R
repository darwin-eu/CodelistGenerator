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

checkDbType <- function(cdm, type = "cdm_reference", messageStore) {
  dbInheritsCheck <- inherits(cdm, type)
  checkmate::assertTRUE(dbInheritsCheck,
                        add = messageStore)
  if (!isTRUE(dbInheritsCheck)) {
    messageStore$push(glue::glue("- cdm must be a CDMConnector {type} object"))
  }
}

checkTableExists <- function(cdm, tableName, messageStore) {
  tableExists <- inherits(cdm[[tableName]], c("tbl_dbi",
                                               "ArrowObject", "ArrowTabular",
                                               "tbl",  "data.frame"))
  checkmate::assertTRUE(tableExists, add = messageStore)
  if (!isTRUE(tableExists)) {
    messageStore$push(glue::glue("- {tableName} is not found in the cdm reference"))
  }
}
