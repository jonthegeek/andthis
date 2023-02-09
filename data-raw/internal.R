.club_gs4_sheet_id <- "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc"
.club_gs4_sheet_approved <- glue::glue(
  "https://docs.google.com/spreadsheets/d/{.club_gs4_sheet_id}/",
  "edit#gid=1952988590"
)

usethis::use_data(
  .club_gs4_sheet_id,
  .club_gs4_sheet_approved,
  internal = TRUE,
  overwrite = TRUE
)

rm(
  .club_gs4_sheet_id,
  .club_gs4_sheet_approved
)
