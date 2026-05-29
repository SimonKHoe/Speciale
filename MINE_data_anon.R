# Load data
df <-
  read_sav("260428_ekstra.sav") |>
  select(
    -Q47,
    -starts_with("Create_new_Field"),
    -LUCIDUserfacinghistory
  )

df |>
  haven::write_sav("260528_data.sav")
