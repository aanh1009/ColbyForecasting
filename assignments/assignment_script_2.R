DB = brickman_database() |> print()

brickman_variables("all")

db = DB |>
  dplyr::filter(scenario == "RCP85", 
                year == 2075,
                interval == "mon")
x = read_brickman(db)
x

points = read_model_input(scientificname = "Centropristis striata")
grouped_points = points |> 
  dplyr::group_by(month, class) |>
  print()

selected_points = grouped_points |>
  slice(1) |>
  dplyr::ungroup() |>
  print(n = 24)


wide_values = extract_brickman(x, selected_points, form = "wide") |>
  print(n = 24)

final_df = wide_values |>
  dplyr::select(-MLD, -Sbtm, -U, -V, -Xbtm) |>
  print(n = 24)
