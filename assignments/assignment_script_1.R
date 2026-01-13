DB = brickman_database() |> print()

brickman_variables("all")

db = DB |> 
  dplyr::filter(interval == "static")
static_vars = read_brickman(db)
static_vars

plot(static_vars['depth'], axes = TRUE)

db = DB |>
  dplyr::filter(scenario == "RCP85", 
                year == 2075,
                interval == "mon")
x = read_brickman(db)
x
plot(x['Tbtm'], axes = FALSE)

buoys = gom_buoys()
buoys
plot(x['Tbtm'] |> dplyr::filter(month == "Jan"), axes = FALSE, reset = FALSE)
plot(sf::st_geometry(buoys), col = "orange", pch = 20, add = TRUE)
text(buoys, labels = buoys$id, adj = c(1,1), col = "blue", cex = 0.8)

long_values = extract_brickman(x, buoys)
long_values

wide_values = extract_brickman(x, buoys |> filter(id == "M01"), form = "wide")
print(wide_values, n = 72)

ggplot(data = transform(wide_values |> dplyr::filter(.id == "p5"), month = factor(month, levels = month.abb)),
       mapping = aes(x = month, y = SST)) + 
  geom_point() + 
  labs(title = "RCP4.5 2055 SST at Buoy M01")

#questions to ask: how do we know which one is M05? the .id is not their names
# how to filter NA's in a stars