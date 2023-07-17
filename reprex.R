# Setup ----

set.seed(123)
options(box.path = getwd())

# Required packages/functions
box::use(
  # reading and handling data
  vroom[vroom],
  sf,
  dplyr[mutate, rename_with],
  sfheaders[sf_polygon],
  tidyr[pivot_longer],
  # modelling packages
  spm = spmodel,
  gs = gstat,
  # visualisation
  ggplot2[...]
)


# Data & EDA ----

ch_precip = vroom("data/switzerland_precip.dat")
ch_border = vroom("data/switzerland_border.dat")

# convert to sf objects
ch_precip = sf$st_as_sf(ch_precip, coords = paste0(c("x", "y"), "coord"))
ch_border = sf_polygon(ch_border)

# use a square root transformation for the response
head(ch_precip)
ch_precip = mutate(ch_precip, sqrt_rain = sqrt(rain))

# visualise the data (spatially)
ggplot() +
  geom_sf(data = ch_border) +
  geom_sf(aes(color = sqrt_rain), data = ch_precip) +
  theme_void() +
  scale_color_viridis_c()

# empirical variogram
evg = spm$esv(sqrt_rain ~ 1, data = ch_precip)
ggplot(evg, aes(dist, gamma)) +
  geom_point()


# Modelling & kriging ----

# Fit an isotropic model to the data using a spheric correlation function
# and restricted maximum likelihood.
# Works really well!
m_spher = spm$splm(
  sqrt_rain ~ 1,
  data = ch_precip,
  spcov_type = "spherical",
  estmethod = "reml"
)

summary(m_spher)

# Create a grid for kriging / predictions
ch_grid = sf$st_make_grid(ch_border, what = "centers", n = c(100, 100))
ch_grid = ch_grid[ch_border] |>
  # convert from `sfc_POINT` to `sf`, which makes `predict.splm()` work
  sf$st_as_sf()
plot(ch_grid)


# Try to do kriging using the spmodel package.
# Works now (but long computation time).
kg_spm = predict(m_spher, newdata = ch_grid, interval = "prediction")


# I still prefer using gstat as it is much faster.
# However, I cannot tell if it systematically performs better or worse yet.
params = coef(m_spher, type = "spcov")

vgm_spher = gs$vgm(
  model = "Sph",
  psill = params["de"],
  nugget = params["ie"],
  range = params["range"]
)

kg_gs = gs$krige(
  sqrt_rain ~ 1,
  ch_precip,
  ch_grid,
  model = vgm_spher
)

kg_gs = cbind(
  kg_gs,
  as.data.frame(sf$st_coordinates(kg_gs)) |>
    rename_with(\(x) paste0(tolower(x), "coord"))
)

ggplot() +
  geom_sf(data = ch_border) +
  theme_void() +
  geom_raster(
    aes(xcoord, ycoord, fill = value),
    data = pivot_longer(kg_gs, paste0("var1.", c("pred", "var")))
  ) +
  facet_wrap(
    ~ name,
    labeller = labeller(
      name = c("var1.pred" = "Prediction", "var1.var" = "Variance")
    )
  ) +
  scale_fill_viridis_c(name = NULL)


(5.356966 - 2.0813387) / qnorm(0.975)

2.0813387 - qnorm(0.975) * 1.671269
