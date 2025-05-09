#####################################################################
##  GLOBAL MORAN'S I  –  Are deaths_per_hab spatially clustered?  ##
#####################################################################

## pick the variable you want to test --------------------------------
##   → use deaths_per_area, deaths_per_hab, or any other numeric field
gvar <- deaths_map$deaths_per_hab |>          # numeric vector (one per district)
  tidyr::replace_na(0)                        # moran can't handle NAs

## 1) Build a neighbour list  (Queen contiguity: any shared border/vertex)
##    spdep needs a Spatial* object → convert on-the-fly
nb <- spdep::poly2nb(
  sf::as_Spatial(deaths_map),                 # districts polygons
  queen = TRUE                                # TRUE = Queen, FALSE = Rook
)

## 2) Turn neighbours into a row-standardised weight matrix
lw <- spdep::nb2listw(
  nb, style = "W", zero.policy = TRUE         # zero.policy handles islands
)

## 3) Global Moran's I  – Monte-Carlo permutation test (999 perms)
set.seed(2025)                                # reproducibility
glob_moran <- spdep::moran.mc(
  x           = gvar,
  listw       = lw,
  nsim        = 9999,
  zero.policy = TRUE,
  na.action   = na.exclude
)

print(glob_moran)

mt <- spdep::moran.test(gvar, lw, zero.policy = TRUE)
mt                             # mostra I esperado, variância, z-score

print(glob_moran, digits = 4)
