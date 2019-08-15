library(jsonlite)
fn <- path.expand("~/Package/growthscreener/allegrosultum/Voorbeeld antwoordbericht (2019-08-12).json")
z <- fromJSON(fn)
z
fromJSON(toJSON(z))

