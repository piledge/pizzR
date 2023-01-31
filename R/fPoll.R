fPoll <- function(species, dbh_cm, height_m) {
  dbh = dbh_cm / 10
  height = height_m * 10
  
  if (species == 1 & dbh > 1.05) { #picea, other coniferous wood
    b1 = 0.46818
    b2 = -0.013919
    b3 = -28.213
    b4 = 0.37474
    b5 = -0.28875
    b6 = 28.279
    b7 = 0
  }
  if (species == 1 & dbh <= 1.05) { #picea, other coniferous wood
    b1 = 0.563443
    b2 = -0.12731
    b3 = -8.55022
    b4 = 0
    b5 = 0
    b6 = 7.6331
    b7 = 0
  }
  if (species == 2 & dbh > 1.05) { #abies
    b1 = 0.580223
    b2 = -0.0307373
    b3 = -17.1507
    b4 = 0.089869
    b5 = -0.080557
    b6 = 19.661
    b7 = -2.45844
  }
  if (species == 2 & dbh <= 1.05) { #abies
    b1 = 0.560673
    b2 = 0.15468
    b3 = -0.65583
    b4 = 0.03321
    b5 = 0
    b6 = 0
    b7 = -0
  }
  if (species == 3 & dbh > 1.05) { #larix
    b1 = 0.609443
    b2 = -0.0455748
    b3 = -18.6631
    b4 = -0.248736
    b5 = 0.126594
    b6 = 36.9783
    b7 = -14.204
  }
  if (species == 3 & dbh <= 1.05) { #larix
    b1 = 0.48727
    b2 = 0
    b3 = -2.04291
    b4 = 0
    b5 = 0
    b6 = 5.9995
    b7 = 0
  }
  if (species == 4) { #white pine
    b1 = 0.435949
    b2 = -0.0149083
    b3 = 5.21091
    b4 = 0
    b5 = 0.028702
    b6 = 0
    b7 = 0
  }
  if (species == 5) { #black pine
    b1 = 0.53438
    b2 = -0.00763
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 2.2414
  }
  if (species == 6) { #pinus cembra
    b1 = 0.525744
    b2 = -0.0334896
    b3 = 7.38943
    b4 = -0.10646
    b5 = 0
    b6 = 0
    b7 = 3.34479
  }
  if (species == 10 & dbh > 1.05) { #fagus sylvatica, aesculus hippocastanum, robinia pseudoacacia, sorbus
    b1 = 0.686253
    b2 = -0.0371508
    b3 = -31.0674
    b4 = -0.386321
    b5 = 0.219462
    b6 = 49.6163
    b7 = -22.3719
  }
  if (species == 10  & dbh <= 1.05) { #fagus sylvatica, aesculus hippocastanum, robinia pseudoacacia, sorbus
    b1 = 0.5173
    b2 = 0
    b3 = -13.62144
    b4 = 0
    b5 = 0
    b6 = 9.9888
    b7 = 0
  }
  if (species == 11 & dbh > 1.05) { #oak
    b1 = 0.115631
    b2 = 0
    b3 = 65.9961
    b4 = 1.20321
    b5 = -0.930406
    b6 = -215.758
    b7 = 168.477
  }
  if (species == 11 & dbh <= 1.05) { #oak
    b1 = 0.417118
    b2 = 0.21941
    b3 = 13.32594
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
  }
  if (species == 12) { #carpinus betulus
    b1 = 0.32473
    b2 = 0.02432
    b3 = 0
    b4 = 0.23972
    b5 = 0
    b6 = -9.9388
    b7 = 0
  }
  if (species == 13) { #fraxinus
    b1 = 0.48122
    b2 = -0.01489
    b3 = -10.83056
    b4 = 0
    b5 = 0
    b6 = 9.3936
    b7 = 0
  }
  if (species == 14 ) { #acer, tilia
    b1 = 0.50101
    b2 = -0.03521
    b3 = -8.07176
    b4 = 0
    b5 = 0.03521
    b6 = 0
    b7 = 0
  }
  if (species == 15) { #ulmus
    b1 = 0.44215
    b2 = -0.02446
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 2.87714
  }
  if (species == 19) { #betula
    b1 = 0.42831
    b2 = -0.06643
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 8.4307
    b7 = 0
  }
  if (species == 20 & dbh > 1.05) { #alnus
    b1 = 0.42937
    b2 = 0
    b3 = -4.10259
    b4 = 0
    b5 = 0
    b6 = 16.7578
    b7 = -5.16631
  }
  if (species == 20 & dbh <= 1.05) { #alnus
    b1 = 0.387399
    b2 = 0
    b3 = 7.17123
    b4 = 0.04407
    b5 = 0
    b6 = 0
    b7 = 0
  }
  if (species == 22 & dbh > 1.05) { #populus alba
    b1 = 0.31525
    b2 = 0
    b3 = 0
    b4 = 0.51079
    b5 = -0.34279
    b6 = -26.08
    b7 = 28.6334
  }
  if (species == 22 & dbh <= 1.05) { #populus alba
    b1 = 0.366419
    b2 = 0
    b3 = 1.13323
    b4 = 0.1306
    b5 = 0
    b6 = 0
    b7 = 0
  }
  if (species == 23 ) {  #populus nigra, populus tremula
    b1 = 0.4115
    b2 = -0.00989
    b3 = -28.27478
    b4 = 0.35599
    b5 = -0.21986
    b6 = 21.4913
    b7 = 0
  }
  if (species == 26 ) { #salix, prunus, other broadleaved trees
    b1 = 0.54008
    b2 = -0.02716
    b3 = -25.11447
    b4 = 0.08327
    b5 = 0
    b6 = 9.3988
    b7 = 0
  }
  if (species == 28 ) { #corylus, pinus mugo
    b1 = 0
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
  }
  if (species == 30 & dbh > 1.05) { #other broadleaved trees
    b1 = 0.686253
    b2 = -0.0371508
    b3 = -31.0674
    b4 = -0.386321
    b5 = 0.219462
    b6 = 49.6163
    b7 = -22.3719
  }
  if (species == 30  & dbh <= 1.05) { #other broadleaved trees
    b1 = 0.5173
    b2 = 0
    b3 = -13.62144
    b4 = 0
    b5 = 0
    b6 = 9.9888
    b7 = 0
  }

  return(b1 + b2 * (log(dbh))^2 + b3 * (1 / height) + b4 * (1 / dbh) + b5 * (1 / (dbh^2)) + b6 * (1 / (dbh * height)) + b7 * (1 / (dbh^2 * height)))
}

fPoll <- Vectorize(fPoll)
