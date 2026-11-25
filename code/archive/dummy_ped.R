# set up dummy ped for PMx

# get sex data
wmzf <- read.table("odata/wmzf.txt")

# create dummy ped
ped <- data.frame(ID = wmzf$V2,
                  DamID = if_else(wmzf$V2 == wmzf$V2[wmzf$V3 == 2][1], "WILD", if_else(wmzf$V2 == wmzf$V2[wmzf$V3 == 1][1], "WILD", wmzf$V2[wmzf$V3 == 2][1])),
                  SireID = if_else(wmzf$V2 == wmzf$V2[wmzf$V3 == 2][1], "WILD", if_else(wmzf$V2 == wmzf$V2[wmzf$V3 == 1][1], "WILD", wmzf$V2[wmzf$V3 == 1][1])),
                  BirthDate = if_else(wmzf$V2 == wmzf$V2[wmzf$V3 == 2][1], "20010117", if_else(wmzf$V2 == wmzf$V2[wmzf$V3 == 1][1], "20010117", "20030117")),
                  BirthDateEst = "",
                  Sex = if_else(wmzf$V3 == 2, 0, 1),
                  DeathDate = "",
                  Location = "",
                  LocalID = "",
                  SocialGroup = "",
                  Selected = "T",
                  Dead = "F",
                  OtherID = "")

# write dummy ped
write.table(ped, "pmx/dummy_ped.txt", row.names = F, col.names = T, quote = F, sep = "\t")
