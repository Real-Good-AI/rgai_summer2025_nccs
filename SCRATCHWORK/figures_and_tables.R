####### Cleaning BMF File #######
# Before dropping orgs with missing county FIPS code in BMF cleaning 
df.temp <- bmf_subset |> filter(county.census.geoid == "00000") # 6815 records
sum(is.na(df.temp$NTEEV2)) # 189 records

df.temp <- df.temp |> filter(!is.na(NTEEV2))
plot(table(df.temp$NTEEV2), 
     main = "NTEE Distribution in Organizations with Missing County Location",
     xlab = "NTEE Broad Category",
     ylab = "Count")

df.temp <- bmf_subset |> filter(county.census.geoid != "00000") # 3,437,320 records
sum(is.na(df.temp$NTEEV2)) # 166,256 records

df.temp <- df.temp |> filter(!is.na(NTEEV2))
plot(table(df.temp$NTEEV2), 
     main = "NTEE Distribution in Organizations with no Missing County Location",
     xlab = "NTEE Broad Category",
     ylab = "Count")

# After creating a column n to indicate if record is duplicated
df.temp <- bmf_subset |> filter(n > 1) |> mutate(state.FIPS = as.integer(state.FIPS)) # 14,306 records
df.temp <- df.temp |> filter(state.FIPS <= 56) # because these codes correspond to US States, not territories, left with 14,258
df.temp <- df.temp |> select(EIN2, state.FIPS) |> distinct() # 7128 records
plot(table(as.numeric(df.temp$state.FIPS)), 
     main = "State Distribution, Organizations with Conflicting NTEE code",
     xlab = "State FIPS code",
     ylab = "Count")

df.temp <- bmf_subset |> filter(n == 1) |> mutate(state.FIPS = as.integer(state.FIPS)) # 3,423,014 records
df.temp <- df.temp |> filter(state.FIPS <= 56) # because these codes correspond to US States, not territories, left with 3,416,270
df.temp <- df.temp |> select(EIN2, state.FIPS) |> distinct() # doesn't change because these were unique to begin with
plot(table(as.numeric(df.temp$state.FIPS)), 
     main = "State Distribution, Organizations with no NTEE Conflict",
     xlab = "State FIPS code",
     ylab = "Count")