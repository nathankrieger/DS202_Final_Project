Analysis on NFL Play-By-Play Data
================
Nathan Krieger, Caleb Moe

``` r
pbp <- readr::read_csv("https://nflsavant.com/pbp_data.php?year=2024")
```

    ## New names:
    ## Rows: 53283 Columns: 45
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (10): OffenseTeam, DefenseTeam, Description, Formation, PlayType, PassT... dbl
    ## (29): GameId, Quarter, Minute, Second, Down, ToGo, YardLine, SeriesFirs... lgl
    ## (5): ...11, ...13, ...17, ...18, Challenger date (1): GameDate
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...11`
    ## • `` -> `...13`
    ## • `` -> `...17`
    ## • `` -> `...18`

**Introduction**

The NFL has become increasingly data-driven, and play-by-play (PBP)
information provides one of the richest views into how teams behave on
the field. In recent years, analysts have used PBP data to uncover
trends such as fourth-down aggression, pass-rate over expectation, route
combinations, motion usage, and situational efficiency.

Our project explores play-by-play trends across the NFL, focusing on how
offensive decision-making and performance vary by:

Down & distance

Field position

Play type (run/pass)

Quarter & game situation

Score differential

Team tendencies

The goal is to identify meaningful behavioral patterns and performance
outcomes that can help explain how modern offenses operate.

We also evaluate the robustness of our findings with multiple approaches
and highlight unintuitive results requiring further investigation.

**Data**

Our analysis uses play-by-play data from NFLsavant.com, a publicly
accessible resource containing detailed information for every offensive
snap. The dataset includes 45 variables such as:

GameId

Quarter

Minute

YardLine

Formation

And many more…

``` r
# data exploration
head(pbp)
```

    ## # A tibble: 6 × 45
    ##      GameId GameDate   Quarter Minute Second OffenseTeam DefenseTeam  Down  ToGo
    ##       <dbl> <date>       <dbl>  <dbl>  <dbl> <chr>       <chr>       <dbl> <dbl>
    ## 1    2.02e9 2024-12-29       3      7      3 GB          MIN             1    10
    ## 2    2.02e9 2024-12-29       3      9     44 MIN         GB              0     0
    ## 3    2.02e9 2024-12-29       3      9     44 MIN         GB              0     0
    ## 4    2.02e9 2024-12-29       3      9     50 MIN         GB              1    10
    ## 5    2.02e9 2024-12-29       3     15      0 MIN         GB              0     0
    ## 6    2.02e9 2024-12-29       2      2      0 GB          MIN             0     0
    ## # ℹ 36 more variables: YardLine <dbl>, ...11 <lgl>, SeriesFirstDown <dbl>,
    ## #   ...13 <lgl>, NextScore <dbl>, Description <chr>, TeamWin <dbl>,
    ## #   ...17 <lgl>, ...18 <lgl>, SeasonYear <dbl>, Yards <dbl>, Formation <chr>,
    ## #   PlayType <chr>, IsRush <dbl>, IsPass <dbl>, IsIncomplete <dbl>,
    ## #   IsTouchdown <dbl>, PassType <chr>, IsSack <dbl>, IsChallenge <dbl>,
    ## #   IsChallengeReversed <dbl>, Challenger <lgl>, IsMeasurement <dbl>,
    ## #   IsInterception <dbl>, IsFumble <dbl>, IsPenalty <dbl>, …

``` r
nrow(pbp)
```

    ## [1] 53283

``` r
pbp %>%
  filter(IsPass == 1) %>%
  summarize(
    n_passes = n(),
    n_ints   = sum(IsInterception == 1, na.rm = TRUE),
    int_rate = mean(IsInterception == 1, na.rm = TRUE)
  )
```

    ## # A tibble: 1 × 3
    ##   n_passes n_ints int_rate
    ##      <int>  <int>    <dbl>
    ## 1    19551    442   0.0226

``` r
pbp %>%
  filter(IsPass == 1,
         YardLine >= 90, YardLine <= 99) %>%
  summarize(
    n_passes = n(),
    n_ints   = sum(IsInterception == 1, na.rm = TRUE),
    int_rate = mean(IsInterception == 1, na.rm = TRUE)
  )
```

    ## # A tibble: 1 × 3
    ##   n_passes n_ints int_rate
    ##      <int>  <int>    <dbl>
    ## 1       37      0        0

``` r
#We can make PlayType a factor, I think we probably should.
#We can also make Pass Type a factor
pbp <- pbp %>%
  mutate(
    PlayType = factor(PlayType),
    PassType = factor(PassType)
  )
levels(pbp$PlayType)
```

    ##  [1] "CLOCK STOP"           "EXCEPTION"            "EXTRA POINT"         
    ##  [4] "FIELD GOAL"           "FUMBLES"              "KICK OFF"            
    ##  [7] "NO PLAY"              "PASS"                 "PENALTY"             
    ## [10] "PUNT"                 "QB KNEEL"             "RUSH"                
    ## [13] "SACK"                 "SCRAMBLE"             "TIMEOUT"             
    ## [16] "TWO-POINT CONVERSION"

``` r
levels(pbp$PassType)
```

    ## [1] "BACK TO"      "DEEP LEFT"    "DEEP MIDDLE"  "DEEP RIGHT"   "SHORT LEFT"  
    ## [6] "SHORT MIDDLE" "SHORT RIGHT"

``` r
#inspecting field goals to prepare to add "score" variables to the dataset
pbp %>%
  filter(PlayType == "FIELD GOAL")
```

    ## # A tibble: 1,162 × 45
    ##      GameId GameDate   Quarter Minute Second OffenseTeam DefenseTeam  Down  ToGo
    ##       <dbl> <date>       <dbl>  <dbl>  <dbl> <chr>       <chr>       <dbl> <dbl>
    ##  1   2.02e9 2024-12-29       4      2     24 NYG         IND             4     5
    ##  2   2.02e9 2024-12-29       1     10     50 JAX         TEN             4     6
    ##  3   2.02e9 2024-12-29       2      0      3 NO          LV              3     9
    ##  4   2.02e9 2024-12-29       4      0      2 ATL         WAS             1    10
    ##  5   2.02e9 2024-12-29       1      6      7 MIN         GB              4     3
    ##  6   2.02e9 2024-12-29       2      9     29 IND         NYG             4     9
    ##  7   2.02e9 2024-12-29       1      3     48 IND         NYG             4     5
    ##  8   2.02e9 2024-12-22       2      0      3 CIN         CLE             4    14
    ##  9   2.02e9 2024-12-25       1      2     51 BAL         HOU             4     9
    ## 10   2.02e9 2024-12-22       3      6     38 DAL         TB              4     6
    ## # ℹ 1,152 more rows
    ## # ℹ 36 more variables: YardLine <dbl>, ...11 <lgl>, SeriesFirstDown <dbl>,
    ## #   ...13 <lgl>, NextScore <dbl>, Description <chr>, TeamWin <dbl>,
    ## #   ...17 <lgl>, ...18 <lgl>, SeasonYear <dbl>, Yards <dbl>, Formation <chr>,
    ## #   PlayType <fct>, IsRush <dbl>, IsPass <dbl>, IsIncomplete <dbl>,
    ## #   IsTouchdown <dbl>, PassType <fct>, IsSack <dbl>, IsChallenge <dbl>,
    ## #   IsChallengeReversed <dbl>, Challenger <lgl>, IsMeasurement <dbl>, …

``` r
pbp <- pbp %>%
  mutate(
    FieldGoalResult = case_when(
      # BLOCKED FG
      PlayType == "FIELD GOAL" &
        str_detect(tolower(Description), "blocked") ~ "BLOCKED",
      
      # GOOD FG (contains "good" but not "no good")
      PlayType == "FIELD GOAL" &
        str_detect(tolower(Description), "good") &
        !str_detect(tolower(Description), "no good") ~ "GOOD",
      
      # NO GOOD FG
      PlayType == "FIELD GOAL" &
        str_detect(tolower(Description), "no good") ~ "NO GOOD",
      
      # Everything else
      TRUE ~ "N/A"
    )
  )

#making FieldGoalResult a factor

pbp$FieldGoalResult <- factor(
  pbp$FieldGoalResult,
  levels = c("NO GOOD", "BLOCKED", "GOOD", "N/A")
)
pbp$FieldGoalResult
```

    ##     [1] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##    [10] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##    [19] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##    [28] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##    [37] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##    [46] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##    [55] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##    [64] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##    [73] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##    [82] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##    [91] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [100] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [109] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD
    ##   [118] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [127] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [136] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [145] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [154] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [163] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [172] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [181] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [190] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##   [199] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [208] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [217] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [226] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [235] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##   [244] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [253] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [262] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [271] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [280] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [289] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [298] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [307] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##   [316] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##   [325] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [334] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##   [343] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [352] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [361] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [370] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [379] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [388] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [397] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [406] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [415] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [424] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [433] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [442] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##   [451] N/A     N/A     BLOCKED N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [460] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [469] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [478] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [487] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [496] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [505] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [514] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [523] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##   [532] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [541] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##   [550] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [559] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [568] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [577] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ##   [586] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [595] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [604] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [613] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [622] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##   [631] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [640] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [649] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##   [658] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [667] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##   [676] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [685] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [694] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [703] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [712] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [721] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [730] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [739] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [748] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [757] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [766] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [775] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [784] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [793] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [802] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [811] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [820] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [829] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##   [838] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [847] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [856] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##   [865] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [874] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [883] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [892] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [901] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [910] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [919] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [928] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [937] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [946] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [955] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##   [964] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [973] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [982] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##   [991] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1000] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1009] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1018] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1027] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1036] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [1045] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1054] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1063] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1072] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1081] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1090] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1099] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [1108] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1117] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [1126] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1135] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1144] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1153] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1162] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1171] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1180] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1189] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1198] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1207] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1216] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [1225] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1234] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1243] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1252] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [1261] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1270] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1279] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1288] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1297] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [1306] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1315] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1324] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [1333] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [1342] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1351] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1360] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1369] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1378] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1387] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1396] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1405] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [1414] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1423] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [1432] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1441] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1450] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ##  [1459] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1468] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1477] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [1486] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1495] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1504] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1513] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1522] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1531] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1540] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1549] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1558] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [1567] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1576] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [1585] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1594] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1603] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [1612] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1621] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1630] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1639] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1648] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1657] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1666] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1675] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1684] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [1693] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [1702] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1711] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1720] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [1729] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1738] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1747] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1756] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1765] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [1774] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1783] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1792] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1801] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1810] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1819] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1828] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1837] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1846] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1855] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1864] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1873] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1882] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1891] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1900] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     GOOD    N/A    
    ##  [1909] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [1918] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1927] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1936] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [1945] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1954] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1963] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1972] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1981] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1990] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [1999] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2008] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2017] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2026] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2035] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2044] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2053] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2062] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [2071] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2080] N/A     GOOD    N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [2089] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2098] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2107] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2116] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2125] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2134] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2143] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2152] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2161] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2170] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2179] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2188] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2197] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2206] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2215] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2224] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2233] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2242] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2251] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2260] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2269] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2278] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2287] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [2296] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2305] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [2314] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2323] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2332] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2341] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2350] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2359] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ##  [2368] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2377] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [2386] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2395] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2404] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2413] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2422] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2431] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2440] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [2449] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2458] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     NO GOOD N/A    
    ##  [2467] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2476] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2485] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2494] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2503] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2512] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2521] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2530] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2539] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2548] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [2557] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2566] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2575] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [2584] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2593] N/A     NO GOOD N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ##  [2602] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ##  [2611] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2620] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2629] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2638] N/A     N/A     GOOD    N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [2647] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [2656] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2665] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2674] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2683] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2692] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2701] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2710] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2719] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2728] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2737] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2746] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2755] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2764] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [2773] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2782] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2791] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2800] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2809] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2818] GOOD    N/A     GOOD    N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [2827] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [2836] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2845] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [2854] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2863] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [2872] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2881] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [2890] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2899] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2908] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2917] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2926] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2935] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2944] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2953] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2962] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2971] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     BLOCKED
    ##  [2980] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2989] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [2998] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3007] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3016] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [3025] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [3034] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3043] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [3052] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3061] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3070] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [3079] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3088] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3097] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3106] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3115] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [3124] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [3133] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3142] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [3151] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3160] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3169] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3178] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [3187] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3196] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [3205] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3214] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3223] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [3232] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [3241] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3250] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3259] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3268] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3277] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3286] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3295] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3304] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3313] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3322] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [3331] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [3340] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3349] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3358] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3367] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3376] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3385] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3394] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3403] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [3412] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3421] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3430] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3439] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [3448] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3457] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3466] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3475] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [3484] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3493] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3502] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3511] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [3520] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3529] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3538] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3547] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3556] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3565] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3574] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3583] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3592] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3601] N/A     GOOD    N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [3610] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [3619] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3628] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3637] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3646] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3655] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3664] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3673] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [3682] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3691] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3700] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3709] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3718] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3727] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3736] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3745] N/A     N/A     N/A     BLOCKED N/A     N/A     GOOD    N/A     N/A    
    ##  [3754] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3763] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3772] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3781] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3790] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ##  [3799] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3808] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [3817] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3826] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [3835] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3844] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [3853] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3862] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3871] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3880] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3889] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ##  [3898] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3907] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3916] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3925] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD
    ##  [3934] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3943] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3952] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3961] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3970] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3979] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3988] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [3997] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4006] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [4015] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4024] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4033] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4042] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4051] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4060] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4069] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4078] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4087] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4096] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4105] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4114] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4123] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ##  [4132] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4141] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4150] N/A     GOOD    N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ##  [4159] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [4168] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4177] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4186] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4195] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4204] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4213] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [4222] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [4231] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4240] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4249] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4258] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4267] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [4276] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD
    ##  [4285] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4294] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4303] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4312] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4321] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [4330] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4339] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4348] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [4357] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4366] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4375] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4384] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [4393] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4402] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4411] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4420] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4429] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4438] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4447] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4456] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4465] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [4474] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4483] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4492] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [4501] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4510] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4519] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4528] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4537] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4546] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [4555] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4564] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [4573] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4582] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4591] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [4600] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4609] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4618] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4627] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4636] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [4645] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4654] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4663] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     GOOD   
    ##  [4672] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4681] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4690] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4699] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4708] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [4717] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4726] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4735] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [4744] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4753] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4762] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [4771] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4780] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4789] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4798] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4807] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4816] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4825] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4834] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [4843] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4852] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     GOOD   
    ##  [4861] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4870] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4879] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4888] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4897] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4906] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4915] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4924] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4933] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4942] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4951] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4960] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4969] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [4978] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [4987] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [4996] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5005] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5014] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5023] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5032] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5041] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5050] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5059] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5068] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5077] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5086] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5095] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5104] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5113] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5122] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5131] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5140] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5149] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5158] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [5167] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5176] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5185] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5194] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5203] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5212] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5221] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5230] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5239] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5248] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5257] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5266] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5275] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [5284] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5293] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5302] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [5311] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5320] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5329] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5338] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [5347] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5356] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5365] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5374] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5383] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5392] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5401] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5410] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5419] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [5428] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5437] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5446] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5455] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5464] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [5473] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5482] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5491] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5500] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5509] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5518] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5527] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5536] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [5545] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5554] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5563] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5572] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5581] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5590] N/A     N/A     N/A     BLOCKED N/A     N/A     N/A     N/A     N/A    
    ##  [5599] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [5608] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [5617] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5626] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5635] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5644] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [5653] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5662] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [5671] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [5680] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5689] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5698] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5707] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5716] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5725] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [5734] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5743] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5752] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [5761] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ##  [5770] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5779] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5788] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5797] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5806] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [5815] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5824] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5833] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5842] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5851] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5860] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5869] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5878] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     NO GOOD N/A    
    ##  [5887] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [5896] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5905] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [5914] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [5923] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5932] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [5941] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5950] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5959] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5968] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5977] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [5986] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [5995] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6004] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6013] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6022] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [6031] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6040] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ##  [6049] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6058] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6067] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [6076] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6085] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6094] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6103] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6112] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [6121] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ##  [6130] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6139] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6148] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6157] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     NO GOOD N/A    
    ##  [6166] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [6175] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [6184] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6193] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6202] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [6211] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6220] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6229] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6238] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6247] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6256] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6265] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6274] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6283] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ##  [6292] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6301] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6310] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6319] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6328] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6337] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6346] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6355] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [6364] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6373] GOOD    N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [6382] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6391] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6400] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6409] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [6418] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6427] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6436] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6445] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [6454] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6463] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6472] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6481] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6490] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6499] GOOD    N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [6508] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6517] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6526] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ##  [6535] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6544] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6553] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6562] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [6571] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6580] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6589] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6598] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6607] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6616] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [6625] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [6634] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6643] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6652] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6661] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ##  [6670] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6679] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6688] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6697] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [6706] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6715] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6724] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6733] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6742] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6751] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6760] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6769] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6778] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6787] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6796] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6805] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6814] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6823] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6832] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6841] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6850] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6859] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6868] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6877] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [6886] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [6895] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6904] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6913] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6922] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ##  [6931] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6940] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6949] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6958] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6967] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6976] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6985] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [6994] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7003] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7012] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD
    ##  [7021] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7030] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ##  [7039] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [7048] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [7057] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7066] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7075] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7084] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7093] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7102] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7111] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7120] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [7129] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7138] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7147] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7156] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7165] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7174] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7183] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7192] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7201] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7210] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [7219] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7228] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7237] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7246] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7255] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7264] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7273] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7282] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7291] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7300] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7309] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7318] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7327] GOOD    N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [7336] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7345] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7354] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7363] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7372] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7381] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7390] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7399] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7408] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7417] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7426] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7435] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7444] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7453] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [7462] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7471] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7480] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7489] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [7498] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [7507] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7516] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [7525] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7534] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7543] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7552] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ##  [7561] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [7570] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7579] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7588] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7597] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7606] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7615] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [7624] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7633] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ##  [7642] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7651] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7660] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7669] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7678] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7687] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7696] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7705] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [7714] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7723] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [7732] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7741] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [7750] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7759] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ##  [7768] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [7777] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7786] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7795] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7804] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7813] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7822] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7831] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7840] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7849] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7858] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7867] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7876] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7885] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7894] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7903] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7912] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7921] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7930] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7939] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7948] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7957] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7966] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [7975] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7984] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [7993] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8002] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8011] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8020] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8029] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8038] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [8047] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8056] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8065] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8074] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8083] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [8092] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8101] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8110] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8119] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8128] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8137] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [8146] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [8155] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8164] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8173] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8182] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [8191] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [8200] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8209] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8218] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8227] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8236] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8245] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8254] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8263] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8272] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8281] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8290] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [8299] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8308] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [8317] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8326] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [8335] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8344] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8353] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8362] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8371] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [8380] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8389] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8398] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8407] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8416] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8425] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8434] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8443] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8452] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8461] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8470] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8479] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8488] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8497] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8506] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8515] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8524] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8533] N/A     N/A     N/A     N/A     BLOCKED N/A     N/A     N/A     N/A    
    ##  [8542] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8551] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8560] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8569] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8578] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8587] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8596] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8605] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8614] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [8623] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8632] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [8641] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8650] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8659] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [8668] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8677] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [8686] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8695] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8704] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8713] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8722] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8731] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [8740] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8749] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ##  [8758] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8767] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [8776] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [8785] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8794] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8803] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8812] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8821] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8830] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8839] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8848] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8857] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8866] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8875] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ##  [8884] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8893] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8902] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [8911] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8920] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8929] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8938] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [8947] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8956] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8965] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8974] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [8983] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [8992] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9001] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9010] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9019] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9028] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9037] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [9046] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9055] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ##  [9064] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9073] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9082] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9091] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ##  [9100] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9109] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9118] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [9127] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9136] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9145] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9154] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9163] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9172] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9181] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9190] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9199] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ##  [9208] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9217] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [9226] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [9235] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9244] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9253] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9262] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9271] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9280] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9289] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [9298] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9307] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9316] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9325] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9334] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9343] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [9352] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [9361] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9370] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9379] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9388] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9397] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9406] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [9415] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9424] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9433] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ##  [9442] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9451] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9460] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [9469] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [9478] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9487] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9496] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9505] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [9514] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9523] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9532] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ##  [9541] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9550] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9559] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9568] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9577] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9586] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9595] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9604] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [9613] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9622] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9631] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9640] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [9649] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9658] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9667] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9676] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [9685] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ##  [9694] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9703] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [9712] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9721] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9730] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9739] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9748] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9757] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9766] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9775] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ##  [9784] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9793] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9802] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9811] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9820] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD
    ##  [9829] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9838] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9847] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9856] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9865] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9874] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9883] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9892] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9901] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9910] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ##  [9919] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9928] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9937] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9946] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9955] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9964] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9973] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9982] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ##  [9991] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10000] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10009] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10018] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [10027] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [10036] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [10045] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10054] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     GOOD    N/A    
    ## [10063] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [10072] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10081] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10090] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10099] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10108] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10117] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10126] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10135] GOOD    N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [10144] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10153] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [10162] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10171] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10180] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10189] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [10198] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10207] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10216] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10225] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10234] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [10243] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ## [10252] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10261] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10270] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10279] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10288] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10297] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10306] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10315] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     GOOD   
    ## [10324] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10333] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [10342] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10351] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10360] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10369] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10378] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10387] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10396] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10405] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10414] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10423] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10432] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10441] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10450] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10459] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10468] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10477] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10486] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [10495] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [10504] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10513] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [10522] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10531] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10540] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10549] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10558] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10567] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [10576] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [10585] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10594] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD   
    ## [10603] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [10612] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10621] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10630] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10639] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10648] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10657] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10666] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [10675] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10684] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [10693] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10702] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD
    ## [10711] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10720] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10729] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [10738] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10747] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10756] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10765] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10774] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10783] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10792] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10801] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10810] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10819] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [10828] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10837] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10846] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10855] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10864] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10873] N/A     GOOD    N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [10882] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [10891] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10900] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10909] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10918] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [10927] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10936] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10945] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [10954] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10963] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [10972] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10981] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [10990] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [10999] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11008] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11017] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11026] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11035] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11044] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11053] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11062] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11071] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11080] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [11089] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11098] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11107] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11116] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11125] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11134] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [11143] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11152] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11161] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [11170] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD
    ## [11179] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11188] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11197] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11206] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11215] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11224] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11233] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11242] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11251] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [11260] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11269] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11278] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11287] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11296] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11305] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11314] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11323] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [11332] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11341] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11350] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11359] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11368] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11377] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11386] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11395] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11404] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11413] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [11422] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11431] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11440] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11449] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11458] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     GOOD    N/A    
    ## [11467] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11476] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [11485] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11494] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11503] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11512] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [11521] N/A     GOOD    N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [11530] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11539] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11548] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11557] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11566] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11575] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [11584] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11593] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11602] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [11611] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11620] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [11629] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11638] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11647] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11656] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11665] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [11674] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [11683] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11692] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [11701] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11710] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11719] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11728] N/A     GOOD    N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [11737] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [11746] GOOD    N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [11755] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11764] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11773] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11782] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11791] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11800] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11809] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11818] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11827] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11836] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11845] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11854] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11863] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [11872] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [11881] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [11890] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [11899] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [11908] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11917] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11926] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11935] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11944] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11953] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11962] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     GOOD   
    ## [11971] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11980] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [11989] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [11998] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12007] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12016] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [12025] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12034] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [12043] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12052] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12061] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12070] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12079] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12088] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [12097] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12106] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12115] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12124] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12133] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12142] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12151] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12160] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12169] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12178] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12187] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12196] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [12205] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [12214] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12223] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12232] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12241] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12250] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [12259] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12268] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12277] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12286] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12295] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12304] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [12313] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12322] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [12331] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12340] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12349] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [12358] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12367] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12376] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     GOOD   
    ## [12385] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [12394] N/A     NO GOOD N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [12403] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12412] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12421] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12430] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12439] GOOD    N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [12448] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [12457] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12466] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12475] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12484] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     NO GOOD
    ## [12493] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12502] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12511] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12520] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12529] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12538] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12547] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12556] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12565] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12574] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12583] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12592] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12601] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [12610] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12619] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [12628] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12637] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12646] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12655] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12664] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12673] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [12682] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12691] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [12700] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12709] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12718] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12727] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [12736] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12745] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12754] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [12763] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12772] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12781] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12790] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12799] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [12808] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12817] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12826] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12835] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [12844] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12853] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [12862] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [12871] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12880] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12889] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12898] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12907] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12916] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12925] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12934] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12943] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [12952] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12961] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [12970] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12979] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12988] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [12997] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13006] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13015] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13024] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [13033] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13042] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [13051] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13060] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13069] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13078] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13087] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13096] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13105] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [13114] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13123] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13132] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13141] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13150] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13159] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13168] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13177] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13186] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13195] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13204] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13213] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13222] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13231] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13240] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13249] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13258] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [13267] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13276] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13285] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13294] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13303] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13312] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13321] N/A     GOOD    N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [13330] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13339] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13348] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13357] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [13366] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13375] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [13384] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13393] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13402] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13411] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [13420] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [13429] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13438] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13447] N/A     NO GOOD N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [13456] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13465] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [13474] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13483] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13492] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13501] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13510] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [13519] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [13528] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13537] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [13546] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13555] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13564] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13573] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13582] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13591] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13600] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13609] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13618] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     BLOCKED
    ## [13627] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13636] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [13645] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13654] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [13663] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13672] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13681] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13690] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [13699] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13708] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [13717] N/A     N/A     N/A     N/A     BLOCKED N/A     N/A     N/A     GOOD   
    ## [13726] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13735] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13744] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13753] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [13762] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13771] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13780] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13789] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13798] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     GOOD   
    ## [13807] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13816] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [13825] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13834] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [13843] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13852] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [13861] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13870] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13879] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [13888] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [13897] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13906] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [13915] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13924] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13933] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [13942] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13951] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13960] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13969] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13978] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13987] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [13996] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14005] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14014] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14023] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14032] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14041] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14050] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14059] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [14068] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [14077] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14086] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14095] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14104] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14113] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14122] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14131] GOOD    N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [14140] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14149] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14158] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [14167] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14176] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14185] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [14194] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14203] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14212] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14221] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14230] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14239] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14248] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [14257] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14266] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14275] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14284] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14293] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [14302] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14311] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14320] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14329] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14338] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14347] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14356] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14365] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [14374] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [14383] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14392] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14401] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [14410] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [14419] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [14428] N/A     NO GOOD N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [14437] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14446] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14455] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14464] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14473] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [14482] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14491] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14500] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14509] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [14518] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [14527] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14536] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [14545] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14554] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14563] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14572] NO GOOD N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [14581] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [14590] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [14599] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [14608] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14617] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14626] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14635] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14644] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [14653] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14662] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14671] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14680] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [14689] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [14698] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14707] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [14716] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14725] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14734] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [14743] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14752] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14761] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14770] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [14779] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14788] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14797] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [14806] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14815] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [14824] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14833] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [14842] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14851] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14860] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [14869] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [14878] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [14887] N/A     N/A     N/A     N/A     N/A     BLOCKED N/A     N/A     N/A    
    ## [14896] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14905] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14914] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14923] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14932] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14941] GOOD    N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [14950] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [14959] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [14968] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14977] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14986] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [14995] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [15004] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15013] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15022] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15031] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15040] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15049] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15058] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15067] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15076] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [15085] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15094] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15103] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15112] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15121] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15130] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [15139] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [15148] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15157] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15166] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15175] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15184] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15193] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15202] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15211] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15220] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15229] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15238] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15247] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15256] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [15265] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15274] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15283] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [15292] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15301] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15310] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [15319] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [15328] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15337] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15346] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15355] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15364] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15373] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15382] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15391] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15400] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [15409] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15418] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15427] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15436] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15445] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15454] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15463] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15472] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15481] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15490] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [15499] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15508] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15517] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15526] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15535] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15544] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [15553] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15562] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15571] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [15580] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15589] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15598] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15607] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15616] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15625] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15634] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [15643] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [15652] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15661] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [15670] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15679] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15688] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15697] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15706] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15715] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15724] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15733] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [15742] N/A     N/A     BLOCKED N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15751] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15760] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15769] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15778] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15787] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15796] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15805] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15814] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15823] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15832] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [15841] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15850] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15859] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15868] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15877] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [15886] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15895] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15904] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15913] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15922] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [15931] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [15940] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [15949] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [15958] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [15967] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15976] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [15985] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD   
    ## [15994] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16003] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16012] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16021] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [16030] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16039] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16048] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16057] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16066] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16075] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [16084] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [16093] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [16102] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16111] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16120] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16129] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16138] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16147] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16156] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16165] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [16174] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [16183] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16192] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16201] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [16210] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16219] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16228] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16237] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [16246] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [16255] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16264] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16273] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16282] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16291] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16300] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16309] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [16318] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16327] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16336] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16345] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [16354] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16363] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16372] N/A     GOOD    N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [16381] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16390] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16399] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16408] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16417] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [16426] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16435] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16444] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16453] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16462] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16471] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16480] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [16489] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16498] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [16507] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16516] N/A     N/A     N/A     BLOCKED N/A     N/A     N/A     N/A     N/A    
    ## [16525] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16534] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16543] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16552] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16561] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16570] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16579] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16588] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16597] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16606] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [16615] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16624] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16633] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16642] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [16651] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16660] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16669] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16678] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16687] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [16696] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16705] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16714] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16723] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [16732] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16741] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [16750] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16759] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16768] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16777] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16786] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16795] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16804] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [16813] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [16822] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ## [16831] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16840] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [16849] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16858] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16867] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16876] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16885] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16894] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16903] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16912] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16921] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16930] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16939] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16948] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16957] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [16966] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16975] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [16984] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [16993] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17002] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [17011] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17020] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17029] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17038] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17047] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17056] N/A     N/A     N/A     N/A     BLOCKED N/A     N/A     N/A     N/A    
    ## [17065] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17074] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17083] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [17092] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17101] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17110] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [17119] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17128] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17137] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17146] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17155] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17164] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [17173] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17182] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17191] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17200] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17209] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17218] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17227] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17236] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [17245] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [17254] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [17263] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17272] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17281] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [17290] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17299] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17308] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17317] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17326] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17335] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17344] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [17353] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17362] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [17371] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17380] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17389] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17398] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17407] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17416] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17425] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17434] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [17443] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [17452] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17461] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17470] N/A     N/A     N/A     GOOD    N/A     GOOD    N/A     N/A     N/A    
    ## [17479] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17488] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17497] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [17506] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17515] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17524] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD
    ## [17533] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17542] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17551] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [17560] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17569] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [17578] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [17587] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17596] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17605] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17614] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [17623] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17632] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17641] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17650] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17659] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17668] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17677] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17686] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17695] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [17704] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17713] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17722] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17731] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [17740] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17749] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17758] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17767] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17776] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17785] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17794] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17803] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17812] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17821] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17830] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17839] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17848] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17857] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17866] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17875] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17884] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD   
    ## [17893] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17902] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17911] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17920] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17929] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17938] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17947] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17956] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17965] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17974] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17983] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [17992] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18001] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18010] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18019] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18028] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18037] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18046] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18055] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18064] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18073] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18082] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18091] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18100] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18109] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18118] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [18127] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18136] N/A     N/A     N/A     N/A     N/A     BLOCKED N/A     N/A     N/A    
    ## [18145] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18154] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18163] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18172] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18181] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18190] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [18199] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18208] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18217] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18226] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18235] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [18244] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18253] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18262] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18271] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18280] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [18289] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18298] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18307] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18316] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [18325] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18334] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18343] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [18352] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18361] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18370] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18379] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18388] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18397] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18406] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18415] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18424] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18433] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18442] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18451] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18460] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18469] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18478] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18487] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18496] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [18505] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18514] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [18523] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [18532] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [18541] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18550] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18559] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [18568] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18577] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18586] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18595] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [18604] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18613] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18622] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [18631] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [18640] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18649] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18658] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18667] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18676] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18685] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18694] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18703] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18712] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18721] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18730] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18739] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18748] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18757] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18766] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18775] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18784] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18793] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18802] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18811] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18820] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18829] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18838] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18847] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18856] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18865] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18874] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18883] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [18892] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18901] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [18910] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18919] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [18928] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18937] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18946] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18955] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [18964] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18973] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [18982] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [18991] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19000] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19009] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [19018] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19027] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19036] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19045] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [19054] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [19063] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19072] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19081] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19090] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19099] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19108] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [19117] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19126] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19135] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19144] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [19153] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [19162] N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A    
    ## [19171] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19180] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19189] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [19198] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19207] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19216] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19225] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19234] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19243] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19252] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [19261] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19270] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [19279] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19288] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19297] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [19306] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19315] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19324] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [19333] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19342] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19351] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [19360] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [19369] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19378] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19387] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19396] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19405] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19414] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19423] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [19432] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19441] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19450] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19459] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19468] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19477] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19486] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19495] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [19504] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19513] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [19522] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19531] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19540] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19549] N/A     N/A     N/A     N/A     N/A     N/A     BLOCKED N/A     N/A    
    ## [19558] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19567] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [19576] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19585] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19594] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19603] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19612] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19621] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19630] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [19639] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19648] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19657] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [19666] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19675] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19684] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [19693] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19702] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19711] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19720] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19729] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [19738] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19747] N/A     NO GOOD N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [19756] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19765] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19774] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19783] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19792] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19801] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [19810] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [19819] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19828] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19837] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19846] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19855] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19864] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19873] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19882] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19891] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19900] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19909] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19918] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19927] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19936] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19945] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [19954] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [19963] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19972] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19981] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19990] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [19999] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20008] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20017] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20026] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20035] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20044] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20053] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20062] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [20071] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20080] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20089] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20098] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20107] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [20116] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20125] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20134] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20143] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20152] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [20161] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20170] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [20179] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20188] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20197] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20206] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [20215] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20224] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20233] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [20242] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20251] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20260] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20269] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [20278] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20287] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20296] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20305] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [20314] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20323] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20332] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20341] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20350] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20359] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20368] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20377] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20386] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [20395] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20404] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20413] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20422] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20431] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20440] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20449] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20458] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20467] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20476] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20485] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [20494] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20503] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20512] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20521] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20530] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20539] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20548] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20557] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20566] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20575] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [20584] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [20593] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20602] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20611] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20620] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20629] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20638] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20647] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20656] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [20665] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20674] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20683] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [20692] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [20701] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20710] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [20719] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20728] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [20737] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [20746] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20755] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20764] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20773] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [20782] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20791] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20800] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20809] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20818] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20827] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20836] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20845] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20854] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20863] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20872] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20881] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20890] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20899] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20908] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20917] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20926] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20935] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20944] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [20953] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20962] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20971] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20980] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [20989] GOOD    N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [20998] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21007] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21016] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21025] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21034] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21043] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21052] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21061] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21070] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21079] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21088] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21097] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21106] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21115] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [21124] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21133] N/A     GOOD    N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [21142] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21151] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21160] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21169] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21178] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21187] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21196] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21205] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21214] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21223] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21232] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21241] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21250] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21259] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [21268] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21277] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21286] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21295] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21304] N/A     N/A     GOOD    N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [21313] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21322] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21331] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21340] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21349] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [21358] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21367] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     BLOCKED N/A    
    ## [21376] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21385] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21394] N/A     N/A     N/A     N/A     BLOCKED N/A     N/A     N/A     N/A    
    ## [21403] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21412] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21421] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [21430] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [21439] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [21448] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21457] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [21466] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [21475] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     NO GOOD
    ## [21484] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21493] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21502] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21511] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [21520] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21529] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [21538] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21547] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21556] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21565] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21574] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21583] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21592] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21601] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [21610] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [21619] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [21628] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21637] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21646] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21655] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [21664] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [21673] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [21682] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [21691] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21700] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21709] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21718] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21727] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21736] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21745] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21754] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [21763] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21772] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21781] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21790] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     BLOCKED
    ## [21799] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     GOOD    N/A    
    ## [21808] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21817] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21826] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21835] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [21844] N/A     GOOD    N/A     N/A     GOOD    N/A     GOOD    N/A     N/A    
    ## [21853] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [21862] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21871] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21880] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21889] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21898] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21907] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21916] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21925] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [21934] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21943] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21952] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21961] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21970] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21979] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21988] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [21997] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22006] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22015] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22024] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22033] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22042] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22051] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22060] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22069] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22078] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [22087] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22096] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22105] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22114] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22123] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22132] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     GOOD    N/A    
    ## [22141] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22150] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [22159] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22168] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22177] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22186] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22195] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22204] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22213] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [22222] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22231] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [22240] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22249] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [22258] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22267] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22276] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22285] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22294] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [22303] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22312] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [22321] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22330] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22339] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22348] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22357] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22366] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [22375] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22384] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [22393] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22402] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22411] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22420] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22429] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     GOOD    N/A    
    ## [22438] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22447] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22456] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22465] N/A     N/A     N/A     N/A     N/A     GOOD    GOOD    N/A     N/A    
    ## [22474] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22483] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22492] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22501] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22510] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [22519] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [22528] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [22537] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22546] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22555] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22564] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [22573] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22582] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     NO GOOD N/A    
    ## [22591] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [22600] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22609] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22618] N/A     N/A     N/A     GOOD    GOOD    N/A     N/A     N/A     N/A    
    ## [22627] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [22636] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22645] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22654] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22663] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [22672] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22681] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22690] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22699] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22708] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [22717] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [22726] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [22735] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [22744] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22753] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [22762] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [22771] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22780] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22789] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22798] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22807] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22816] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22825] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [22834] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [22843] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22852] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22861] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22870] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22879] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22888] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22897] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22906] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22915] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22924] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22933] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [22942] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22951] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22960] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22969] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [22978] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [22987] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [22996] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23005] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23014] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23023] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23032] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23041] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23050] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23059] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23068] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23077] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23086] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23095] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [23104] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23113] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23122] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [23131] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [23140] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [23149] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [23158] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [23167] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23176] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23185] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23194] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23203] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23212] N/A     GOOD    N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [23221] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [23230] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23239] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23248] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23257] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [23266] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     NO GOOD N/A    
    ## [23275] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [23284] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23293] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23302] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [23311] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23320] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23329] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     BLOCKED
    ## [23338] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23347] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23356] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23365] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23374] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23383] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23392] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [23401] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23410] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23419] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23428] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23437] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23446] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23455] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23464] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [23473] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23482] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23491] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23500] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23509] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23518] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ## [23527] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23536] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23545] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23554] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [23563] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23572] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [23581] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23590] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23599] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23608] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23617] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23626] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [23635] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23644] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23653] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [23662] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23671] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23680] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23689] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23698] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23707] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [23716] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23725] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23734] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23743] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [23752] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23761] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23770] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [23779] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23788] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23797] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [23806] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23815] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23824] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23833] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [23842] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [23851] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23860] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23869] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23878] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23887] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23896] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23905] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23914] N/A     GOOD    N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [23923] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23932] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23941] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [23950] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23959] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23968] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [23977] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23986] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [23995] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24004] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [24013] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24022] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24031] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24040] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24049] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24058] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24067] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [24076] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [24085] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24094] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24103] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24112] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [24121] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24130] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24139] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24148] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24157] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24166] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24175] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24184] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24193] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [24202] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24211] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24220] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24229] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24238] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24247] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24256] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24265] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [24274] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24283] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [24292] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24301] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24310] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [24319] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24328] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24337] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24346] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24355] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24364] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24373] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [24382] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [24391] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24400] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [24409] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24418] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [24427] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24436] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [24445] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24454] N/A     N/A     N/A     N/A     N/A     GOOD    GOOD    N/A     N/A    
    ## [24463] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24472] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24481] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [24490] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24499] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24508] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [24517] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24526] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24535] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [24544] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24553] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24562] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24571] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24580] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24589] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24598] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [24607] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [24616] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24625] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24634] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24643] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24652] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24661] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [24670] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24679] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24688] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24697] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24706] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24715] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24724] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24733] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24742] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [24751] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24760] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [24769] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24778] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ## [24787] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24796] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24805] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24814] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24823] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [24832] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ## [24841] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [24850] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24859] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [24868] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24877] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [24886] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24895] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24904] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24913] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [24922] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24931] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24940] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24949] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24958] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [24967] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24976] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24985] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [24994] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [25003] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [25012] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25021] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25030] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25039] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25048] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25057] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25066] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25075] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     GOOD   
    ## [25084] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25093] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25102] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25111] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25120] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25129] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [25138] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25147] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25156] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [25165] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25174] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25183] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25192] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [25201] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25210] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25219] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [25228] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25237] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [25246] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25255] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [25264] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25273] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [25282] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [25291] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [25300] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25309] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [25318] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25327] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [25336] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25345] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25354] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25363] GOOD    N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [25372] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25381] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25390] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [25399] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25408] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25417] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25426] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25435] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [25444] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [25453] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25462] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [25471] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [25480] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [25489] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25498] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25507] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25516] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25525] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25534] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [25543] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25552] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [25561] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25570] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [25579] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25588] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ## [25597] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [25606] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [25615] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25624] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25633] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25642] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25651] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25660] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25669] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25678] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25687] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25696] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25705] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [25714] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25723] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25732] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25741] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25750] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25759] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25768] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25777] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25786] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25795] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [25804] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25813] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25822] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25831] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25840] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25849] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25858] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25867] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [25876] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [25885] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25894] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25903] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [25912] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25921] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25930] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25939] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25948] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25957] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25966] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25975] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25984] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [25993] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [26002] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [26011] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26020] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26029] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26038] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26047] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26056] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26065] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26074] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26083] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [26092] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26101] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26110] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26119] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26128] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26137] N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26146] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26155] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26164] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26173] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26182] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26191] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26200] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26209] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26218] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26227] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26236] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26245] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26254] N/A     N/A     N/A     N/A     BLOCKED N/A     N/A     N/A     N/A    
    ## [26263] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26272] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26281] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26290] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26299] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26308] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26317] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26326] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [26335] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26344] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [26353] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26362] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26371] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26380] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26389] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26398] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [26407] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26416] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26425] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26434] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26443] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26452] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [26461] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [26470] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26479] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26488] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26497] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26506] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [26515] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26524] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26533] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26542] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26551] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26560] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [26569] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26578] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26587] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26596] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26605] N/A     BLOCKED N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ## [26614] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26623] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [26632] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26641] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26650] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26659] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26668] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26677] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26686] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26695] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26704] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [26713] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26722] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26731] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26740] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26749] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26758] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26767] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26776] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26785] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26794] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26803] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [26812] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26821] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26830] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [26839] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26848] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26857] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26866] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26875] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26884] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26893] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [26902] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26911] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26920] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26929] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [26938] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26947] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26956] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26965] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26974] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26983] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [26992] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [27001] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27010] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27019] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27028] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27037] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27046] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [27055] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27064] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27073] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [27082] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27091] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27100] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27109] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27118] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27127] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27136] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27145] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27154] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27163] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27172] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27181] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27190] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27199] N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27208] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27217] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27226] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27235] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [27244] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27253] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27262] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27271] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27280] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27289] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27298] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27307] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27316] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27325] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27334] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27343] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27352] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27361] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27370] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27379] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27388] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27397] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27406] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27415] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27424] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27433] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [27442] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27451] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [27460] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27469] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27478] N/A     N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A    
    ## [27487] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27496] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27505] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27514] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [27523] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27532] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27541] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27550] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [27559] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27568] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27577] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27586] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27595] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27604] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [27613] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27622] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27631] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27640] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27649] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27658] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27667] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27676] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27685] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [27694] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27703] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27712] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27721] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27730] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27739] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27748] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27757] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27766] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27775] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27784] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27793] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27802] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27811] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27820] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27829] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27838] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27847] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27856] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [27865] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27874] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27883] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27892] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27901] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27910] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27919] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27928] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27937] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27946] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27955] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27964] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27973] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27982] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [27991] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28000] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28009] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28018] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28027] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28036] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28045] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28054] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28063] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28072] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28081] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28090] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28099] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28108] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28117] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28126] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28135] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28144] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28153] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28162] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28171] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28180] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28189] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28198] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28207] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28216] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28225] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28234] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28243] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28252] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28261] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28270] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28279] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28288] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28297] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28306] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28315] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28324] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28333] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28342] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28351] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28360] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28369] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28378] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28387] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28396] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28405] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28414] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28423] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28432] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28441] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28450] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28459] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28468] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28477] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28486] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28495] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28504] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28513] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28522] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28531] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28540] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28549] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28558] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28567] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28576] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28585] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28594] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28603] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28612] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28621] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28630] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28639] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28648] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28657] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28666] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28675] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28684] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28693] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28702] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28711] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28720] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28729] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28738] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28747] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28756] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28765] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28774] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28783] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28792] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28801] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [28810] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28819] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28828] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28837] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28846] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28855] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28864] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28873] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28882] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28891] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28900] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28909] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28918] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28927] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28936] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28945] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28954] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28963] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28972] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28981] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28990] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [28999] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29008] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29017] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29026] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29035] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29044] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29053] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29062] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29071] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29080] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29089] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29098] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29107] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29116] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29125] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29134] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29143] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29152] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29161] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29170] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29179] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29188] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29197] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29206] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29215] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29224] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29233] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29242] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29251] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29260] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29269] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29278] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29287] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29296] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29305] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29314] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29323] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29332] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29341] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29350] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29359] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29368] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29377] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29386] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29395] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29404] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29413] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29422] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29431] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29440] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29449] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29458] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29467] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29476] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29485] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29494] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29503] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29512] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29521] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29530] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29539] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29548] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29557] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29566] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29575] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29584] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29593] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29602] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29611] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29620] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29629] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29638] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29647] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29656] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29665] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29674] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29683] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29692] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29701] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29710] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29719] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29728] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29737] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29746] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29755] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29764] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29773] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29782] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29791] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29800] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29809] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29818] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29827] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29836] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29845] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29854] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29863] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29872] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29881] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29890] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29899] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29908] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29917] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29926] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29935] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29944] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29953] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29962] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29971] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29980] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29989] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [29998] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30007] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30016] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30025] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30034] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30043] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30052] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30061] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30070] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30079] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30088] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30097] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30106] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30115] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30124] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30133] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30142] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30151] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30160] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30169] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30178] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30187] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30196] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30205] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30214] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30223] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30232] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30241] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30250] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30259] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30268] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30277] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30286] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30295] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30304] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30313] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30322] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30331] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30340] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30349] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30358] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30367] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30376] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30385] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30394] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30403] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30412] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30421] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30430] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30439] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30448] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30457] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30466] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30475] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30484] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30493] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30502] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30511] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30520] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30529] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30538] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30547] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30556] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30565] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30574] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30583] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30592] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30601] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30610] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30619] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30628] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30637] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30646] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30655] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30664] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30673] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30682] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30691] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30700] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30709] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30718] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30727] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30736] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30745] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30754] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30763] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30772] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30781] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30790] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30799] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30808] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30817] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30826] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30835] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30844] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30853] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30862] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30871] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30880] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30889] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30898] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30907] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30916] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30925] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30934] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30943] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30952] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30961] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30970] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30979] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30988] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [30997] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31006] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31015] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31024] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31033] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31042] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31051] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31060] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31069] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31078] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31087] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31096] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31105] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31114] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31123] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31132] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31141] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31150] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31159] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31168] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31177] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31186] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31195] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31204] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31213] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31222] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31231] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31240] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31249] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31258] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31267] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31276] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31285] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31294] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31303] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31312] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31321] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31330] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31339] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31348] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31357] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31366] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31375] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31384] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31393] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31402] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31411] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31420] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31429] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31438] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31447] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31456] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31465] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31474] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31483] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31492] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31501] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31510] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31519] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31528] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31537] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31546] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31555] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31564] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31573] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31582] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31591] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31600] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31609] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31618] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31627] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31636] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31645] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31654] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31663] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31672] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31681] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31690] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31699] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31708] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31717] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31726] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31735] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31744] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31753] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31762] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31771] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31780] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31789] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31798] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31807] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31816] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31825] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31834] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31843] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31852] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31861] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31870] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31879] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31888] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31897] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31906] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31915] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31924] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31933] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31942] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31951] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31960] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31969] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31978] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31987] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [31996] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32005] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32014] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32023] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32032] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32041] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32050] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32059] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32068] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32077] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32086] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32095] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32104] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32113] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32122] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32131] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32140] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32149] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32158] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32167] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32176] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32185] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32194] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32203] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32212] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32221] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32230] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32239] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32248] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32257] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32266] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32275] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32284] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32293] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32302] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32311] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32320] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32329] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32338] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32347] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32356] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32365] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32374] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32383] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32392] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32401] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32410] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32419] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32428] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32437] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32446] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32455] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32464] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32473] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32482] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32491] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32500] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32509] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32518] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32527] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32536] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32545] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32554] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32563] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32572] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32581] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32590] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32599] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32608] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32617] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32626] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32635] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32644] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32653] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32662] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32671] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32680] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32689] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32698] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32707] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32716] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32725] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32734] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32743] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32752] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32761] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32770] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32779] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32788] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32797] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32806] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32815] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32824] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32833] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32842] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32851] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32860] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32869] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32878] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32887] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32896] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32905] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32914] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32923] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32932] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32941] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32950] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32959] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32968] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32977] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32986] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [32995] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33004] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33013] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33022] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33031] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33040] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33049] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33058] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33067] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33076] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33085] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33094] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33103] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33112] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33121] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33130] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33139] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33148] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33157] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33166] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33175] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33184] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33193] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33202] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33211] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33220] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33229] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33238] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33247] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33256] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33265] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33274] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33283] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33292] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33301] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33310] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33319] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33328] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33337] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33346] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33355] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33364] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33373] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33382] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33391] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33400] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33409] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33418] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33427] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33436] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33445] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33454] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33463] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33472] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33481] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33490] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33499] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33508] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33517] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33526] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33535] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33544] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33553] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33562] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33571] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33580] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33589] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33598] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33607] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33616] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33625] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33634] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33643] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33652] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33661] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33670] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33679] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33688] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33697] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33706] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33715] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33724] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33733] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33742] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33751] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33760] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33769] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33778] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33787] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33796] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33805] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33814] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33823] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33832] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33841] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33850] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33859] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33868] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33877] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33886] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33895] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33904] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33913] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33922] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33931] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33940] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33949] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33958] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33967] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33976] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33985] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [33994] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34003] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34012] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34021] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34030] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34039] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34048] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34057] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34066] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34075] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34084] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34093] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34102] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34111] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34120] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34129] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34138] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34147] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34156] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34165] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34174] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34183] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34192] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34201] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34210] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34219] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34228] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34237] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34246] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34255] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34264] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34273] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34282] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34291] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34300] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34309] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [34318] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34327] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34336] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34345] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34354] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34363] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34372] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [34381] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34390] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34399] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34408] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34417] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34426] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34435] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34444] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34453] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34462] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34471] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34480] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34489] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34498] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34507] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34516] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34525] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34534] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34543] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34552] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34561] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34570] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34579] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34588] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34597] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [34606] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34615] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34624] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34633] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34642] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34651] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34660] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34669] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34678] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34687] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34696] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34705] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34714] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34723] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34732] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34741] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34750] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34759] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34768] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34777] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34786] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34795] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34804] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34813] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34822] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34831] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34840] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34849] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34858] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34867] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34876] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34885] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34894] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34903] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34912] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34921] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34930] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34939] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34948] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34957] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34966] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34975] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34984] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [34993] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35002] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35011] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35020] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35029] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35038] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35047] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35056] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35065] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35074] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35083] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35092] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35101] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35110] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35119] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35128] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35137] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35146] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35155] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35164] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35173] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35182] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35191] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35200] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35209] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35218] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35227] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35236] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35245] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35254] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35263] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35272] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35281] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35290] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35299] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35308] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35317] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35326] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35335] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35344] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35353] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35362] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35371] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35380] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35389] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35398] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35407] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35416] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35425] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35434] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35443] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35452] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35461] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35470] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35479] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35488] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35497] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35506] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35515] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35524] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35533] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35542] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35551] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35560] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35569] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35578] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35587] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35596] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35605] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35614] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35623] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35632] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35641] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35650] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35659] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35668] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35677] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35686] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35695] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35704] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35713] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35722] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35731] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35740] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35749] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35758] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35767] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35776] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35785] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35794] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35803] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35812] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35821] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35830] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35839] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35848] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35857] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35866] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35875] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35884] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35893] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35902] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35911] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35920] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35929] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35938] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35947] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35956] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35965] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35974] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35983] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [35992] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36001] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36010] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36019] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36028] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36037] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36046] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36055] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36064] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36073] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36082] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36091] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36100] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36109] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36118] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36127] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36136] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36145] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36154] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36163] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36172] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36181] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36190] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36199] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36208] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36217] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36226] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36235] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36244] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36253] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36262] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36271] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36280] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36289] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36298] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36307] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36316] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36325] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36334] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36343] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36352] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36361] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36370] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36379] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36388] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36397] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36406] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36415] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36424] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36433] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36442] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36451] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36460] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36469] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36478] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36487] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36496] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36505] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36514] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36523] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36532] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36541] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36550] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36559] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36568] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36577] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36586] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36595] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36604] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36613] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36622] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36631] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36640] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36649] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36658] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36667] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36676] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36685] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36694] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36703] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36712] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36721] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36730] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36739] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36748] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36757] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36766] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36775] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36784] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36793] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36802] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36811] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36820] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36829] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36838] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36847] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36856] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36865] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36874] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36883] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36892] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36901] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36910] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36919] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36928] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36937] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36946] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36955] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36964] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36973] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36982] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [36991] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37000] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37009] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37018] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37027] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [37036] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37045] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37054] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37063] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37072] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37081] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37090] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37099] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37108] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37117] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37126] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37135] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37144] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37153] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37162] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37171] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37180] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37189] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37198] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37207] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37216] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37225] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37234] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37243] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37252] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37261] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37270] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37279] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37288] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37297] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37306] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37315] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37324] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37333] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37342] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37351] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37360] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37369] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37378] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37387] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37396] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37405] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37414] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37423] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37432] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37441] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37450] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37459] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37468] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37477] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [37486] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [37495] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37504] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37513] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37522] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37531] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37540] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37549] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37558] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37567] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37576] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37585] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37594] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37603] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37612] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37621] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37630] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37639] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37648] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [37657] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37666] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37675] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37684] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37693] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37702] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37711] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37720] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37729] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37738] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37747] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37756] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37765] N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A     N/A    
    ## [37774] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37783] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37792] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37801] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37810] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37819] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37828] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37837] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37846] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37855] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37864] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37873] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37882] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37891] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37900] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37909] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37918] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37927] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37936] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37945] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37954] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37963] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37972] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37981] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37990] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [37999] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38008] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38017] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38026] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38035] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38044] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38053] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [38062] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38071] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38080] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38089] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38098] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38107] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38116] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38125] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38134] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38143] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38152] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38161] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38170] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38179] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38188] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38197] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38206] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38215] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38224] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38233] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38242] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38251] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38260] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38269] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38278] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38287] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38296] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38305] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38314] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38323] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38332] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38341] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38350] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38359] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38368] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38377] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38386] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38395] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38404] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38413] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38422] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38431] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38440] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38449] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38458] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38467] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38476] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38485] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38494] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38503] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38512] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38521] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38530] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38539] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38548] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38557] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38566] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38575] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38584] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38593] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38602] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38611] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38620] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [38629] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38638] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38647] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38656] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38665] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38674] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38683] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38692] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38701] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38710] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38719] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38728] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38737] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38746] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38755] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38764] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38773] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38782] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38791] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38800] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38809] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38818] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38827] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38836] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38845] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38854] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38863] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38872] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38881] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38890] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38899] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38908] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38917] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38926] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38935] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38944] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38953] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38962] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [38971] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38980] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38989] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [38998] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39007] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39016] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39025] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39034] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39043] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39052] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39061] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39070] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39079] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39088] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39097] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39106] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39115] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39124] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39133] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39142] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39151] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39160] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39169] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39178] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39187] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39196] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39205] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39214] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39223] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39232] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39241] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39250] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39259] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39268] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39277] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39286] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39295] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39304] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [39313] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39322] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [39331] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39340] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39349] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39358] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39367] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39376] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39385] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39394] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39403] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39412] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39421] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39430] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39439] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39448] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39457] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39466] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39475] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39484] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39493] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39502] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [39511] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39520] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39529] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39538] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39547] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39556] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39565] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39574] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39583] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39592] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39601] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [39610] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39619] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39628] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39637] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39646] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [39655] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [39664] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39673] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39682] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39691] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39700] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39709] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [39718] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39727] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39736] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39745] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39754] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39763] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39772] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39781] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39790] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39799] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39808] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39817] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39826] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39835] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39844] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39853] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39862] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [39871] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39880] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39889] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39898] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39907] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39916] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39925] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39934] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39943] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39952] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39961] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39970] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39979] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39988] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [39997] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40006] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40015] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40024] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40033] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40042] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40051] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40060] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40069] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40078] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40087] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40096] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40105] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40114] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [40123] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40132] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40141] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40150] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [40159] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40168] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40177] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40186] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40195] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40204] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40213] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40222] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40231] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40240] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40249] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40258] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40267] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40276] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40285] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40294] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40303] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40312] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40321] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40330] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40339] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40348] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40357] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40366] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40375] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40384] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40393] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40402] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40411] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40420] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40429] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40438] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40447] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40456] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40465] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40474] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40483] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40492] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40501] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40510] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40519] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40528] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40537] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40546] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40555] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40564] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40573] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40582] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40591] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40600] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40609] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40618] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40627] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40636] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40645] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40654] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40663] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40672] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40681] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40690] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40699] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40708] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40717] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40726] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40735] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40744] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40753] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40762] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40771] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40780] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40789] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40798] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40807] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40816] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40825] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40834] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40843] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40852] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40861] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40870] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40879] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40888] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40897] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40906] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40915] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40924] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40933] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40942] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40951] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40960] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40969] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40978] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40987] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [40996] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41005] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41014] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41023] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41032] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41041] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41050] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [41059] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41068] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41077] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41086] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41095] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41104] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41113] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41122] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41131] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41140] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41149] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41158] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [41167] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41176] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [41185] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41194] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41203] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [41212] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41221] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41230] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41239] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41248] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41257] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41266] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41275] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41284] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41293] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [41302] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41311] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41320] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41329] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41338] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41347] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41356] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41365] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41374] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41383] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41392] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41401] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41410] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [41419] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41428] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41437] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41446] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41455] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41464] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41473] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41482] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41491] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41500] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41509] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41518] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41527] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41536] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41545] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41554] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41563] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41572] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41581] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [41590] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41599] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41608] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41617] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41626] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41635] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41644] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41653] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41662] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41671] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41680] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41689] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41698] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41707] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41716] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [41725] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41734] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41743] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41752] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41761] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41770] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41779] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41788] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41797] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41806] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [41815] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41824] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41833] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [41842] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41851] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41860] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41869] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41878] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41887] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41896] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41905] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41914] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41923] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41932] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41941] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41950] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41959] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41968] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41977] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41986] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [41995] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42004] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42013] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42022] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42031] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42040] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42049] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42058] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42067] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42076] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42085] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42094] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42103] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42112] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42121] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42130] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42139] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42148] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42157] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42166] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42175] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42184] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42193] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42202] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42211] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42220] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD
    ## [42229] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42238] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42247] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42256] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42265] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42274] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42283] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42292] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42301] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42310] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42319] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42328] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42337] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42346] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42355] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42364] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42373] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42382] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42391] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42400] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42409] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42418] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42427] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42436] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42445] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42454] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42463] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42472] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42481] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42490] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42499] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42508] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42517] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42526] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42535] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42544] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42553] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42562] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42571] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42580] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42589] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42598] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42607] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42616] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42625] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42634] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42643] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42652] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [42661] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42670] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42679] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42688] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42697] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42706] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42715] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42724] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42733] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42742] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42751] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42760] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42769] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [42778] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42787] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42796] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42805] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42814] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42823] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42832] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42841] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42850] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42859] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42868] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42877] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42886] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42895] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [42904] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42913] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42922] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42931] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42940] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [42949] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42958] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42967] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42976] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42985] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [42994] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43003] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43012] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43021] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43030] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43039] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43048] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43057] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43066] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43075] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43084] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43093] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43102] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43111] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43120] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43129] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43138] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43147] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43156] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43165] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43174] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43183] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43192] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43201] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43210] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43219] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43228] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43237] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43246] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43255] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43264] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43273] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43282] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43291] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43300] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43309] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43318] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43327] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43336] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43345] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43354] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43363] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43372] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43381] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43390] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43399] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43408] NO GOOD N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43417] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43426] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43435] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43444] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43453] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43462] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43471] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43480] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43489] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43498] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43507] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43516] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43525] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43534] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43543] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43552] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43561] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43570] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43579] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43588] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43597] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43606] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43615] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43624] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43633] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43642] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43651] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43660] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43669] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43678] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43687] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43696] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43705] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43714] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43723] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43732] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43741] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43750] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43759] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43768] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43777] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43786] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43795] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43804] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43813] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43822] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43831] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43840] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43849] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43858] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43867] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43876] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43885] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43894] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43903] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43912] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43921] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43930] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43939] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43948] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43957] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43966] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43975] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43984] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [43993] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44002] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44011] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44020] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44029] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44038] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44047] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44056] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44065] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44074] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44083] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44092] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44101] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44110] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44119] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44128] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44137] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44146] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44155] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44164] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44173] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44182] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44191] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44200] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44209] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44218] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44227] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44236] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44245] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44254] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44263] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44272] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44281] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44290] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44299] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44308] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44317] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44326] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44335] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44344] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44353] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44362] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44371] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44380] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44389] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44398] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44407] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44416] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44425] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44434] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44443] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44452] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44461] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44470] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44479] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44488] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44497] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44506] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44515] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44524] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44533] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44542] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44551] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44560] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44569] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44578] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44587] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44596] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44605] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44614] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44623] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44632] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44641] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44650] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44659] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44668] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44677] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44686] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44695] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44704] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44713] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44722] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44731] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44740] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44749] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44758] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44767] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44776] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44785] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44794] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44803] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44812] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44821] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44830] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44839] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44848] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44857] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44866] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44875] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44884] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44893] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44902] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44911] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44920] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44929] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44938] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44947] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44956] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44965] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44974] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44983] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [44992] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45001] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45010] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45019] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45028] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45037] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45046] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45055] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45064] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45073] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45082] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45091] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45100] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45109] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45118] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45127] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45136] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45145] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45154] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45163] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45172] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45181] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45190] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45199] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45208] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45217] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45226] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45235] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45244] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45253] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45262] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45271] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45280] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45289] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45298] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45307] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45316] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45325] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45334] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45343] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45352] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45361] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45370] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45379] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45388] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45397] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45406] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45415] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45424] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45433] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45442] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45451] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45460] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45469] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45478] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45487] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45496] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45505] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45514] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45523] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45532] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45541] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45550] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45559] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45568] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45577] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45586] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45595] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45604] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45613] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45622] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45631] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45640] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45649] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45658] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45667] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45676] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45685] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45694] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45703] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45712] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45721] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45730] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45739] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45748] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45757] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45766] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45775] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45784] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45793] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45802] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45811] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45820] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45829] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45838] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45847] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45856] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45865] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45874] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45883] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45892] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45901] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45910] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45919] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45928] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45937] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45946] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45955] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45964] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45973] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45982] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [45991] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46000] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46009] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46018] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46027] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46036] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46045] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46054] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46063] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46072] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46081] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46090] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46099] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46108] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46117] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46126] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46135] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46144] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46153] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46162] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46171] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46180] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46189] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46198] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46207] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46216] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46225] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46234] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46243] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46252] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46261] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46270] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46279] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46288] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46297] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46306] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46315] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46324] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46333] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46342] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46351] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46360] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46369] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46378] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46387] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46396] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46405] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [46414] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46423] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46432] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46441] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46450] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46459] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46468] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46477] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46486] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46495] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46504] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46513] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46522] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46531] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46540] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46549] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46558] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46567] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46576] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46585] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46594] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46603] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46612] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46621] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46630] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46639] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46648] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46657] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46666] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46675] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46684] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46693] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46702] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46711] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46720] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46729] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46738] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46747] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46756] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46765] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46774] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46783] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46792] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46801] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46810] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46819] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46828] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46837] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46846] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46855] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46864] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46873] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46882] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46891] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46900] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46909] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46918] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46927] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46936] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46945] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46954] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46963] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46972] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46981] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46990] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [46999] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47008] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47017] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47026] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47035] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47044] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47053] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47062] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47071] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47080] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47089] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47098] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47107] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47116] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47125] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47134] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47143] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47152] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47161] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47170] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47179] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47188] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47197] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47206] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47215] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47224] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47233] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47242] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47251] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47260] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47269] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47278] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47287] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47296] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47305] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47314] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47323] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47332] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47341] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47350] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47359] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47368] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47377] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47386] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47395] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47404] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47413] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47422] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47431] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47440] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47449] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47458] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47467] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47476] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47485] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47494] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47503] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47512] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47521] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47530] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47539] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47548] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47557] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47566] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47575] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47584] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47593] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47602] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47611] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47620] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47629] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47638] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47647] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [47656] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47665] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47674] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47683] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47692] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47701] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47710] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47719] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47728] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47737] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47746] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47755] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47764] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47773] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47782] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47791] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47800] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47809] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47818] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47827] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47836] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47845] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47854] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47863] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47872] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47881] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47890] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47899] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47908] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47917] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47926] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47935] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47944] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47953] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47962] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47971] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47980] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47989] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [47998] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48007] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48016] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48025] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48034] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48043] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48052] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48061] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48070] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48079] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48088] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48097] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48106] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48115] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48124] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48133] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48142] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48151] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48160] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48169] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48178] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48187] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48196] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [48205] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48214] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48223] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48232] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48241] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48250] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48259] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48268] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48277] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [48286] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48295] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48304] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48313] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48322] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48331] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48340] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48349] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48358] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48367] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48376] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48385] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48394] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48403] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48412] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48421] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48430] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48439] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48448] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48457] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48466] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48475] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48484] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48493] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48502] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48511] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48520] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48529] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48538] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [48547] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48556] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48565] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48574] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48583] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48592] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48601] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48610] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48619] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48628] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48637] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48646] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48655] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48664] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48673] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48682] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48691] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48700] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48709] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48718] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48727] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48736] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48745] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48754] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [48763] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48772] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48781] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48790] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48799] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48808] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48817] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48826] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48835] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48844] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48853] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48862] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48871] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48880] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48889] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48898] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48907] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48916] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48925] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48934] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48943] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48952] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48961] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48970] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48979] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48988] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [48997] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49006] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49015] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49024] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49033] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49042] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49051] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49060] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49069] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49078] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49087] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49096] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49105] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49114] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49123] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49132] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49141] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49150] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49159] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49168] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49177] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49186] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49195] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49204] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49213] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49222] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49231] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49240] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49249] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49258] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49267] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49276] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49285] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49294] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49303] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49312] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49321] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49330] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49339] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49348] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49357] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49366] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49375] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49384] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49393] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49402] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49411] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49420] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49429] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49438] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49447] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49456] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49465] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49474] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49483] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49492] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [49501] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49510] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49519] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49528] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49537] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49546] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49555] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49564] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49573] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49582] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49591] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49600] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49609] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD   
    ## [49618] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49627] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49636] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49645] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49654] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49663] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49672] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49681] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49690] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49699] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49708] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49717] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49726] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49735] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49744] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49753] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49762] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49771] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49780] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49789] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49798] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49807] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49816] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49825] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49834] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49843] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49852] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49861] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49870] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49879] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49888] N/A     N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A    
    ## [49897] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49906] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49915] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49924] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49933] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49942] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49951] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49960] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49969] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49978] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49987] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [49996] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50005] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50014] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50023] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50032] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50041] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50050] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50059] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50068] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50077] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50086] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50095] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50104] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50113] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50122] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50131] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50140] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50149] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50158] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50167] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50176] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50185] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50194] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50203] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50212] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50221] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50230] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50239] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50248] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50257] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50266] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50275] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50284] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50293] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50302] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50311] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50320] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50329] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50338] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50347] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50356] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50365] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50374] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50383] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50392] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50401] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50410] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50419] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50428] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50437] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50446] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50455] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50464] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50473] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50482] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50491] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50500] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50509] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50518] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50527] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50536] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50545] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50554] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50563] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50572] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50581] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50590] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50599] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50608] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50617] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50626] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50635] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50644] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50653] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50662] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50671] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50680] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50689] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50698] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50707] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50716] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50725] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50734] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50743] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50752] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50761] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50770] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50779] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50788] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50797] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50806] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50815] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50824] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50833] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50842] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50851] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50860] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50869] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50878] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50887] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50896] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50905] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50914] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50923] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50932] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50941] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50950] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50959] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50968] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50977] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50986] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [50995] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51004] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51013] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51022] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51031] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51040] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51049] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51058] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51067] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51076] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51085] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51094] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51103] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51112] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51121] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51130] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51139] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51148] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51157] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51166] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51175] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51184] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51193] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51202] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51211] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51220] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51229] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51238] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51247] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51256] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51265] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51274] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51283] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51292] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51301] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51310] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51319] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51328] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51337] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51346] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51355] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     GOOD    N/A    
    ## [51364] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51373] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51382] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51391] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51400] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51409] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51418] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51427] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51436] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51445] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51454] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51463] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51472] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51481] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [51490] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51499] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51508] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51517] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51526] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51535] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51544] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51553] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51562] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51571] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51580] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51589] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51598] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51607] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51616] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51625] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51634] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51643] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51652] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51661] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51670] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51679] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51688] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51697] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51706] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51715] N/A     NO GOOD N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [51724] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51733] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51742] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51751] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51760] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51769] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51778] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51787] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51796] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51805] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51814] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51823] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51832] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51841] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51850] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51859] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51868] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51877] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [51886] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51895] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51904] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51913] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51922] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51931] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51940] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51949] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51958] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51967] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51976] N/A     N/A     N/A     N/A     N/A     N/A     N/A     NO GOOD N/A    
    ## [51985] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [51994] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52003] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52012] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52021] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52030] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52039] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52048] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52057] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52066] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52075] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52084] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52093] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52102] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52111] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52120] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52129] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52138] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52147] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [52156] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52165] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52174] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52183] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52192] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52201] N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A    
    ## [52210] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52219] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52228] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52237] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52246] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52255] N/A     N/A     N/A     N/A     NO GOOD N/A     N/A     N/A     N/A    
    ## [52264] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52273] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52282] N/A     N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A    
    ## [52291] GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52300] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52309] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52318] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52327] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52336] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52345] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52354] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52363] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52372] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52381] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52390] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52399] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52408] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52417] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52426] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52435] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52444] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52453] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52462] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52471] N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A     N/A    
    ## [52480] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52489] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52498] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52507] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52516] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52525] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52534] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52543] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52552] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52561] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52570] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52579] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52588] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52597] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52606] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52615] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52624] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52633] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52642] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52651] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52660] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52669] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52678] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52687] N/A     N/A     N/A     N/A     N/A     N/A     GOOD    N/A     N/A    
    ## [52696] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52705] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52714] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52723] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52732] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52741] N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52750] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52759] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52768] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52777] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52786] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52795] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52804] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52813] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52822] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52831] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52840] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52849] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52858] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52867] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52876] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52885] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52894] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52903] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52912] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52921] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52930] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52939] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52948] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52957] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52966] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52975] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52984] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [52993] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53002] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53011] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53020] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53029] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53038] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53047] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53056] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53065] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53074] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53083] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53092] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53101] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53110] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53119] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53128] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53137] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53146] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53155] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53164] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53173] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53182] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53191] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53200] N/A     N/A     GOOD    N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53209] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53218] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53227] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53236] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53245] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53254] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53263] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53272] N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A     N/A    
    ## [53281] N/A     N/A     N/A    
    ## Levels: NO GOOD BLOCKED GOOD N/A

``` r
#inspecting safetys
pbp %>%
  filter(str_detect(tolower(Description), "safety")) %>%
  select(GameId, Quarter, Minute, Second, OffenseTeam, DefenseTeam, Description)
```

    ## # A tibble: 15 × 7
    ##        GameId Quarter Minute Second OffenseTeam DefenseTeam Description         
    ##         <dbl>   <dbl>  <dbl>  <dbl> <chr>       <chr>       <chr>               
    ##  1 2024122905       2      2     34 NYJ         BUF         (2:34) (SHOTGUN) 8-…
    ##  2 2024090809       2      4     36 SEA         DEN         (4:36) (SHOTGUN) 26…
    ##  3 2025011900       3      0     32 PHI         LA          (:32) (SHOTGUN) 1-J…
    ##  4 2024090809       2     11     28 SEA         DEN         (11:28) (SHOTGUN) 7…
    ##  5 2024091505       4      1     48 JAX         CLE         (1:48) (SHOTGUN) 16…
    ##  6 2024093000       4      2     33 MIA         TEN         (2:33) (SHOTGUN) 18…
    ##  7 2024093001       4      2      5 DET         SEA         (2:05) 16-J.GOFF SA…
    ##  8 2024100602       2      5     52 BAL         CIN         (5:52) 22-D.HENRY R…
    ##  9 2024102400       4      1     42 MIN         LA          (1:42) (SHOTGUN) 14…
    ## 10 2024102704       3      6     22 MIA         ARI         (6:22) (SHOTGUN) 1-…
    ## 11 2024102706       4     12     11 ATL         TB          (12:11) 18-K.COUSIN…
    ## 12 2024110309       4      7     58 CHI         ARI         (7:58) (NO HUDDLE, …
    ## 13 2024112402       4      1     17 HOU         TEN         (1:17) (SHOTGUN) 7-…
    ## 14 2024121601       2      5     39 LV          ATL         (5:39) (SHOTGUN) 22…
    ## 15 2024122501       2     10     15 BAL         HOU         (10:15) (SHOTGUN) 2…

``` r
pbp <- pbp %>%
  mutate(
    IsSafety = if_else(
      str_detect(tolower(Description), "safety"),
      1L,
      0L
    )
  )

#inspecting extra point
pbp %>%
  filter(PlayType == "EXTRA POINT") %>%
  select(GameId, Quarter, Minute, Second, OffenseTeam, DefenseTeam, Description)
```

    ## # A tibble: 1,304 × 7
    ##        GameId Quarter Minute Second OffenseTeam DefenseTeam Description         
    ##         <dbl>   <dbl>  <dbl>  <dbl> <chr>       <chr>       <chr>               
    ##  1 2024122907       3      9     44 MIN         GB          16-W.REICHARD EXTRA…
    ##  2 2024122909       4     13     30 TB          CAR         4-C.MCLAUGHLIN EXTR…
    ##  3 2024122909       3      5     48 TB          CAR         4-C.MCLAUGHLIN EXTR…
    ##  4 2024122907       2     11     52 MIN         GB          16-W.REICHARD EXTRA…
    ##  5 2024122906       2      8     59 JAX         TEN         39-C.LITTLE EXTRA P…
    ##  6 2024122903       4      2     57 NYG         IND         9-G.GANO EXTRA POIN…
    ##  7 2024122903       4      6     38 IND         NYG         7-M.GAY EXTRA POINT…
    ##  8 2024122903       4      8     39 NYG         IND         9-G.GANO EXTRA POIN…
    ##  9 2024122905       3      0     12 BUF         NYJ         2-T.BASS EXTRA POIN…
    ## 10 2024122905       3      1     15 BUF         NYJ         2-T.BASS EXTRA POIN…
    ## # ℹ 1,294 more rows

``` r
pbp <- pbp %>%
  mutate(
    ExtraPointResult = case_when(
      # BLOCKED XP
      PlayType == "EXTRA POINT" &
        str_detect(tolower(Description), "blocked") ~ "BLOCKED",
      
      # GOOD XP
      PlayType == "EXTRA POINT" &
        str_detect(tolower(Description), "good") &
        !str_detect(tolower(Description), "no good") ~ "GOOD",
      
      # NO GOOD XP
      PlayType == "EXTRA POINT" &
        str_detect(tolower(Description), "no good") ~ "NO GOOD",
      
      # Everything else (not an XP)
      TRUE ~ "N/A"
    )
  )

#investigating blocked XP or returned 2 point conversions, there was one for PHILI Eagles vs Buccs
pbp %>%
  filter(
    str_detect(tolower(Description), "defensive two-point")
  )
```

    ## # A tibble: 8 × 48
    ##      GameId GameDate   Quarter Minute Second OffenseTeam DefenseTeam  Down  ToGo
    ##       <dbl> <date>       <dbl>  <dbl>  <dbl> <chr>       <chr>       <dbl> <dbl>
    ## 1    2.02e9 2024-10-06       2      0      9 CIN         BAL             0     0
    ## 2    2.02e9 2024-09-29       3      6     49 TB          PHI             0     0
    ## 3    2.03e9 2025-01-11       4     10     38 LAC         HOU             0     0
    ## 4    2.02e9 2024-12-30       1      3     42 DET         SF              0     0
    ## 5    2.02e9 2024-11-17       4     12     45 NE          LA              0     0
    ## 6    2.02e9 2024-12-01       2      7     39 NYJ         SEA             0     0
    ## 7    2.02e9 2024-12-01       1      3     25 NYJ         SEA             0     0
    ## 8    2.02e9 2024-12-28       3      8     31 ARI         LA              0     0
    ## # ℹ 39 more variables: YardLine <dbl>, ...11 <lgl>, SeriesFirstDown <dbl>,
    ## #   ...13 <lgl>, NextScore <dbl>, Description <chr>, TeamWin <dbl>,
    ## #   ...17 <lgl>, ...18 <lgl>, SeasonYear <dbl>, Yards <dbl>, Formation <chr>,
    ## #   PlayType <fct>, IsRush <dbl>, IsPass <dbl>, IsIncomplete <dbl>,
    ## #   IsTouchdown <dbl>, PassType <fct>, IsSack <dbl>, IsChallenge <dbl>,
    ## #   IsChallengeReversed <dbl>, Challenger <lgl>, IsMeasurement <dbl>,
    ## #   IsInterception <dbl>, IsFumble <dbl>, IsPenalty <dbl>, …

``` r
#adding defensive two-points
pbp <- pbp %>%
  mutate(
    DefensiveTwoPoint = if_else(
      PlayType %in% c("EXTRA POINT", "TWO-POINT CONVERSION") &
        str_detect(tolower(Description), "two-point attempt") &
        str_detect(tolower(Description), "attempt succeeds"),
      1L,
      0L
    )
  )

#Okay now we should have data on all scoring plays
```

``` r
#now that we have gathered all data on scoring we can try to impliment some sort of scoring function.
pbp <- pbp %>%
  arrange(GameId, Quarter, desc(Minute), desc(Second)) %>%
  group_by(GameId) %>%
  mutate(
    PlayIndex = row_number(),

    # Offensive points on this play
    OffPointsPlay =
      if_else(IsTouchdown == 1, 6L, 0L) +
      if_else(FieldGoalResult == "GOOD", 3L, 0L) +
      if_else(ExtraPointResult == "GOOD", 1L, 0L) +
      if_else(IsTwoPointConversionSuccessful == 1 & DefensiveTwoPoint == 0, 2L, 0L),

    # Defensive points on this play
    DefPointsPlay =
      if_else(IsSafety == 1, 2L, 0L) +
      if_else(DefensiveTwoPoint == 1, 2L, 0L)
  ) %>%
  ungroup()



#Build long-format team scoring table

team_scores_long <- pbp %>%
  select(GameId, PlayIndex, OffenseTeam, DefenseTeam, OffPointsPlay, DefPointsPlay) %>%
  
  # Offensive scoring rows
  transmute(
    GameId,
    PlayIndex,
    Team   = OffenseTeam,
    Points = OffPointsPlay
  ) %>%
  
  # Bind defensive scoring rows
  bind_rows(
    pbp %>%
      select(GameId, PlayIndex, OffenseTeam, DefenseTeam, OffPointsPlay, DefPointsPlay) %>%
      transmute(
        GameId,
        PlayIndex,
        Team   = DefenseTeam,
        Points = DefPointsPlay
      )
  ) %>%
  
  arrange(GameId, PlayIndex) %>%
  group_by(GameId, Team) %>%
  mutate(TeamScore = cumsum(Points)) %>%
  ungroup()


# Join back to original PBP 

pbp <- pbp %>%
  left_join(
    team_scores_long %>% rename(OffenseScore = TeamScore),
    by = c("GameId", "PlayIndex", "OffenseTeam" = "Team")
  ) %>%
  left_join(
    team_scores_long %>% rename(DefenseScore = TeamScore),
    by = c("GameId", "PlayIndex", "DefenseTeam" = "Team")
  ) %>%
  
  mutate(
    ScoreDiff = OffenseScore - DefenseScore
  )
```

``` r
#investigating to make sure that was right
#Example, Kansas City vs Ravens:
#KC (1), BAL (2)
#0 - 7, 7-7, 10-7, 13-7, 13-10, 20- 10, 20 - 17, 27 -17, 27- 20
pbp %>%
  filter(GameId == 2024090500) 
```

    ## # A tibble: 193 × 57
    ##      GameId GameDate   Quarter Minute Second OffenseTeam DefenseTeam  Down  ToGo
    ##       <dbl> <date>       <dbl>  <dbl>  <dbl> <chr>       <chr>       <dbl> <dbl>
    ##  1   2.02e9 2024-09-05       1     15      0 BAL         KC              0     0
    ##  2   2.02e9 2024-09-05       1     15      0 BAL         KC              1    10
    ##  3   2.02e9 2024-09-05       1     14     19 BAL         KC              2     8
    ##  4   2.02e9 2024-09-05       1     13     55 BAL         KC              2    13
    ##  5   2.02e9 2024-09-05       1     13     20 BAL         KC              3    11
    ##  6   2.02e9 2024-09-05       1     12     43 BAL         KC              1    10
    ##  7   2.02e9 2024-09-05       1     12      0 BAL         KC              2     9
    ##  8   2.02e9 2024-09-05       1     11     55 BAL         KC              3     9
    ##  9   2.02e9 2024-09-05       1     11     49 BAL         KC              3     9
    ## 10   2.02e9 2024-09-05       1     11      5 BAL         KC              1    10
    ## # ℹ 183 more rows
    ## # ℹ 48 more variables: YardLine <dbl>, ...11 <lgl>, SeriesFirstDown <dbl>,
    ## #   ...13 <lgl>, NextScore <dbl>, Description <chr>, TeamWin <dbl>,
    ## #   ...17 <lgl>, ...18 <lgl>, SeasonYear <dbl>, Yards <dbl>, Formation <chr>,
    ## #   PlayType <fct>, IsRush <dbl>, IsPass <dbl>, IsIncomplete <dbl>,
    ## #   IsTouchdown <dbl>, PassType <fct>, IsSack <dbl>, IsChallenge <dbl>,
    ## #   IsChallengeReversed <dbl>, Challenger <lgl>, IsMeasurement <dbl>, …

``` r
#Just discovered something horrible about our data... if a play is a touchdown which is later reversed due to ref decision, then the isTouchdown variable is still marked as a 1, not a 0. This is detrimental to our scoring system, I will have to try to find if there is some consistent way to get the scores corrected or if we are just doomed forever.

#looking for isTouchdown and description contains reversed
pbp %>%
  filter(
    IsTouchdown == 1,
    str_detect(tolower(Description), "reversed")
  )
```

    ## # A tibble: 71 × 57
    ##      GameId GameDate   Quarter Minute Second OffenseTeam DefenseTeam  Down  ToGo
    ##       <dbl> <date>       <dbl>  <dbl>  <dbl> <chr>       <chr>       <dbl> <dbl>
    ##  1   2.02e9 2024-09-05       4      0      5 BAL         KC              3    10
    ##  2   2.02e9 2024-09-08       2      5     24 CIN         NE              2    11
    ##  3   2.02e9 2024-09-08       3      5      0 CAR         NO              4     3
    ##  4   2.02e9 2024-09-08       4      1     38 WAS         TB              1     1
    ##  5   2.02e9 2024-09-08       2      2      0 DET         LA              2     1
    ##  6   2.02e9 2024-09-16       4      7      9 PHI         ATL             3     3
    ##  7   2.02e9 2024-09-19       2      8     33 NYJ         NE              3     1
    ##  8   2.02e9 2024-09-29       3      7      6 TB          PHI             2     2
    ##  9   2.02e9 2024-09-29       4      5      2 PHI         TB              2    15
    ## 10   2.02e9 2024-09-30       2     10     51 SEA         DET             2     8
    ## # ℹ 61 more rows
    ## # ℹ 48 more variables: YardLine <dbl>, ...11 <lgl>, SeriesFirstDown <dbl>,
    ## #   ...13 <lgl>, NextScore <dbl>, Description <chr>, TeamWin <dbl>,
    ## #   ...17 <lgl>, ...18 <lgl>, SeasonYear <dbl>, Yards <dbl>, Formation <chr>,
    ## #   PlayType <fct>, IsRush <dbl>, IsPass <dbl>, IsIncomplete <dbl>,
    ## #   IsTouchdown <dbl>, PassType <fct>, IsSack <dbl>, IsChallenge <dbl>,
    ## #   IsChallengeReversed <dbl>, Challenger <lgl>, IsMeasurement <dbl>, …

``` r
#we might be ****ed: (5:00) (SHOTGUN) 9-B.YOUNG SCRAMBLES UP THE MIDDLE TO NO 1 FOR 2 YARDS (29-P.ADEBO). FUMBLES (29-P.ADEBO), RECOVERED BY CAR-15-J.MINGO AT NO 1. THE REPLAY OFFICIAL REVIEWED THE SHORT OF THE GOAL LINE RULING, AND THE PLAY WAS REVERSED. (SHOTGUN) 9-B.YOUNG SCRAMBLES UP THE MIDDLE FOR 3 YARDS, TOUCHDOWN. 

#that text displays how even when "reversed" occurs, it doesn't necessarily mean the touchdown was reversed... I have no clue what to do.

#it looks like if "TOUCHDOWN" occurs after "reversed" in the description then it is a touchdown, otherwise it is not a touchdown.
```

``` r
# Pass Rate By Down

pbp %>%
  filter(Down %in% 1:4) %>%
  group_by(Down) %>%
  summarize(pass_rate = mean(IsPass == 1, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Down), y = pass_rate)) +
  geom_col() +
  labs(
    title = "Pass Rate by Down",
    x = "Down",
    y = "Pass Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Pass vs Run by Field Position

pbp %>%
  filter(YardLine >= 1, YardLine <= 99) %>%
  group_by(YardLine) %>%
  summarize(pass_rate = mean(IsPass == 1, na.rm = TRUE)) %>%
  ggplot(aes(YardLine, pass_rate)) +
  geom_line(size = 1.1) +
  labs(
    title = "Pass Rate by Yard Line",
    x = "Field Position (1 = Own Endzone, 99 = Opponent Goal)",
    y = "Pass Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Yards Gained by Down

pbp %>%
  filter(Down %in% 1:4, Yards > -20 & Yards < 100) %>%  # removes oddities
  group_by(Down) %>%
  summarize(avg_yards = mean(Yards, na.rm = TRUE)) %>%
  ggplot(aes(factor(Down), avg_yards)) +
  geom_col() +
  labs(
    title = "Average Yards Gained by Down",
    x = "Down",
    y = "Average Yards Gained"
  ) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Play Type Frequency (Run, Pass, Sack)

pbp %>%
  mutate(
    play_group = case_when(
      IsPass == 1 ~ "Pass",
      IsRush == 1 ~ "Rush",
      IsSack == 1 ~ "Sack",
      TRUE ~ "Other"
    )
  ) %>%
  count(play_group) %>%
  ggplot(aes(x = reorder(play_group, n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Distribution of Play Types",
      x = "Play Type",
      y = "Count"
    ) +
    theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#Rate of big plays (20 yards or more) by down
pbp %>%
  filter(Down %in% 1:4, !is.na(Yards)) %>%
  mutate(big_play = Yards >= 20) %>%
  group_by(Down) %>%
  summarize(
    big_play_count = sum(big_play),
    total_plays = n(),
    big_play_rate = big_play_count / total_plays
  ) %>%
  ggplot(aes(x = factor(Down), y = big_play_rate)) +
  geom_col() +
  labs(
    title = "Number of Big Plays (20+ Yards) by Down",
    x = "Down",
    y = "Rate of Big Plays"
  ) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# Yards to go on third vs likelyhood of run or pass
pbp %>%
  filter(Down == 3, !is.na(ToGo), ToGo > 0, ToGo <= 20) %>%
  mutate(
    togo_bin = cut(
      ToGo,
      breaks = c(0, 2, 4, 6, 8, 10, 15, 20),
      include.lowest = TRUE,
      right = TRUE,
      labels = c("1–2", "3–4", "5–6", "7–8", "9–10", "11–15", "16–20")
    )
  ) %>%
  group_by(togo_bin) %>%
  summarize(
    pass_rate = mean(IsPass == 1, na.rm = TRUE),
    run_rate = mean(IsRush == 1, na.rm = TRUE),
    n_plays = n()
  ) %>%
  pivot_longer(
    cols = c(pass_rate, run_rate),
    names_to = "play_type",
    values_to = "rate"
  ) %>%
  mutate(
    play_type = recode(play_type,
      "pass_rate" = "Pass",
      "run_rate" = "Run")
  ) %>%
  ggplot(aes(x = togo_bin, y = rate, fill = play_type)) +
  geom_col(position = "dodge") +
  labs(
    title = "3rd Down Play Call by Yards to Go",
    x = "Yards to Go (Binned)",
    y = "Rate",
    fill = "Play Type"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Based on field position, how likely to go for it on 4th
pbp %>%
  filter(Down == 4,
         YardLine >= 1, YardLine <= 99) %>%
  mutate(
    go_for_it = if_else(IsPass == 1 | IsRush == 1, 1L, 0L),
    yard_bin = cut(
      YardLine,
      breaks = seq(0, 100, by = 10),
      include.lowest = TRUE,
      right = TRUE,
      labels = c("1–10", "11–20", "21–30", "31–40", "41–50",
                 "51–60", "61–70", "71–80", "81–90", "91–99")
    )
  ) %>%
  group_by(yard_bin) %>%
  summarize(
    go_rate = mean(go_for_it, na.rm = TRUE),
    n_plays = n()
  ) %>%
  ggplot(aes(x = yard_bin, y = go_rate, group = 1)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Go-For-It Rate on 4th Down by Field Position (Binned)",
    x = "Field Position Bin",
    y = "Go-For-It Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#Field goal count by quarter
pbp %>%
  filter(PlayType == "FIELD GOAL") %>%
  count(Quarter) %>%
  ggplot(aes(x = factor(Quarter), y = n)) +
  geom_col() +
  labs(
    title = "Field Goals Attempted by Quarter",
    x = "Quarter",
    y = "Field Goal Attempts"
  ) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#likelyhood of interceptions based on down

pbp %>%
  filter(IsPass == 1, Down %in% 1:4) %>%
  group_by(Down) %>%
  summarize(
    int_rate = mean(IsInterception == 1, na.rm = TRUE),
    n_passes = n()
  ) %>%
  ggplot(aes(x = factor(Down), y = int_rate)) +
  geom_col() +
  labs(
    title = "Interception Rate by Down (Pass Plays Only)",
    x = "Down",
    y = "Interception Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# Likelihood of interceptions based on field position 
#Some bins only have a couple interceptions at most so it might not necessarily be the most reliable data
pbp %>%
  filter(IsPass == 1,
         YardLine >= 1, YardLine <= 99) %>%
  mutate(
    yard_bin = cut(
      YardLine,
      breaks = seq(0, 100, by = 10),
      include.lowest = TRUE,
      right = TRUE,
      labels = c("1–10", "11–20", "21–30", "31–40", "41–50",
                 "51–60", "61–70", "71–80", "81–90", "91–99")
    )
  ) %>%
  group_by(yard_bin) %>%
  summarize(
    int_rate = mean(IsInterception == 1, na.rm = TRUE),
    n_passes = n()
  ) %>%
  ggplot(aes(x = yard_bin, y = int_rate, group = 1)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Interception Rate by Field Position (Pass Plays Only, Binned)",
    x = "Field Position Bin",
    y = "Interception Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#Tracking "success" by using the 40%,60%,100% rule

pbp %>%
  filter(Down %in% 1:4) %>%
  mutate(
    success = case_when(
      Down == 1 ~ Yards >= 0.4 * ToGo,
      Down == 2 ~ Yards >= 0.6 * ToGo,
      Down %in% 3:4 ~ Yards >= ToGo
    )
  ) %>%
  group_by(Down) %>%
  summarize(success_rate = mean(success, na.rm = TRUE)) %>%
  ggplot(aes(factor(Down), success_rate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title="Success Rate by Down", x="Down", y="Success Rate")
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
#Pass rate by Downs and Distance (excluding plays with more than 20 yards till first down/touchdown)
pbp %>%
  filter(Down %in% 1:4, ToGo <= 20) %>%
  group_by(Down, ToGo) %>%
  summarize(pass_rate = mean(IsPass == 1), .groups="drop") %>%
  ggplot(aes(ToGo, factor(Down), fill = pass_rate)) +
  geom_tile() +
  scale_fill_viridis_c(labels = scales::percent) +
  labs(title="Pass Rate Heatmap by Down & Distance",
       x="Yards to Go", y="Down", fill="Pass Rate")
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
