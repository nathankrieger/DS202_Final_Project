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
#pbp$PlayType
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

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
