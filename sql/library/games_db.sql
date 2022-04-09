select
    count(*) as total_games,
    countif(win is true) as home_wins,
    countif(win is false) as home_losses,
    countif(win is true) / COUNT(*) as home_win_percentage,
    avg(points_game - opp_points_game) as avg_game_diff
from
    bigquery-public-data.ncaa_basketball.mbb_teams_games_sr
where
  (lead_changes >= 12.739) and
  (home_team = true) and
  (neutral_site <> true) and
  (division_name like 'NCAA%')
;


select
    max(lead_changes + times_tied) as max,
    avg(lead_changes + times_tied) as avg,
    stddev(lead_changes + times_tied) as stdev
from bigquery-public-data.ncaa_basketball.mbb_games_sr
limit 3
;


select
    points_game - opp_points_game as point_diff,
    win,
    points,
    points_game,
    opp_points_game
from
    bigquery-public-data.ncaa_basketball.mbb_teams_games_sr
where home_team = true
;
