select
  count(*),
  avg(lead_changes),
  std(lead_changes)
from games
where
  -- games where the number of lead ...
  -- changes is more than 2 standard ...
  -- deviations above the average
  lead_changes >= 12.739
;
