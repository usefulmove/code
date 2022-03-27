select
  o.region, sum(o.population) as population
from
  (
  select
    p.state as state,
    s.Region as region,
    p.population
  from
    population_us_states_2010_2018 p left join us_states s
    on p.state = s.State
  where
    (p.year = 2018) and (region is not null)
  ) o
group by o.region
;
