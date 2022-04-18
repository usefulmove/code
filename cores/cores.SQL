-- most common categories contained in most viewed videos (top 10%)
select
  q.category,
  count(*) as count
from
  ( select
      o.id, o.categories, c.category
    from
      /* cores o left join core_categories c */
      ( select * -- top 10% of cores by number of views
        from cores
        where nb_views >= 8.5555e5
        order by nb_views desc
      ) as o
      left join core_categories as c
        on instr(o.categories, c.category) > 0
    -- order by id
  ) as q
group by q.category
order by count desc
limit 20;
