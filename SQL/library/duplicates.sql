-- check for duplicates
select value
from ( select
         value,
         count(*) as count
       from origin
       group by value
     ) as count_view
where count > 1 ;

