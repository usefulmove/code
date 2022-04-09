-- create table for example
if exists temp_table
  drop table temp_table ;
create table temp_table (
  id integer not null auto_increment,
  dept varchar(32),
  primary key (id)
) ;
alter table temp_table auto_increment=1 ;
insert into temp_table
  (dept)
  select dept from survey_one ;


-- delete duplicate records from table
delete from temp_table
where id not in
        ( -- minimum ids for each grouped duplicate addresses
          select id from -- i don't understand why MySQL requires this
          (
            select min(a.id) as id
            from temp_table a
             -- the reason for this join is to get around a MySQL
             -- shortcoming that will not allow deletion on a table
             -- that is used in the where clause
             left join temp_table b on a.id = b.id
            group by a.dept
          ) as o
        ) ;
