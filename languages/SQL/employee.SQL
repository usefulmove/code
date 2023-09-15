create table department (
  id integer,
  name varchar(64)
) ;

describe employee;
select * from employee;
describe department;
select * from department;

insert into department
select
  rank() over (order by department asc) as id,
  d.department as name
from ( select distinct
         department
       from
         employee
     ) as d ;
     
alter table employee
  add column manager_id integer ;
     
alter table employee
  add column department_id integer
  after benchmark75 ;

set sql_safe_updates = 0;

update employee as e, department as d
  set e.department_id = d.id
where
  e.department = d.name ;
  
alter table employee
  drop column department ;

alter table employee
  add column manager_id integer
  after department_id ;
  
update employee
  set manager_id = 999
where
  manager like '%Edmonds%' ;
  
alter table employee
  drop column manager ;
  
insert into employee (
  employee_id,
  name,
  hire_date,
  position
)
values (
  999,
  "Edmonds, Robert",
  "2021-08-17",
  "VP Engineering"
)
