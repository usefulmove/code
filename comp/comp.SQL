create schema development;

show databases;

show tables ;

use development ;

set sql_safe_updates = 0 ;

-- update to proposed base for all
update employee
  set base_salary = proposed_base,
	  proposed_base = null ;

-- update promotions and benchmarks
update employee
  set position = proposed_position,
	  benchmark25 = proposed_benchmark25,
	  benchmark50 = proposed_benchmark50,
	  benchmark75 = proposed_benchmark75,
      proposed_position = null,
	  proposed_benchmark25 = null,
	  proposed_benchmark50 = null,
	  proposed_benchmark75 = null
where
  proposed_position is not null ;

desc employee ;

select * from employee ;
