delete from dataframe
where o.id not in
	( select id from
		( select min(a.id) as id
		from dataframe as a
			left join dataframe as b
				on a.id = b.id
		group by a.feature
        ) as o
	) ;
