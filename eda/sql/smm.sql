-- murder crime report
from
  crime_scene_report
where
  date = 20180115
  and type = 'murder'
  and city = 'SQL City'


-- possible witness no. 1
from
  person
where
  address_street_name ilike '%northwestern%'
  and address_number in ((select min(address_number) as address_number
                          from person
                          where address_street_name ilike '%northwestern%')
                          union
                          (select max(address_number) as address_number
                          from person
                          where address_street_name ilike '%northwestern%'))


-- possible witness no. 2
from
  person
where
  address_street_name ilike '%franklin ave%'
  and name ilike '%annabel%'


-- witness interviews
from
  interview
where
  person_id in (14887, 89906, 16371)


-- persons of interest
with members_on_date as (
  select membership_id
  from get_fit_now_check_in
  where check_in_date = 20180109)

from
  get_fit_now_member
where
  id in (select membership_id from members_on_date)
  and id ilike '48z%'
  and membership_status = 'gold'


-- license plate search
with poi_licenses as (
  from drivers_license
  where plate_number ilike '%h42w%')

from
  person
where
  license_id in (select id from poi_licenses)


-- person of interest facebook event check-ins
from
  facebook_event_checkin
where
  person_id = 67318


-- and interview
from
  interview
where
  person_id = 67318


-- search for 2nd person of interest (from interview description)
from
  drivers_license l
  left join person p on l.id = p.license_id
  left join income i on p.ssn = i.ssn
where
  gender = 'female'
  and height between 65 and 67
  and hair_color = 'red'
  and car_make = 'Tesla'
  and car_model = 'Model S'


-- search for 2nd person of interest (from event check-ins)
with from_event_checkins as (
  select person_id, count(*) as count
  from facebook_event_checkin
  where
    event_id = 1143
    and date between 20171200 and 20171232
  group by person_id
  having count(*) = 3
  order by count(*) desc)

from
  person p
where
  p.id in (select person_id from from_event_checkins)


-- search for 2nd person of interest (cross event checkins search and description search)
with from_event_checkins as (
  select person_id, count(*) as count
  from facebook_event_checkin
  where
    event_id = 1143
    and date between 20171200 and 20171232
  group by person_id
  having count(*) = 3
  order by count(*) desc),

persons_of_interest as (
  from person p
  where p.id in (select person_id from from_event_checkins))

from
  drivers_license l
  left join person p on l.id = p.license_id
  left join income i on p.ssn = i.ssn
where
  gender = 'female'
  and height between 65 and 67
  and hair_color = 'red'
  and car_make = 'Tesla'
  and car_model = 'Model S'
  and i.ssn in (select ssn from persons_of_interest)


-- full info on persons of interest (Jeremy Bowers and Miranda Priestly)
select
  p.ssn,
  name,
  age,
  gender,
  height,
  eye_color,
  hair_color,
  address_number,
  address_street_name,
  license_id,
  plate_number,
  car_make,
  car_model,
  annual_income
from
  drivers_license l
  left join person p on l.id = p.license_id
  left join income i on p.ssn = i.ssn
where
  l.id in (423327, 202298)
