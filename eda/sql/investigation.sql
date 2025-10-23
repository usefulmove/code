/* A crime has taken place and the detective needs your help. The detective gave you the crime
   scene report, but you somehow lost it. You vaguely remember that the crime was a murder
   that occurred sometime on Jan.15, 2018 and that it took place in SQL City. Start by
   retrieving the corresponding crime scene report from the police department’s database. */


-- crime scene report ---------------------------------------------------------
from
    crime_scene_report
where
    date = 20180115
    and type = 'murder'
    and city = 'SQL City'
-- witness one: lives at last house on Northwestern Dr
-- witness two: Annabel, lives on Franklin Ave


-- search for witness one -----------------------------------------------------
from
    person
where
    address_street_name ilike '%northwestern%'
    and address_number in (
        from person
        select min(address_number)
        where address_street_name ilike '%northwestern%'
        union
        from person
        select max(address_number)
        where address_street_name ilike '%northwestern%')
-- Marty Schapiro, id 14887
-- Kinsey Erickson, id 89906


-- search for witness two -----------------------------------------------------
from
    person
where
    name ilike '%annabel%'
    and address_street_name ilike '%franklin%'
-- Annabel Miller, id: 16371


-- search witness interviews --------------------------------------------------
from
    interview
where
    person_id in (select id
                  from person
                  where
                      address_street_name ilike '%northwestern%'
                      and address_number in (
                          from person
                          select min(address_number)
                          where address_street_name ilike '%northwestern%'
                          union
                          from person
                          select max(address_number)
                          where address_street_name ilike '%northwestern%')
                  union
                  select id
                  from person
                  where
                      name ilike '%annabel%'
                      and address_street_name ilike '%franklin%')
-- witness one testimony: person of interest is Get Fit Now member, membership no. starts with "48Z",
--                        gold member, drove away in car with "H42W" in the license plate no.
-- witness two testimony: saw murder, murderer was in the gym previous week on 20180109


-- search for person of interest ----------------------------------------------
with member_candidates as (
    select
        distinct membership_id
    from
        get_fit_now_check_in
    where
        check_in_date = 20180109
        and membership_id ilike '48Z%'),
-- cross with membership information
member_pois as (
    from
        member_candidates c
        left join get_fit_now_member m
            on c.membership_id = m.id
    where
        membership_status = 'gold')
-- cross with personal and dmv data
from
    member_pois mp
    left join person p on mp.person_id = p.id
    left join drivers_license l on p.license_id = l.id
where
    l.plate_number ilike '%h42w%'
-- Jeremy Bowers, ssn 871539279, person_id 67318 (our person of interest)


-- search for interviews ------------------------------------------------------
from
    interview
where
    person_id = '67318'
-- hired by woman, wealthy, 65-67", red hair, drives Tesla Model S, attended
-- SQL Symphony 3 times in Dec 2017


-- search event logs ----------------------------------------------------------
with second_pois as (
    select
        person_id,
        count(*) as count
    from
        facebook_event_checkin
    where
        --event_name ilike '%SQL Symphony%'
        event_id = 1143
        and date between 20171201 and 20171231
    group by
        person_id
    having
        count(*) = 3)
-- cross with personal and dmv data
from
    second_pois sp
    left join person p on sp.person_id = p.id
    left join drivers_license d on p.license_id = d.id
where
    d.gender = 'female'
    and car_make ilike '%Tesla%'
    and car_model ilike '%Model S%'
    and hair_color = 'red'
-- Miranda Priestly, ssn 987756388, person_id 99716 (our second person of interest)


-- full info on persons of interest -------------------------------------------
select
    p.id, p.ssn, p.name, d.gender,
    p.address_number, p.address_street_name,
    d.age, d.height, d.eye_color, d.hair_color,
    p.license_id, d.plate_number, d.car_make, d.car_model,
    i.annual_income
from
    person p
    left join drivers_license d on p.license_id = d.id
    left join income i on p.ssn = i.ssn
where
    p.id in (67318, 99716)