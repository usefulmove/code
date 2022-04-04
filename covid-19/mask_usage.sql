-- county mask scores view
(
  SELECT
    o.county_fips_code AS fips,
    (o.never * -20.0 + o.rarely * -10.0 + o.sometimes * 5.0 + o.frequently * 15.0 + o.always * 25.0) AS mask_score
  FROM
  `bigquery-public-data.covid19_nyt.mask_use_by_county` AS o
)

-- county fips, name, state view
(
  SELECT
    DISTINCT c.county_fips_code,
    c.county,
    c.state_name
  FROM `bigquery-public-data.covid19_nyt.us_counties` AS c
)


-- county mask scores view
(
  SELECT
    counties.county,
    counties.state_name,
    scores.mask_score
  FROM
    (
      SELECT
        o.county_fips_code AS fips,
        (o.never * -20.0 + o.rarely * -10.0 + o.sometimes * 5.0 + o.frequently * 15.0 + o.always * 25.0) AS mask_score
      FROM
        `bigquery-public-data.covid19_nyt.mask_use_by_county` AS o
    ) AS scores
      LEFT JOIN
        (
          SELECT
            DISTINCT c.county_fips_code,
            c.county,
            c.state_name
          FROM `bigquery-public-data.covid19_nyt.us_counties` AS c
        ) AS counties
        ON scores.fips = counties.county_fips_code
  ORDER BY scores.mask_score ASC
)
