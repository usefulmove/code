SELECT  E.Name, S.id, 
  DATEADD('hour', 
    CASE
      WHEN DATEADD('hour',-7,P.CREATED_DATE ) < DATEADD('hour',2,to_date('2021-11-07'))
      THEN -7 //2021 PST

      WHEN DATEADD('hour',2,to_date('2021-11-07'))< DATEADD('hour',-8,P.CREATED_DATE )
        AND DATEADD('hour',-8,P.CREATED_DATE) < DATEADD('hour',2,to_date('2022-03-13'))
      THEN -8 //2021-2022 PDT

      WHEN DATEADD('hour',2,to_date('2022-03-13'))< DATEADD('hour',-7,P.CREATED_DATE )
        AND DATEADD('hour',-7,P.CREATED_DATE) < DATEADD('hour',2,to_date('2022-11-06'))
      THEN -7 //2022 PST

      WHEN DATEADD('hour',2,to_date('2022-11-06'))< DATEADD('hour',-8,P.CREATED_DATE )
        AND DATEADD('hour',-8,P.CREATED_DATE) < DATEADD('hour',2,to_date('2023-03-12'))
      THEN -8 //2022-2023 PDT

      ELSE -7 END,
    P.CREATED_DATE) AS PLATE_CREATED_DATE, 
  date_trunc('MONTH',PLATE_CREATED_DATE) AS month_of_created_date
FROM LIMS_SAMPLE_C AS S
LEFT JOIN LIMS_PLATES_C AS P
ON S.LIMS_PLATE_C=P.ID
LEFT JOIN LIMS_EQUIPMENT_C AS E
ON P.EXTRACTION_UNIT_C=E.ID
WHERE S.Testing_Record_C=False
AND S.Sample_Type_C='Clinical Samples'
AND DATE(month_of_created_date)>= DATE_TRUNC('MONTH',DATEADD(month,-3,current_date())) ;
