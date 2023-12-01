select cohort_Definition_id, count(*)
from scratch_jweave17.epi_792_hes
group by cohort_definition_id


select visit_concept_id, concept_name, count(*)
from cdm_cprd_v1715_epi_792.visit_occurrence
join cdm_cprd_v1715_epi_792.concept
on visit_concept_id = concept_id
group by visit_concept_id, concept_name


select * from cdm_optum_extended_dod_v2050.person limit 10

select 
  cohort_definition_id,
  count(distinct subject_id) persons,
  count(*) records,
  avg(datediff(d, cohort_start_date, cohort_end_date)) avg_cohort_length
from scratch_jweave17.epi_964
group by cohort_definition_id
order by 1


-- 8422 [964] Remicade and methotrexate (90d surveillance)
-- persons records avg_length
-- 6501	6501	573

-- 8449 [964] Rheumatoid Arthritis
-- persons records avg_length
-- 1140095	1140095	1082

with
ref as (
  select 
    8422 exposure_cohort_id,
    8449 indicatoin_cohort_id,
    84228449 exposure_indication_cohort_id
  union all
  select 
    8423 exposure_cohort_id,
    8449 indication_cohort_id,
    84238449 exposure_indication_cohort_id
  union all
  select
    8424 exposure_cohort_id,
    8450 indication_cohort_id,
    84248450 exposure_indication_cohort_id
)
--select * from ref
select
  exposure_indication_cohort_id
  t.cohort_definition_id,
  t.subject_id,
  t.cohort_start_date,
  t.cohort_end_date,
  r.indication_cohort_id
from scratch_jweave17.epi_964 t
inner join scratch_jweave17.epi_964 i
  on t.subject_id = i.subject_id
  and i.cohort_start_date <= t.cohort_start_date
where t.cohort_definition_id = r.exposure_cohort_id
and i.cohort_definition_id = r.indication_cohort_id
inner join ref r
  on r.epoxsure_cohort_id = t.cohort_definition_id
  and r.indication_cohort_id = i.indication_cohort_id
  





drop table if exists #exposure_indication_cohorts;






select
  ei.cohot_definition_id,
  ei.subject_id,
  ei.cohort_start_date,
  ei.cohort_end_date
from (


select
  exposure_indication_cohort_id,
  cohort_definition_id,
  cohort_definition_id indication_cohort_id,
  subject_id,
  cohort_start_date,
  cohort_end_date
  --exposure_indication_cohort_id,
  --count(*)
from (
  select
    *
  from (
    select
      t.cohort_definition_id,
      i.cohort_definition_id indication_cohort_id,
      t.subject_id,
      t.cohort_start_date,
      t.cohort_end_date
    from scratch_jweave17.epi_964 t
    inner join scratch_jweave17.epi_964 i
      on t.subject_id = i.subject_id
      and i.cohort_start_date <= t.cohort_start_date 
  ) a
  inner join (
    select 
      8422 exposure_cohort_id,
      8449 indication_cohort_id,
      84228449 exposure_indication_cohort_id
    union all
    select 
      8423 exposure_cohort_id,
      8449 indication_cohort_id,
      84238449 exposure_indication_cohort_id
    union all
    select
      8424 exposure_cohort_id,
      8450 indication_cohort_id,
      84248450 exposure_indication_cohort_id
  ) b
    on a.cohort_definition_id = b.exposure_cohort_id
    and a.indication_cohort_id = b.indication_cohort_id
) q
--group by exposure_indication_cohort_id
;












drop table if exists #exposure_indication_cohorts
;

select
  b.exposure_indication_cohort_id cohort_definition_id,
  a.subject_id,
  a.cohort_start_date,
  a.cohort_end_date
into #exposure_indication_cohorts
from (
  select
    t.cohort_definition_id exposure_cohort_id,
    i.cohort_definition_id indication_cohort_id,
    t.subject_id,
    t.cohort_start_date,
    t.cohort_end_date
  from scratch_jweave17.epi_964 t
  inner join scratch_jweave17.epi_964 i
    on t.subject_id = i.subject_id
    and i.cohort_start_date <= t.cohort_start_date 
    --and t.cohort_definition_id = 8422
    --and i.cohort_definition_id = 8449    
) a
inner join (
  select 
    8422 exposure_cohort_id,
    8449 indication_cohort_id,
    84228449 exposure_indication_cohort_id
  union all
  select 
    8423 exposure_cohort_id,
    8449 indication_cohort_id,
    84238449 exposure_indication_cohort_id
  union all
  select
    8424 exposure_cohort_id,
    8450 indication_cohort_id,
    84248450 exposure_indication_cohort_id
 ) b
  on a.exposure_cohort_id = b.exposure_cohort_id
  and a.indication_cohort_id = b.indication_cohort_id
;


select cohort_definition_id, count(*) 
from #exposure_indication_cohorts
group by cohort_definition_id


select cohort_definition_id, count(*) 
from scratch_jweave17.epi_964
group by cohort_definition_id
order by 1

--optum done
-- ccae done
-- pharmetrics done
-- ehr done
-- amb emr done
-- delete from scratch_jweave17.epi_964
-- where cohort_definition_id in (
-- 84228449,84238449,84248531,84258531,84248450,84258450,84248451,84258451,84248452,84258452,84248453,84258453,84268449,84308449,84318449,84278449,84438449,84448449,84288449,84458449,84468449,84298449,84478449,84488449,84268531,84308531,84318531,84278531,84438531,84448531,84288531,84458531,84468531,84298531,84478531,84488531,84268450,84308450,84318450,84278450,84438450,84448450,84288450,84458450,84468450,84298450,84478450,84488450,84268451,84308451,84318451,84278451,84438451,84448451,84288451,84458451,84468451,84298451,84478451,84488451,84268452,84308452,84318452,84278452,84438452,84448452,84288452,84458452,84468452,84298452,84478452,84488452,84268453,84308453,84318453,84278453,84438453,84448453,84288453,84458453,84468453,84298453,84478453,84488453
-- )


--optum_ehr
-- delete from scratch_jweave17.epi_964_4
-- where cohort_definition_id in (9205, 92058449) -- etanercept, certolizumab pegol, tocilizumab (RA) w Rheumatoid Arthritis(92058449)

select cohort_definition_id, count(*) 
from scratch_jweave17.epi_964_4
group by cohort_definition_id
order by 1


--pharmetrics
-- delete from scratch_jweave17.epi_964_4
-- where cohort_definition_id in (9205, 92058449)


-- optum_dod
-- delete from scratch_jweave17.epi_964_4
-- where cohort_definition_id in (9205, 92058449)



select * from cdm_iqvia_amb_emr_v1979.person limit 10


select drug_concept_id, count(*) from cdm_optum_extended_dod_v2050.drug_exposure where drug_source_value = 'J1745' group by drug_concept_id

select * from cdm_optum_extended_dod_v2050.concept where concept_id = 46275595


select count(*)
from results_truven_ccae_v2044.cohort t
inner join results_truven_ccae_v2044.cohort c
on t.subject_id = c.subject_id
where t.cohort_definition_id = 8422
and c.cohort_definition_id = 8980



select count(*)
from scratch_jweave17.epi_964_2 t
inner join scratch_jweave17.epi_964_2 c
on t.subject_id = c.subject_id
where t.cohort_definition_id = 84228449
and c.cohort_definition_id = 89808449


select min(cohort_start_date)
from scratch_jweave17.epi_964_2
where cohort_definition_id = 84228449






-- fix overlap problem

drop table if exists #exposure_indication_cohorts;

with
exposure_indication_cohort_ref as (
  select 84228449 as exposure_indication_cohort_id, 8422 as exposure_cohort_id, 8449 as indication_cohort_id union all -- remmtx w prior ra
  select 84248451 as exposure_indication_cohort_id, 8424 as exposure_cohort_id, 8451 as indication_cohort_id union all
  select 84249028 as exposure_indication_cohort_id, 8424 as exposure_cohort_id, 9028 as indication_cohort_id union all
  select 84249029 as exposure_indication_cohort_id, 8424 as exposure_cohort_id, 9029 as indication_cohort_id union all
  select 89808449 as exposure_indication_cohort_id, 8980 as exposure_cohort_id, 8449 as indication_cohort_id union all -- groupab w prior ra
  select 89808451 as exposure_indication_cohort_id, 8980 as exposure_cohort_id, 8451 as indication_cohort_id union all
  select 89809028 as exposure_indication_cohort_id, 8980 as exposure_cohort_id, 9028 as indication_cohort_id union all
  select 89809029 as exposure_indication_cohort_id, 8980 as exposure_cohort_id, 9029 as indication_cohort_id
)
select
  b.exposure_indication_cohort_id cohort_definition_id,
  a.subject_id,
  a.cohort_start_date,
  a.cohort_end_date
into #exposure_indication_cohorts
from (
  select
    t.cohort_definition_id exposure_cohort_id,
    i.cohort_definition_id indication_cohort_id,
    t.subject_id,
    t.cohort_start_date,
    t.cohort_end_date
  from scratch_jweave17.epi_964_2 t
  inner join scratch_jweave17.epi_964_2 i
    on t.subject_id = i.subject_id
    and i.cohort_start_date <= t.cohort_start_date
) a
inner join (
  select
    exposure_indication_cohort_id,
    exposure_cohort_id,
    indication_cohort_id
  from exposure_indication_cohort_ref
 ) b
  on a.exposure_cohort_id = b.exposure_cohort_id
  and a.indication_cohort_id = b.indication_cohort_id
;


select cohort_definition_id, count(*) from #exposure_indication_cohorts group by cohort_definition_id











