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
  from @cohort_database_schema.@cohort_table t
  inner join @cohort_database_schema.@cohort_table i
    on t.subject_id = i.subject_id
    and i.cohort_start_date <= t.cohort_start_date
) a
inner join (
  select
    exposure_indication_cohort_id,
    exposure_cohort_id,
    indication_cohort_id
  from #exposure_indication_cohort_ref
 ) b
  on a.exposure_cohort_id = b.exposure_cohort_id
  and a.indication_cohort_id = b.indication_cohort_id
;

insert into @cohort_database_schema.@cohort_table (
  cohort_definition_id,
  subject_id,
  cohort_start_date,
  cohort_end_date
)
select
  cohort_definition_id,
  subject_id,
  cohort_start_date,
  cohort_end_date
FROM #exposure_indication_cohorts
;

truncate table #exposure_indication_cohort_ref;
drop table #exposure_indication_cohort_ref;

truncate table #exposure_indication_cohorts;
drop table #exposure_indication_cohorts;
