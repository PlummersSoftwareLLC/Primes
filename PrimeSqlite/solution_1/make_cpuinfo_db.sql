.separator :
    create table cpuinfo (
    field,value
);
.import /proc/cpuinfo cpuinfo
create table cpuinfo_sum as
select
    (select distinct trim(value) from cpuinfo where field like 'model name%') as model_name,
    (select distinct trim(value) from cpuinfo where field like 'cpu MHz%')     as cpu_MHz,
    (select distinct trim(value) from cpuinfo where field like 'cpu cores%')   as cpu_cores
;

