select 
    solution,
    duration 
from results 
where 
    step = 'End' 
order by duration desc
;