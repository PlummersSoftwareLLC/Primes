CREATE or REPLACE TYPE number_t as OBJECT (n number, isPrime number(1));
/

CREATE or REPLACE TYPE bit_tab as TABLE of number_t;
/

declare
    bit_array       bit_tab;
    num             number_t;
begin
    num := number_t(1,1);
    bit_array := bit_tab(num);
    for n in (select isPrime from table(bit_array))
    loop
      dbms_output.put_line('=' || n.isPrime );
    end loop;

end;
/


CREATE or REPLACE TYPE flag_t as OBJECT (isPrime number(1));
/

CREATE or REPLACE TYPE bit_tab as TABLE of flag_t;
/

declare
    bit_array       bit_tab;
    flag            flag_t;
    max_limit       INT         default 1000;
    flag_0          flag_t      default flag_t(0);
    flag_1          flag_t      default flag_t(1);
begin
    flag := flag_t(1);
    bit_array := bit_tab(flag);

    select flag_t(1)
        bulk collect
    into
        bit_array
    from 
        (
            select 2 as n from dual
            union all
            select n from (
            select level, ((level * 2) +1) as n
            from dual
            connect by level <(max_limit/2)
            )
        )
    ;

    for rec in (select isPrime from table(bit_array))
    loop
      dbms_output.put_line('=' || rec.isPrime );
    end loop;

end;
/