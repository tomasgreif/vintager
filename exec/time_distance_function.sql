
CREATE OR REPLACE FUNCTION time_distance(to_date date, from_date date, granularity varchar)
  RETURNS integer AS
$BODY$

declare
	distance 	int;

begin

   if granularity = 'month' then
         distance := (extract(months from age(date_trunc('month',to_date), date_trunc('month',from_date))))::int + 12*(extract(years from age(date_trunc('month',to_date), date_trunc('month',from_date))))::int;
   elsif granularity = 'quarter' then
       distance := trunc(((extract(months from age(date_trunc('quarter',to_date), date_trunc('quarter',from_date)))) + 12*(extract(years from age(date_trunc('quarter',to_date), date_trunc('quarter',from_date)))))/3)::integer;
   elsif granularity = 'year' then
       distance := ((trunc(extract(years from age(date_trunc('month',to_date), date_trunc('month',from_date)))) ))::integer;
   else
       distance:= -1;
   end if;

	return distance;

end;
$BODY$
  LANGUAGE plpgsql IMMUTABLE COST 100;
