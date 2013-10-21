drop table if exists small_portfolio;

CREATE TABLE small_portfolio
(
  id integer,
  product character varying,
  origination_date date,
  repaid_date date
);

insert into small_portfolio values (1,'A','2012-01-01','2012-02-01');
insert into small_portfolio values (2,'A','2012-01-01','2012-04-01');
insert into small_portfolio values (3,'A','2012-01-01',NULL);
insert into small_portfolio values (4,'A','2012-02-01','2012-03-01');
insert into small_portfolio values (5,'A','2012-02-01','2012-04-01');
insert into small_portfolio values (6,'A','2012-02-01',NULL);
insert into small_portfolio values (7,'A','2012-02-01',NULL);
insert into small_portfolio values (8,'A','2012-03-01','2012-04-01');
insert into small_portfolio values (9,'A','2012-03-01','2012-04-01');
insert into small_portfolio values (10,'A','2012-03-01',NULL);
insert into small_portfolio values (11,'A','2012-03-01',NULL);
insert into small_portfolio values (12,'A','2012-03-01',NULL);
insert into small_portfolio values (13,'B','2012-01-01','2012-01-01');
insert into small_portfolio values (14,'B','2012-01-01','2012-01-01');
insert into small_portfolio values (15,'B','2012-01-01','2012-03-01');
insert into small_portfolio values (16,'B','2012-01-01',NULL);
insert into small_portfolio values (17,'B','2012-01-01',NULL);
insert into small_portfolio values (18,'B','2012-02-01','2012-02-01');
insert into small_portfolio values (19,'B','2012-02-01','2012-03-01');
insert into small_portfolio values (20,'B','2012-02-01',NULL);
insert into small_portfolio values (21,'B','2012-03-01','2012-04-01');
insert into small_portfolio values (22,'B','2012-03-01',NULL);
insert into small_portfolio values (23,'B','2012-03-01',NULL);
insert into small_portfolio values (24,'B','2012-03-01',NULL);
