drop table if exists listing;
create table if not exists listing (
  id serial not null primary key,
  sale_price numeric (10) not null,
  yearly_rent numeric(10) not null,
  toto boolean not null,
  foo varchar(20) not null
);

insert into listing (id, sale_price, yearly_rent, toto, foo)
  values (1, 30.4, 12.1, '0', 'bar');
insert into listing (id, sale_price, yearly_rent, toto, foo)
  values (2, 12.0, 242.11, '0', 'baz');
insert into listing (id, sale_price, yearly_rent, toto, foo)
  values (3, 195.12, 3.3, '1', 'qux');
