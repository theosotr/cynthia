drop table if exists listing;
create table if not exists listing (
  id serial not null primary key,
  sale_price numeric (10) not null,
  yearly_rent numeric(10) not null,
  toto boolean not null
);

insert into listing (id, sale_price, yearly_rent, toto)
  values (1, 30.4, 12.1, '0');
insert into listing (id, sale_price, yearly_rent, toto)
  values (2, 12.0, 242.11, '0');
insert into listing (id, sale_price, yearly_rent, toto)
  values (3, 195.12, 3.3, '1');
