drop table listings;
create table if not exists listings (
  id serial not null primary key,
  sale_price numeric (10) not null,
  yearly_rent numeric(10) not null,
  toto boolean not null
);

insert into listings (sale_price, yearly_rent, toto)
values (30.4, 12.1, false);
insert into listings (sale_price, yearly_rent, toto)
values (12.0, 242.11, false);
insert into listings (sale_price, yearly_rent, toto)
values (195.12, 3.3, true);
