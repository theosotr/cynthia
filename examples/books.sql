drop table if exists review;
drop table if exists book;
drop table if exists author;
CREATE TABLE author (
      id integer,
      first_name varchar(50) NOT NULL,
      surname varchar(50) NOT NULL,
      PRIMARY KEY (id)
);

CREATE TABLE book (
      id integer,
      title varchar(100) NOT NULL,
      author_id integer NOT NULL,
      isbn char(12),
      unique(isbn),
      PRIMARY KEY (id),
      FOREIGN KEY (author_id) REFERENCES author(id) ON DELETE CASCADE
);

CREATE TABLE review (
      id integer,
      book_id integer NOT NULL,
      reviewer_name varchar(255),
      content varchar(255),
      rating integer,
      PRIMARY KEY (id),
      FOREIGN KEY (book_id) REFERENCES book(id) ON DELETE CASCADE
);


insert into author(id, first_name, surname)
  values(1, 'Joe', 'Coecker');
insert into author(id, first_name, surname)
  values(2, 'Foel', 'Jessie');
insert into author(id, first_name, surname)
  values(3, 'James', 'Brown');

insert into book(id, title, author_id, isbn)
  values (1, 'Programming in C', 1, '12345678-012');
insert into book(id, title, author_id, isbn)
  values (2, 'Random book', 1, '12345679-012');
insert into book(id, title, author_id, isbn)
  values (3, 'ABC Inc.', 2, '12345678-014');
insert into book(id, title, author_id, isbn)
  values (4, 'travel world', 3, '12345678-015');
insert into book(id, title, author_id, isbn)
  values (5, 'What else?', 3, '12345678-016');
insert into book(id, title, author_id, isbn)
  values (6, 'ABCD_F', 3, '12345678-017');

insert into review(id, book_id, reviewer_name, content, rating)
  values(1, 1, 'Reviewer A', 'Super', 5);
insert into review(id, book_id, reviewer_name, content, rating)
  values(2, 1, 'Apollo12', 'Fairly Good', 4);
insert into review(id, book_id, reviewer_name, content, rating)
  values(3, 2, 'Reviewer A', 'Moderate', 3);
insert into review(id, book_id, reviewer_name, content, rating)
  values(5, 2, 'Kine Shot', 'Decent', 3);
insert into review(id, book_id, reviewer_name, content, rating)
  values(6, 2, 'Jessica Alba', 'Not good', 1);
insert into review(id, book_id, reviewer_name, content, rating)
  values(7, 3, 'Reviewer A', 'Dissapointing', 1);
insert into review(id, book_id, reviewer_name, content, rating)
  values(8, 3, 'Jessica Alba', 'Excellent', 5);
insert into review(id, book_id, reviewer_name, content, rating)
  values(9, 4, 'random rev', 'Blah', 2);
insert into review(id, book_id, reviewer_name, content, rating)
  values(10, 6, 'random rev', 'Good', 4);
insert into review(id, book_id, reviewer_name, content, rating)
  values(11, 6, 'Reviewer A', 'Briliant', 5);
