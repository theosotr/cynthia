drop table if exists book;
CREATE TABLE book (
      id serial primary key,
      title varchar(100) NOT NULL,
      author varchar(100) NOT NULL,
      isbn char(12),
      unique(isbn)
);

drop table if exists review;
CREATE TABLE review (
      id serial primary key,
      book_id integer NOT NULL,
      reviewer_name varchar(255),
      content varchar(255),
      rating integer,
      FOREIGN KEY (book_id) REFERENCES book(id) ON DELETE CASCADE
);


insert into book(id, title, author, isbn)
  values (1, "Programming in C", "Joe Coecker", "12345678-012");
insert into book(id, title, author, isbn)
  values (2, "Random book", "Foel Jessie", "12345679-012");
insert into book(id, title, author, isbn)
  values (3, "ABC Inc.", "James Brown", "12345678-014");

insert into review(id, book_id, reviewer_name, content, rating)
  values(1, 1, "Reviewer A", "Super", 5);
insert into review(id, book_id, reviewer_name, content, rating)
  values(2, 1, "Apollo12", "Fairly Good", 4);
insert into review(id, book_id, reviewer_name, content, rating)
  values(3, 2, "Reviewer A", "Moderate", 3);
insert into review(id, book_id, reviewer_name, content, rating)
  values(5, 2, "Kine Shot", "Decent", 3);
insert into review(id, book_id, reviewer_name, content, rating)
  values(6, 2, "Jessica Alba", "Not good", 1);
insert into review(id, book_id, reviewer_name, content, rating)
  values(7, 3, "Reviewer A", "Dissapointing", 1);
insert into review(id, book_id, reviewer_name, content, rating)
  values(8, 3, "Jessica Alba", "Excellent", 5);
