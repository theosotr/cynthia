#! /usr/bin/env python3
import argparse
import sqlite3
import psycopg2
import mysql.connector


MODEL_TEMPLATE = 'class {} < ActiveRecord::Base\n\tself.table_name = "{}"\nend\n'
POSTGRES_PORT = '5432'
MYSQL_PORT = '3306'
HOST = 'localhost'


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Dump activerecord models from an SQL schema'
    )
    subparsers = parser.add_subparsers(title='engine')

    sqlite_parser = subparsers.add_parser('sqlite')
    sqlite_parser.add_argument("db")
    sqlite_parser.set_defaults(which='sqlite')

    postgres_parser = subparsers.add_parser('postgres')
    postgres_parser.add_argument("user")
    postgres_parser.add_argument("password")
    postgres_parser.add_argument("dbname")
    postgres_parser.set_defaults(which='postgres')

    mysql_parser = subparsers.add_parser('mysql')
    mysql_parser.add_argument("user")
    mysql_parser.add_argument("password")
    mysql_parser.add_argument("dbname")
    mysql_parser.set_defaults(which='mysql')

    args = parser.parse_args()
    which = args.which

    table_names = []
    if which == 'sqlite':
        con = sqlite3.connect(
            args.db
        )
        cursor = con.cursor()
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
        table_names = [i[0] for i in cursor.fetchall()]
    elif which == 'postgres':
        con = psycopg2.connect(
            host=HOST, port=POSTGRES_PORT,
            dbname=args.dbname, user=args.user,
            password=args.password
        )
        cursor = con.cursor()
        q = "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';"
        cursor.execute(q)
        table_names = [i[0] for i in cursor.fetchall()]
    elif which == 'mysql':
        con = mysql.connector.connect(
            host=HOST, port=MYSQL_PORT,
            database=args.dbname, user=args.user,
            password=args.password
        )
        cursor = con.cursor()
        q = "SELECT table_name FROM information_schema.tables WHERE table_schema = '{}';".format(args.dbname)
        cursor.execute(q)
        table_names = [i[0] for i in cursor.fetchall()]
    for t in table_names:
        m = MODEL_TEMPLATE.format(t.capitalize(), t)
        print(m)
