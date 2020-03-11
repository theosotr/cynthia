import sqlite3

import psycopg2
import MySQLdb as mysql


class BaseDatabase(object):
    NAME = None

    def __init__(self, dbname, user=None, password=None):
        self.dbname = dbname
        self.user = user
        self.password = password

    def __enter__(self):
        self.connect()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.disconnect()

    def connect(self):
        raise NotImplementedError('`connect()` must be implemented')

    def disconnect(self):
        if self.dbcon:
            self.dbcon.close()

    def createdb(self):
        raise NotImplementedError('`createdb()` must be implemented')

    def setupSchema(self, schema):
        raise NotImplementedError('`setupSchema()` must be implemented')


class Postgres(BaseDatabase):
    NAME = 'postgres'
    HOST = 'localhost'

    def connect(self):
        con = psycopg2.connect(dbname=self.dbname or 'postgres',
                               user=self.user, host=self.HOST,
                               password=self.password)
        con.autocommit = True
        self.dbcon = con

    def createdb(self, dbname):
        cur = self.dbcon.cursor()
        # Check if database exists
        cur.execute(
            "SELECT 1 FROM pg_catalog.pg_database WHERE datname = '{}'".format(
                dbname))
        exists = cur.fetchone()
        if not exists:
            cur.execute('CREATE DATABASE {}'.format(dbname))

    def setupSchema(self, schema):
        cursor = self.dbcon.cursor()
        with open(schema) as f:
            cursor.execute(f.read())


class MySQL(BaseDatabase):
    NAME = 'mysql'
    HOST = 'localhost'

    def connect(self):
        self.dbcon = mysql.connect(host=self.HOST,
                                   user=self.user,
                                   passwd=self.password,
                                   db=self.dbname or 'sys')

    def createdb(self, dbname):
        cur = self.dbcon.cursor()
        cur.execute('CREATE DATABASE IF NOT EXISTS {}'.format(dbname))
        cur.close()
        self.dbcon.commit()

    def setupSchema(self, schema):
        cursor = self.dbcon.cursor()
        with open(schema) as f:
            cursor.execute(f.read().replace('\n', ' '))
            cursor.close()
            self.dbcon.commit()


class SQLite3(BaseDatabase):
    name = 'sqlite3'
    HOST = ''

    def connect(self):
        self.dbcon = sqlite3.connect('db')

    def createdb(self, dbname):
        pass

    def setupSchema(self, schema):
        pass
