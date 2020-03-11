class BaseDatabase(object):
    NAME = None

    def __init__(self, dbname, user=None, password=None):
        self.dbname = dbname
        self.user = user
        self.password = password

    def createdb(self):
        raise NotImplementedError('`createdb()` must be implemented')

    def setupSchema(self, schema):
        raise NotImplementedError('`setupSchema()` must be implemented')


class Postgres(BaseDatabase):
    NAME = 'postgres'

    def createdb(self):
        pass

    def setupSchema(self, schema):
        pass


class MySQL(BaseDatabase):
    NAME = 'mysql'

    def createdb(self):
        pass

    def setupSchema(self, schema):
        pass


class SQLite3(BaseDatabase):
    name = 'sqlite3'

    def createdb(self):
        pass

    def setupSchema(self, schema):
        pass
