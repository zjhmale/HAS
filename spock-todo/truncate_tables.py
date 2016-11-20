import MySQLdb
import os

if __name__ == '__main__':
  username = os.getenv("MySQLUSERNAME")
  password = os.getenv("MySQLPASSWORD")
  db = MySQLdb.connect("localhost", username, password, "todo")
  cursor = db.cursor()
  cursor.execute("SET FOREIGN_KEY_CHECKS = 0")
  cursor.execute("TRUNCATE user")
  cursor.execute("TRUNCATE post")
  cursor.execute("SET FOREIGN_KEY_CHECKS = 1")
  db.close()
