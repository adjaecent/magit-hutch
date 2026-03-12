import os

GLOBAL_VAR = 42

def helper():
    return True

class UserService:
    def __init__(self, db):
        self.db = db

    def find_user(self, user_id):
        query = f"SELECT * FROM users WHERE id = {user_id}"
        return self.db.execute(query)

    def delete_user(self, user_id):
        self.db.execute(f"DELETE FROM users WHERE id = {user_id}")

def standalone():
    x = 1
    y = 2
    return x + y
