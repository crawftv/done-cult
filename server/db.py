import json
import os
import sqlite3
from typing import Any, Dict

from server.types import SaveData

DB_NAME = os.path.realpath(os.path.join(os.getcwd(), os.path.dirname(__file__), "done.db"))


def create_db():
    with sqlite3.connect(DB_NAME) as conn:
        conn.execute("""CREATE TABLE IF NOT EXISTS users (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            auth0_user_id TEXT UNIQUE NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );""")


def add_user(user_id: str):
    with sqlite3.connect(DB_NAME) as conn:
        conn.execute("INSERT INTO users (auth0_user_id) VALUES (?)", (user_id,))


def create_app_data_table():
    with sqlite3.connect(DB_NAME) as conn:
        # Drop existing table if it exists to add UNIQUE constraint
        conn.execute("DROP TABLE IF EXISTS app_data;")

        conn.execute("""CREATE TABLE IF NOT EXISTS app_data (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            auth0_user_id TEXT NOT NULL UNIQUE,
            date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            dataset TEXT NOT NULL,
            FOREIGN KEY (auth0_user_id) REFERENCES users(auth0_user_id)
        );""")


def insert_app_data(user_id: str, dataset: SaveData) -> None:
    """
    Insert or update app data for a user.

    Args:
        user_id: The Auth0 user ID
        dataset: Dictionary containing the app data to store

    The function will:
    1. Serialize the dataset to JSON
    2. Insert new record or update existing one based on auth0_user_id
    3. Update the timestamp automatically
    """
    with sqlite3.connect(DB_NAME) as conn:
        # Convert dataset to JSON string
        dataset_json = dataset.json()

        conn.execute("""
            INSERT INTO app_data (auth0_user_id, dataset)
            VALUES (?, ?)
            ON CONFLICT(auth0_user_id) 
            DO UPDATE SET 
                dataset = excluded.dataset,
                date_updated = CURRENT_TIMESTAMP;
        """, (user_id, dataset_json))


def get_app_data(user_id: str) -> Dict[str, Any]:
    """
    Retrieve app data for a user.

    Args:
        user_id: The Auth0 user ID

    Returns:
        Dictionary containing the deserialized app data
    """
    with sqlite3.connect(DB_NAME) as conn:
        cursor = conn.execute(
            "SELECT dataset FROM app_data WHERE auth0_user_id = ?",
            (user_id,)
        )
        result = cursor.fetchone()

        return {} if result is None else json.loads(result[0])


if __name__ == "__main__":
    create_db()
    create_app_data_table()
