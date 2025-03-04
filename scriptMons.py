import json
import sqlite3
import sys

def insert_monsters_from_file(json_file, db_path="database.db"):
    with open(json_file, "r", encoding="utf-8") as file:
        json_data = json.load(file)
    
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("""
        CREATE TABLE IF NOT EXISTS Monsters (
            monsterId INTEGER PRIMARY KEY AUTOINCREMENT,
            monsterName TEXT NOT NULL,
            image TEXT NOT NULL
        )
    """)
    
    for monster in json_data:
        cursor.execute("INSERT INTO Monsters (monsterName, image) VALUES (?, ?)", (monster["monsterName"], monster["image"]))
    
    conn.commit()
    conn.close()

    print("Monsters inserted successfully!")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py <json_file>")
        sys.exit(1)
    
    json_file = sys.argv[1]
    insert_monsters_from_file(json_file)