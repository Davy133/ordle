import json
import sqlite3
import sys

def insert_characters_from_file(json_file, db_path="database.db"):
    with open(json_file, "r", encoding="utf-8") as file:
        json_data = json.load(file)
    
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # Ensure the table exists (Modify accordingly)
    cursor.execute("DROP TABLE IF EXISTS Characters")
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS Characters (
            characterId INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT,
            age TEXT,
            status TEXT,
            association TEXT,
            first_appearance TEXT,
            actor TEXT,
            affinity TEXT,
            gender TEXT,
            emojis TEXT,
            imageChar TEXT,
            quote TEXT
        )
    """)
    
    for character in json_data:
        cursor.execute("""
            INSERT INTO Characters (
                name, age, status, association, first_appearance,
                actor, affinity, gender, emojis, imageChar, quote
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            character["name"],
            character["age"],
            character["status"],
            character["association"],
            character["first_appearance"],
            character["actor"],
            character["affinity"],
            character["gender"],
            character["emojis"],
            character["imageChar"],
            character["quote"]
        ))
    
    conn.commit()
    conn.close()

    print("Characters inserted successfully!")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py <json_file>")
        sys.exit(1)
    
    json_file = sys.argv[1]
    insert_characters_from_file(json_file)
