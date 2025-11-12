# load_vocab.py
import psycopg2
from psycopg2.extras import execute_values
import sys
import re

# --- Configuration ---
DB_NAME = "smollm"
DB_USER = "sean"
DB_PASS = ""  # Leave empty if you have no password
DB_HOST = "localhost"
DB_PORT = "5432"
DICTIONARY_FILE = "dictionary.txt"

def main():
    """
    Parses dictionary.txt and bulk-inserts the words into the PostgreSQL database.
    """
    print(f"--- Vocabulary Loader for SmolLM ---")

    # 1. Parse the dictionary file
    print(f"\n[1/3] Loading and parsing '{DICTIONARY_FILE}'...")
    words = set()
    try:
        with open(DICTIONARY_FILE, 'r', encoding='utf-8') as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                
                # The word is the first token on the line before a space
                parts = line.split(' ', 1)
                if parts:
                    word = parts[0]
                    # Clean up the word: remove trailing punctuation and ensure it's a valid word
                    # This regex keeps letters, hyphens, and apostrophes
                    cleaned_word = re.sub(r"[^a-zA-Z-']", "", word)
                    if cleaned_word and len(cleaned_word) > 0:
                        words.add(cleaned_word.lower())

    except FileNotFoundError:
        print(f"--- FATAL: Dictionary file not found: '{DICTIONARY_FILE}' ---")
        print("Please make sure your dictionary file is in the same directory.")
        sys.exit(1)

    if not words:
        print("--- FATAL: No words were extracted from the dictionary file. ---")
        sys.exit(1)

    # Sort words for consistent IDs and assign an ID to each
    sorted_words = sorted(list(words))
    data_to_insert = [(i, word) for i, word in enumerate(sorted_words)]
    
    print(f"  ✓ Found and cleaned {len(data_to_insert)} unique words.")

    # 2. Connect to the database
    print(f"\n[2/3] Connecting to database '{DB_NAME}'...")
    conn = None
    try:
        conn = psycopg2.connect(
            dbname=DB_NAME,
            user=DB_USER,
            password=DB_PASS,
            host=DB_HOST,
            port=DB_PORT
        )
        cur = conn.cursor()
        print("  ✓ Connected successfully!")
    except psycopg2.OperationalError as e:
        print(f"--- FATAL: Could not connect to the database. ---")
        print(f"Error: {e}")
        print("Please ensure PostgreSQL is running and the connection details are correct.")
        sys.exit(1)

    # 3. Create table and insert data
    try:
        print("\n[3/3] Preparing database and loading data...")
        print("  - Creating 'vocabulary' table (if it doesn't exist)...")
        cur.execute("""
            CREATE TABLE IF NOT EXISTS vocabulary (
                id INT PRIMARY KEY,
                token TEXT NOT NULL
            );
        """)
        print("  - Granting permissions...")
        cur.execute("GRANT SELECT ON vocabulary TO sean;")


        print("  - Clearing any old data from the table...")
        cur.execute("TRUNCATE TABLE vocabulary;")
        
        print(f"  - Bulk inserting {len(data_to_insert)} words... (this may take a moment)")
        execute_values(
            cur,
            "INSERT INTO vocabulary (id, token) VALUES %s",
            data_to_insert,
            page_size=1000 # Process in chunks for efficiency
        )
        # NOTE: cur.rowcount only reports the size of the LAST batch.
        # The total number of rows should be the full length of the data list.
        print(f"  ✓ Bulk insert command sent for {len(data_to_insert)} rows.")

        # Commit the transaction
        conn.commit()
        print("\n🎉 SUCCESS! Vocabulary has been loaded into the database.")

    except (Exception, psycopg2.DatabaseError) as error:
        print(f"--- FATAL: Database error ---")
        print(error)
        conn.rollback()
    finally:
        if conn is not None:
            cur.close()
            conn.close()
            print("Database connection closed.")

if __name__ == "__main__":
    main()
