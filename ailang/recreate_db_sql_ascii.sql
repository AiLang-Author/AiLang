-- ============================================================================
-- Recreate testdb with SQL_ASCII encoding to support COBOL binary data
-- ============================================================================

-- Terminate all existing connections to testdb
SELECT pg_terminate_backend(pg_stat_activity.pid)
FROM pg_stat_activity
WHERE pg_stat_activity.datname = 'testdb'
  AND pid <> pg_backend_pid();

-- Drop and recreate the database with SQL_ASCII
DROP DATABASE IF EXISTS testdb;

CREATE DATABASE testdb 
    ENCODING 'SQL_ASCII'
    LC_COLLATE 'C'
    LC_CTYPE 'C'
    TEMPLATE template0;

-- Connect to the new database
\c testdb

-- Recreate tables
CREATE TABLE IF NOT EXISTS active_jobs (
    pid INTEGER PRIMARY KEY,
    job_id INTEGER NOT NULL,
    program_name TEXT NOT NULL,
    started_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS api_requests (
    job_id INTEGER PRIMARY KEY,
    request_data TEXT,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS api_responses (
    job_id INTEGER PRIMARY KEY,
    response_data TEXT,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS cobol_files (
    file_name TEXT PRIMARY KEY,
    organization TEXT,
    access_mode TEXT,
    record_key TEXT,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS cobol_sequential_files (
    file_name TEXT NOT NULL,
    seq_id SERIAL,
    record_data TEXT,
    created_at TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (file_name, seq_id)
);

CREATE TABLE IF NOT EXISTS cobol_indexed_files (
    file_name TEXT NOT NULL,
    key_value TEXT NOT NULL,
    record_data TEXT,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (file_name, key_value)
);

GRANT ALL PRIVILEGES ON DATABASE testdb TO testuser;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO testuser;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO testuser;

SELECT 
    datname AS database,
    pg_encoding_to_char(encoding) AS encoding,
    datcollate AS collation,
    datctype AS ctype
FROM pg_database 
WHERE datname = 'testdb';
