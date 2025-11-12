-- setup_exec85_test.sql
-- Simple test data for EXEC85

-- First, make sure tables exist (in case they don't)
CREATE TABLE IF NOT EXISTS CONTROL_CARD_FILE_seq (
    seq_id SERIAL PRIMARY KEY,
    record_data VARCHAR(4096),
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS POPULATION_FILE_seq (
    seq_id SERIAL PRIMARY KEY,
    record_data VARCHAR(4096),
    created_at TIMESTAMP DEFAULT NOW()
);

-- Clear any existing test data
TRUNCATE TABLE CONTROL_CARD_FILE_seq;
TRUNCATE TABLE POPULATION_FILE_seq;

-- Insert control cards (monitoring config + simple update)
INSERT INTO CONTROL_CARD_FILE_seq (record_data) VALUES 
('*OPTIONS    Y                                                                  '),
('X-TEST01                                                                        '),
('*END-MONITOR                                                                    '),
('*BEGIN-UPDATE                                                                   '),
('*END-UPDATE                                                                     ');

-- Insert a simple test program in the population file
INSERT INTO POPULATION_FILE_seq (record_data) VALUES
('*HEADER,COBOL,TEST01                                                            '),
('000100 IDENTIFICATION DIVISION.                                                 '),
('000200 PROGRAM-ID. TEST01.                                                      '),
('000300 PROCEDURE DIVISION.                                                      '),
('000400     DISPLAY "HELLO FROM TEST01".                                         '),
('000500     STOP RUN.                                                            '),
('*END-OF,TEST01                                                                  ');

-- Show what we inserted
SELECT 'Control Cards:' as table_name, COUNT(*) as rows FROM CONTROL_CARD_FILE_seq
UNION ALL
SELECT 'Population File:', COUNT(*) FROM POPULATION_FILE_seq;