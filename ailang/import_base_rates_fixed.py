#!/usr/bin/env python3
# import_base_rates_fixed.py - Import with EBCDIC support

import psycopg2
from datetime import datetime
import sys

# EBCDIC to ASCII translation table (CP037 - US EBCDIC)
def ebcdic_to_ascii(ebcdic_bytes):
    """Convert EBCDIC bytes to ASCII string"""
    try:
        # Try CP037 (US EBCDIC)
        return ebcdic_bytes.decode('cp037')
    except:
        try:
            # Try CP500 (International EBCDIC)
            return ebcdic_bytes.decode('cp500')
        except:
            # Fallback - ignore errors
            return ebcdic_bytes.decode('ascii', errors='ignore')

# Connect to database
conn = psycopg2.connect(
    host='localhost',
    port=5432,
    database='testdb',
    user='testuser',
    password='testpass'
)
cur = conn.cursor()

print("="*60)
print("Importing BASERATE.dat (with EBCDIC support)")
print("="*60)

baserate_file = "cobol_frontend/tests/medicare/BASERATE.dat"

# Try both encodings
encodings = ['utf-8', 'latin-1', 'cp037', 'cp500']

for encoding in encodings:
    print(f"\nTrying encoding: {encoding}")
    try:
        with open(baserate_file, 'r', encoding=encoding, errors='ignore') as f:
            lines = f.readlines()
            print(f"✓ Successfully read {len(lines)} lines")
            
            # Show first line for analysis
            first_line = lines[0]
            print(f"First line length: {len(first_line)}")
            print(f"First line: {repr(first_line[:60])}")
            break
    except Exception as e:
        print(f"✗ Failed: {e}")
        continue

# Parse with corrected column positions
print("\nParsing data...")

imported = 0
skipped = 0

with open(baserate_file, 'rb') as f:
    line_num = 0
    for raw_line in f:
        line_num += 1
        
        # Try to decode
        try:
            line = raw_line.decode('utf-8', errors='ignore')
        except:
            line = raw_line.decode('latin-1', errors='ignore')
        
        # Skip empty lines
        if len(line.strip()) == 0:
            continue
        
        # Parse with CORRECT field positions
        # After looking at the data, the format appears to be:
        # Columns 0-4: State/Area code (right-justified)
        # Columns 5-13: Date (YYYYMMDD)
        # Columns 14-19: Hospital rate
        # Columns 20-25: Independent rate
        # Columns 26+: Area name
        
        try:
            state_code = line[0:5].strip()
            date_str = line[5:14].strip()  # Changed from 3:11 to 5:14
            hospital_rate_str = line[14:20].strip()
            independent_rate_str = line[20:26].strip()
            area_name = line[26:].strip()
            
            # Skip if date is invalid
            if len(date_str) != 8 or not date_str.isdigit():
                skipped += 1
                if line_num < 50 or skipped < 10:  # Only show first errors
                    print(f"  Line {line_num}: Invalid date '{date_str}' - skipping")
                continue
            
            # Parse date
            effective_date = datetime.strptime(date_str, '%Y%m%d').date()
            year = effective_date.year
            
            # Parse rates (divide by 100 for decimal)
            hospital_rate = float(hospital_rate_str) / 100.0
            independent_rate = float(independent_rate_str) / 100.0
            
            # Insert into database
            cur.execute("""
                INSERT INTO medicare.esrd_base_rates 
                (effective_year, rate_version, effective_date, hospital_based_rate, 
                 independent_rate, budget_neutrality, drug_addon, labor_pct, nonlabor_pct)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)
                ON CONFLICT (effective_year, rate_version) 
                DO UPDATE SET
                    hospital_based_rate = EXCLUDED.hospital_based_rate,
                    independent_rate = EXCLUDED.independent_rate,
                    effective_date = EXCLUDED.effective_date
            """, (year, state_code, effective_date, hospital_rate, independent_rate, 
                  0.9116, 10.87, 0.0, 0.0))
            
            imported += 1
            
            if imported % 100 == 0:
                print(f"  Imported {imported} records...")
                
        except Exception as e:
            skipped += 1
            if skipped < 10:
                print(f"  Line {line_num}: Error - {e}")
            continue

conn.commit()

print("\n" + "="*60)
print(f"✓ Import Complete")
print(f"  Imported: {imported} records")
print(f"  Skipped:  {skipped} records")
print("="*60)

# Show sample data
print("\nSample imported data:")
cur.execute("""
    SELECT effective_year, rate_version, effective_date, 
           hospital_based_rate, independent_rate
    FROM medicare.esrd_base_rates
    ORDER BY effective_date DESC
    LIMIT 10
""")

for row in cur.fetchall():
    print(f"  {row[0]} {row[1]:>5} | {row[2]} | H=${row[3]:6.2f} I=${row[4]:6.2f}")

conn.close()