#!/bin/bash
# import_base_rates.sh - Import BASERATE.dat and BASECBSA.dat

DB_NAME="testdb"
DB_USER="testuser"
DB_HOST="localhost"
DB_PORT="5432"

GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "=========================================="
echo "Importing Medicare Base Rate Data"
echo "=========================================="
echo ""

# Python script to parse and import the data
python3 << 'PYPYTHON'
import psycopg2
import sys
from datetime import datetime

# Connect to database
conn = psycopg2.connect(
    host='localhost',
    port=5432,
    database='testdb',
    user='testuser',
    password='testpass'
)
cur = conn.cursor()

# ============================================================================
# IMPORT BASERATE.dat
# Format: State_Code(2) Date(8) Hospital_Rate(6) Independent_Rate(6) State_Name(variable)
# Example: 01 20050401 012757 012318 ALABAMA
# ============================================================================

print("Importing BASERATE.dat...")
baserate_file = "cobol_frontend/tests/medicare/BASERATE.dat"

try:
    with open(baserate_file, 'r') as f:
        line_num = 0
        for line in f:
            line_num += 1
            
            # Skip empty lines
            if len(line.strip()) == 0:
                continue
            
            # Parse fixed-width format
            state_code = line[0:2].strip()
            date_str = line[3:11].strip()
            hospital_rate_str = line[12:18].strip()
            independent_rate_str = line[19:25].strip()
            state_name = line[26:].strip()
            
            # Convert date YYYYMMDD to DATE
            try:
                effective_date = datetime.strptime(date_str, '%Y%m%d').date()
            except:
                print(f"  Warning: Invalid date on line {line_num}: {date_str}")
                continue
            
            # Convert rates (stored as integers, need to divide by 100 for decimal)
            try:
                hospital_rate = float(hospital_rate_str) / 100.0
                independent_rate = float(independent_rate_str) / 100.0
            except:
                print(f"  Warning: Invalid rate on line {line_num}")
                continue
            
            # Extract year
            year = effective_date.year
            
            # Insert into database
            cur.execute("""
                INSERT INTO medicare.esrd_base_rates 
                (effective_year, rate_version, effective_date, hospital_based_rate, 
                 independent_rate, budget_neutrality, drug_addon, labor_pct, nonlabor_pct)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)
                ON CONFLICT (effective_year, rate_version) DO UPDATE
                SET hospital_based_rate = EXCLUDED.hospital_based_rate,
                    independent_rate = EXCLUDED.independent_rate
            """, (year, f"{state_code}", effective_date, hospital_rate, independent_rate, 
                  0.9116, 10.87, 0.0, 0.0))
            
            if line_num % 100 == 0:
                print(f"  Processed {line_num} BASERATE records...")
        
        conn.commit()
        print(f"✓ Imported {line_num} BASERATE records")

except FileNotFoundError:
    print(f"✗ File not found: {baserate_file}")

# ============================================================================
# IMPORT BASECBSA.dat  
# Format: State_Code(4) Date(8) Rate(6) State_Name(variable)
# Example:    01 20060101 008885 ALABAMA
# ============================================================================

print("\nImporting BASECBSA.dat...")
basecbsa_file = "cobol_frontend/tests/medicare/BASECBSA.dat"

try:
    with open(basecbsa_file, 'r') as f:
        line_num = 0
        for line in f:
            line_num += 1
            
            # Skip empty lines
            if len(line.strip()) == 0:
                continue
            
            # Parse fixed-width format
            state_code = line[0:4].strip()
            date_str = line[5:13].strip()
            rate_str = line[14:20].strip()
            state_name = line[21:].strip()
            
            # Convert date
            try:
                effective_date = datetime.strptime(date_str, '%Y%m%d').date()
            except:
                print(f"  Warning: Invalid date on line {line_num}: {date_str}")
                continue
            
            # Convert rate
            try:
                cbsa_rate = float(rate_str) / 100.0
            except:
                print(f"  Warning: Invalid rate on line {line_num}")
                continue
            
            year = effective_date.year
            
            # Insert as CBSA adjustment factor
            cur.execute("""
                INSERT INTO medicare.adjustment_factors
                (effective_year, factor_type, range_low, range_high, multiplier)
                VALUES (%s, %s, %s, %s, %s)
                ON CONFLICT (effective_year, factor_type, range_low) DO UPDATE
                SET multiplier = EXCLUDED.multiplier
            """, (year, f"CBSA_{state_code}", 0, 999999, cbsa_rate))
            
            if line_num % 100 == 0:
                print(f"  Processed {line_num} BASECBSA records...")
        
        conn.commit()
        print(f"✓ Imported {line_num} BASECBSA records")

except FileNotFoundError:
    print(f"✗ File not found: {basecbsa_file}")

# Show summary
print("\n" + "="*50)
print("Data Summary")
print("="*50)

cur.execute("SELECT COUNT(*) FROM medicare.esrd_base_rates")
baserate_count = cur.fetchone()[0]
print(f"Base Rates:         {baserate_count} records")

cur.execute("SELECT COUNT(*) FROM medicare.adjustment_factors")
adjustment_count = cur.fetchone()[0]
print(f"Adjustment Factors: {adjustment_count} records")

# Show sample data
print("\nSample Base Rates:")
cur.execute("""
    SELECT effective_year, rate_version, hospital_based_rate, independent_rate
    FROM medicare.esrd_base_rates
    ORDER BY effective_year DESC
    LIMIT 5
""")
for row in cur.fetchall():
    print(f"  {row[0]} {row[1]}: Hospital=${row[2]:.2f}, Independent=${row[3]:.2f}")

conn.close()
print("\n✓ Import complete!")

PYPYTHON