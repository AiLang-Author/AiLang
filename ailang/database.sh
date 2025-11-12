#!/bin/bash
# PostgreSQL Database Schema Checker for JCL System
# Verifies all required tables exist with correct schemas

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

DB_NAME="testdb"
DB_USER="testuser"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}JCL System Database Schema Checker${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Check if PostgreSQL is accessible
echo -e "${YELLOW}[1/5] Testing database connection...${NC}"
if psql -U $DB_USER -d $DB_NAME -c "SELECT 1;" > /dev/null 2>&1; then
    echo -e "${GREEN}✓ Database connection successful${NC}"
else
    echo -e "${RED}✗ Cannot connect to database${NC}"
    echo -e "${RED}   Please ensure PostgreSQL is running and credentials are correct${NC}"
    exit 1
fi

# Function to check if table exists
check_table() {
    local table_name=$1
    local description=$2
    
    if psql -U $DB_USER -d $DB_NAME -tc "SELECT 1 FROM pg_tables WHERE tablename = '$table_name';" | grep -q 1; then
        echo -e "${GREEN}✓ Table exists: $table_name${NC} - $description"
        return 0
    else
        echo -e "${RED}✗ Table missing: $table_name${NC} - $description"
        return 1
    fi
}

# Check all required tables
echo ""
echo -e "${YELLOW}[2/5] Checking required tables...${NC}"

TABLES_OK=1

check_table "api_requests" "HTTP Gateway request storage" || TABLES_OK=0
check_table "api_responses" "HTTP Gateway response storage" || TABLES_OK=0
check_table "active_jobs" "Container job registration" || TABLES_OK=0

# Optional tables (used by COBOL programs)
echo ""
echo -e "${YELLOW}[3/5] Checking optional/program-specific tables...${NC}"
echo -e "${BLUE}(These may not exist if no programs need them)${NC}"

psql -U $DB_USER -d $DB_NAME -c "\dt" 2>&1 | grep -v "api_requests\|api_responses\|active_jobs" | grep "public" || echo -e "${BLUE}No additional tables${NC}"

# Display detailed schemas
echo ""
echo -e "${YELLOW}[4/5] Displaying table schemas...${NC}"

echo ""
echo -e "${BLUE}--- api_requests schema ---${NC}"
psql -U $DB_USER -d $DB_NAME -c "\d api_requests" 2>&1 || echo -e "${RED}Table does not exist${NC}"

echo ""
echo -e "${BLUE}--- api_responses schema ---${NC}"
psql -U $DB_USER -d $DB_NAME -c "\d api_responses" 2>&1 || echo -e "${RED}Table does not exist${NC}"

echo ""
echo -e "${BLUE}--- active_jobs schema ---${NC}"
psql -U $DB_USER -d $DB_NAME -c "\d active_jobs" 2>&1 || echo -e "${RED}Table does not exist${NC}"

# Check for data
echo ""
echo -e "${YELLOW}[5/5] Checking table contents...${NC}"

echo -e "${BLUE}api_requests:${NC}"
REQUEST_COUNT=$(psql -U $DB_USER -d $DB_NAME -tc "SELECT COUNT(*) FROM api_requests;" 2>/dev/null | tr -d ' ' || echo "0")
echo "  Records: $REQUEST_COUNT"

echo -e "${BLUE}api_responses:${NC}"
RESPONSE_COUNT=$(psql -U $DB_USER -d $DB_NAME -tc "SELECT COUNT(*) FROM api_responses;" 2>/dev/null | tr -d ' ' || echo "0")
echo "  Records: $RESPONSE_COUNT"

echo -e "${BLUE}active_jobs:${NC}"
ACTIVE_COUNT=$(psql -U $DB_USER -d $DB_NAME -tc "SELECT COUNT(*) FROM active_jobs;" 2>/dev/null | tr -d ' ' || echo "0")
echo "  Records: $ACTIVE_COUNT (currently running jobs)"

# Final summary
echo ""
echo -e "${BLUE}========================================${NC}"

if [ $TABLES_OK -eq 1 ]; then
    echo -e "${GREEN}✓ All required tables exist${NC}"
    echo ""
    echo -e "${GREEN}Database is ready for JCL operations!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some tables are missing${NC}"
    echo ""
    echo -e "${YELLOW}To create missing tables, run:${NC}"
    echo "./create_jcl_database.sh"
    exit 1
fi