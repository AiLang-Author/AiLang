


Junk drawer plan for cobol systems runtime , upgrade path for Banks, governments and more. 
Loose development planning. 




THE SETUP
┌─────────────────────────────────────────────────────────────┐
│ OLD PC #1: Database Server                                   │
│ - Debian Minimal (500MB RAM)                                 │
│ - PostgreSQL 15                                              │
│ - SSH access only                                            │
└─────────────────────────────────────────────────────────────┘
                           ▲
                           │ Port 5432
                           │
┌─────────────────────────────────────────────────────────────┐
│ OLD PC #2: Application Server                                │
│ - Alpine Linux (128MB RAM!) or Debian Minimal               │
│ - AILANG runtime (compiled binary)                           │
│ - AILANG HTTP Server (serves web UI + handles API)          │
│ - Port 8080                                                  │
└─────────────────────────────────────────────────────────────┘
                           ▲
                           │ HTTP
                           │
┌─────────────────────────────────────────────────────────────┐
│ Client: ANY Web Browser                                      │
│ - Firefox, Chromium, even lightweight browsers              │
│ - Pure HTML/CSS/Vanilla JS (NO frameworks)                  │
│ - ~100KB total                                               │
└─────────────────────────────────────────────────────────────┘
Total footprint: ~200MB for entire stack vs. ~500MB for just Electron!

1. LINUX SERVER SETUP (PostgreSQL Box)
Option A: Debian Minimal (Recommended)
bash# Install Debian netinst (350MB)
# During install: UNCHECK everything except "SSH server"

# After boot:
apt update
apt install postgresql postgresql-contrib
systemctl enable postgresql
systemctl start postgresql

# Configure to accept remote connections
nano /etc/postgresql/15/main/postgresql.conf
# Change: listen_addresses = '*'

nano /etc/postgresql/15/main/pg_hba.conf
# Add: host    all    all    192.168.1.0/24    md5

systemctl restart postgresql

# Create database
sudo -u postgres psql
CREATE DATABASE ssa_claims_db;
CREATE USER claims_processor WITH PASSWORD 'secure_password';
GRANT ALL PRIVILEGES ON DATABASE ssa_claims_db TO claims_processor;
\q
Memory usage: ~150MB idle
Option B: Alpine Linux (Ultra-Minimal)
bash# Even lighter: 130MB installed
apk add postgresql postgresql-contrib
rc-update add postgresql
rc-service postgresql start

2. LINUX CLIENT SETUP (AILANG App Server)
Alpine Linux (My Recommendation)
bash# Install Alpine Extended (~200MB)
apk add bash gcc musl-dev linux-headers

# Copy your AILANG compiler and libraries
# Compile AILANG programs directly on this machine
Debian Minimal
bash# Debian netinst, SSH + standard utilities only
apt install gcc make



THE ARCHITECTURE: COBOL Program Runner Daemon
┌─────────────────────────────────────────────────────────────┐
│ /opt/ailang/cobol/                                           │
│ ├── programs/           ← Transpiled AILANG binaries         │
│ │   ├── CLAIMS01.exec   (64KB)                              │
│ │   ├── PAYCALC.exec    (48KB)                              │
│ │   ├── VALIDATE.exec   (32KB)                              │
│ │   └── REPORTS.exec    (56KB)                              │
│ ├── jcl/               ← Job definitions (like mainframe)    │
│ │   ├── daily_batch.jcl                                     │
│ │   └── claims_process.jcl                                  │
│ ├── data/              ← Shared data files                  │
│ │   ├── CLAIMS.DAT                                          │
│ │   └── MASTER.DAT                                          │
│ └── logs/              ← Execution logs                     │
└─────────────────────────────────────────────────────────────┘
                           ▲
                           │
┌─────────────────────────────────────────────────────────────┐
│ ailang-cobol-daemon (systemd service)                        │
│                                                              │
│ ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│ │   Scheduler  │  │   Registry   │  │  Job Queue   │      │
│ │  (like cron) │  │  (catalog)   │  │  (pending)   │      │
│ └──────────────┘  └──────────────┘  └──────────────┘      │
│                                                              │
│ ┌──────────────────────────────────────────────────────┐   │
│ │  Execution Engine                                     │   │
│ │  - Spawns programs                                    │   │
│ │  - Manages environment (COMP-3, file I/O)           │   │
│ │  - Handles CALL (inter-program communication)        │   │
│ │  - Collects logs/output                              │   │
│ └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
                    PostgreSQL DB

1. SYSTEMD SERVICE UNIT
ini# /etc/systemd/system/ailang-cobol.service
[Unit]
Description=AILANG COBOL Runtime Daemon
After=network.target postgresql.service
Requires=postgresql.service

[Service]
Type=simple
User=ailang
Group=ailang
WorkingDirectory=/opt/ailang/cobol
ExecStart=/opt/ailang/bin/cobol-daemon
Restart=on-failure
RestartSec=5
StandardOutput=journal
StandardError=journal

# Resource limits (optional)
MemoryMax=512M
CPUQuota=50%

[Install]
WantedBy=multi-user.target
Install & manage:
bash# Install
sudo systemctl enable ailang-cobol
sudo systemctl start ailang-cobol

# Status
sudo systemctl status ailang-cobol

# Logs
journalctl -u ailang-cobol -f



3. JCL FILE FORMAT (Job Control Language)
jcl# /opt/ailang/cobol/jcl/daily_batch.jcl
# Daily claims processing batch job

JOB DAILY_CLAIMS_BATCH
EXEC CLAIMS01.exec
DD CLAIMS=/opt/ailang/cobol/data/CLAIMS.DAT
DD OUTPUT=/opt/ailang/cobol/data/PROCESSED.DAT
PARM DB_HOST=localhost
PARM DB_PORT=5432
jcl# /opt/ailang/cobol/jcl/claims_process.jcl
# Single claim processing

JOB PROCESS_CLAIM
EXEC VALIDATE.exec
PARM CLAIM_ID=CLM-2025-001234

STEP CALCULATE
EXEC PAYCALC.exec

STEP APPROVE
EXEC CLAIMS01.exec






5. DIRECTORY STRUCTURE
bash/opt/ailang/
├── bin/
│   ├── cobol-daemon         # Main daemon binary
│   └── ailang               # AILANG runtime
├── cobol/
│   ├── programs/            # Transpiled COBOL programs
│   │   ├── CLAIMS01.exec
│   │   ├── PAYCALC.exec
│   │   ├── VALIDATE.exec
│   │   └── REPORTS.exec
│   ├── jcl/                 # Job control files
│   │   ├── daily_batch.jcl
│   │   └── claims_process.jcl
│   ├── data/                # Shared data files
│   │   ├── CLAIMS.DAT
│   │   └── MASTER.DAT
│   └── logs/                # Execution logs
│       ├── job_1001.log
│       └── daemon.log
└── lib/
    ├── Library.COBOL.ailang
    ├── Library.PostgreSQL.ailang
    └── Library.JCL.ailang

COMPLETE WORKFLOW

Install daemon:

bashsudo systemctl enable ailang-cobol
sudo systemctl start ailang-cobol

Drop in transpiled programs:

bashcp CLAIMS01.exec /opt/ailang/cobol/programs/

Submit job via JCL:

bashcp daily_batch.jcl /opt/ailang/cobol/jcl/
# Daemon auto-picks it up

Or submit via web UI:

bashcurl -X POST http://localhost:8080/api/jobs/submit \
  -d '{"program":"CLAIMS01.exec","claim_id":"CLM-001"}'

Monitor:

bashjournalctl -u ailang-cobol -f
tail -f /opt/ailang/cobol/logs/job_1001.log


USER IN BROWSER
    ↓ clicks "Calculate Benefits"
    ↓
┌─────────────────────────────────────────────┐
│ Web Server (AILANG HTTP Server)             │
│ - Receives HTTP request                     │
│ - Creates SESSION for this user             │
│ - Spawns COBOL program instance FOR USER    │
└─────────────────────────────────────────────┘
    ↓
┌─────────────────────────────────────────────┐
│ COBOL Program Instance (PAYCALC.exec)       │
│ - Runs ON SERVER (not client machine!)     │
│ - Gets user's data from session             │
│ - Calculates benefit amount                 │
│ - Returns result to web server              │
└─────────────────────────────────────────────┘
    ↓
┌─────────────────────────────────────────────┐
│ PostgreSQL Database                         │
│ - Stores calculation results                │
│ - Updates beneficiary record                │
└─────────────────────────────────────────────┘
    ↓
Back to browser as JSON: {"benefit_amount": 1547}