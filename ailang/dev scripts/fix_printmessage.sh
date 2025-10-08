#!/bin/bash
# fix_printmessage.sh - Wrap or comment out PrintMessage calls outside Debug blocks

echo "Backing up redis_server.ailang..."
cp redis_server.ailang redis_server.ailang.backup

echo "Fixing PrintMessage calls outside Debug blocks..."

# Create a temporary file
cat > fix_printmessage.sed << 'EOF'
# Line 669 - Wrap in Debug block
669s/PrintMessage/Debug("error.store", level=1) { PrintMessage/
669a\
                                    }

# Line 1292 - Wrap in Debug block  
1292s/PrintMessage/Debug("error.xread", level=1) { PrintMessage/
1292a\
                                    }

# Line 1340 - Wrap in Debug block
1340s/PrintMessage/Debug("error.null", level=1) { PrintMessage/
1340a\
                                                    }

# Line 1841 - Wrap in Debug block
1841s/PrintMessage/Debug("error.flushdb", level=1) { PrintMessage/
1841a\
                                }

# Line 2123-2125 - Wrap verification read in Debug block
2123s/PrintMessage/Debug("main.verify", level=1) { PrintMessage/
2125s/PrintMessage/PrintMessage/
2125a\
    }

# Line 2129 - Wrap in Debug block
2129s/PrintMessage/Debug("error.store_creation", level=1) { PrintMessage/
2129a\
        }

# Line 2155 - Wrap server listening message in Debug block
2155s/PrintMessage/Debug("server.start", level=1) { PrintMessage/
2155a\
    }
EOF

# Apply the fixes
sed -i -f fix_printmessage.sed redis_server.ailang

echo "Fixes applied!"
echo ""
echo "Compiling without debug..."
python3 main.py redis_server.ailang

if [ $? -eq 0 ]; then
    echo "Compilation successful!"
    echo ""
    echo "Quick test:"
    # Kill any existing server
    pkill -f redis_server_exec 2>/dev/null
    
    # Start the server in background
    ./redis_server_exec &
    SERVER_PID=$!
    
    # Wait for server to start
    sleep 1
    
    # Test performance
    echo "Testing PING performance (10 iterations)..."
    total_time=0
    for i in {1..10}; do
        start=$(date +%s.%N)
        redis-cli PING > /dev/null 2>&1
        end=$(date +%s.%N)
        duration=$(echo "scale=3; ($end - $start) * 1000" | bc)
        echo "  PING $i: ${duration}ms"
        total_time=$(echo "scale=3; $total_time + $duration" | bc)
    done
    
    avg=$(echo "scale=3; $total_time / 10" | bc)
    echo ""
    echo "Average PING time: ${avg}ms"
    
    if (( $(echo "$avg < 1" | bc -l) )); then
        echo "✅ Performance FIXED! Sub-1ms response times achieved!"
    else
        echo "⚠️ Still slow. May need to check for other issues."
    fi
    
    # Kill the test server
    kill $SERVER_PID 2>/dev/null
    
    rm fix_printmessage.sed
else
    echo "Compilation failed!"
    echo "Restoring backup..."
    mv redis_server.ailang.backup redis_server.ailang
fi