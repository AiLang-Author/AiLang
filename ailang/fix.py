#!/usr/bin/env python3
"""
Fix the Redis server store pointer issue
The problem: HandleClient gets store=0 on second connection
"""

import sys
from pathlib import Path

def create_fixed_redis_server():
    """Create a fixed version that properly maintains the store pointer"""
    
    fixed_code = '''// redis_server_fixed.ailang
// Fixed version with proper store handling

LibraryImport.RESP
LibraryImport.XArrays

FixedPool.RedisData {
    "store_ptr": Initialize=0
    "store_initialized": Initialize=0  // Add flag to track initialization
}

// ... [Keep all the helper functions and data types as-is] ...

Function.Server.HandleClient {
    Input: client_socket: Integer
    Body: {
        Debug("client.connect", level=1) { 
            PrintMessage("[DEBUG] New client connected.")
        }
        
        // CRITICAL FIX: Check if store is initialized, if not, something is wrong
        store_init_flag = RedisData.store_initialized
        IfCondition EqualTo(store_init_flag, 0) ThenBlock: {
            PrintMessage("[FATAL] Store was never initialized! Pool corruption detected.")
            SocketClose(client_socket)
            ReturnValue(0)
        }
        
        // Get the store pointer fresh each time - don't cache it
        store = RedisData.store_ptr
        
        // Validate the store pointer
        IfCondition EqualTo(store, 0) ThenBlock: {
            PrintMessage("[ERROR] Store pointer is NULL despite initialization flag!")
            PrintMessage("[DEBUG] Attempting to recover from Main's initialization...")
            
            // This shouldn't happen - indicates pool variable corruption
            SocketClose(client_socket)
            ReturnValue(0)
        }
        
        Debug("client.store", level=1) {
            PrintMessage("[DEBUG] Store pointer value: ")
            PrintNumber(store)
        }
        
        // Continue with normal client handling...
        // [Rest of HandleClient code remains the same]
    }
}

SubRoutine.Main {
    PrintMessage("[MAIN] Starting Main subroutine\\n")
    
    // Create the store and save it
    local_store = XSHash.XCreate(1024)
    PrintMessage("[MAIN] Created store at: ")
    PrintNumber(local_store)
    PrintMessage("\\n")
    
    // Store it in the pool variable
    RedisData.store_ptr = local_store
    RedisData.store_initialized = 1  // Set initialization flag
    
    // Verify it was stored correctly
    verify_store = RedisData.store_ptr
    PrintMessage("[MAIN] Verification read: ")
    PrintNumber(verify_store)
    PrintMessage("\\n")
    
    IfCondition NotEqual(verify_store, local_store) ThenBlock: {
        PrintMessage("[FATAL] Pool variable corruption detected at startup!")
        Exit(1)
    }
    
    // Continue with server setup...
    // [Rest of Main code remains the same]
}
'''
    
    # Save the fixed version
    fixed_file = Path("redis_server_fixed.ailang")
    
    # For now, just show what needs to be changed
    print("Key changes needed in redis_server.ailang:")
    print("\n1. Add initialization flag to pool:")
    print("   FixedPool.RedisData {")
    print('       "store_ptr": Initialize=0')
    print('       "store_initialized": Initialize=1')
    print("   }")
    
    print("\n2. In HandleClient, don't cache the store:")
    print("   Replace: store = RedisData.store_ptr")
    print("   With checks and fresh reads each time it's needed")
    
    print("\n3. Alternative: Pass store as parameter to HandleClient")
    print("   Change: Function.Server.HandleClient {")
    print("           Input: client_socket: Integer")
    print("           Input: store: Address")
    
    return fixed_code

def create_minimal_test():
    """Create a minimal test to isolate the pool variable issue"""
    
    test_code = '''// test_pool_persistence.ailang
// Test if pool variables maintain their values across function calls

LibraryImport.XArrays

FixedPool.TestData {
    "persistent_value": Initialize=0
}

Function.SetValue {
    Body: {
        PrintMessage("Setting value to 0xABCDEF\\n")
        TestData.persistent_value = 0xABCDEF
        
        value = TestData.persistent_value
        PrintMessage("Read back immediately: ")
        PrintNumber(value)
        PrintMessage("\\n")
    }
}

Function.CheckValue {
    Body: {
        PrintMessage("Checking value in different function...\\n")
        value = TestData.persistent_value
        PrintMessage("Value is: ")
        PrintNumber(value)
        PrintMessage("\\n")
        
        IfCondition EqualTo(value, 0xABCDEF) ThenBlock: {
            PrintMessage("SUCCESS: Pool variable persisted!\\n")
        } ElseBlock: {
            PrintMessage("FAILURE: Pool variable lost!\\n")
        }
    }
}

SubRoutine.Main {
    PrintMessage("Testing pool variable persistence\\n")
    
    // Set in one function
    SetValue()
    
    // Check in another function
    CheckValue()
    
    // Check again in Main
    main_value = TestData.persistent_value
    PrintMessage("Value in Main: ")
    PrintNumber(main_value)
    PrintMessage("\\n")
}

RunTask(Main)
'''
    
    test_file = Path("test_pool_persistence.ailang")
    test_file.write_text(test_code)
    print(f"\nCreated test: {test_file}")
    return test_file

# Create the test
test_file = create_minimal_test()
print("\nRun this test to verify pool variables work across function calls:")
print(f"  python3 main.py {test_file}")
print("  ./test_pool_persistence_exec")

# Show the fixes needed
print("\n" + "="*60)
create_fixed_redis_server()