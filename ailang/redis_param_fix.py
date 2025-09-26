#!/usr/bin/env python3
"""
Apply parameter passing fix to redis_server.ailang
"""

from pathlib import Path
import re

def apply_fix():
    redis_file = Path("redis_server.ailang")
    
    if not redis_file.exists():
        print(f"ERROR: {redis_file} not found!")
        return False
        
    content = redis_file.read_text()
    
    # Backup original
    backup = redis_file.with_suffix('.ailang.backup')
    backup.write_text(content)
    print(f"Backed up to {backup}")
    
    # 1. Fix HandleClient function signature
    old_signature = r'Function\.Server\.HandleClient \{\s*Input: client_socket: Integer'
    new_signature = 'Function.Server.HandleClient {\n    Input: client_socket: Integer\n    Input: store_ptr: Integer'
    
    content = re.sub(old_signature, new_signature, content)
    print("✓ Updated HandleClient signature to accept store_ptr parameter")
    
    # 2. Replace the store = RedisData.store_ptr line
    old_store_line = r'(\s*)store = RedisData\.store_ptr'
    new_store_line = r'\1// Use the passed store pointer\n\1store = store_ptr'
    
    content = re.sub(old_store_line, new_store_line, content)
    print("✓ Updated HandleClient to use passed store_ptr")
    
    # 3. Fix the HandleClient call in Main
    # Find the pattern where HandleClient is called
    old_call = r'Server\.HandleClient\(client_socket\)'
    new_call = 'Server.HandleClient(client_socket, RedisData.store_ptr)'
    
    content = re.sub(old_call, new_call, content)
    print("✓ Updated HandleClient call to pass store_ptr from Main")
    
    # Write fixed version
    redis_file.write_text(content)
    print(f"\n✓ Fixed {redis_file}")
    
    return True

def verify_fix():
    """Verify the fix was applied correctly"""
    redis_file = Path("redis_server.ailang")
    content = redis_file.read_text()
    
    checks = [
        ("HandleClient accepts store_ptr", "Input: store_ptr: Integer"),
        ("Uses passed store_ptr", "store = store_ptr"),
        ("Main passes store_ptr", "Server.HandleClient(client_socket, RedisData.store_ptr)")
    ]
    
    print("\nVerifying fixes:")
    all_good = True
    for desc, pattern in checks:
        if pattern in content:
            print(f"  ✓ {desc}")
        else:
            print(f"  ✗ {desc} - NOT FOUND")
            all_good = False
            
    return all_good

if __name__ == "__main__":
    print("Redis Server Parameter Passing Fix")
    print("=" * 50)
    
    if apply_fix():
        if verify_fix():
            print("\n✓ All fixes applied successfully!")
            print("\nNow recompile and test:")
            print("  python3 main.py redis_server.ailang")
            print("  ./redis_server_exec &")
            print("  redis-cli ping")
        else:
            print("\n✗ Some fixes may not have applied correctly")
            print("Check redis_server.ailang manually")
    else:
        print("\n✗ Fix failed to apply")