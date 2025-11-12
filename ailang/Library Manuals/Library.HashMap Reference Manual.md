# Library.HashMap Reference Manual

**Version:** 1.0  
**Module:** `Library.HashMap.ailang`  
**Type:** Pure AILANG Implementation  
**Dependencies:** `Library.XArrays`

---

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [API Styles](#api-styles)
4. [Nested Hash Tables](#nested-hash-tables)
5. [Simple Hash Tables](#simple-hash-tables)
6. [Advanced Implementations](#advanced-implementations)
7. [Memory Management](#memory-management)
8. [Usage Examples](#usage-examples)
9. [Performance Characteristics](#performance-characteristics)

---

## Overview

`Library.HashMap` provides multiple hash table implementations for different use cases:

- **Nested Hashes** - Redis-style hash-of-hashes (HSET, HGET, HGETALL)
- **Simple Hashes** - Direct hash pointer interface for single hash operations
- **Open Addressing** - Robin Hood hashing for cache-friendly performance
- **Concurrent Hashes** - Striped locking for thread-safe operations
- **LRU Cache** - Built-in least-recently-used eviction

### When to Use

| Use Case | Recommended Type |
|----------|------------------|
| Storing user records with fields | Nested Hash |
| Redis-compatible commands | Nested Hash |
| Single hash table operations | Simple Hash |
| High-performance lookups | Open Addressing |
| Multi-threaded access | Concurrent Hash |
| Limited memory with eviction | LRU Cache |

---

## Installation

```ailang
LibraryImport.XArrays    // Required dependency
LibraryImport.HashMap    // Or: Import("Library.HashMap")

// Now HashMap functions are available
map = HashMap.Create()
HashMap.HSet(map, "user:1000", "name", "Alice")
```

---

## API Styles

HashMap provides two distinct APIs:

### 1. Nested Hash API (4 parameters)

Manages multiple named hashes in a single structure:

```ailang
manager = HashMap.Create()
HashMap.HSet(manager, "user:1000", "name", "Alice")    // 4 params
value = HashMap.HGet(manager, "user:1000", "name")      // 3 params
```

### 2. Simple Hash API (3 parameters)

Direct operations on a single hash instance:

```ailang
hash = HashMap.CreateSimple()
HashMap.HSetSimple(hash, "name", "Alice")    // 3 params
value = HashMap.HGetSimple(hash, "name")      // 2 params
```

**Choosing the Right API:**
- Use **Nested** when managing multiple related hashes (like Redis keys)
- Use **Simple** when you just need one hash table

---

## Nested Hash Tables

Redis-style hash-of-hashes where each key contains its own hash table.

### HashMap.Create

Create a new hash-of-hashes manager.

**Signature:**
```ailang
Function.HashMap.Create {
    Output: Address
}
```

**Example:**
```ailang
// Create manager for multiple hashes
hashmap = HashMap.Create()
```

**Structure:** Creates an XSHash with 256 buckets to store inner hashes

---

### HashMap.HSet

Set a field in a hash.

**Signature:**
```ailang
Function.HashMap.HSet {
    Input: hashmap: Address
    Input: key: Address
    Input: field: Address
    Input: value: Address
    Output: Integer  // 1 if new field, 0 if updated
}
```

**Example:**
```ailang
hashmap = HashMap.Create()

// Set fields for user:1000
HashMap.HSet(hashmap, "user:1000", "name", "Alice")
HashMap.HSet(hashmap, "user:1000", "email", "alice@example.com")
HashMap.HSet(hashmap, "user:1000", "age", "30")

// Set fields for user:2000
HashMap.HSet(hashmap, "user:2000", "name", "Bob")
HashMap.HSet(hashmap, "user:2000", "email", "bob@example.com")

// Update existing field
is_new = HashMap.HSet(hashmap, "user:1000", "age", "31")  // Returns 0
```

**Behavior:**
- Creates inner hash automatically if key doesn't exist
- Updates existing field if present
- Frees old value when updating

---

### HashMap.HGet

Get a field value from a hash.

**Signature:**
```ailang
Function.HashMap.HGet {
    Input: hashmap: Address
    Input: key: Address
    Input: field: Address
    Output: Address  // Returns 0 if not found
}
```

**Example:**
```ailang
name = HashMap.HGet(hashmap, "user:1000", "name")

IfCondition EqualTo(name, 0) ThenBlock: {
    PrintMessage("Field not found")
} ElseBlock: {
    PrintString(name)
}

// Non-existent key
value = HashMap.HGet(hashmap, "user:9999", "name")  // Returns 0

// Non-existent field
value = HashMap.HGet(hashmap, "user:1000", "phone")  // Returns 0
```

---

### HashMap.HGetAll

Get all fields and values from a hash.

**Signature:**
```ailang
Function.HashMap.HGetAll {
    Input: hashmap: Address
    Input: key: Address
    Output: Address  // XArray with alternating field/value pairs
}
```

**Example:**
```ailang
// Get all fields for a user
pairs = HashMap.HGetAll(hashmap, "user:1000")
count = XArray.XSize(pairs)

// Pairs: [field1, value1, field2, value2, ...]
i = 0
WhileLoop LessThan(i, count) {
    field = XArray.XGet(pairs, i)
    value = XArray.XGet(pairs, Add(i, 1))
    
    PrintString(field)
    PrintMessage(": ")
    PrintString(value)
    
    i = Add(i, 2)  // Skip by 2
}

XArray.XDestroy(pairs)
```

---

### HashMap.HDel

Delete a field from a hash.

**Signature:**
```ailang
Function.HashMap.HDel {
    Input: hashmap: Address
    Input: key: Address
    Input: field: Address
    Output: Integer  // 1 if deleted, 0 if not found
}
```

**Example:**
```ailang
deleted = HashMap.HDel(hashmap, "user:1000", "age")

IfCondition EqualTo(deleted, 1) ThenBlock: {
    PrintMessage("Field deleted")
} ElseBlock: {
    PrintMessage("Field not found")
}
```

**Behavior:**
- Frees the field value
- If hash becomes empty, removes it from main hashmap
- Returns 1 on success, 0 if field didn't exist

---

### HashMap.HExists

Check if a field exists.

**Signature:**
```ailang
Function.HashMap.HExists {
    Input: hashmap: Address
    Input: key: Address
    Input: field: Address
    Output: Integer  // 1 if exists, 0 otherwise
}
```

**Example:**
```ailang
has_email = HashMap.HExists(hashmap, "user:1000", "email")
has_phone = HashMap.HExists(hashmap, "user:1000", "phone")

IfCondition has_email ThenBlock: {
    email = HashMap.HGet(hashmap, "user:1000", "email")
    PrintString(email)
}
```

---

### HashMap.HKeys

Get all field names from a hash.

**Signature:**
```ailang
Function.HashMap.HKeys {
    Input: hashmap: Address
    Input: key: Address
    Output: Address  // XArray of field names (copies)
}
```

**Example:**
```ailang
fields = HashMap.HKeys(hashmap, "user:1000")
count = XArray.XSize(fields)

PrintMessage("Fields: ")
i = 0
WhileLoop LessThan(i, count) {
    field = XArray.XGet(fields, i)
    PrintString(field)
    PrintMessage(", ")
    
    // Must free the field copy
    Deallocate(field, 0)
    i = Add(i, 1)
}

XArray.XDestroy(fields)
```

⚠️ **Memory:** Returns copies that must be freed!

---

### HashMap.HVals

Get all values from a hash.

**Signature:**
```ailang
Function.HashMap.HVals {
    Input: hashmap: Address
    Input: key: Address
    Output: Address  // XArray of values (not copies)
}
```

**Example:**
```ailang
values = HashMap.HVals(hashmap, "user:1000")
count = XArray.XSize(values)

i = 0
WhileLoop LessThan(i, count) {
    value = XArray.XGet(values, i)
    PrintString(value)
    i = Add(i, 1)
}

XArray.XDestroy(values)
```

---

### HashMap.HLen

Get the number of fields in a hash.

**Signature:**
```ailang
Function.HashMap.HLen {
    Input: hashmap: Address
    Input: key: Address
    Output: Integer
}
```

**Example:**
```ailang
field_count = HashMap.HLen(hashmap, "user:1000")
PrintMessage("User has ")
PrintNumber(field_count)
PrintMessage(" fields")
```

---

### HashMap.HMSet

Set multiple fields at once.

**Signature:**
```ailang
Function.HashMap.HMSet {
    Input: hashmap: Address
    Input: key: Address
    Input: fields_values: Address  // XArray of alternating field/value
    Output: Integer  // Number of new fields created
}
```

**Example:**
```ailang
pairs = XArray.XCreate(10)
XArray.XPush(pairs, "name")
XArray.XPush(pairs, "Alice")
XArray.XPush(pairs, "email")
XArray.XPush(pairs, "alice@example.com")
XArray.XPush(pairs, "age")
XArray.XPush(pairs, "30")

new_fields = HashMap.HMSet(hashmap, "user:1000", pairs)
PrintMessage("Created ")
PrintNumber(new_fields)
PrintMessage(" new fields")

XArray.XDestroy(pairs)
```

---

### HashMap.HMGet

Get multiple fields at once.

**Signature:**
```ailang
Function.HashMap.HMGet {
    Input: hashmap: Address
    Input: key: Address
    Input: fields: Address  // XArray of field names
    Output: Address  // XArray of values (0 for non-existent)
}
```

**Example:**
```ailang
fields = XArray.XCreate(3)
XArray.XPush(fields, "name")
XArray.XPush(fields, "email")
XArray.XPush(fields, "phone")  // Doesn't exist

values = HashMap.HMGet(hashmap, "user:1000", fields)

name = XArray.XGet(values, 0)    // "Alice"
email = XArray.XGet(values, 1)   // "alice@example.com"
phone = XArray.XGet(values, 2)   // 0 (not found)

XArray.XDestroy(fields)
XArray.XDestroy(values)
```

---

### HashMap.HIncrBy

Increment a numeric field.

**Signature:**
```ailang
Function.HashMap.HIncrBy {
    Input: hashmap: Address
    Input: key: Address
    Input: field: Address
    Input: increment: Integer
    Output: Integer  // New value after increment
}
```

**Example:**
```ailang
// Initialize counter
HashMap.HSet(hashmap, "stats", "page_views", "0")

// Increment
new_value = HashMap.HIncrBy(hashmap, "stats", "page_views", 1)
// new_value = 1

new_value = HashMap.HIncrBy(hashmap, "stats", "page_views", 5)
// new_value = 6

// Decrement with negative increment
new_value = HashMap.HIncrBy(hashmap, "stats", "page_views", -2)
// new_value = 4
```

**Behavior:**
- Treats non-existent fields as 0
- Parses current value as integer
- Stores result as string

---

### HashMap.Destroy

Destroy the entire hash-of-hashes and all contents.

**Signature:**
```ailang
Function.HashMap.Destroy {
    Input: hashmap: Address
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
hashmap = HashMap.Create()
// ... use hashmap ...
HashMap.Destroy(hashmap)
```

**Memory:** Frees all inner hashes, all values, all keys, and the main structure

---

## Simple Hash Tables

Direct operations on a single hash instance (not a hash-of-hashes).

### HashMap.CreateSimple

Create a standalone hash table.

**Signature:**
```ailang
Function.HashMap.CreateSimple {
    Output: Address
}
```

**Example:**
```ailang
hash = HashMap.CreateSimple()
```

**Default:** Creates hash with 16 buckets

---

### HashMap.HSetSimple

Set a field-value pair.

**Signature:**
```ailang
Function.HashMap.HSetSimple {
    Input: hash_ptr: Address
    Input: field: Address
    Input: value: Address
    Output: Integer  // 1 if new, 0 if updated
}
```

**Example:**
```ailang
hash = HashMap.CreateSimple()

HashMap.HSetSimple(hash, "name", "Alice")
HashMap.HSetSimple(hash, "email", "alice@example.com")
HashMap.HSetSimple(hash, "age", "30")

// Update
HashMap.HSetSimple(hash, "age", "31")  // Returns 0
```

---

### HashMap.HGetSimple

Get a field value.

**Signature:**
```ailang
Function.HashMap.HGetSimple {
    Input: hash_ptr: Address
    Input: field: Address
    Output: Address  // Returns 0 if not found
}
```

**Example:**
```ailang
name = HashMap.HGetSimple(hash, "name")
phone = HashMap.HGetSimple(hash, "phone")  // Returns 0
```

---

### HashMap.HDelSimple

Delete a field.

**Signature:**
```ailang
Function.HashMap.HDelSimple {
    Input: hash_ptr: Address
    Input: field: Address
    Output: Integer  // 1 if deleted, 0 if not found
}
```

---

### HashMap.HExistsSimple

Check if field exists.

**Signature:**
```ailang
Function.HashMap.HExistsSimple {
    Input: hash_ptr: Address
    Input: field: Address
    Output: Integer  // 1 if exists, 0 otherwise
}
```

---

### HashMap.HLenSimple

Get number of fields.

**Signature:**
```ailang
Function.HashMap.HLenSimple {
    Input: hash_ptr: Address
    Output: Integer
}
```

**Example:**
```ailang
count = HashMap.HLenSimple(hash)
PrintMessage("Hash has ")
PrintNumber(count)
PrintMessage(" fields")
```

---

### HashMap.HIncrBySimple

Increment a numeric field.

**Signature:**
```ailang
Function.HashMap.HIncrBySimple {
    Input: hash_ptr: Address
    Input: field: Address
    Input: increment: Integer
    Output: Integer  // New value
}
```

**Example:**
```ailang
HashMap.HSetSimple(hash, "counter", "0")
new_val = HashMap.HIncrBySimple(hash, "counter", 5)  // Returns 5
new_val = HashMap.HIncrBySimple(hash, "counter", 3)  // Returns 8
```

---

### HashMap.HKeysSimple

Get all field names.

**Signature:**
```ailang
Function.HashMap.HKeysSimple {
    Input: hash_ptr: Address
    Output: Address  // XArray of field names
}
```

**Example:**
```ailang
keys = HashMap.HKeysSimple(hash)
count = XArray.XSize(keys)

i = 0
WhileLoop LessThan(i, count) {
    key = XArray.XGet(keys, i)
    PrintString(key)
    i = Add(i, 1)
}

XArray.XDestroy(keys)
```

---

### HashMap.HValsSimple

Get all values.

**Signature:**
```ailang
Function.HashMap.HValsSimple {
    Input: hash_ptr: Address
    Output: Address  // XArray of values
}
```

---

### HashMap.HGetAllSimple

Get all field-value pairs.

**Signature:**
```ailang
Function.HashMap.HGetAllSimple {
    Input: hash_ptr: Address
    Output: Address  // XArray of alternating field/value
}
```

**Example:**
```ailang
pairs = HashMap.HGetAllSimple(hash)
count = XArray.XSize(pairs)

i = 0
WhileLoop LessThan(i, count) {
    field = XArray.XGet(pairs, i)
    value = XArray.XGet(pairs, Add(i, 1))
    
    PrintString(field)
    PrintMessage(" = ")
    PrintString(value)
    
    i = Add(i, 2)
}

XArray.XDestroy(pairs)
```

---

### HashMap.DestroySimple

Destroy a simple hash table.

**Signature:**
```ailang
Function.HashMap.DestroySimple {
    Input: hash_ptr: Address
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
hash = HashMap.CreateSimple()
// ... use hash ...
HashMap.DestroySimple(hash)
```

**Memory:** Frees all keys, values, and the hash structure

---

## Advanced Implementations

### Open Addressing Hash (Robin Hood Hashing)

Cache-friendly hash table using open addressing with Robin Hood collision resolution.

#### HashMap.CreateOpen

**Signature:**
```ailang
Function.HashMap.CreateOpen {
    Input: capacity: Integer
    Output: Address
}
```

**Structure (40 bytes):**
```
[0-7]:   capacity
[8-15]:  size
[16-23]: keys array
[24-31]: values array
[32-39]: deleted_flags array
```

---

#### HashMap.InsertRobinHood

Insert using Robin Hood hashing.

**Signature:**
```ailang
Function.HashMap.InsertRobinHood {
    Input: table: Address
    Input: key: Address
    Input: value: Address
    Output: Integer  // 1 if new, 0 if updated
}
```

**Features:**
- Auto-resizes at 75% load factor
- Robin Hood: swaps entries to reduce max probe distance
- Better cache performance than chaining

**Example:**
```ailang
table = HashMap.CreateOpen(16)
HashMap.InsertRobinHood(table, "key1", "value1")
HashMap.InsertRobinHood(table, "key2", "value2")
// Auto-resizes to 32 when 75% full
```

---

#### HashMap.BatchInsert

Batch insert for better cache performance.

**Signature:**
```ailang
Function.HashMap.BatchInsert {
    Input: table: Address
    Input: keys: Address    // XArray of keys
    Input: values: Address  // XArray of values
    Output: Integer  // Number inserted
}
```

---

### Concurrent Hash Table

Thread-safe hash table using striped locking.

#### HashMap.CreateConcurrent

**Signature:**
```ailang
Function.HashMap.CreateConcurrent {
    Input: num_stripes: Integer
    Output: Address
}
```

**Structure (24 bytes):**
```
[0-7]:   num_stripes
[8-15]:  tables_array (one per stripe)
[16-23]: locks_array (one per stripe)
```

**Example:**
```ailang
// Create with 16 stripes (locks)
concurrent = HashMap.CreateConcurrent(16)
```

---

#### HashMap.ConcurrentPut

Thread-safe insert.

**Signature:**
```ailang
Function.HashMap.ConcurrentPut {
    Input: concurrent_map: Address
    Input: key: Address
    Input: value: Address
    Output: Integer
}
```

**Features:**
- Automatically selects stripe based on key hash
- Acquires stripe lock
- Only blocks on same stripe, not entire map

---

#### HashMap.DestroyConcurrent

**Signature:**
```ailang
Function.HashMap.DestroyConcurrent {
    Input: concurrent_map: Address
    Output: Integer  // Returns 1
}
```

---

### LRU Cache

Hash table with automatic eviction of least-recently-used entries.

#### HashMap.CreateLRU

**Signature:**
```ailang
Function.HashMap.CreateLRU {
    Input: capacity: Integer
    Output: Address
}
```

**Structure (40 bytes):**
```
[0-7]:   capacity
[8-15]:  size
[16-23]: map (backing hash)
[24-31]: head (sentinel node)
[32-39]: tail (sentinel node)
```

**Example:**
```ailang
// Create LRU cache with capacity 100
cache = HashMap.CreateLRU(100)
```

---

#### HashMap.LRUPut

Insert/update with LRU tracking.

**Signature:**
```ailang
Function.HashMap.LRUPut {
    Input: lru: Address
    Input: key: Address
    Input: value: Address
    Output: Integer  // 1 if new, 0 if updated
}
```

**Behavior:**
- Adds new entry at front
- Moves accessed entries to front
- Evicts least-recently-used when capacity exceeded

**Example:**
```ailang
cache = HashMap.CreateLRU(3)

HashMap.LRUPut(cache, "a", "value_a")
HashMap.LRUPut(cache, "b", "value_b")
HashMap.LRUPut(cache, "c", "value_c")
// Cache: [c, b, a] (c is most recent)

HashMap.LRUPut(cache, "d", "value_d")
// Evicts "a", Cache: [d, c, b]
```

---

#### HashMap.LRUGet

Get value with LRU update.

**Signature:**
```ailang
Function.HashMap.LRUGet {
    Input: lru: Address
    Input: key: Address
    Output: Address  // Returns 0 if not found
}
```

**Behavior:**
- Moves accessed entry to front (marks as most recent)

**Example:**
```ailang
HashMap.LRUGet(cache, "b")
// Cache order: [b, d, c] (b moved to front)
```

---

#### HashMap.DestroyLRU

**Signature:**
```ailang
Function.HashMap.DestroyLRU {
    Input: lru: Address
    Output: Integer  // Returns 1
}
```

**Memory:** Frees all nodes, sentinel nodes, backing map, and LRU structure

---

## Memory Management

### Allocation Summary

| Structure | Size | Notes |
|-----------|------|-------|
| Nested Hash (main) | 24 bytes | XSHash structure |
| Nested Hash (inner) | 24 bytes | One per key |
| Simple Hash | 16 + (capacity × 24) | Header + entries |
| Open Addressing | 40 bytes | + 3 arrays |
| Concurrent Hash | 24 bytes | + stripes × (hash + lock) |
| LRU Cache | 40 bytes | + nodes + hash |
| LRU Node | 32 bytes | key, value, prev, next |

### Cleanup Guidelines

**Nested Hash:**
```ailang
hashmap = HashMap.Create()
// ... use ...
HashMap.Destroy(hashmap)  // Frees everything
```

**Simple Hash:**
```ailang
hash = HashMap.CreateSimple()
// ... use ...
HashMap.DestroySimple(hash)  // Frees everything
```

**LRU Cache:**
```ailang
cache = HashMap.CreateLRU(100)
// ... use ...
HashMap.DestroyLRU(cache)  // Frees all nodes and values
```

**Concurrent Hash:**
```ailang
concurrent = HashMap.CreateConcurrent(16)
// ... use ...
HashMap.DestroyConcurrent(concurrent)  // Frees all stripes
```

---

## Usage Examples

### Example 1: User Session Store

```ailang
Function.CreateSessionStore {
    Output: Address
    Body: {
        store = HashMap.Create()
        ReturnValue(store)
    }
}

Function.CreateSession {
    Input: store: Address
    Input: user_id: Address
    Input: token: Address
    Body: {
        session_id = StringConcat("session:", token)
        
        HashMap.HSet(store, session_id, "user_id", user_id)
        HashMap.HSet(store, session_id, "created_at", GetTimestamp())
        HashMap.HSet(store, session_id, "last_accessed", GetTimestamp())
    }
}

Function.GetSession {
    Input: store: Address
    Input: token: Address
    Output: Address
    Body: {
        session_id = StringConcat("session:", token)
        
        // Check if exists
        exists = HashMap.HExists(store, session_id, "user_id")
        IfCondition NotEqual(exists, 1) ThenBlock: {
            ReturnValue(0)
        }
        
        // Update last access
        HashMap.HSet(store, session_id, "last_accessed", GetTimestamp())
        
        // Return user ID
        user_id = HashMap.HGet(store, session_id, "user_id")
        ReturnValue(user_id)
    }
}

// Usage:
store = CreateSessionStore()
CreateSession(store, "user_12345", "abc123token")
user_id = GetSession(store, "abc123token")
```

### Example 2: Page View Counter with LRU

```ailang
Function.CreatePageViewCache {
    Output: Address
    Body: {
        // Cache last 1000 page views
        cache = HashMap.CreateLRU(1000)
        ReturnValue(cache)
    }
}

Function.RecordPageView {
    Input: cache: Address
    Input: page_url: Address
    Body: {
        // Get current count
        count_str = HashMap.LRUGet(cache, page_url)
        
        count = 0
        IfCondition NotEqual(count_str, 0) ThenBlock: {
            count = StringToNumber(count_str)
        }
        
        // Increment
        count = Add(count, 1)
        count_str = NumberToString(count)
        
        // Store back
        HashMap.LRUPut(cache, page_url, count_str)
    }
}

Function.GetTopPages {
    Input: cache: Address
    Output: Address
    Body: {
        // Get all pages and their counts
        // (In real implementation, would sort)
        
        // For now, just return all as array
        // Would need sorting implementation
        
        ReturnValue(0)  // Placeholder
    }
}
```

### Example 3: Configuration Manager

```ailang
Function.LoadConfig {
    Input: filename: Address
    Output: Address
    Body: {
        config = HashMap.CreateSimple()
        
        // Read file
        content = ReadTextFile(filename)
        lines = StringSplit(content, "\n")
        line_count = XArray.XSize(lines)
        
        i = 0
        WhileLoop LessThan(i, line_count) {
            line = XArray.XGet(lines, i)
            line = StringTrim(line)
            
            // Skip comments and empty lines
            IfCondition GreaterThan(StringLength(line), 0) ThenBlock: {
                first_char = StringCharAt(line, 0)
                
                // Skip comments (# = 35)
                IfCondition NotEqual(first_char, 35) ThenBlock: {
                    // Parse key=value
                    eq_pos = StringIndexOf(line, "=")
                    
                    IfCondition GreaterThan(eq_pos, 0) ThenBlock: {
                        key = StringSubstring(line, 0, eq_pos)
                        key = StringTrim(key)
                        
                        value_start = Add(eq_pos, 1)
                        line_len = StringLength(line)
                        value = StringSubstring(line, value_start, line_len)
                        value = StringTrim(value)
                        
                        HashMap.HSetSimple(config, key, value)
                    }
                }
            }
            
            i = Add(i, 1)
        }
        
        // Cleanup
        XArray.XDestroy(lines)
        
        ReturnValue(config)
    }
}

Function.GetConfigValue {
    Input: config: Address
    Input: key: Address
    Input: default: Address
    Output: Address
    Body: {
        value = HashMap.HGetSimple(config, key)
        
        IfCondition EqualTo(value, 0) ThenBlock: {
            ReturnValue(default)
        }
        
        ReturnValue(value)
    }
}

// Usage:
config = LoadConfig("settings.conf")
host = GetConfigValue(config, "host", "localhost")
port_str = GetConfigValue(config, "port", "8080")
port = StringToNumber(port_str)
```

---

## Performance Characteristics

| Implementation | Insert | Lookup | Delete | Space | Notes |
|----------------|--------|--------|--------|-------|-------|
| Nested Hash | O(1)* | O(1)* | O(1)* | O(n) | Double hash overhead |
| Simple Hash | O(1)* | O(1)* | O(1)* | O(n) | Single hash |
| Open Addressing | O(1)* | O(1)* | O(1)* | O(n) | Cache-friendly |
| Concurrent Hash | O(1)* | O(1)* | O(1)* | O(n×s) | s = stripes |
| LRU Cache | O(1) | O(1) | O(1) | O(n) | With eviction |

\* Average case with good hash function

### Load Factor Impact

| Load Factor | Nested/Simple | Open Addressing |
|-------------|---------------|-----------------|
| < 50% | Excellent | Excellent |
| 50-70% | Good | Good |
| 70-90% | Fair | Degrading |
| > 90% | Poor | Very poor |

**Recommendation:** Size hash tables to keep load factor < 70%

---

## Best Practices

1. **Choose the right implementation:**
   ```ailang
   // Redis-style multi-hash → Nested
   store = HashMap.Create()
   
   // Single hash table → Simple
   hash = HashMap.CreateSimple()
   
   // High performance → Open Addressing
   table = HashMap.CreateOpen(capacity)
   
   // Limited memory → LRU Cache
   cache = HashMap.CreateLRU(max_items)
   ```

2. **Size appropriately:**
   ```ailang
   // Good: Size for expected load
   hash = HashMap.CreateOpen(256)  // For ~180 items (70% load)
   
   // Avoid: Too small
   hash = HashMap.CreateOpen(10)   // Will resize frequently
   ```

3. **Always clean up:**
   ```ailang
   hashmap = HashMap.Create()
   // ... use ...
   HashMap.Destroy(hashmap)  // Don't forget!
   ```

4. **Check for existence before access:**
   ```ailang
   IfCondition HashMap.HExists(map, key, field) ThenBlock: {
       value = HashMap.HGet(map, key, field)
       ProcessValue(value)
   }
   ```

5. **Use batch operations when possible:**
   ```ailang
   // Better: Single call
   pairs = CreatePairs(...)
   HashMap.HMSet(map, key, pairs)
   
   // Avoid: Multiple calls
   HashMap.HSet(map, key, "f1", "v1")
   HashMap.HSet(map, key, "f2", "v2")
   HashMap.HSet(map, key, "f3", "v3")
   ```

---

## Version History

### Version 1.0 (Current)
- Nested hash tables (Redis-compatible)
- Simple hash tables
- Open addressing with Robin Hood hashing
- Concurrent hash with striped locking
- LRU cache implementation
- Helper functions for string/int conversion

---

## See Also

- **Library.XArrays Manual**: Underlying data structures
- **Library.StringUtils Manual**: String utilities
- **AILANG Core Language Manual**: Built-in primitives
- **Redis Documentation**: Command reference for HSET, HGET, etc.

---

## License

Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering  
Licensed under the Sean Collins Software License (SCSL)