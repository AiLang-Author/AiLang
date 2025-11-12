# Library.XArrays Reference Manual

**Version:** 1.0  
**Module:** `Library.XArrays.ailang`  
**Type:** Pure AILANG Implementation  
**Dependencies:** None

---

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Constants and Configuration](#constants-and-configuration)
4. [Dynamic Arrays (XArray)](#dynamic-arrays-xarray)
5. [Stacks (XStack)](#stacks-xstack)
6. [Queues (XQueue)](#queues-xqueue)
7. [Hash Tables (XHash, XSHash)](#hash-tables-xhash-xshash)
8. [Linked Lists (XList)](#linked-lists-xlist)
9. [Utility Functions (XUtil)](#utility-functions-xutil)
10. [Memory Management](#memory-management)
11. [Usage Examples](#usage-examples)
12. [Performance Characteristics](#performance-characteristics)

---

## Overview

`Library.XArrays` provides a comprehensive collection of data structures for AILANG, including:

- **Dynamic Arrays**: Auto-resizing arrays with push/pop operations
- **Stacks**: LIFO data structure built on dynamic arrays
- **Queues**: FIFO circular queue implementation
- **Hash Tables**: Both integer and string-keyed hash tables
- **Linked Lists**: Singly-linked lists with head/tail pointers
- **Utilities**: Sorting, searching, and array manipulation functions

### Why X-Prefix?

All functions use an "X" prefix (`XArray`, `XStack`, etc.) to avoid conflicts with AILANG's built-in keywords and future language features.

### Design Philosophy

- **Self-Contained**: No external dependencies
- **Memory Explicit**: All allocations and deallocations are clearly documented
- **Type Flexible**: Uses `Integer`/`Address` types interchangeably for flexibility
- **Performance Conscious**: Efficient implementations with amortized O(1) operations where possible

---

## Installation

```ailang
LibraryImport.XArrays

// Now all XArrays functions are available
arr = XArray.XCreate(10)
XArray.XPush(arr, 42)
```

---

## Constants and Configuration

The library defines several constants in the `XArrays` fixed pool:

```ailang
XArrays.XMAX_ARRAY_SIZE  = 65536    // Maximum array size
XArrays.XMAX_HASH_BUCKETS = 127     // Maximum hash table buckets
XArrays.XSTACK_SIZE      = 1024     // Default stack size
XArrays.XQUEUE_SIZE      = 1024     // Default queue size
XArrays.XNULL            = -1       // Null/not found indicator
XArrays.XERROR           = -999999  // Error value
```

**Usage:**
```ailang
result = XArray.XGet(arr, 5)
IfCondition EqualTo(result, XArrays.XERROR) ThenBlock: {
    PrintMessage("Index out of bounds!")
}
```

---

## Dynamic Arrays (XArray)

Dynamic arrays automatically resize when capacity is exceeded.

### Structure

**Memory Layout (24 bytes):**
```
[0-7]:   capacity (Integer)
[8-15]:  size (Integer)
[16-23]: data_ptr (Address to underlying array)
```

### XArray.XCreate

Create a new dynamic array.

**Signature:**
```ailang
Function.XArray.XCreate {
    Input: initial_capacity: Integer
    Output: Address
}
```

**Example:**
```ailang
// Create array with capacity for 10 elements
arr = XArray.XCreate(10)

// Create small array
small = XArray.XCreate(2)
```

**Memory:** Allocates 24 bytes for structure + array of `initial_capacity` elements

---

### XArray.XPush

Add an element to the end of the array. Auto-resizes if needed.

**Signature:**
```ailang
Function.XArray.XPush {
    Input: arr: Address
    Input: value: Integer
    Output: Integer  // Returns 1 on success
}
```

**Example:**
```ailang
arr = XArray.XCreate(2)
XArray.XPush(arr, 10)  // size = 1
XArray.XPush(arr, 20)  // size = 2
XArray.XPush(arr, 30)  // size = 3, auto-resizes to capacity 4
XArray.XPush(arr, 40)  // size = 4
XArray.XPush(arr, 50)  // size = 5, auto-resizes to capacity 8
```

**Performance:** 
- Amortized O(1) - occasional O(n) when resizing
- Doubles capacity when full

---

### XArray.XPop

Remove and return the last element.

**Signature:**
```ailang
Function.XArray.XPop {
    Input: arr: Address
    Output: Integer  // Returns XArrays.XERROR if empty
}
```

**Example:**
```ailang
arr = XArray.XCreate(5)
XArray.XPush(arr, 100)
XArray.XPush(arr, 200)
XArray.XPush(arr, 300)

value = XArray.XPop(arr)  // Returns 300, size = 2
value = XArray.XPop(arr)  // Returns 200, size = 1
value = XArray.XPop(arr)  // Returns 100, size = 0
value = XArray.XPop(arr)  // Returns XArrays.XERROR (empty)
```

**Performance:** O(1)

---

### XArray.XGet

Get element at specific index.

**Signature:**
```ailang
Function.XArray.XGet {
    Input: arr: Address
    Input: index: Integer
    Output: Integer  // Returns XArrays.XERROR if out of bounds
}
```

**Example:**
```ailang
arr = XArray.XCreate(5)
XArray.XPush(arr, 10)
XArray.XPush(arr, 20)
XArray.XPush(arr, 30)

first = XArray.XGet(arr, 0)   // Returns 10
second = XArray.XGet(arr, 1)  // Returns 20
invalid = XArray.XGet(arr, 5) // Returns XArrays.XERROR
```

**Performance:** O(1)

---

### XArray.XSet

Set element at specific index.

**Signature:**
```ailang
Function.XArray.XSet {
    Input: arr: Address
    Input: index: Integer
    Input: value: Integer
    Output: Integer  // Returns 1 on success, 0 if out of bounds
}
```

**Example:**
```ailang
arr = XArray.XCreate(5)
XArray.XPush(arr, 10)
XArray.XPush(arr, 20)
XArray.XPush(arr, 30)

XArray.XSet(arr, 1, 999)  // Changes [10, 20, 30] to [10, 999, 30]

// Can't set beyond current size
result = XArray.XSet(arr, 10, 50)  // Returns 0 (failed)
```

**Performance:** O(1)

---

### XArray.XSize

Get the current number of elements.

**Signature:**
```ailang
Function.XArray.XSize {
    Input: arr: Address
    Output: Integer
}
```

**Example:**
```ailang
arr = XArray.XCreate(10)
size1 = XArray.XSize(arr)  // Returns 0

XArray.XPush(arr, 42)
size2 = XArray.XSize(arr)  // Returns 1
```

**Performance:** O(1)

---

### XArray.XCapacity

Get the current capacity (before next resize).

**Signature:**
```ailang
Function.XArray.XCapacity {
    Input: arr: Address
    Output: Integer
}
```

**Example:**
```ailang
arr = XArray.XCreate(4)
cap1 = XArray.XCapacity(arr)  // Returns 4

XArray.XPush(arr, 1)
XArray.XPush(arr, 2)
XArray.XPush(arr, 3)
XArray.XPush(arr, 4)
XArray.XPush(arr, 5)  // Triggers resize

cap2 = XArray.XCapacity(arr)  // Returns 8 (doubled)
```

**Performance:** O(1)

---

### XArray.XClear

Remove all elements (keeps capacity).

**Signature:**
```ailang
Function.XArray.XClear {
    Input: arr: Address
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
arr = XArray.XCreate(10)
XArray.XPush(arr, 1)
XArray.XPush(arr, 2)
XArray.XPush(arr, 3)

size_before = XArray.XSize(arr)     // Returns 3
XArray.XClear(arr)
size_after = XArray.XSize(arr)      // Returns 0
cap = XArray.XCapacity(arr)         // Still 10
```

**Performance:** O(1)

---

### XArray.XShift

Remove and return the first element, shifting all others down.

**Signature:**
```ailang
Function.XArray.XShift {
    Input: array: Address
    Output: Address  // Returns removed element or 0 if empty
}
```

**Example:**
```ailang
arr = XArray.XCreate(5)
XArray.XPush(arr, 10)
XArray.XPush(arr, 20)
XArray.XPush(arr, 30)

first = XArray.XShift(arr)  // Returns 10
// Array now contains [20, 30]

second = XArray.XShift(arr)  // Returns 20
// Array now contains [30]
```

**Performance:** O(n) - must shift all remaining elements

---

### XArray.XDestroy

Free the dynamic array and its underlying data.

**Signature:**
```ailang
Function.XArray.XDestroy {
    Input: arr: Address
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
arr = XArray.XCreate(10)
// ... use array ...
XArray.XDestroy(arr)
```

**Memory:** Frees 24 bytes for structure + underlying array

---

## Stacks (XStack)

LIFO (Last-In-First-Out) data structure built on top of XArray.

### XStack.XCreate

**Signature:**
```ailang
Function.XStack.XCreate {
    Input: capacity: Integer
    Output: Address
}
```

**Example:**
```ailang
stack = XStack.XCreate(100)
```

---

### XStack.XPush

Push value onto stack.

**Signature:**
```ailang
Function.XStack.XPush {
    Input: stack: Address
    Input: value: Integer
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
stack = XStack.XCreate(10)
XStack.XPush(stack, 10)
XStack.XPush(stack, 20)
XStack.XPush(stack, 30)
// Stack: [10, 20, 30] (30 is top)
```

---

### XStack.XPop

Pop value from stack.

**Signature:**
```ailang
Function.XStack.XPop {
    Input: stack: Address
    Output: Integer  // Returns XArrays.XERROR if empty
}
```

**Example:**
```ailang
value1 = XStack.XPop(stack)  // Returns 30
value2 = XStack.XPop(stack)  // Returns 20
value3 = XStack.XPop(stack)  // Returns 10
value4 = XStack.XPop(stack)  // Returns XArrays.XERROR
```

---

### XStack.XPeek

View top element without removing it.

**Signature:**
```ailang
Function.XStack.XPeek {
    Input: stack: Address
    Output: Integer  // Returns XArrays.XERROR if empty
}
```

**Example:**
```ailang
XStack.XPush(stack, 100)
top = XStack.XPeek(stack)    // Returns 100
size = XArray.XSize(stack)   // Still 1 (not removed)
```

---

### XStack.XIsEmpty

Check if stack is empty.

**Signature:**
```ailang
Function.XStack.XIsEmpty {
    Input: stack: Address
    Output: Integer  // Returns 1 if empty, 0 otherwise
}
```

**Example:**
```ailang
stack = XStack.XCreate(10)
empty1 = XStack.XIsEmpty(stack)  // Returns 1

XStack.XPush(stack, 42)
empty2 = XStack.XIsEmpty(stack)  // Returns 0
```

---

## Queues (XQueue)

FIFO (First-In-First-Out) circular queue implementation.

### Structure

**Memory Layout (40 bytes):**
```
[0-7]:   capacity
[8-15]:  size
[16-23]: head (read position)
[24-31]: tail (write position)
[32-39]: data_ptr
```

### XQueue.XCreate

**Signature:**
```ailang
Function.XQueue.XCreate {
    Input: capacity: Integer
    Output: Address
}
```

**Example:**
```ailang
queue = XQueue.XCreate(100)
```

---

### XQueue.XEnqueue

Add element to back of queue.

**Signature:**
```ailang
Function.XQueue.XEnqueue {
    Input: queue: Address
    Input: value: Integer
    Output: Integer  // Returns 1 on success, 0 if full
}
```

**Example:**
```ailang
queue = XQueue.XCreate(3)
XQueue.XEnqueue(queue, 10)  // [10]
XQueue.XEnqueue(queue, 20)  // [10, 20]
XQueue.XEnqueue(queue, 30)  // [10, 20, 30]
result = XQueue.XEnqueue(queue, 40)  // Returns 0 (full)
```

**Note:** Unlike XArray, queues don't auto-resize

---

### XQueue.XDequeue

Remove and return element from front of queue.

**Signature:**
```ailang
Function.XQueue.XDequeue {
    Input: queue: Address
    Output: Integer  // Returns XArrays.XERROR if empty
}
```

**Example:**
```ailang
first = XQueue.XDequeue(queue)   // Returns 10
second = XQueue.XDequeue(queue)  // Returns 20
third = XQueue.XDequeue(queue)   // Returns 30
fourth = XQueue.XDequeue(queue)  // Returns XArrays.XERROR
```

---

## Hash Tables (XHash, XSHash)

Two hash table implementations:
- **XHash**: Integer keys → Integer values
- **XSHash**: String keys → Integer/Address values (most commonly used)

### XSHash.XCreate

Create string-keyed hash table.

**Signature:**
```ailang
Function.XSHash.XCreate {
    Input: bucket_count: Integer
    Output: Address
}
```

**Example:**
```ailang
// Create hash with 16 buckets
map = XSHash.XCreate(16)

// Create large hash with 256 buckets
large_map = XSHash.XCreate(256)
```

**Memory Layout (24 bytes):**
```
[0-7]:   bucket_count
[8-15]:  item_count
[16-23]: buckets_ptr (array of linked list heads)
```

---

### XSHash.XInsert

Insert or update key-value pair.

**Signature:**
```ailang
Function.XSHash.XInsert {
    Input: hash_table: Address
    Input: key: Address
    Input: value: Integer
    Output: Integer  // Returns 1 if new, 0 if updated
}
```

**Example:**
```ailang
map = XSHash.XCreate(16)

// Insert new keys
XSHash.XInsert(map, "name", "Alice")      // Returns 1 (new)
XSHash.XInsert(map, "age", "30")          // Returns 1 (new)
XSHash.XInsert(map, "city", "New York")   // Returns 1 (new)

// Update existing key
XSHash.XInsert(map, "age", "31")          // Returns 0 (updated)
```

**Collision Handling:** Uses chaining with linked lists

---

### XSHash.XLookup

Look up value by key.

**Signature:**
```ailang
Function.XSHash.XLookup {
    Input: hash_table: Address
    Input: key: Address
    Output: Integer  // Returns XArrays.XNULL if not found
}
```

**Example:**
```ailang
name = XSHash.XLookup(map, "name")
IfCondition EqualTo(name, XArrays.XNULL) ThenBlock: {
    PrintMessage("Name not found")
} ElseBlock: {
    PrintString(name)
}
```

---

### XSHash.XExists

Check if key exists.

**Signature:**
```ailang
Function.XSHash.XExists {
    Input: hash_table: Address
    Input: key: Address
    Output: Integer  // Returns 1 if exists, 0 otherwise
}
```

**Example:**
```ailang
has_name = XSHash.XExists(map, "name")    // Returns 1
has_email = XSHash.XExists(map, "email")  // Returns 0
```

---

### XSHash.XDelete

Delete key-value pair.

**Signature:**
```ailang
Function.XSHash.XDelete {
    Input: hash_table: Address
    Input: key: Address
    Output: Integer  // Returns value or XArrays.XNULL if not found
}
```

**Example:**
```ailang
old_value = XSHash.XDelete(map, "age")
IfCondition NotEqual(old_value, XArrays.XNULL) ThenBlock: {
    PrintMessage("Deleted age: ")
    PrintString(old_value)
}
```

**Memory:** Automatically frees the key string and node

---

### XSHash.XKeys

Get all keys as an XArray.

**Signature:**
```ailang
Function.XSHash.XKeys {
    Input: hash_table: Address
    Output: Address  // XArray of string copies
}
```

**Example:**
```ailang
keys = XSHash.XKeys(map)
key_count = XArray.XSize(keys)

i = 0
WhileLoop LessThan(i, key_count) {
    key = XArray.XGet(keys, i)
    PrintString(key)
    
    // Must free the key copy
    Deallocate(key, Add(StringLength(key), 1))
    i = Add(i, 1)
}

XArray.XDestroy(keys)
```

⚠️ **Memory:** Returns copies of keys that must be freed!

---

### XSHash.XDestroy

Destroy hash table and all its contents.

**Signature:**
```ailang
Function.XSHash.XDestroy {
    Input: hash_table: Address
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
map = XSHash.XCreate(16)
// ... use map ...
XSHash.XDestroy(map)
```

**Memory:** Frees all keys, nodes, buckets array, and table structure

---

### Address-Value Variants

For storing addresses (pointers) instead of integers:

**XSHash.XInsertAddr** - Insert key → address  
**XSHash.XLookupAddr** - Lookup returns address  
**XSHash.XDeleteAddr** - Delete returns address  
**XSHash.XExistsAddr** - Check existence

**Example:**
```ailang
// Store pointers to other data structures
users = XSHash.XCreate(32)

user1_data = Allocate(100)
user2_data = Allocate(100)

XSHash.XInsertAddr(users, "user:1000", user1_data)
XSHash.XInsertAddr(users, "user:2000", user2_data)

data = XSHash.XLookupAddr(users, "user:1000")
```

---

## Linked Lists (XList)

Singly-linked list with head/tail pointers.

### Structure

**List Structure (24 bytes):**
```
[0-7]:   head (Address)
[8-15]:  tail (Address)
[16-23]: size (Integer)
```

**Node Structure (16 bytes):**
```
[0-7]:  value (Integer/Address)
[8-15]: next (Address)
```

### XList.XCreate

**Signature:**
```ailang
Function.XList.XCreate {
    Output: Address
}
```

**Example:**
```ailang
list = XList.XCreate()
```

---

### XList.XAppend

Add element to end of list.

**Signature:**
```ailang
Function.XList.XAppend {
    Input: list: Address
    Input: value: Integer
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
list = XList.XCreate()
XList.XAppend(list, 10)  // [10]
XList.XAppend(list, 20)  // [10, 20]
XList.XAppend(list, 30)  // [10, 20, 30]
```

**Performance:** O(1)

---

### XList.XPrepend

Add element to beginning of list.

**Signature:**
```ailang
Function.XList.XPrepend {
    Input: list: Address
    Input: value: Integer
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
list = XList.XCreate()
XList.XPrepend(list, 10)  // [10]
XList.XPrepend(list, 20)  // [20, 10]
XList.XPrepend(list, 30)  // [30, 20, 10]
```

**Performance:** O(1)

---

### XList.XPopFront

Remove and return first element.

**Signature:**
```ailang
Function.XList.XPopFront {
    Input: list: Address
    Output: Integer  // Returns 0 if empty
}
```

**Example:**
```ailang
value = XList.XPopFront(list)  // Returns 30
// List now: [20, 10]
```

**Performance:** O(1)

---

### XList.XPopBack

Remove and return last element.

**Signature:**
```ailang
Function.XList.XPopBack {
    Input: list: Address
    Output: Integer  // Returns 0 if empty
}
```

**Example:**
```ailang
value = XList.XPopBack(list)  // Returns 10
// List now: [20]
```

**Performance:** O(n) - must traverse to find second-to-last node

---

### XList.XSize

Get number of elements.

**Signature:**
```ailang
Function.XList.XSize {
    Input: list: Address
    Output: Integer
}
```

---

### XList.XDestroy

Destroy list and free all nodes.

**Signature:**
```ailang
Function.XList.XDestroy {
    Input: list: Address
    Output: Integer  // Returns 1
}
```

**Note:** Assumes values are pointers and attempts to free them if > 1000

---

### XList.XDestroyDeep

Destroy list and free all string values.

**Signature:**
```ailang
Function.XList.XDestroyDeep {
    Input: list: Address
    Output: Integer  // Returns 1
}
```

**Use for:** Lists where every value is a string that needs freeing

---

## Utility Functions (XUtil)

### XUtil.XBinarySearch

Binary search in sorted array.

**Signature:**
```ailang
Function.XUtil.XBinarySearch {
    Input: arr: Address
    Input: target: Integer
    Output: Integer  // Returns index or XArrays.XNULL
}
```

**Example:**
```ailang
arr = XArray.XCreate(10)
XArray.XPush(arr, 10)
XArray.XPush(arr, 20)
XArray.XPush(arr, 30)
XArray.XPush(arr, 40)
XArray.XPush(arr, 50)

index = XUtil.XBinarySearch(arr, 30)  // Returns 2
not_found = XUtil.XBinarySearch(arr, 35)  // Returns XArrays.XNULL
```

**Requirements:** Array must be sorted!  
**Performance:** O(log n)

---

### XUtil.XQuickSort

Sort array in-place using quicksort.

**Signature:**
```ailang
Function.XUtil.XQuickSort {
    Input: arr: Address
    Input: low: Integer
    Input: high: Integer
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
arr = XArray.XCreate(10)
XArray.XPush(arr, 50)
XArray.XPush(arr, 20)
XArray.XPush(arr, 80)
XArray.XPush(arr, 10)
XArray.XPush(arr, 30)

size = XArray.XSize(arr)
XUtil.XQuickSort(arr, 0, Subtract(size, 1))

// Array now: [10, 20, 30, 50, 80]
```

**Performance:** O(n log n) average, O(n²) worst case

---

### XUtil.XFindMax

Find maximum value in array.

**Signature:**
```ailang
Function.XUtil.XFindMax {
    Input: arr: Address
    Output: Integer  // Returns XArrays.XERROR if empty
}
```

**Example:**
```ailang
arr = XArray.XCreate(5)
XArray.XPush(arr, 42)
XArray.XPush(arr, 17)
XArray.XPush(arr, 99)
XArray.XPush(arr, 31)

max = XUtil.XFindMax(arr)  // Returns 99
```

**Performance:** O(n)

---

### XUtil.XFindMin

Find minimum value in array.

**Signature:**
```ailang
Function.XUtil.XFindMin {
    Input: arr: Address
    Output: Integer  // Returns XArrays.XERROR if empty
}
```

**Performance:** O(n)

---

### XUtil.XReverse

Reverse array in-place.

**Signature:**
```ailang
Function.XUtil.XReverse {
    Input: arr: Address
    Output: Integer  // Returns 1
}
```

**Example:**
```ailang
arr = XArray.XCreate(5)
XArray.XPush(arr, 1)
XArray.XPush(arr, 2)
XArray.XPush(arr, 3)
XArray.XPush(arr, 4)
XArray.XPush(arr, 5)

XUtil.XReverse(arr)
// Array now: [5, 4, 3, 2, 1]
```

**Performance:** O(n)

---

## Memory Management

### Allocation Summary

| Structure | Size | Contents |
|-----------|------|----------|
| XArray | 24 bytes | capacity, size, data_ptr |
| XStack | 24 bytes | (same as XArray) |
| XQueue | 40 bytes | capacity, size, head, tail, data_ptr |
| XHash/XSHash | 24 bytes | bucket_count, item_count, buckets_ptr |
| XList | 24 bytes | head, tail, size |
| XList Node | 16 bytes | value, next |
| XHash Node | 24 bytes | key_copy, value, next |

### Cleanup Checklist

**Dynamic Arrays:**
```ailang
arr = XArray.XCreate(10)
// ... use array ...
XArray.XDestroy(arr)  // Frees 24 bytes + underlying array
```

**Hash Tables:**
```ailang
map = XSHash.XCreate(16)
// ... use map ...
XSHash.XDestroy(map)  // Frees all keys, nodes, and structure
```

**Linked Lists:**
```ailang
list = XList.XCreate()
// ... use list ...
XList.XDestroy(list)  // Frees all nodes and structure
// OR
XList.XDestroyDeep(list)  // Also frees string values
```

**XSHash.XKeys Result:**
```ailang
keys = XSHash.XKeys(map)
count = XArray.XSize(keys)

// Free each key copy
i = 0
WhileLoop LessThan(i, count) {
    key = XArray.XGet(keys, i)
    Deallocate(key, Add(StringLength(key), 1))
    i = Add(i, 1)
}

XArray.XDestroy(keys)
```

---

## Usage Examples

### Example 1: To-Do List

```ailang
Function.CreateTodoList {
    Output: Address
    Body: {
        todos = XArray.XCreate(10)
        ReturnValue(todos)
    }
}

Function.AddTodo {
    Input: todos: Address
    Input: task: Address
    Body: {
        XArray.XPush(todos, task)
    }
}

Function.PrintTodos {
    Input: todos: Address
    Body: {
        count = XArray.XSize(todos)
        PrintMessage("To-Do List:")
        
        i = 0
        WhileLoop LessThan(i, count) {
            task = XArray.XGet(todos, i)
            PrintMessage("  ")
            PrintNumber(Add(i, 1))
            PrintMessage(". ")
            PrintString(task)
            i = Add(i, 1)
        }
    }
}

// Usage:
todos = CreateTodoList()
AddTodo(todos, "Buy groceries")
AddTodo(todos, "Call dentist")
AddTodo(todos, "Finish project")
PrintTodos(todos)
XArray.XDestroy(todos)
```

### Example 2: User Database

```ailang
Function.CreateUserDB {
    Output: Address
    Body: {
        // Hash table with 128 buckets
        db = XSHash.XCreate(128)
        ReturnValue(db)
    }
}

Function.AddUser {
    Input: db: Address
    Input: username: Address
    Input: email: Address
    Body: {
        // Create user record
        user_data = XSHash.XCreate(8)
        XSHash.XInsert(user_data, "email", email)
        XSHash.XInsert(user_data, "status", "active")
        
        // Store in main database
        XSHash.XInsertAddr(db, username, user_data)
    }
}

Function.GetUserEmail {
    Input: db: Address
    Input: username: Address
    Output: Address
    Body: {
        user_data = XSHash.XLookupAddr(db, username)
        
        IfCondition EqualTo(user_data, XArrays.XNULL) ThenBlock: {
            ReturnValue(0)
        }
        
        email = XSHash.XLookup(user_data, "email")
        ReturnValue(email)
    }
}

// Usage:
db = CreateUserDB()
AddUser(db, "alice", "alice@example.com")
AddUser(db, "bob", "bob@example.com")

email = GetUserEmail(db, "alice")
PrintString(email)

XSHash.XDestroy(db)
```

### Example 3: Expression Evaluator (Stack)

```ailang
Function.EvaluatePostfix {
    Input: expr: Address
    Output: Integer
    Body: {
        stack = XStack.XCreate(100)
        tokens = StringSplit(expr, " ")
        token_count = XArray.XSize(tokens)
        
        i = 0
        WhileLoop LessThan(i, token_count) {
            token = XArray.XGet(tokens, i)
            
            // Check if operator
            is_op = Or(Or(StringEquals(token, "+"), StringEquals(token, "-")),
                      Or(StringEquals(token, "*"), StringEquals(token, "/")))
            
            IfCondition is_op ThenBlock: {
                // Pop two operands
                b = XStack.XPop(stack)
                a = XStack.XPop(stack)
                
                // Perform operation
                result = 0
                IfCondition StringEquals(token, "+") ThenBlock: {
                    result = Add(a, b)
                }
                IfCondition StringEquals(token, "-") ThenBlock: {
                    result = Subtract(a, b)
                }
                IfCondition StringEquals(token, "*") ThenBlock: {
                    result = Multiply(a, b)
                }
                IfCondition StringEquals(token, "/") ThenBlock: {
                    result = Divide(a, b)
                }
                
                XStack.XPush(stack, result)
            } ElseBlock: {
                // It's a number
                num = StringToNumber(token)
                XStack.XPush(stack, num)
            }
            
            i = Add(i, 1)
        }
        
        result = XStack.XPop(stack)
        XArray.XDestroy(stack)
        // (Should also free tokens)
        
        ReturnValue(result)
    }
}

// Usage:
result = EvaluatePostfix("3 4 + 2 *")  // (3 + 4) * 2 = 14
PrintNumber(result)
```

### Example 4: LRU Cache (Queue)

```ailang
Function.CreateLRUCache {
    Input: capacity: Integer
    Output: Address
    Body: {
        cache = XSHash.XCreate(32)
        XSHash.XInsert(cache, "_capacity", capacity)
        
        // Create queue for order tracking
        queue = XQueue.XCreate(capacity)
        XSHash.XInsertAddr(cache, "_queue", queue)
        
        ReturnValue(cache)
    }
}

Function.CacheGet {
    Input: cache: Address
    Input: key: Address
    Output: Address
    Body: {
        value = XSHash.XLookup(cache, key)
        ReturnValue(value)
    }
}

Function.CachePut {
    Input: cache: Address
    Input: key: Address
    Input: value: Address
    Body: {
        capacity = XSHash.XLookup(cache, "_capacity")
        queue = XSHash.XLookupAddr(cache, "_queue")
        
        // Check if key exists
        exists = XSHash.XExists(cache, key)
        
        IfCondition NotEqual(exists, 1) ThenBlock: {
            // New key - check capacity
            size = XArray.XSize(queue)
            
            IfCondition GreaterEqual(size, capacity) ThenBlock: {
                // Evict oldest
                old_key = XQueue.XDequeue(queue)
                XSHash.XDelete(cache, old_key)
            }
            
            // Add to queue
            XQueue.XEnqueue(queue, key)
        }
        
        // Store value
        XSHash.XInsert(cache, key, value)
    }
}
```

---

## Performance Characteristics

| Operation | XArray | XStack | XQueue | XSHash | XList |
|-----------|--------|--------|--------|--------|-------|
| Insert/Push | O(1)* | O(1)* | O(1) | O(1)** | O(1) |
| Remove/Pop | O(1) | O(1) | O(1) | O(1)** | O(1) front, O(n) back |
| Get/Lookup | O(1) | O(1) | - | O(1)** | O(n) |
| Search | O(n) | - | - | - | O(n) |
| Sort | O(n log n) | - | - | - | - |

\* Amortized - occasional O(n) when resizing  
\*\* Average case with good hash function

### Space Complexity

- **XArray**: O(n) + overhead for unused capacity
- **XSHash**: O(n + m) where m = bucket count
- **XList**: O(n) with 16-byte overhead per element
- **XQueue**: O(n) fixed size

---

## Best Practices

1. **Choose the right structure:**
   - Need random access? → XArray
   - Need LIFO? → XStack
   - Need FIFO? → XQueue
   - Need key-value lookup? → XSHash
   - Need frequent insertion/deletion at ends? → XList

2. **Pre-size when possible:**
   ```ailang
   // Good: Pre-size if you know the size
   arr = XArray.XCreate(1000)
   
   // Avoid: Many resizes
   arr = XArray.XCreate(2)  // Will resize many times
   ```

3. **Always clean up:**
   ```ailang
   arr = XArray.XCreate(10)
   // ... use it ...
   XArray.XDestroy(arr)  // Don't forget!
   ```

4. **Check for errors:**
   ```ailang
   value = XArray.XGet(arr, i)
   IfCondition EqualTo(value, XArrays.XERROR) ThenBlock: {
       // Handle error
   }
   ```

5. **Hash table sizing:**
   ```ailang
   // Good: Size proportional to expected items
   users = XSHash.XCreate(256)  // Expecting ~200 users
   
   // Avoid: Too small (many collisions) or too large (wasted memory)
   ```

---

## Version History

### Version 1.0 (Current)
- Initial release
- 6 data structures (XArray, XStack, XQueue, XHash, XSHash, XList)
- 9 utility functions
- Full memory management support
- Address-value variants for XSHash

---

## See Also

- **Library.HashMap Manual**: Advanced hash table with LRU cache
- **Library.StringUtils Manual**: String manipulation utilities
- **AILANG Memory Management Guide**: Allocate/Deallocate patterns
- **AILANG Core Language Manual**: Built-in primitives and syntax

---

## Contributing

To extend this library:

1. Maintain the X-prefix convention
2. Document memory allocations clearly
3. Provide cleanup functions for all structures
4. Add usage examples
5. Update performance characteristics
6. Increment version number

---

## License

Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering  
Licensed under the Sean Collins Software License (SCSL)