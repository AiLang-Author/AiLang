#!/usr/bin/env python3
from pathlib import Path

redis_file = Path("redis_server.ailang")
content = redis_file.read_text()
backup = redis_file.with_suffix('.ailang.debug_backup')
backup.write_text(content)

# Add debugging right before CompareStreamIDs call
old_compare = '''                                                comparison = Helpers.CompareStreamIDs(entry_id_ptr, start_id_str)'''

new_compare = '''                                                PrintMessage("[DEBUG] XREAD: About to compare IDs\\n")
                                                PrintMessage("  entry_id_ptr = ")
                                                PrintNumber(entry_id_ptr)
                                                PrintMessage("\\n  start_id_str = ")
                                                PrintNumber(start_id_str)
                                                PrintMessage("\\n")
                                                
                                                // Check if these are valid before comparing
                                                IfCondition Or(EqualTo(entry_id_ptr, 0), EqualTo(start_id_str, 0)) ThenBlock: {
                                                    PrintMessage("[ERROR] NULL pointer in ID comparison\\n")
                                                    comparison = -2
                                                } ElseBlock: {
                                                    comparison = Helpers.CompareStreamIDs(entry_id_ptr, start_id_str)
                                                }'''

content = content.replace(old_compare, new_compare)

# Also add debug at the start of ParseStreamID
old_parse = '''Function.Helpers.ParseStreamID {
    Input: id_str: Address
    Output: Address // Pointer to an XArray with 2 integers [ms, seq]
    Body: {
        // Find the position of the '-' separator
        dash_pos = StringIndexOf(id_str, "-")'''

new_parse = '''Function.Helpers.ParseStreamID {
    Input: id_str: Address
    Output: Address // Pointer to an XArray with 2 integers [ms, seq]
    Body: {
        PrintMessage("[DEBUG] ParseStreamID: parsing ")
        PrintString(id_str)
        PrintMessage("\\n")
        
        // Find the position of the '-' separator
        dash_pos = StringIndexOf(id_str, "-")
        PrintMessage("[DEBUG] ParseStreamID: dash_pos = ")
        PrintNumber(dash_pos)
        PrintMessage("\\n")'''

content = content.replace(old_parse, new_parse)

redis_file.write_text(content)
print("Added debugging to stream comparison")
