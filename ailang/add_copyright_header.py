#!/usr/bin/env python3
"""
Simple script to add SCSL copyright headers to Python files.
Run: python add_copyright_header.py
"""
import os
import sys

# Project-specific customizations:
COPYRIGHT_HOLDER = "Sean Collins, 2 Paws Machine and Engineering"
YEAR = "2025"
LICENSE = "SCSL (Sean Collins Software License)"
EXCLUDE_DIRS = {'__pycache__', '.git', 'node_modules', 'venv', '.pytest_cache', 'build', 'dist'}
EXCLUDE_FILES = {'setup.py', '__init__.py'}  # Add specific files to skip

HEADER_TEMPLATE = f"""# Copyright (c) {YEAR} {COPYRIGHT_HOLDER}. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.
"""

def has_header(file_path):
    """Check if the file already contains the copyright header."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            # Read first 20 lines to catch headers after shebangs/docstrings
            first_chunk = ''.join([f.readline() for _ in range(20)])
            return COPYRIGHT_HOLDER in first_chunk
    except (UnicodeDecodeError, IOError):
        return False

def extract_special_lines(content):
    """
    Extract shebang and encoding lines that must stay at the top.
    Returns: (special_lines, rest_of_content)
    """
    lines = content.split('\n')
    special = []
    i = 0
    
    # Check for shebang (must be line 1)
    if lines and lines[0].startswith('#!'):
        special.append(lines[0])
        i = 1
    
    # Check for encoding declaration (must be line 1 or 2)
    if i < len(lines) and ('coding' in lines[i] or 'encoding' in lines[i]):
        special.append(lines[i])
        i += 1
    
    # Rejoin the rest
    rest = '\n'.join(lines[i:])
    return special, rest

def add_header(file_path):
    """Prepend the copyright header to the file, preserving shebang/encoding."""
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Skip empty files
    if not content.strip():
        print(f"⚠ Skipping empty file: {file_path}")
        return
    
    # Extract special lines (shebang, encoding)
    special_lines, rest = extract_special_lines(content)
    
    # Build new content: special lines + blank line + header + rest
    parts = []
    if special_lines:
        parts.extend(special_lines)
        parts.append('')  # Blank line after shebang/encoding
    
    parts.append(HEADER_TEMPLATE.rstrip())  # Header without trailing newline
    
    # Add rest of content, ensuring single blank line separation
    if rest.startswith('\n'):
        parts.append(rest)
    else:
        parts.append('')
        parts.append(rest)
    
    new_content = '\n'.join(parts)
    
    # Write back
    with open(file_path, 'w', encoding='utf-8') as f:
        f.write(new_content)
    
    print(f"✓ Added SCSL header to {file_path}")

def scan_and_add(root_dir='.'):
    """Scan directory for .py files and add headers."""
    added_count = 0
    skipped_count = 0
    
    for dirpath, dirnames, filenames in os.walk(root_dir):
        # Skip excluded dirs (modifies in-place to prune walk)
        dirnames[:] = [d for d in dirnames if d not in EXCLUDE_DIRS]
        
        for filename in filenames:
            if filename.endswith('.py'):
                # Skip excluded files
                if filename in EXCLUDE_FILES:
                    print(f"⊘ Excluded: {filename}")
                    continue
                
                file_path = os.path.join(dirpath, filename)
                
                if not has_header(file_path):
                    try:
                        add_header(file_path)
                        added_count += 1
                    except Exception as e:
                        print(f"✗ ERROR processing {file_path}: {e}")
                else:
                    skipped_count += 1
                    print(f"⚠ Already has header: {file_path}")
    
    print(f"\n{'='*50}")
    print(f"✓ Added headers to {added_count} files")
    print(f"⚠ Skipped {skipped_count} files (already have headers)")
    print(f"{'='*50}")

if __name__ == '__main__':
    print("SCSL Copyright Header Tool")
    print(f"License: {LICENSE}")
    print(f"Holder: {COPYRIGHT_HOLDER}\n")
    
    # Safety check
    if not os.path.exists('.git'):
        response = input("⚠ WARNING: No .git directory found. Have you committed your code first? (y/n): ")
        if response.lower() != 'y':
            print("Aborted. Please commit your code before adding headers.")
            sys.exit(1)
    
    if len(sys.argv) > 1:
        scan_and_add(sys.argv[1])
    else:
        scan_and_add()