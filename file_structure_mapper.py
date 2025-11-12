#!/usr/bin/env python3
"""
Clean File Structure Mapper
Generates readable file hierarchy WITHOUT special tree characters
"""

import os
import sys
from pathlib import Path
from collections import defaultdict
import argparse

class CleanFileMapper:
    def __init__(self, root_path, exclude_dirs=None, exclude_patterns=None, max_depth=None):
        self.root_path = Path(root_path).resolve()
        self.exclude_dirs = exclude_dirs or {
            '__pycache__', '.git', '.pytest_cache', 'node_modules',
            '.venv', 'venv', 'env', '.idea', '.vscode', 'build', 'dist',
            '.mypy_cache', '.tox', 'htmlcov', '.coverage'
        }
        self.exclude_patterns = exclude_patterns or {
            '.pyc', '.pyo', '.pyd', '.so', '.dll', '.dylib', '.exe',
            '.o', '.a', '.lib', '.DS_Store', 'Thumbs.db', '.swp', '.swo'
        }
        self.max_depth = max_depth
        
        self.file_stats = defaultdict(int)
        self.total_files = 0
        self.total_dirs = 0
    
    def should_exclude(self, path):
        """Check if path should be excluded"""
        name = path.name
        
        # Check if directory should be excluded
        if path.is_dir() and name in self.exclude_dirs:
            return True
        
        # Check if file matches exclude patterns
        if path.is_file():
            for pattern in self.exclude_patterns:
                if name.endswith(pattern):
                    return True
        
        return False
    
    def get_file_extension(self, path):
        """Get file extension or 'no_extension'"""
        if path.is_file():
            ext = path.suffix
            return ext if ext else 'no_extension'
        return None
    
    def get_relative_path(self, path):
        """Get path relative to root"""
        try:
            return path.relative_to(self.root_path)
        except ValueError:
            return path
    
    def scan_directory(self, directory, depth=0, output_lines=None):
        """
        Recursively scan directory and build clean structure
        Uses simple indentation with spaces only
        """
        if output_lines is None:
            output_lines = []
        
        # Check max depth
        if self.max_depth is not None and depth > self.max_depth:
            return output_lines
        
        try:
            items = sorted(directory.iterdir(), key=lambda x: (not x.is_dir(), x.name.lower()))
        except PermissionError:
            return output_lines
        
        # Filter out excluded items
        items = [item for item in items if not self.should_exclude(item)]
        
        indent = "  " * depth  # Simple 2-space indentation
        
        for item in items:
            rel_path = self.get_relative_path(item)
            
            if item.is_dir():
                self.total_dirs += 1
                # Directory with trailing slash
                output_lines.append(f"{indent}{item.name}/")
                
                # Recurse into subdirectory
                self.scan_directory(item, depth + 1, output_lines)
            
            else:
                self.total_files += 1
                # Track file extension
                ext = self.get_file_extension(item)
                if ext:
                    self.file_stats[ext] += 1
                
                # Just filename, no size
                output_lines.append(f"{indent}{item.name}")
        
        return output_lines
    
    def generate_summary(self):
        """Generate summary statistics"""
        lines = []
        lines.append("")
        lines.append("=" * 80)
        lines.append("SUMMARY STATISTICS")
        lines.append("=" * 80)
        lines.append(f"Total Directories: {self.total_dirs}")
        lines.append(f"Total Files: {self.total_files}")
        lines.append("")
        lines.append("Files by Extension:")
        lines.append("-" * 80)
        
        # Sort by count descending
        sorted_exts = sorted(self.file_stats.items(), key=lambda x: -x[1])
        
        for ext, count in sorted_exts:
            ext_display = ext if ext != 'no_extension' else '(no extension)'
            percentage = (count / self.total_files * 100) if self.total_files > 0 else 0
            lines.append(f"  {ext_display:20} {count:4} files ({percentage:5.1f}%)")
        
        return lines
    
    def generate_map(self, include_summary=True, output_file=None):
        """Generate clean file structure map"""
        lines = []
        
        # Header
        lines.append("=" * 80)
        lines.append(f"FILE STRUCTURE: {self.root_path.name}")
        lines.append("=" * 80)
        lines.append(f"Root: {self.root_path}")
        lines.append("")
        
        # Root directory name
        lines.append(f"{self.root_path.name}/")
        
        # Scan directory structure
        tree_lines = self.scan_directory(self.root_path, depth=1)
        lines.extend(tree_lines)
        
        # Add summary
        if include_summary:
            summary_lines = self.generate_summary()
            lines.extend(summary_lines)
        
        # Output
        output = "\n".join(lines)
        
        if output_file:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(output)
            print(f"File structure map saved to: {output_file}")
            print(f"Total: {self.total_dirs} directories, {self.total_files} files")
        else:
            print(output)
        
        return output


def main():
    parser = argparse.ArgumentParser(
        description="Generate clean file structure map (no special characters)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Map current directory
  python3 file_structure_mapper.py .
  
  # Map and save to file
  python3 file_structure_mapper.py . -o FILE-STRUCTURE.txt
  
  # Limit depth to 3 levels
  python3 file_structure_mapper.py . -o FILE-STRUCTURE.txt --max-depth 3
  
  # No summary
  python3 file_structure_mapper.py . --no-summary
        """
    )
    
    parser.add_argument(
        'directory',
        nargs='?',
        default='.',
        help='Directory to map (default: current directory)'
    )
    
    parser.add_argument(
        '-o', '--output',
        help='Output file path (default: print to console)'
    )
    
    parser.add_argument(
        '--max-depth',
        type=int,
        help='Maximum directory depth to scan'
    )
    
    parser.add_argument(
        '--exclude-dirs',
        nargs='+',
        help='Additional directories to exclude'
    )
    
    parser.add_argument(
        '--exclude-ext',
        nargs='+',
        help='Additional file extensions to exclude (e.g., .log .tmp)'
    )
    
    parser.add_argument(
        '--no-summary',
        action='store_true',
        help='Omit summary statistics'
    )
    
    args = parser.parse_args()
    
    # Prepare exclusions
    exclude_dirs = {
        '__pycache__', '.git', '.pytest_cache', 'node_modules',
        '.venv', 'venv', 'env', '.idea', '.vscode', 'build', 'dist'
    }
    
    if args.exclude_dirs:
        exclude_dirs.update(args.exclude_dirs)
    
    exclude_patterns = {
        '.pyc', '.pyo', '.pyd', '.so', '.dll', '.dylib', '.exe',
        '.o', '.a', '.lib', '.DS_Store', 'Thumbs.db'
    }
    
    if args.exclude_ext:
        exclude_patterns.update(args.exclude_ext)
    
    # Create mapper and generate
    mapper = CleanFileMapper(
        args.directory,
        exclude_dirs=exclude_dirs,
        exclude_patterns=exclude_patterns,
        max_depth=args.max_depth
    )
    
    try:
        mapper.generate_map(
            include_summary=not args.no_summary,
            output_file=args.output
        )
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()