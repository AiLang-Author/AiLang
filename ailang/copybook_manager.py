"""
COBOL COPY Book Manager

Handles COPY statement processing for COBOL transpiler.
Critical for government modernization RFP.

Design:
1. Pre-load all COPY books into a registry
2. When parser encounters COPY statement, substitute the copybook content
3. Parse the inlined content as if it were part of the original source
4. Track dependencies for proper compilation order

NIST Format:
  *HEADER,CLBRY,COPYNAME
  <copybook content - paragraphs, data structures, etc.>
  *END-OF,COPYNAME
"""

import re
from pathlib import Path
from typing import Dict, List, Optional, Set
from dataclasses import dataclass

@dataclass
class CopyBook:
    """Represents a loaded COPY book"""
    name: str
    content: str  # The actual COBOL code (minus headers)
    lines: List[str]
    source_file: Path
    dependencies: Set[str]  # Other copybooks this one references
    
    def __repr__(self):
        return f"CopyBook({self.name}, {len(self.lines)} lines)"


class CopyBookManager:
    """
    Manages COPY book library for COBOL compilation.
    
    Responsibilities:
    - Load copybooks from filesystem
    - Resolve COPY statements
    - Handle nested COPY statements (copybook includes another copybook)
    - Track dependencies
    - Detect circular references
    """
    
    def __init__(self, debug: bool = False):
        self.copybooks: Dict[str, CopyBook] = {}
        self.debug = debug
        self.search_paths: List[Path] = []
        
    def add_search_path(self, path: Path):
        """Add directory to search for copybooks"""
        if path.exists() and path.is_dir():
            self.search_paths.append(path)
            if self.debug:
                print(f"[COPYBOOK] Added search path: {path}")
        else:
            print(f"[COPYBOOK] Warning: Invalid search path: {path}")
    
    def load_copybook_from_file(self, filepath: Path) -> Optional[CopyBook]:
        """
        Load a single copybook from a file.
        
        Handles two formats:
        1. NIST format: *HEADER,CLBRY,NAME ... *END-OF,NAME
        2. Plain format: Just COBOL code (no program structure)
        """
        if not filepath.exists():
            return None
        
        try:
            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            # Check for NIST format
            if lines and lines[0].startswith('*HEADER,CLBRY,'):
                # Extract copybook name from header
                header = lines[0].strip()
                parts = header.split(',')
                if len(parts) >= 3:
                    name = parts[2].strip()
                else:
                    name = filepath.stem
                
                # Extract content (skip header and footer)
                content_lines = []
                for line in lines[1:]:
                    if line.startswith('*END-OF,'):
                        break
                    content_lines.append(line)
                
                content = ''.join(content_lines)
                
            else:
                # Plain format - entire file is the copybook
                name = filepath.stem
                content = ''.join(lines)
                content_lines = lines
            
            # Detect dependencies (other COPY statements within this copybook)
            dependencies = self._extract_copy_dependencies(content)
            
            copybook = CopyBook(
                name=name,
                content=content,
                lines=content_lines,
                source_file=filepath,
                dependencies=dependencies
            )
            
            self.copybooks[name] = copybook
            
            if self.debug:
                print(f"[COPYBOOK] Loaded: {name} ({len(content_lines)} lines)")
                if dependencies:
                    print(f"[COPYBOOK]   Dependencies: {dependencies}")
            
            return copybook
            
        except Exception as e:
            print(f"[COPYBOOK] Error loading {filepath}: {e}")
            return None
    
    def load_copybooks_from_directory(self, directory: Path):
        """Load all copybooks from a directory"""
        if not directory.exists():
            print(f"[COPYBOOK] Directory not found: {directory}")
            return
        
        # Load all .cbl files that are copybooks
        loaded = 0
        for filepath in directory.glob('*.cbl'):
            # Check if it's a copybook (has CLBRY header or no IDENTIFICATION DIVISION)
            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                first_line = f.readline()
            
            if 'CLBRY' in first_line:
                if self.load_copybook_from_file(filepath):
                    loaded += 1
        
        if self.debug:
            print(f"[COPYBOOK] Loaded {loaded} copybooks from {directory}")
    
    def _extract_copy_dependencies(self, content: str) -> Set[str]:
        """Find all COPY statements within content"""
        dependencies = set()
        
        # Pattern: COPY COPYBOOK-NAME.
        # Can also be: COPY COPYBOOK-NAME REPLACING ...
        pattern = r'\s+COPY\s+([A-Z0-9\-]+)'
        
        for match in re.finditer(pattern, content, re.IGNORECASE):
            copybook_name = match.group(1)
            dependencies.add(copybook_name)
        
        return dependencies
    
    def get_copybook(self, name: str) -> Optional[CopyBook]:
        """Get a copybook by name"""
        # Try exact match first
        if name in self.copybooks:
            return self.copybooks[name]
        
        # Try case-insensitive match
        name_upper = name.upper()
        for cb_name, copybook in self.copybooks.items():
            if cb_name.upper() == name_upper:
                return copybook
        
        # Try searching in search paths
        for search_path in self.search_paths:
            potential_file = search_path / f"{name}.cbl"
            if potential_file.exists():
                return self.load_copybook_from_file(potential_file)
        
        return None
    
    def expand_copy_statement(self, copy_name: str, replacing_clauses: List = None) -> str:
        """
        Expand a COPY statement into its actual content.
        
        Args:
            copy_name: Name of copybook to include
            replacing_clauses: Optional REPLACING clauses for text substitution
        
        Returns:
            The expanded COBOL code
        """
        copybook = self.get_copybook(copy_name)
        
        if not copybook:
            if self.debug:
                print(f"[COPYBOOK] Warning: Copybook '{copy_name}' not found")
            # Return comment indicating missing copybook
            return f"*> COPYBOOK {copy_name} NOT FOUND\n"
        
        content = copybook.content
        
        # Handle REPLACING clause
        if replacing_clauses:
            for old_text, new_text in replacing_clauses:
                content = content.replace(old_text, new_text)
        
        # Recursively expand nested COPY statements
        if copybook.dependencies:
            content = self._expand_nested_copies(content, visited={copy_name})
        
        if self.debug:
            print(f"[COPYBOOK] Expanded: {copy_name}")
        
        return content
    
    def _expand_nested_copies(self, content: str, visited: Set[str]) -> str:
        """
        Recursively expand COPY statements within copybooks.
        Detects circular dependencies.
        """
        # Find all COPY statements
        pattern = r'(\s+COPY\s+([A-Z0-9\-]+)\.?)'
        
        def replace_copy(match):
            full_match = match.group(1)
            copy_name = match.group(2)
            
            # Detect circular dependency
            if copy_name in visited:
                return f"*> CIRCULAR DEPENDENCY: {copy_name}\n"
            
            # Expand this copybook
            copybook = self.get_copybook(copy_name)
            if not copybook:
                return full_match  # Keep original COPY statement if not found
            
            # Mark as visited and expand
            new_visited = visited | {copy_name}
            expanded = self._expand_nested_copies(copybook.content, new_visited)
            
            return expanded
        
        return re.sub(pattern, replace_copy, content, flags=re.IGNORECASE)
    
    def list_copybooks(self):
        """List all loaded copybooks"""
        print(f"\n{'='*60}")
        print(f"COPYBOOK LIBRARY ({len(self.copybooks)} copybooks)")
        print(f"{'='*60}")
        
        for name, copybook in sorted(self.copybooks.items()):
            deps = f" → {copybook.dependencies}" if copybook.dependencies else ""
            print(f"  {name:20s} ({len(copybook.lines):3d} lines){deps}")


# ═══════════════════════════════════════════════════════════════
# INTEGRATION WITH PARSER
# ═══════════════════════════════════════════════════════════════

class COBOLParserWithCopyBooks:
    """
    Extended parser that handles COPY statements.
    
    Usage in your parser:
    
    1. Initialize with copybook manager:
       parser = COBOLParserWithCopyBooks(tokens, copybook_mgr)
    
    2. When encountering COPY statement:
       if self.match(COBOLTokenType.COPY):
           self.parse_copy_statement()
    
    3. The expanded content is parsed as if it were inline
    """
    
    def __init__(self, tokens, copybook_manager: CopyBookManager):
        self.tokens = tokens
        self.copybook_manager = copybook_manager
        # ... rest of parser initialization
    
    def parse_copy_statement(self):
        """
        Parse COPY statement and expand copybook inline.
        
        Syntax:
          COPY copybook-name.
          COPY copybook-name REPLACING ==old== BY ==new==.
        """
        self.consume(COBOLTokenType.COPY)
        
        # Get copybook name
        copybook_name = self.current_token().value
        self.advance()
        
        # Check for REPLACING clause
        replacing_clauses = []
        if self.match(COBOLTokenType.REPLACING):
            replacing_clauses = self.parse_replacing_clause()
        
        # Consume period
        self.consume(COBOLTokenType.PERIOD)
        
        # Expand the copybook
        expanded_content = self.copybook_manager.expand_copy_statement(
            copybook_name,
            replacing_clauses
        )
        
        # Tokenize the expanded content
        from cobol_frontend.cobol_lexer import COBOLLexer
        lexer = COBOLLexer(expanded_content)
        new_tokens = lexer.tokenize()
        
        # Insert new tokens at current position
        self.inject_tokens(new_tokens)
    
    def inject_tokens(self, new_tokens):
        """Insert tokens from copybook expansion"""
        # Insert new tokens at current position
        self.tokens = (
            self.tokens[:self.pos] +
            new_tokens +
            self.tokens[self.pos:]
        )


# ═══════════════════════════════════════════════════════════════
# USAGE EXAMPLE
# ═══════════════════════════════════════════════════════════════

if __name__ == '__main__':
    # Initialize copybook manager
    mgr = CopyBookManager(debug=True)
    
    # Add search paths
    mgr.add_search_path(Path('cobol_frontend/tests/nist_programs'))
    
    # Load all copybooks
    mgr.load_copybooks_from_directory(Path('cobol_frontend/tests/nist_programs'))
    
    # List what we found
    mgr.list_copybooks()
    
    # Test expansion
    print("\nTesting COPY expansion:")
    print("="*60)
    copybook = mgr.get_copybook('KP001')
    if copybook:
        print(f"Content of KP001:\n{copybook.content[:500]}")