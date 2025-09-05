#!/usr/bin/env python3
"""
Type Tagging System for AILANG
Uses odd/even addresses to distinguish types:
- Numbers: EVEN (bit 0 = 0)
- String pointers: ODD (bit 0 = 1)
"""

class TypeTagging:
    """Helper methods for type tagging operations"""
    
    @staticmethod
    def tag_string_address(address):
        """Tag an address as a string pointer by setting bit 0"""
        return address | 1
    
    @staticmethod
    def untag_string_address(address):
        """Remove string tag to get actual pointer"""
        return address & ~1
    
    @staticmethod
    def is_string_address(address):
        """Check if address is tagged as string (odd)"""
        return address & 1 == 1
    
    @staticmethod
    def ensure_number(value):
        """Ensure value is tagged as number (even)"""
        return value & ~1