#!/usr/bin/env python3
"""
Network Operations Module for AILANG Compiler
Implements TCP socket operations for Redis server
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class NetworkOps:
    """Handles network/socket operations"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        
    def compile_operation(self, node):
        """Route network operations to specific handlers"""
        try:
            if isinstance(node, FunctionCall):
                function_name = node.function
                
                if function_name == 'SocketCreate':
                    return self.compile_socket_create(node)
                elif function_name == 'SocketBind':
                    return self.compile_socket_bind(node)
                elif function_name == 'SocketListen':
                    return self.compile_socket_listen(node)
                elif function_name == 'SocketAccept':
                    return self.compile_socket_accept(node)
                elif function_name == 'SocketRead':
                    return self.compile_socket_read(node)
                elif function_name == 'SocketWrite':
                    return self.compile_socket_write(node)
                elif function_name == 'SocketClose':
                    return self.compile_socket_close(node)
                else:
                    return False
            
            return False
            
        except Exception as e:
            print(f"ERROR: Network operation compilation failed: {str(e)}")
            raise
    
    def compile_socket_create(self, node):
        """Create a TCP socket - socket(AF_INET, SOCK_STREAM, 0)"""
        print("DEBUG: Compiling SocketCreate")
        
        # socket syscall number = 41
        self.asm.emit_mov_rax_imm64(41)      # socket syscall
        self.asm.emit_mov_rdi_imm64(2)       # AF_INET
        self.asm.emit_mov_rsi_imm64(1)       # SOCK_STREAM
        self.asm.emit_mov_rdx_imm64(0)       # protocol = 0
        self.asm.emit_syscall()
        
        # RAX now contains socket fd (or -1 on error)
        print("DEBUG: SocketCreate completed")
        return True
    
    def compile_socket_bind(self, node):
        """Bind socket to address and port - bind(sockfd, addr, addrlen)"""
        print("DEBUG: Compiling SocketBind")
        
        if len(node.arguments) < 3:
            print("ERROR: SocketBind requires socket, address, port")
            self.asm.emit_mov_rax_imm64(-1)
            return True
        
        # Get socket fd
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()  # Socket fd in RDI
        
        # Build sockaddr_in on stack
        # Total size: 16 bytes
        # Layout: family(2) + port(2) + addr(4) + padding(8)
        
        # Allocate 16 bytes on stack
        self.asm.emit_bytes(0x48, 0x83, 0xEC, 0x10)  # SUB RSP, 16
        
        # Set family (AF_INET = 2) at [RSP]
        self.asm.emit_bytes(0x66, 0xC7, 0x04, 0x24, 0x02, 0x00)  # MOV WORD [RSP], 2
        
        # Get port and convert to network byte order
        self.compiler.compile_expression(node.arguments[2])
        # Swap bytes for network order (e.g., 8080 = 0x1F90 -> 0x901F)
        self.asm.emit_bytes(0x86, 0xC4)  # XCHG AH, AL (swap bytes in AX)
        # Store at [RSP+2]
        self.asm.emit_bytes(0x66, 0x89, 0x44, 0x24, 0x02)  # MOV [RSP+2], AX
        
        # Get IP address - needs to be in network byte order too!
        self.compiler.compile_expression(node.arguments[1])
        
        # Check if it's 0 (INADDR_ANY) - don't swap
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        no_swap_label = self.asm.create_label()
        self.asm.emit_bytes(0x74, 0x07)  # JZ +7 (skip byte swap if 0)
        
        # For non-zero IP, convert to network byte order
        # 127.0.0.1 = 0x7F000001 should become 0x0100007F in memory
        self.asm.emit_bytes(0x0F, 0xC8)  # BSWAP EAX (reverse all 4 bytes)
        
        # Store at [RSP+4]
        self.asm.emit_bytes(0x89, 0x44, 0x24, 0x04)  # MOV [RSP+4], EAX
        
        # Zero padding at [RSP+8] (8 bytes)
        self.asm.emit_bytes(0x48, 0xC7, 0x44, 0x24, 0x08, 0x00, 0x00, 0x00, 0x00)  # MOV QWORD [RSP+8], 0
        
        # Set up bind syscall parameters
        self.asm.emit_mov_rax_imm64(49)  # bind syscall
        # RDI already has socket fd
        self.asm.emit_mov_rsi_rsp()  # RSI = pointer to sockaddr_in
        self.asm.emit_mov_rdx_imm64(16)  # RDX = sizeof(sockaddr_in)
        self.asm.emit_syscall()
        
        # Clean up stack
        self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x10)  # ADD RSP, 16
        
        print("DEBUG: SocketBind completed")
        return True
    
    def compile_socket_listen(self, node):
        """Listen for connections - listen(sockfd, backlog)"""
        print("DEBUG: Compiling SocketListen")
        
        if not node.arguments:
            print("ERROR: SocketListen requires socket")
            self.asm.emit_mov_rax_imm64(-1)
            return True
        
        # Get socket fd
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()
        
        # listen syscall
        self.asm.emit_mov_rax_imm64(50)      # listen syscall
        self.asm.emit_mov_rsi_imm64(5)       # backlog = 5
        self.asm.emit_syscall()
        
        print("DEBUG: SocketListen completed")
        return True
    
    def compile_socket_accept(self, node):
        """Accept connection - accept(sockfd, addr, addrlen)"""
        print("DEBUG: Compiling SocketAccept")
        
        if not node.arguments:
            print("ERROR: SocketAccept requires socket")
            self.asm.emit_mov_rax_imm64(-1)
            return True
        
        # Get socket fd
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()
        
        # accept syscall
        self.asm.emit_mov_rax_imm64(43)      # accept syscall
        self.asm.emit_mov_rsi_imm64(0)       # addr = NULL (don't care about client addr)
        self.asm.emit_mov_rdx_imm64(0)       # addrlen = NULL
        self.asm.emit_syscall()
        
        # RAX contains client socket fd
        print("DEBUG: SocketAccept completed")
        return True
    
    def compile_socket_read(self, node):
        """Read from socket - read(fd, buffer, count)"""
        print("DEBUG: Compiling SocketRead")
        
        if len(node.arguments) < 3:
            print("ERROR: SocketRead requires socket, buffer, count")
            self.asm.emit_mov_rax_imm64(-1)
            return True
        
        # Get socket fd
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()
        
        # Get buffer
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rsi_rax()
        
        # Get count
        self.compiler.compile_expression(node.arguments[2])
        self.asm.emit_mov_rdx_rax()
        
        # read syscall
        self.asm.emit_mov_rax_imm64(0)       # read syscall
        self.asm.emit_syscall()
        
        # RAX contains bytes read
        print("DEBUG: SocketRead completed")
        return True
    
    def compile_socket_write(self, node):
        """Write to socket - write(fd, buffer, count)"""
        print("DEBUG: Compiling SocketWrite")
        
        if len(node.arguments) < 3:
            print("ERROR: SocketWrite requires socket, buffer, count")
            self.asm.emit_mov_rax_imm64(-1)
            return True
        
        # Get socket fd
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()
        
        # Get buffer
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rsi_rax()
        
        # Get count
        self.compiler.compile_expression(node.arguments[2])
        self.asm.emit_mov_rdx_rax()
        
        # write syscall
        self.asm.emit_mov_rax_imm64(1)       # write syscall
        self.asm.emit_syscall()
        
        # RAX contains bytes written
        print("DEBUG: SocketWrite completed")
        return True
    
    def compile_socket_close(self, node):
        """Close socket - close(fd)"""
        print("DEBUG: Compiling SocketClose")
        
        if not node.arguments:
            print("ERROR: SocketClose requires socket")
            self.asm.emit_mov_rax_imm64(-1)
            return True
        
        # Get socket fd
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()
        
        # close syscall
        self.asm.emit_mov_rax_imm64(3)       # close syscall
        self.asm.emit_syscall()
        
        print("DEBUG: SocketClose completed")
        return True
