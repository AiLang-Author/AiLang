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
        """Bind socket to port - bind(sockfd, addr, addrlen)"""
        print("DEBUG: Compiling SocketBind")
        
        if len(node.arguments) < 2:
            print("ERROR: SocketBind requires socket and port")
            self.asm.emit_mov_rax_imm64(-1)
            return True
        
        # Get socket fd
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()  # Save socket fd
        
        # Get port number
        self.compiler.compile_expression(node.arguments[1])
        # Don't push port, keep it in RAX for immediate use
        
        # Allocate 16 bytes on stack for sockaddr_in
        self.asm.emit_bytes(0x48, 0x83, 0xEC, 0x10)  # SUB RSP, 16
        
        # Fill structure
        # sin_family = AF_INET (2)
        self.asm.emit_bytes(0x66, 0xC7, 0x04, 0x24, 0x02, 0x00)  # MOV WORD [RSP], 2
        
        # sin_port = htons(port) - port is already in RAX
        self.asm.emit_bytes(0x86, 0xE0)  # XCHG AH, AL (byte swap for network order)
        self.asm.emit_bytes(0x66, 0x89, 0x44, 0x24, 0x02)  # MOV WORD [RSP+2], AX
        
        # sin_addr = INADDR_ANY (0)
        self.asm.emit_bytes(0xC7, 0x44, 0x24, 0x04, 0x00, 0x00, 0x00, 0x00)  # MOV DWORD [RSP+4], 0
        
        # Now get the socket fd from where we saved it
        # It's at RSP+16 because we allocated 16 bytes
        self.asm.emit_bytes(0x48, 0x8B, 0x7C, 0x24, 0x10)  # MOV RDI, [RSP+16]
        
        # Call bind syscall
        self.asm.emit_mov_rax_imm64(49)      # bind syscall
        # RDI already has socket fd
        self.asm.emit_mov_rsi_rsp()          # Address of sockaddr_in
        self.asm.emit_mov_rdx_imm64(16)      # Size of sockaddr_in
        self.asm.emit_syscall()
        
        # Clean up stack (remove sockaddr_in AND the pushed socket fd)
        self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x18)  # ADD RSP, 24 (16 + 8)
        
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
