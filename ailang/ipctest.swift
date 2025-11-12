
LibraryImport.JCL_IPC

LoopMain.Main {
    PrintMessage("Creating socket...\n")
    server_fd = IPC.CreateServerSocket("/tmp/test_daemon.sock")
    
    PrintMessage("Listening...\n")
    
    client_fd = SystemCall(43, server_fd, 0, 0)
    PrintMessage("Got connection\n")
    
    recv_buffer = IPC.ReceiveMessage(client_fd)
    PrintMessage("Received: ")
    PrintString(recv_buffer)
    PrintMessage("\n")
    
    IPC.SendMessage(client_fd, "OK\n")
    SystemCall(3, client_fd)
    SystemCall(3, server_fd)
    
    PrintMessage("Done\n")
}
