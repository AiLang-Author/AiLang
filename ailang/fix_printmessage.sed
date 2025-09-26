# Line 669 - Wrap in Debug block
669s/PrintMessage/Debug("error.store", level=1) { PrintMessage/
669a\
                                    }

# Line 1292 - Wrap in Debug block  
1292s/PrintMessage/Debug("error.xread", level=1) { PrintMessage/
1292a\
                                    }

# Line 1340 - Wrap in Debug block
1340s/PrintMessage/Debug("error.null", level=1) { PrintMessage/
1340a\
                                                    }

# Line 1841 - Wrap in Debug block
1841s/PrintMessage/Debug("error.flushdb", level=1) { PrintMessage/
1841a\
                                }

# Line 2123-2125 - Wrap verification read in Debug block
2123s/PrintMessage/Debug("main.verify", level=1) { PrintMessage/
2125s/PrintMessage/PrintMessage/
2125a\
    }

# Line 2129 - Wrap in Debug block
2129s/PrintMessage/Debug("error.store_creation", level=1) { PrintMessage/
2129a\
        }

# Line 2155 - Wrap server listening message in Debug block
2155s/PrintMessage/Debug("server.start", level=1) { PrintMessage/
2155a\
    }
