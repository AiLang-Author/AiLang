// Absolutely minimal function test

PrintMessage("=== MINIMAL FUNCTION TEST ===")

// Test 1: Function with no parameters
Function.NoParams {
    Output: Integer
    
    PrintMessage("NoParams called")
    ReturnValue(42)
}

PrintMessage("Test 1: No parameters")
r1 = NoParams()
PrintMessage("Result (should be 42):")
PrintNumber(r1)

// Test 2: Function with 1 parameter
Function.OneParam {
    Input: x: Integer
    Output: Integer
    
    PrintMessage("OneParam called with x:")
    PrintNumber(x)
    ReturnValue(x)
}

PrintMessage("\nTest 2: One parameter")
r2 = OneParam(99)
PrintMessage("Result (should be 99):")
PrintNumber(r2)

// Test 3: Function with 2 parameters
Function.TwoParams {
    Input: a: Integer
    Input: b: Integer
    Output: Integer
    
    PrintMessage("TwoParams called")
    PrintMessage("a:")
    PrintNumber(a)
    PrintMessage("b:")
    PrintNumber(b)
    
    result = Add(a, b)
    PrintMessage("Sum:")
    PrintNumber(result)
    
    ReturnValue(result)
}

PrintMessage("\nTest 3: Two parameters")
r3 = TwoParams(10, 20)
PrintMessage("Result (should be 30):")
PrintNumber(r3)

// Test without function - direct calculation
PrintMessage("\nDirect calculation (no function):")
direct = Add(10, 20)
PrintNumber(direct)

PrintMessage("\n=== END TEST ===")