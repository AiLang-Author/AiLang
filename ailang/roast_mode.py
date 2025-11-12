"""
ROAST MODE - Professional error messages with optional savage commentary
========================================================================

Usage:
    python3 cobol_converter.py program.cbl --roast-mode

Features:
- Default: Professional, helpful error messages
- Roast mode: Same errors + therapeutic developer commentary
- Still passes all the same info for fixing bugs
- Releases endorphins when dealing with 1970s COBOL
"""

import random

# ============================================================================
# ROAST MODE STATE (set by CLI flag)
# ============================================================================

ROAST_MODE_ENABLED = False  # Set by command line flag


def enable_roast_mode():
    """Enable savage error messages"""
    global ROAST_MODE_ENABLED
    ROAST_MODE_ENABLED = True


def is_roast_mode():
    """Check if roast mode is active"""
    return ROAST_MODE_ENABLED


# ============================================================================
# ROAST MESSAGE LIBRARY
# ============================================================================

COMMA_ROASTS = [
    "You have a comma with nothing before it. Even JavaScript would reject this.",
    "Missing value before comma. What were you planning to put there? Your hopes and dreams?",
    "That's not how commas work. This isn't Twitter, you can't just... trail off...",
    "Empty slot before comma detected. Did you forget what you were typing halfway through?",
    "COBOL saw your comma and said 'nope'. COBOL. The language that accepts GO TO.",
    "There's nothing before this comma. Like your documentation, it doesn't exist.",
    "Comma with no preceding value. Did you mean to type something, or just mash the keyboard?",
    "This comma is as useful as a screen door on a submarine.",
    "Your comma has commitment issues. It's there, but nothing came before it.",
    "DISPLAY A, , B is not valid. Neither was your decision to skip testing.",
    "That comma is more lonely than a COBOL programmer at a JavaScript conference.",
    "Missing value before comma. This is what happens when you code without coffee.",
    "Nothing before the comma? That's not minimalism, that's a syntax error.",
    "Your comma is an orphan. It deserves a value parent. Be better.",
    "This empty comma slot has the same energy as 'let's circle back on that'.",
]

LEVEL_NUMBER_ROASTS = [
    "You forgot a period. Again. This is why we can't have nice things.",
    "Missing period detected. COBOL is not free-form. Welcome to 1959.",
    "Level number appearing in expression = You forgot to end the previous statement.",
    "The statement above this line is missing a period. Go find it. I'll wait.",
    "Roses are red, COBOL is old, finish your statements or your code won't compile.",
    "COBOL requires periods. It's not a suggestion. It's not optional. It's THE LAW.",
    "Missing period detected. Grace Hopper is disappointed in you from beyond the grave.",
    "This level number thinks it's in an expression. It's not. Someone forgot a period.",
    "PERIOD. GOES. HERE. Do I need to draw you a diagram?",
    "The previous statement is missing a period. No, not the menstrual kind. The punctuation kind.",
    "Level numbers in expressions? That's not a feature, that's a cry for help.",
    "Somewhere in the lines above, you forgot a period. This is your reminder.",
    "This code is missing more periods than a middle school grammar test.",
    "COBOL without periods is like a sentence without... see what I did there?",
    "That level number wandered into the wrong neighborhood. Add a period above.",
    "Missing period: The #1 cause of COBOL parser rage since 1959.",
    "You can't just skip periods in COBOL. This isn't modern poetry.",
    "The statement above needs a period. Like, desperately. It's been waiting since 1983.",
    "PERIOD MISSING. PARSER SAD. FIX CODE. BE HAPPY.",
    "01 level numbers don't belong in expressions. But you knew that. Or should have.",
]

GO_KEYWORD_ROASTS = [
    "GO is a keyword, not a variable. This isn't Python where everything is an object.",
    "Why is GO in an expression? Were you trying to make the parser cry?",
    "GO TO is for control flow. GO is not for arithmetic. GO away with this syntax.",
    "Fun fact: GO TO was considered harmful in 1968. This is worse.",
    "You can't compute with GO. It's not that kind of language feature.",
    "GO in an expression? What's next, DIVISION in a variable name? Oh wait...",
    "GO is for jumping to paragraphs, not computing values. Learn the difference.",
    "Using GO as a variable? That's like using 'if' as a function name in C. Don't.",
    "GO does not compute. Literally. It's a control flow keyword.",
    "This GO has nowhere to go. Because it's in the wrong context entirely.",
    "GO can't be in an expression. It's a keyword. Read the manual. Any manual.",
    "Found GO in expression. Expected: literally anything else.",
    "GO is not a variable. GO is not a function. GO is your cue to fix this.",
    "COMPUTE RESULT = GO makes as much sense as PERFORM MATH WITH FEELINGS.",
    "You tried to use GO in arithmetic. The 1960s called, they want their syntax error back.",
    "GO belongs in GO TO statements. Not expressions. Not anywhere near here.",
]

MISSING_TO_ROASTS = [
    "Missing TO keyword. Even COBOL-74 required this. Were you aiming for COBOL-60?",
    "Expected TO. Got disappointment instead.",
    "TO is not optional here. Unlike your code review process, apparently.",
    "You need TO in a MOVE statement. It's literally in the name. MOVE something TO somewhere.",
    "MOVE without TO is like peanut butter without jelly. Technically possible but deeply wrong.",
    "Where's the TO? Did it get lost in the 40 years this code has been in production?",
    "TO keyword missing. MOVE X Y makes no sense. MOVE X TO Y makes sense. See?",
    "MOVE needs TO. This is COBOL, not telepathy. Specify where the data goes.",
    "Missing TO keyword. The compiler can't read your mind. Neither can your coworkers.",
    "You forgot TO. No really, you forgot the word TO. In a MOVE statement. How.",
    "TO is required. It's three characters. You can type three characters.",
    "MOVE source target is not valid. MOVE source TO target is valid. Notice the difference?",
    "This MOVE statement is missing TO. Like your code is missing comments. And tests.",
    "Expected TO keyword. Got syntax error. Welcome to COBOL.",
]

GENERIC_ROASTS = [
    "This code has seen some things. Mostly compile errors.",
    "Somewhere, a COBOL programmer from 1975 just felt a disturbance in the Force.",
    "This is why we invented structured programming. And you ignored it.",
    "I've seen mainframe batch jobs with better syntax.",
    "Code quality: Somewhere between 'works on my AS/400' and 'what is testing'.",
    "This syntax error is older than most developers reading this message.",
    "Your code has more problems than a math textbook. Starting with this line.",
    "Error detected. Shocked? Me neither.",
    "This looks like someone compiled COBOL after a three-martini lunch in 1973.",
    "Syntax error found. In other news, water is wet and COBOL is old.",
    "I've parsed COBOL from punch cards with better syntax than this.",
    "This code was written by someone who clearly had a deadline. And no supervision.",
    "Congratulations! You've found a new way to make COBOL even harder to read.",
    "This syntax error has been living rent-free in this codebase since the Reagan administration.",
    "Your code has the same energy as 'it works on my machine'.",
    "This is what happens when you copy-paste from StackOverflow but it's a COBOL answer from 1998.",
    "Error found in COBOL code. Truly shocking. Said no one ever.",
    "This syntax is so broken, even the mainframe is judging you.",
    "Code quality assessment: Needs improvement. And by improvement, I mean a rewrite.",
    "This looks like it was written during Y2K panic. And never revisited.",
    "Syntax error detected. Did you write this while the batch job was running?",
    "Your code has issues. The parser has opinions. Both are correct.",
    "This syntax makes the Turing test look easy by comparison.",
    "I've seen better code in COBOL tutorials from 1962. And those were printed on paper.",
    "This code has been in production so long, it remembers when 640K was enough memory.",
]

MISSING_PERIOD_ROASTS = [
    "Missing period. The most common COBOL error since punch cards were invented.",
    "Periods are not optional. Unlike meetings, you actually need these.",
    "COBOL without periods is like SQL without semicolons. Wait, that works. Bad example.",
    "You forgot a period. Somewhere above this line. Good luck finding it.",
    "No period found. Grace Hopper would not approve.",
    "This statement is missing a period. No, you can't use a semicolon. This is COBOL.",
    "PERIOD MISSING. STATUS: CRITICAL. MOOD: ANNOYED.",
    "Every statement needs a period. Yes, EVERY ONE. Not optional.",
    "Missing period detected. This is why we can't migrate to the cloud.",
    "Forgot the period? That's okay. Just add it. To the previous line. Not here.",
]

BY_KEYWORD_ROASTS = [
    "Expected expression before BY. Did you forget what you were dividing by?",
    "BY keyword needs something before it. Like, actual data. Not wishes.",
    "Missing value before BY. Math doesn't work with empty operands.",
    "BY keyword found with nothing before it. That's not how division works.",
    "You can't divide BY nothing. Well, technically you can, but you get infinity and sadness.",
    "Empty expression before BY keyword. Did you mean to put something there?",
    "BY requires a value before it. This is arithmetic, not philosophy.",
]

UNEXPECTED_KEYWORD_ROASTS = [
    "Unexpected keyword found. Like your syntax, it's in the wrong place.",
    "This keyword doesn't belong here. Neither does this code in production.",
    "Wrong keyword in expression. Check the COBOL manual. Or any manual.",
    "That keyword is lost. Like the documentation for this codebase.",
    "Keyword appears in wrong context. Much like your variable naming conventions.",
]

# ============================================================================
# PROFESSIONAL ERROR MESSAGES (always shown)
# ============================================================================

def generate_professional_error(token, context: str, error_type: str) -> str:
    """Generate helpful, professional error message"""
    
    if error_type == "MISSING_COMMA_VALUE":
        return (
            f"Parser error at line {token.line}, column {token.column}: "
            f"Missing {context} before COMMA. "
            f"Expected value in list before comma separator.\n"
            f"  Hint: Check for empty slots like: DISPLAY A, , B"
        )
    
    if error_type == "LEVEL_NUMBER_IN_EXPRESSION":
        return (
            f"Parser error at line {token.line}, column {token.column}: "
            f"Unexpected LEVEL_NUMBER (value={token.value}) in {context}. "
            f"Level numbers cannot appear in expressions.\n"
            f"  Common cause: Missing period at end of previous statement.\n"
            f"  Check line {token.line - 1} for missing period."
        )
    
    if error_type == "GO_IN_EXPRESSION":
        return (
            f"Parser error at line {token.line}, column {token.column}: "
            f"Unexpected token in {context}: GO = 'GO'.\n"
            f"Expected: number, string, identifier, function, or parenthesized expression.\n"
            f"  Note: GO is a statement keyword, not a variable name."
        )
    
    if error_type == "MISSING_TO":
        return (
            f"Parser error at line {token.line}, column {token.column}: "
            f"Expected TO keyword in MOVE statement.\n"
            f"  Syntax: MOVE source-field TO target-field"
        )
    
    # Generic
    return (
        f"Parser error at line {token.line}, column {token.column}: "
        f"Unexpected token in {context}: {token.type.name} = '{token.value}'"
    )


# ============================================================================
# ROAST MODE ENHANCEMENTS (only shown if --roast-mode)
# ============================================================================

def generate_roast_commentary(error_type: str, token=None, special_context=None) -> str:
    """Generate therapeutic savage commentary"""
    
    # Easter egg: Check for Friday deployments
    import datetime
    is_friday = datetime.datetime.now().weekday() == 4
    is_late = datetime.datetime.now().hour >= 16
    
    # Easter egg: Check for specific line numbers
    cursed_lines = [666, 1337, 420, 69]
    is_cursed_line = token and token.line in cursed_lines
    
    # Select roast based on error type
    if error_type == "MISSING_COMMA_VALUE":
        roast = random.choice(COMMA_ROASTS)
        suggestion = random.choice(RECOVERY_SUGGESTIONS["MISSING_COMMA_VALUE"])
        
    elif error_type == "LEVEL_NUMBER_IN_EXPRESSION":
        roast = random.choice(LEVEL_NUMBER_ROASTS)
        suggestion = random.choice(RECOVERY_SUGGESTIONS["LEVEL_NUMBER_IN_EXPRESSION"])
        
    elif error_type == "GO_IN_EXPRESSION":
        roast = random.choice(GO_KEYWORD_ROASTS)
        suggestion = random.choice(RECOVERY_SUGGESTIONS["GO_IN_EXPRESSION"])
        
    elif error_type == "MISSING_TO":
        roast = random.choice(MISSING_TO_ROASTS)
        suggestion = random.choice(RECOVERY_SUGGESTIONS["MISSING_TO"])
        
    elif error_type == "MISSING_PERIOD":
        roast = random.choice(MISSING_PERIOD_ROASTS)
        suggestion = random.choice(RECOVERY_SUGGESTIONS["MISSING_PERIOD"])
        
    else:
        roast = random.choice(GENERIC_ROASTS)
        suggestion = "Fix the syntax error. Consult the COBOL manual. Or any manual. We won't judge."
    
    # Build the roast message
    output = f"\n\n{'='*70}\n💀 ROAST MODE ACTIVATED 💀\n{'='*70}\n"
    
    # Add Easter eggs
    if is_friday and is_late:
        output += "⚠️  FRIDAY 4PM DEPLOYMENT DETECTED ⚠️\n"
        output += "You know what you're doing. Godspeed.\n\n"
    
    if is_cursed_line:
        output += f"🔮 ERROR ON LINE {token.line} 🔮\n"
        if token.line == 666:
            output += "The devil's line number. Of course there's an error here.\n"
        elif token.line == 1337:
            output += "L33T line number. Error is not l33t.\n"
        elif token.line == 420:
            output += "Blaze it... but also fix this error.\n"
        elif token.line == 69:
            output += "Nice line number. Error is not nice.\n"
        output += "\n"
    
    # Add the roast
    output += f"{roast}\n\n"
    
    # Add helpful recovery suggestion
    output += f"💡 Fix: {suggestion}\n"
    output += "="*70 + "\n"
    
    return output


# ============================================================================
# SPECIAL ROASTS: For truly exceptional badness
# ============================================================================

SPECIAL_ROASTS = {
    # Multiple errors in one line
    "MULTIPLE_ERRORS": [
        "This line has multiple errors. It's like a syntax error turducken.",
        "Not just one error. Multiple errors. On the same line. Impressive in the worst way.",
        "You managed to violate several COBOL rules in one statement. That takes skill. Bad skill.",
        "This line is an error buffet. All-you-can-parse syntax violations.",
    ],
    
    # Nested errors
    "NESTED_MADNESS": [
        "This error is inside another error. It's errors all the way down.",
        "Nested errors detected. Like Russian dolls, but with more disappointment.",
        "You found an error within an error. Errorception. We need to go deeper.",
    ],
    
    # Really long lines
    "LINE_TOO_LONG": [
        "This line is longer than the warranty period on a 1970s mainframe.",
        "Did you forget that line breaks exist? Or are you paid by the character?",
        "This line is so long, the punch card operator would have filed a complaint.",
    ],
    
    # Particularly ancient syntax
    "COBOL_60_DETECTED": [
        "This syntax is so old, it predates the moon landing.",
        "COBOL-60 syntax detected. Were you transported here from the Kennedy administration?",
        "This code is older than COBOL-74. And it shows. Boy, does it show.",
    ],
    
    # Spacing disasters
    "SPACING_NIGHTMARE": [
        "Your spacing is more chaotic than a Friday deployment.",
        "Did you format this with a blindfold? Or just without looking?",
        "This spacing makes Python developers cry. And they're used to whitespace issues.",
    ],
}

# ============================================================================
# RECOVERY SUGGESTIONS: Actual helpful advice after the roast
# ============================================================================

RECOVERY_SUGGESTIONS = {
    "MISSING_COMMA_VALUE": [
        "Remove the extra comma, or add the missing value.",
        "Check for double commas like: , , in your DISPLAY or ACCEPT statements.",
        "List items should be: A, B, C not A, , C",
    ],
    
    "LEVEL_NUMBER_IN_EXPRESSION": [
        "Add a period at the end of the previous statement.",
        "Check the statement on the line above this error.",
        "Every COBOL statement needs a period. Yes, every single one.",
        "Search backward from this line for the first statement without a period.",
    ],
    
    "GO_IN_EXPRESSION": [
        "If you want control flow, use: GO TO paragraph-name",
        "If you want a variable, name it anything except GO, MOVE, IF, PERFORM, etc.",
        "COBOL keywords can't be used as variable names. Pick something else.",
    ],
    
    "MISSING_TO": [
        "Add the TO keyword: MOVE source TO target",
        "MOVE requires TO. It's not optional. Ever.",
        "Syntax is: MOVE data-item TO receiving-field",
    ],
    
    "MISSING_PERIOD": [
        "Add a period at the end of the previous statement.",
        "COBOL statements end with periods. Not commas. Not semicolons. Periods.",
        "Check every statement - they ALL need periods.",
    ],
}

def generate_boundary_error(token, context: str = "expression", error_type: str = None) -> str:
    """
    Generate error message with optional roast mode commentary.
    
    Args:
        token: The problematic token
        context: What we were parsing ("expression", "MOVE statement", etc.)
        error_type: Type of error for targeted messages
    
    Returns:
        Complete error message (professional + optional roast)
    """
    # Infer error type from token if not provided
    if error_type is None:
        from cobol_frontend.cobol_lexer import COBOLTokenType
        
        if token.type == COBOLTokenType.COMMA:
            error_type = "MISSING_COMMA_VALUE"
        elif token.type == COBOLTokenType.LEVEL_NUMBER:
            error_type = "LEVEL_NUMBER_IN_EXPRESSION"
        elif token.type == COBOLTokenType.GO:
            error_type = "GO_IN_EXPRESSION"
        elif token.type == COBOLTokenType.TO:
            error_type = "MISSING_TO"
        else:
            error_type = "GENERIC"
    
    # Always include professional error
    error_msg = generate_professional_error(token, context, error_type)
    
    # Add roast if enabled
    if is_roast_mode():
        error_msg += generate_roast_commentary(error_type)
    
    return error_msg


# ============================================================================
# CLI INTEGRATION
# ============================================================================

def add_roast_mode_argument(parser):
    """Add --roast-mode flag to argument parser"""
    parser.add_argument(
        '--roast-mode',
        action='store_true',
        help='Enable savage error messages for therapeutic debugging. '
             'Adds colorful commentary to error messages. '
             'Not recommended for humorless managers or Tuesday mornings.'
    )


def setup_roast_mode_from_args(args):
    """Enable roast mode if flag is set"""
    if hasattr(args, 'roast_mode') and args.roast_mode:
        enable_roast_mode()
        print("\n" + "="*70)
        print("🔥 ROAST MODE ENABLED 🔥")
        print("="*70)
        print("Error messages will now include therapeutic commentary.")
        print("Brace yourself for honesty about your COBOL code.")
        print("="*70 + "\n")


# ============================================================================
# EXAMPLE ERROR MESSAGES
# ============================================================================

"""
PROFESSIONAL MODE (default):
----------------------------
Parser error at line 290, column 29: Missing expression before COMMA.
Expected value in list before comma separator.
  Hint: Check for empty slots like: DISPLAY A, , B


ROAST MODE (--roast-mode):
--------------------------
Parser error at line 290, column 29: Missing expression before COMMA.
Expected value in list before comma separator.
  Hint: Check for empty slots like: DISPLAY A, , B

======================================================================
💀 ROAST MODE ACTIVATED 💀
======================================================================
You have a comma with nothing before it. Even JavaScript would reject this.

💡 Fix: Remove the extra comma or add the missing value. Not that hard.
======================================================================
"""