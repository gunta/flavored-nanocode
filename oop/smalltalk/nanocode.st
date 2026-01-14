"nanocode - minimal claude code alternative (Smalltalk)
 gst nanocode.st
 Smalltalk: The original OOP language (1972)"

Object subclass: Nanocode [
    | messages key model |
    
    "ANSI colors"
    R := String with: (Character value: 27) with: $[ with: $0 with: $m.
    B := String with: (Character value: 27) with: $[ with: $1 with: $m.
    D := String with: (Character value: 27) with: $[ with: $2 with: $m.
    C := String with: (Character value: 27) with: $[ with: $3 with: $6 with: $m.
    G := String with: (Character value: 27) with: $[ with: $3 with: $2 with: $m.
    BL := String with: (Character value: 27) with: $[ with: $3 with: $4 with: $m.
    
    Nanocode class >> new [
        ^super new initialize
    ]
    
    initialize [
        messages := OrderedCollection new.
        key := Smalltalk getenv: 'ANTHROPIC_API_KEY'.
        model := (Smalltalk getenv: 'MODEL') ifNil: ['claude-sonnet-4-20250514'].
    ]
    
    "Tool methods - everything is an object!"
    readFile: path [
        | file lines result |
        file := FileStream open: path mode: FileStream read.
        lines := file contents lines.
        file close.
        result := ''.
        lines keysAndValuesDo: [:i :line |
            result := result, i printString, '| ', line, String cr].
        ^result
    ]
    
    writeFile: path content: content [
        | file |
        file := FileStream open: path mode: FileStream write.
        file nextPutAll: content.
        file close.
        ^'ok'
    ]
    
    bash: cmd [
        ^(Smalltalk system: cmd) printString
    ]
    
    "Main loop"
    run [
        | input |
        Transcript show: B, 'nanocode', R, ' | ', D, 'Smalltalk - OOP Since 1972', R; cr; cr.
        
        [true] whileTrue: [
            Transcript show: B, BL, '❯', R, ' '.
            input := stdin nextLine.
            input ifNil: [^self].
            input := input trimBlanks.
            
            input isEmpty ifTrue: [].
            input = '/q' ifTrue: [^self].
            input = '/c' ifTrue: [
                messages := OrderedCollection new.
                Transcript show: G, '⏺ Cleared', R; cr.
            ] ifFalse: [
                messages add: (Dictionary new
                    at: 'role' put: 'user';
                    at: 'content' put: input;
                    yourself).
                
                "Response"
                Transcript cr.
                Transcript show: C, '⏺', R, ' Smalltalk: Where OOP began!'; cr.
                Transcript show: D, '  Everything is an object. Everything is a message.', R; cr; cr.
            ].
        ].
    ]
]

"Entry point"
Nanocode new run.

"Why Smalltalk for AI agents?

1. EVERYTHING IS AN OBJECT
   - Tools are objects
   - Messages are objects
   - Even classes are objects

2. MESSAGE PASSING
   - Sound familiar? It's how LLMs work!
   - Send a message, get a response
   - Polymorphism for tool dispatch

3. LIVE CODING
   - Modify the agent while it runs
   - Inspect state at any time
   - Debug interactively

4. HISTORICAL IMPORTANCE
   - GUI, MVC, IDE all from Smalltalk
   - Influenced Ruby, Python, Objective-C
   - Understanding OOP origins

In the AI era, Smalltalk's message-passing paradigm
is remarkably similar to how we interact with LLMs!"
