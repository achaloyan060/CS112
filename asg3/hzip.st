#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f


nl := Character nl.

FileStream extend [
    nl := Character nl.
    catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Character extend [
   asHexString [
      |int hexchars|
      hexchars := '0123456789ABCDEF'.
      int := self asInteger.
      ^ (hexchars at: (int quo: 16) + 1) asString,
        (hexchars at: (int rem: 16) + 1) asString.
   ]
].

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]


Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [^ true ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   postorder: visitor [
        |ascii|
        ascii := char asInteger.
        visitor writeBit: 0.
        8 to: 1 by: -1 do: [:index |
            |currBit|
             currBit := ascii bitAt: index.
             visitor writeBit: currBit.
        ].
        ascii =  256 ifTrue: [
            visitor writeBit: 1.
        ].
    ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   left  [ ^ left]
   right [ ^ right]
   isLeaf [^ false]
   
   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   postorder: visitor [
      left postorder: visitor.
      right postorder: visitor.
      visitor writeBit: 1.
    ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
   
   writeBitString: bitString [
        bitString do: [:index |
            index asString = '0'
                ifTrue: [self writeBit: 0]
                ifFalse: [self writeBit: 1].
        ].
    ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

Smalltalk arguments: '-d -c -t -u'
do: [:opt :arg |

    argSize := Smalltalk arguments size.

    ((argSize = 2) | (argSize = 3)) ifTrue: [

    input := FileStream open: (Smalltalk arguments at: 2)
                        mode: FileStream read.
    
    (Smalltalk arguments size ) > 2 ifTrue: [
        outname := Smalltalk arguments at: 3. 
        ] ifFalse: [
            outname := stdout].

    opt = $d ifTrue: [
        stdout << 'Input file: ' << (Smalltalk arguments at: 2) << nl.
        (Smalltalk arguments size ) > 2 ifTrue: [
            stdout << 'Output file: ' << (Smalltalk arguments at: 3).
        ].
    ].

    opt = $u ifTrue: [
        in := BitStream new: input.
        byte := 0.
        stack := OrderedCollection new.
        finished := false.

        [(in atEnd not) & (finished = false)] whileTrue: [
            |bit|
            bit := in nextBit.
            bit = 0 ifTrue: [
                byte := 0.
                8 to: 1 by: -1 do: [:index |
                    bit := in nextBit.
                    byte := byte bitAt: index put: bit.
                ].
        byte = 0 ifTrue: [
            bit := in nextBit.
            bit = 1 ifTrue: [
                byte := byte + 256.
            ].
        ].
        stack addFirst: (Leaf new: byte asCharacter count: 1).
    ] ifFalse: [ 

        stack size = 1 ifTrue: [
                        finished := true.
                    ] ifFalse: [
                        node1 := stack removeFirst.
                        node0 := stack removeFirst.
                        newTree := Tree new: (node1 char) count: 1 
                                        left: node0 right: node1.
                        stack addFirst: newTree.
                    ].
            ].
    ].
        root := stack removeFirst.
        node := root.
        finished := false.
        [(in atEnd not) & (finished = false)] whileTrue: [
            |bit1|
            bit1 := in nextBit.
            bit1 = 0 ifTrue: [
                (node isLeaf) ifTrue: [
                    node char asInteger = 256 ifTrue: [
                        finished := true. ]
                ] ifFalse: [   
                    node := node left.
                    (node isLeaf) ifTrue: [
                        node char asInteger = 256 ifTrue: [
                            finished := true. ] ifFalse: [
                                stdout << node char. 
                            ].
                        node := root.
                    ].
        ].

    ] ifFalse: [
        (node isLeaf) ifTrue: [  
            node char asInteger = 256 ifTrue: [
            finished := true. ]
        ] ifFalse: [    
            node := node right.
            (node isLeaf) ifTrue: [
                node char asInteger = 256 ifTrue: [
                    finished := true. ] ifFalse: [
                        stdout << node char. 
                    ].
                    node := root.
                ].
            ].
        ].
        ]. 
    ].

    opt = $t ifTrue: [
        freqtable := ZeroArray new: 257.

        [input atEnd not] whileTrue: [
            |ordChar|
            ordChar := input next asInteger.
            freqtable incr: ordChar.
        ].
        freqtable at: 256 put: 1.

        queue := SortedCollection new.

        freqtable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
            |node|
            node := Leaf new: key count: value.
            queue add: node
        ].
    ].

        [queue size >= 2] whileTrue: [
            |node1 node2|
            node1 := queue removeFirst.
            node2 := queue removeFirst.
            queue add: (Tree new: (node1 char) 
                  count: ((node1 count)+(node2 count))
                  left: node1 right: node2).
        ].

        encodeString := ZeroArray new: 257.
        tree := queue first.

        tree inorder: [:char :string | encodeString at: (char asInteger)
                                                    put: string.]
             prefix: ''.

    encodeString keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
        |count|
        key = 256 ifTrue: [
            stdout << 'EOF'.
            ] ifFalse: [
                (key asInteger) < 127 ifTrue: [
                (key asInteger) > 32 ifTrue: [
                    stdout << ' ' << key asCharacter asString << ' '. ]
                ifFalse: [
                    stdout << 'x' << key asCharacter asHexString.].
                ].
            ].
            count := freqtable at: key.
            count := (count printPaddedWith: $ to: 8).
            stdout << count << '  '.
            stdout << value << nl.
            ].
        ]
    ].

    opt = $c ifTrue: [
        freqtable := ZeroArray new: 257.

        [input atEnd not] whileTrue: [
            |ordChar|
            ordChar := input next asInteger.
            freqtable incr: ordChar.
        ].

        freqtable at: 256 put: 1.
 
        queue := SortedCollection new.

        freqtable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
            |node|
            node := Leaf new: key count: value.
            queue add: node
        ].
    ].

    [queue size >= 2] whileTrue: [
        |node1 node2|
        node1 := queue removeFirst.
        node2 := queue removeFirst.
        queue add: (Tree new: (node1 char) 
              count: ((node1 count)+(node2 count))
              left: node1 right: node2).
        ].

    encodeString := ZeroArray new: 257.
    tree := queue first.
    tree inorder: [:char :string | encodeString at: (char asInteger) 
                                                put: string.]
         prefix: ''.

    outfile := FileStream open: outname
                          mode: FileStream write.
    output := OutBits new: outfile.
    tree postorder: output.
    output writeBit: 1. 

    input reset.
    [input atEnd not] whileTrue: [
        |ordChar|
        ordChar := input next asInteger.
        output writeBitString: (encodeString at: ordChar).
    ].
    output writeBitString: (encodeString at: 256).
    output flushByte.

    ].
  ] ifFalse: [
        stdout << 'Error: wrong number of arguments'. 
        ObjectMemory quit ].
].

