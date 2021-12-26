  ; iNES header identifier
  .inesprg 1      ; 1x 16KB PRG code
  .ineschr 1      ; 1x  8KB CHR data
  .inesmap 0      ; Mapper 0 = NROM, no bank swapping
  .inesmir 0      ; BCG Horizontal Mirroring

;===================  Constants   =============================
BPMBLINK       = (3600/160)     ; bpm blink choose
SPEEDREADINPUT = 20             ; frames to cooldown control input

;===================  variables   =============================
; Variaveis (RAM Interna do NES)
; ---> Zero Page used here ! $00--$FF (256 Bytes Free RAM) <---
  .zp

reserved        .rs 2       ; Reserved for nes use.
countFrames     .rs 1       ; Counter number of frames 
framesBlink     .rs 1       ; Next Number Frame for Blink (framesBlink = countFrames+#BPMBLINK)
framesInput     .rs 1       ; Next Number Frame for Input (framesInput = countFrames+#BPMBLINK)
p0InputControl  .rs 1       ; Control Press by Player 0 (A,B,SCL,STRT,UP,DOWN,LEFT,RIGHT)
p1InputControl  .rs 1       ; Control Press by Player 1 (A,B,SCL,STRT,UP,DOWN,LEFT,RIGHT)
usoGeral        .rs 1       ; Use for stand by data and aux in multiplication method
pontSimbols     .rs 2       ; Pointer for simbols (LSB,MSB) (Little Endian)
pontGeral       .rs 2       ; Pointer for use generic (LSB,MSB) (Little Endian)
simbols         .rs 9       ; vector contaim tile of ever positon of table game 
turn            .rs 1       ; turn of P0 = 1, P1 = 2, Over = 3
choose          .rs 1       ; Blank Position for next player start option

;===================  Code Segment   =============================
    .code
    ; Start bank code (CPU $8000-$BFFF) Mirrored to (CPU $C000-$FFFF)
    .bank 0
    .org $8000

;===================  Non Maskable Interrupt   =============================
;   Vblank generated a NMI(Non Maskable Interrupt), this period is used to 
;    modify sprites data and compute the game.
;===========================================================================
NMI:
    ; Call DMA-Sprite
    lda #$02
    sta $4014
    lda #$0
    sta $2006
    sta $2006
    ; Get Input Control Players.
    jsr InitControl
    ; Move "cursor" in screen by input control
    jsr MoveChoosePlayer
    ; Select choose 
    jsr SetChoosePlayer
    ; Blink the selected position to play
    jsr BlinkChoose
    ; Update Sprites (Blink, Position set, Game over, etc..)
    jsr UpdateSprites
    ; reset game after some winner
    jsr ResetGame
    ; Set Control Register 
    lda #%10001000
    sta $2000
    lda #%00011110
    sta $2001
    inc countFrames
    rti

;=======================  Enter Point CPU  ================================
;   Boot game, this is enter point of CPU (start of code game)
;===========================================================================
Reset:
    sei
    cld 
    ; Set Stack Ponter
    ldx #$FF
    txs
    inx
    ; Disable graphics and sound
    stx $2000       ; No NMI Call
    stx $2001       ; No rendering
    stx $4010       ; No sound
    stx $4017       ; NTSC 60Hz
    ; Waint for new frame
WaitVblank1:
    bit $2002
    bpl WaitVblank1
    ; Clear Memory Inside NES ($0000-$07FF) Mirrored to ($0800-$1FFF)
    lda #0
ClearMemory:
    sta $0000,x
    sta $0100,x
    sta $0200,x
    sta $0300,x
    sta $0400,x
    sta $0500,x
    sta $0600,x
    sta $0700,x
    inx
    bne ClearMemory
    ; Waint again for new frame
WaitVblank2:
    bit $2002
    bpl WaitVblank2
;======================= Set Variables =============================
    lda #1
    sta turn
;===================================================================
;   Load Sprites Palettes colors, and BackGrid (Grid Table)
    jsr LoadPalettes
    jsr LoadSprites
    jsr PrintGrid
    ; Take a blank table (new game)
    jsr UpdateSprites
    ; Take first position clear (this case first position table (new game))
    jsr FirstClear
    ; Load DMA for input sprites inside VRAM (PPU Memory)
    lda #$02
    sta $4014
    lda #%10001000
    sta $2000
    ; Forever loop, do nothing, waint for a vblank and a NMI call
ForeverLoop:
    nop
    jmp ForeverLoop

;===================================================================
;   Load Palettes colors
;
LoadPalettes:
    bit $2002
    lda #$3F
    sta $2006
    ldx #$00
    stx $2006
loopPalettes:
    lda palettes, x
    sta $2007
    inx
    cpx #32
    bne loopPalettes
    rts

;===================================================================
;   Load Sprites
;
LoadSprites:
    ldx #0
LoopSprites:
    lda Sprites,x
    sta $0200,x
    inx
    cpx #144
    bne LoopSprites
    rts

;===================================================================
;  Read Control Players
;
InitControl:
    lda #$01
    sta p0InputControl
    sta p1InputControl
    sta $4016
    lda #$00
    sta $4016 
    clc
loopReadControl:
    lda $4016
    and #1
    beq P0Release
    sec
P0Release: 
    rol p0InputControl
    clc
    lda $4017
    and #1
    beq P1Release
    sec
P1Release:
    rol p1InputControl
    bcc loopReadControl
    rts

;===================================================================
;  Update Sprites (Modify DMA zone $0200-$02FF)
;
UpdateSprites:
    ldy #0
    ldx #0
    lda #low(simbols)
    sta pontSimbols
    lda #high(simbols)
    sta pontSimbols+1
LoopPrint:
    lda [pontSimbols],y
    jsr PrintSimbols
    iny
    cpy #3
    bne LoopPrint
    inc pontSimbols
    inc pontSimbols
    inc pontSimbols
    ldy #0
    inx
    cpx #3
    bne LoopPrint
    rts

;===================================================================
;  Use in UpdateSprites
;
PrintSimbols:
    sta usoGeral ;->
    txa
    pha ;save X
    tya
    pha ;save y
    lda usoGeral ;<-
    pha ;save A
    lda #48
    jsr Mult    
    pha ; -.-
    tya
    tax
    lda #16
    jsr Mult  
    sta usoGeral
    pla ; -^-
    clc
    adc usoGeral
    tax
    pla ;load A
    sta $0201,x
    sta $0205,x
    sta $0209,x
    sta $020D,x

    pla ;load y
    tay
    pla ;load X
    tax
    rts

;===================================================================
;  Mult value in Register A with Register X, save in A
;
Mult:
    dex
    sta usoGeral
    cpx #0
    beq Multout   ; x = 1 
    cpx #$FF        
    bne LoopMult  ; x != 0
    lda #0
    jmp Multout
LoopMult:
    clc
    adc usoGeral
    dex
    bne LoopMult
Multout:
    rts

;===================================================================
;  Get first position Clear, count by rows and coluns
;
FirstClear:
    ldx #0
loopFirstClear:
    lda simbols,x
    cmp #0
    beq find
    inx
    cpx #9
    bne loopFirstClear
    lda #3
    sta turn
find:
    stx choose
    rts

;===================================================================
;  blink sprite select position player (select position)
;
BlinkChoose:
    lda turn
    cmp #3
    beq outBlinkChoose
    lda choose 
    cmp #9
    beq outBlinkChoose
    lda countFrames
    cmp framesBlink
    bne outBlinkChoose
    clc
    adc #BPMBLINK
    sta framesBlink
    ldx choose
    lda simbols,x
    eor turn
    sta simbols,x
outBlinkChoose:
    rts


;===================================================================
;  Select Choose Player
;  
SetChoosePlayer:
    lda framesInput
    cmp #0
    beq setTimeValid
    ;dec framesInput ; MoveChoosePlayer decrement
    jmp outSetChoosePlayer
setTimeValid:
    ldx turn
    cpx #3
    beq outSetChoosePlayer
    dex
    lda p0InputControl,x
    and #%10000000
    beq outSetChoosePlayer
    ldy choose
    lda turn
    sta simbols,y
    eor #3
    ; Swap Turn
    sta turn
    ; Get new blank position
    jsr FirstClear
    ; Verify end game 
    jsr VerifyGame
    lda #SPEEDREADINPUT
    sta framesInput
outSetChoosePlayer:
    rts

;===================================================================
;  Move "cursor" do player
;   
MoveChoosePlayer:
    lda framesInput
    cmp #0
    beq moveTimeValid
    dec framesInput
    jmp outChoosePlayer
moveTimeValid:
    ldy choose
    cpy #9
    beq outChoosePlayer
    ldx turn
    cpx #3
    beq outChoosePlayer
    dex
    lda p0InputControl,x
    and #%00001000        ; UP
    beq tryDown
    lda #low(UP)
    sta pontGeral
    lda #high(UP)
    sta pontGeral+1
    dey
    dey
UP:
    dey
    jmp moveChoose
tryDown:
    lda p0InputControl,x
    and #%00000100        ; DOWN
    beq tryLeft
    lda #low(DOWN)
    sta pontGeral
    lda #high(DOWN)
    sta pontGeral+1
    iny
    iny
DOWN:
    iny
    jmp moveChoose
tryLeft:
    lda p0InputControl,x
    and #%00000010        ; LEFT
    beq tryRight
    lda #low(LEFT)
    sta pontGeral
    lda #high(LEFT)
    sta pontGeral+1
LEFT:
    dey
    jmp moveChoose
tryRight:
    lda p0InputControl,x
    and #%00000001        ; RIGHT
    beq outChoosePlayer
    lda #low(RIGHT)
    sta pontGeral
    lda #high(RIGHT)
    sta pontGeral+1
RIGHT:
    iny
moveChoose:
    cpy #0
    bcc outChoosePlayer
    cpy #9
    bcs outChoosePlayer
    lda simbols,y  
    cmp #0
    beq validPosition
    jmp [pontGeral]
validPosition:
    ldx choose
    sta simbols,x
    sty choose
    lda #SPEEDREADINPUT
    sta framesInput
outChoosePlayer:
    rts

;===================================================================
;  Verify end game (winer or Draw)
;
VerifyGame:
    lda simbols
    cmp #0
    beq LV2
    ; line horizon 1    
    cmp simbols+1
    bne DiagMain
    cmp simbols+2
    bne DiagMain
    lda #3
    sta turn
    sta simbols
    sta simbols+1
    sta simbols+2
    jmp outVerifyGame
DiagMain:  ; diagonal main
    cmp simbols+4
    bne LV1
    cmp simbols+8
    bne LV1
    lda #3
    sta turn
    sta simbols
    sta simbols+4
    sta simbols+8
    jmp outVerifyGame
LV1: ; Line Vertical 1
    cmp simbols+3
    bne LV2
    cmp simbols+6
    bne LV2
    lda #3
    sta turn
    sta simbols
    sta simbols+3
    sta simbols+6
    jmp outVerifyGame
LV2: ; Line Vertical 2
    lda simbols+1
    cmp #0
    beq LV3
    cmp simbols+4
    bne LV3
    cmp simbols+7
    bne LV3
    lda #3
    sta turn
    sta simbols+1
    sta simbols+4
    sta simbols+7
    jmp outVerifyGame
LV3:
    lda simbols+2
    cmp #0
    beq LH2
    cmp simbols+5
    bne DiagSec
    cmp simbols+8
    bne DiagSec
    lda #3
    sta turn
    sta simbols+2
    sta simbols+5
    sta simbols+8
    jmp outVerifyGame
DiagSec:
    cmp simbols+4
    bne LH2
    cmp simbols+6
    bne LH2
    lda #3
    sta turn
    sta simbols+2
    sta simbols+4
    sta simbols+6
    jmp outVerifyGame
LH2:
    lda simbols+3
    cmp #0
    beq LH3
    cmp simbols+4
    bne LH3
    cmp simbols+5
    bne LH3
    lda #3
    sta turn
    sta simbols+3
    sta simbols+4
    sta simbols+5
    jmp outVerifyGame
LH3:
    lda simbols+6
    cmp #0
    beq outVerifyGame
    cmp simbols+7
    bne outVerifyGame
    cmp simbols+8
    bne outVerifyGame
    lda #3
    sta turn
    sta simbols+6
    sta simbols+7
    sta simbols+8
    jmp outVerifyGame
outVerifyGame:
    rts

;===================================================================
;  Reset Game After Some Winner
;
ResetGame: 
    lda turn
    cmp #3
    bne outResetGame
    lda p0InputControl
    and #%00010000
    bne reset
    lda p1InputControl
    and #%00010000
    bne reset
    jmp outResetGame
reset:
    lda #0
    ldy #1
    ldx #9
    sty turn
    sta countFrames
    sty framesBlink
loopReset:
    sta simbols-1,x
    dex
    bne loopReset
    jsr FirstClear
outResetGame:
    rts
;===================================================================
;  Print Grid (BackGround)
;
PrintGrid:
    ldx #14 
    ldy #2 
    lda #$04 
    sta $2000
    bit $2002
    lda #$21
    sta $2006
    lda #$0C
    sta $2006
    lda #1 
LoopColuns:
    sta $2007 
    dex
    bne LoopColuns
    bit $2002
    lda #$21
    sta $2006
    lda #$11
    sta $2006
    lda #1
    ldx #14
    dey
    bne LoopColuns

    lda #$00 
    sta $2000
    ldx #14 
    ldy #2 
    bit $2002
    lda #$21
    sta $2006
    lda #$88
    sta $2006
    lda #2 
LoopRows:
    sta $2007 
    dex
    bne LoopRows
    bit $2002
    lda #$22
    sta $2006
    lda #$28
    sta $2006
    lda #2
    ldx #14
    dey
    bne LoopRows  

    ldx #3
    bit $2002
    lda #$21
    sta $2006
    lda #$8C
    sta $2006
    stx $2007
    bit $2002
    lda #$21
    sta $2006
    lda #$91
    sta $2006
    stx $2007
    bit $2002
    lda #$22
    sta $2006
    lda #$2C
    sta $2006
    stx $2007
    bit $2002
    lda #$22
    sta $2006
    lda #$31
    sta $2006
    stx $2007
    rts

;===================================================================
;  Print Grid (BackGround)
;
Sprites:
    .byte 80,2,%10000000,74
    .byte 80,2,%11000000,82
    .byte 72,2,%00000000,74
    .byte 72,2,%01000000,82

    .byte 80,2,%10000000,112
    .byte 80,2,%11000000,120
    .byte 72,2,%00000000,112
    .byte 72,2,%01000000,120

    .byte 80,1,%10000000,152
    .byte 80,1,%11000000,160
    .byte 72,1,%00000000,152
    .byte 72,1,%01000000,160
    ;
    .byte 118,1,%10000000,74
    .byte 118,1,%11000000,82
    .byte 110,1,%00000000,74
    .byte 110,1,%01000000,82

    .byte 118,1,%10000000,112
    .byte 118,1,%11000000,120
    .byte 110,1,%00000000,112
    .byte 110,1,%01000000,120

    .byte 118,2,%10000000,152
    .byte 118,2,%11000000,160
    .byte 110,2,%00000000,152
    .byte 110,2,%01000000,160
    ;
    .byte 158,3,%10000000,74
    .byte 158,3,%11000000,82
    .byte 150,3,%00000000,74
    .byte 150,3,%01000000,82

    .byte 158,1,%10000000,112
    .byte 158,1,%11000000,120
    .byte 150,1,%00000000,112
    .byte 150,1,%01000000,120

    .byte 158,1,%10000000,152
    .byte 158,1,%11000000,160
    .byte 150,1,%00000000,152
    .byte 150,1,%01000000,160
; Label used for auto calculate bytes for write in memory := (EndSprites-Sprites) Bytes
EndSprites:
;===================================================================
;  Palettes Color data
;
palettes:
    .byte $00
    .byte $30, $0F, $0F
    .byte $00
    .byte $01, $02, $03
    .byte $00
    .byte $04, $05, $07
    .byte $00
    .byte $08, $09, $0A

    .byte $0F
    .byte $30, $0F, $0F
    .byte $00
    .byte $0B, $0C, $01
    .byte $00
    .byte $02, $03, $04
    .byte $02
    .byte $05, $06, $07
;===================================================================
;  Bank of vectors input address memory
;
    .bank 1
    .org $FFFA

    .dw NMI     ; Non Maskable Interrupt
    .dw Reset   ; Enter Point Address Code
    .dw NMI     ; Vectos interrupt (not use)
;===================================================================
;  Bank of CHR memory (VRAM $0000-$1FFF)
;
    .bank 2
    .org $0000

    ; Tile 0: Blank
    .defchr $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000

    ; Tile 1: Vertical Line
    .defchr $00111100,\
            $00111100,\
            $00111100,\
            $00111100,\
            $00111100,\
            $00111100,\
            $00111100,\
            $00111100

    ; Tile 2: Horizontal Line
    .defchr $00000000,\
            $00000000,\
            $11111111,\
            $11111111,\
            $11111111,\
            $11111111,\
            $00000000,\
            $00000000

    ; Tile 3: Cross Line
    .defchr $00111100,\
            $00111100,\
            $11111111,\
            $11111111,\
            $11111111,\
            $11111111,\
            $00111100,\
            $00111100

    .org $1000  
    ; Tile 0: Blank
    .defchr $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000
    ; Tile 1: P1
    .defchr $11111000,\
            $11001000,\
            $10110000,\
            $10110000,\
            $11001000,\
            $00000100,\
            $00000010,\
            $00000001
    ; Tile 2: P2
    .defchr $01101111,\
            $00001001,\
            $10001101,\
            $10001111,\
            $10010000,\
            $10100001,\
            $01000001,\
            $00111100

    ; Tile 3: Over
    .defchr $11111111,\
            $10000001,\
            $10000111,\
            $10001100,\
            $10111001,\
            $11100101,\
            $01000011,\
            $00000101
    ; Tile 4: Blank (for blank over)
    .defchr $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000,\
            $00000000