;============================= iNES header identifier =============================
    .inesprg 1      ; 1x 16KB PRG code
    .ineschr 1      ; 1x  8KB CHR data
    .inesmap 0      ; Mapper 0 = NROM, no bank swapping
    .inesmir 0      ; BCG Horizontal Mirroring

;=============================  Constants  =============================
BPM            = 90
BPMBLINK       = 3600/(BPM*2) ; bpm blink choose (3600/(BPM*2)), exemple (3600/180) equal 90 bpm blink choose.
SPEEDREADINPUT = 10           ; frames to cooldown control input

;=============================  PPU Registers  =============================
PPUCTRL   = $2000   ; Controller 
PPUMASK   = $2001   ; Mask 
PPUSTATUS = $2002   ; Status 
OAMADDR	  = $2003   ; OAM address
OAMDATA   = $2004   ; OAM data
PPUSCROLL = $2005   ; Scroll 
PPUADDR   = $2006   ; PPU Address 
PPUDATA   = $2007   ; PPU Data 
OAMDMA    = $4014   ; OAM DMA 

;=============================  APU Registers  =============================
; Pulse Channel 1
PULSE1CTRL    = $4000   ; Pulse 1 Control
PULSE1RMPCTRL = $4001   ; Pulse 1 Ramp Control
PULSE1LFT     = $4002   ; Pulse 1 Low bit Frequency
PULSE1HFT     = $4003   ; Pulse 1 High bit Frequency
; Pulse Channel 2
PULSE2CTRL    = $4004   ; Pulse 2 Control
PULSE2RMPCTRL = $4005   ; Pulse 2 Ramp Control
PULSE2LFT     = $4006   ; Pulse 2 Low bit Frequency
PULSE2HFT     = $4007   ; Pulse 2 High bit Frequency
; DMC Channel
DMCIRQ        = $4010   ; DMC IRQ Enable
; Others Uses
APUCTRL       = $4015   ; Control and Status
APUFRMC       = $4017   ; Frame Counter

;=============================  variables   =============================
; Variaveis (RAM Interna do NES)
; ---> Zero Page used here ! $00--$FF (256 Bytes Free RAM) <---
    .zp

reserved        .rs 2   ; Reserved for nes use.
countFrames     .rs 1   ; Counter number of frames 
framesBlink     .rs 1   ; Next Number Frame for Blink (framesBlink = countFrames+#BPMBLINK)
framesInput     .rs 1   ; Next Number Frame for Input (framesInput = countFrames+#BPMBLINK)
inputControl    .rs 2   ; Control Press by Player 0 and 1 (MSB-LSB)==>(A,B,SCL,STRT,UP,DOWN,LEFT,RIGHT)
moveDirPonter   .rs 2   ; Pointer use for repeat same move (LSB,MSB) (Big Endian)
simbols         .rs 9   ; vector contaim tile of ever positon of table game 
turn            .rs 1   ; turn of P0 = 1, P1 = 2, Over = 3
choose          .rs 1   ; Blank Position for next player start option
winner          .rs 1   ; Winner P0 = 1, P1 = 2, Drawn = 3

;=============================  Code Segment   =============================
    .code
    ; Start bank code (CPU $8000-$BFFF) Mirrored to (CPU $C000-$FFFF)
    .bank 0
    .org $8000

;=============================  Non Maskable Interrupt   =============================
;   Vblank generated a NMI(Non Maskable Interrupt), this period is used to 
;    modify sprites data and compute the game.
;===================================================================
NMI:
    ; Call DMA-Sprite
    lda #$02
    sta OAMDMA
    ; Print string of turn player
    jsr PrintTurnPlayer
    ; Blink the selected position to play
    jsr BlinkChoose
    ; Get Input Control Players.
    jsr GetInputControl
    ; Move "cursor" in screen by input control
    jsr MoveChoosePlayer
    ; Select choose 
    jsr SetChoosePlayer
    ; Update Sprites (Blink, Position set, Game over, etc..)
    jsr UpdateSprites 
    ; Reset game after some winner
    jsr ResetEndGame
    ;  Increment Count Frame and Decrement framesInput
    jsr ChangeCounts
    lda #$00
    sta PPUADDR
    sta PPUADDR
    rti

;=============================  Enter Point CPU  =============================
;   Boot game, this is enter point of CPU (start of code game)
;===================================================================
Boot:
    sei
    cld 
    ; Set Stack Ponter
    ldx #$FF
    txs
    inx
    ; Disable graphics and sound
    stx PPUCTRL     ; No NMI Call
    stx PPUMASK     ; No Rendering
    stx DMCIRQ      ; No Sound
    stx APUFRMC     ; No IRQ APU Flag
    ; Waint for new frame
WaitVblank1:
    bit PPUSTATUS
    bpl WaitVblank1
    ; Clear Memory Inside NES ($0000-$07FF) Mirrored to ($0800-$1FFF) (Only Used in Game)
    lda #$00
ClearMemory:
    sta $0000,x ; zero page
    sta $0200,x ; sprites memory
    inx
    bne ClearMemory
    ; Waint again for new frame
WaitVblank2:
    bit PPUSTATUS
    bpl WaitVblank2
;============================= Set Variables =============================
    lda #$01
    sta <turn        ; Player 1 ever start
    lda #$03
    sta <winner      ; Assuming a possible tie
;===================================================================
;   Load Sprites Palettes colors, and BackGrid (Grid Table)
    jsr LoadPalettes
    jsr LoadSprites
    jsr PrintGrid
    ; Take first position clear (this case first position table (new game))
    jsr AnyValidPosition
    ; Load DMA for input sprites inside VRAM (PPU Memory)
    lda #$02
    sta OAMDMA
    ; Enable APU
    lda #%00000011
    sta APUCTRL
    ; Pulse Channel Control Setup
    lda #%10011111
    sta PULSE1CTRL
    sta PULSE2CTRL
    ; Enable NMI in vblank and rendering sprites and backgrounds
    lda #%10001000
    sta PPUCTRL
    lda #%00011110
    sta PPUMASK
    ; Forever loop, do nothing, waint for a vblank and a NMI call
ForeverLoop:
    nop
    jmp ForeverLoop

;===================================================================
;   Load Palettes colors
;
LoadPalettes:
    bit PPUSTATUS
    lda #$3F
    sta PPUADDR
    ldx #$00
    stx PPUADDR
loopPalettes:
    lda Palettes,x
    sta PPUDATA
    inx
    cpx #(EndPalettes-Palettes)
    bne loopPalettes
    rts

;===================================================================
;   Load Sprites
;
LoadSprites:
    ldx #$00
LoopSprites:
    lda Sprites,x
    sta $0200,x
    inx
    cpx #(EndSprites-Sprites)
    bne LoopSprites
    rts

;===================================================================
;  Read Control Players
;
GetInputControl:
    lda #$01
    sta <inputControl+1
    sta $4016
    lda #$00
    sta $4016 
LoopReadControl:
    lda $4016
    lsr a
    rol <inputControl
    lda $4017
    lsr a
    rol <inputControl+1
    bcc LoopReadControl
    rts

;===================================================================
;  Update Sprites (Modify DMA zone $0200-$02FF)
;
UpdateSprites:
    ldy #$00
    ldx #$00
LoopPrint:
    lda <simbols,x
    sta $0201,y
    sta $0205,y
    sta $0209,y
    sta $020D,y
    tya
    clc 
    adc #$10
    tay
    inx
    cpx #$09
    bne LoopPrint
    rts

;===================================================================
;  Get first position Clear, count by rows and coluns
;
AnyValidPosition:
    ldx #$00
LoopFirstClear:
    lda <simbols,x
    beq Found
    inx
    cpx #$09
    bne LoopFirstClear
    lda #$03
    sta <turn
Found:
    stx <choose
    rts

;===================================================================
;  Increment Count Frame and Decrement framesInput
;
ChangeCounts:
    inc <countFrames
    lda <framesInput
    beq NoDecInput
    dec <framesInput
NoDecInput:
    rts

;===================================================================
;  blink sprite select position player (select position)
;
BlinkChoose:
    lda <turn
    cmp #$03
    beq outBlinkChoose
    lda <choose 
    cmp #$09
    beq outBlinkChoose
    lda <countFrames
    cmp <framesBlink
    bne outBlinkChoose
    clc
    adc #BPMBLINK
    sta <framesBlink
    ldx <choose
    lda <simbols,x
    eor <turn
    sta <simbols,x
outBlinkChoose:
    rts

;===================================================================
;  Select Choose Player
;  
SetChoosePlayer:
    lda <framesInput
    beq SetTimeValid
    jmp outSetChoosePlayer
SetTimeValid:
    ldx <turn
    cpx #$03
    beq outSetChoosePlayer
    dex
    lda <inputControl,x
    and #%10000000
    beq outSetChoosePlayer
    ldy <choose
    lda <turn
    sta simbols,y
    ; Swap Turn
    eor #$03
    sta <turn
    jsr VerifyWinner
    ; Get new blank position
    jsr AnyValidPosition
    lda #SPEEDREADINPUT
    sta <framesInput
    jsr Play440
outSetChoosePlayer:
    rts

;===================================================================
;  Move "cursor" do player
;   
MoveChoosePlayer:
    lda <framesInput
    beq MoveTimeValid
    jmp OutChoosePlayer
MoveTimeValid:
    ldy <choose
    cpy #$09
    beq OutChoosePlayer
    ldx <turn
    cpx #$03
    beq OutChoosePlayer
    dex
    lda <inputControl,x
    and #%00001000        ; UP
    beq TryDown
    lda #low(Up)
    sta <moveDirPonter
    lda #high(Up)
    sta <moveDirPonter+1
Up:
    dey
    dey
    dey
    jmp MoveChoose
TryDown:
    lda <inputControl,x
    and #%00000100        ; DOWN
    beq TryLeft
    lda #low(Down)
    sta <moveDirPonter
    lda #high(Down)
    sta <moveDirPonter+1
Down:
    iny
    iny
    iny
    jmp MoveChoose
TryLeft:
    lda <inputControl,x
    and #%00000010        ; LEFT
    beq TryRight
    lda #low(Left)
    sta <moveDirPonter
    lda #high(Left)
    sta <moveDirPonter+1
Left:
    dey
    jmp MoveChoose
TryRight:
    lda <inputControl,x
    and #%00000001        ; RIGHT
    beq OutChoosePlayer
    lda #low(Right)
    sta <moveDirPonter
    lda #high(Right)
    sta <moveDirPonter+1
Right:
    iny
MoveChoose:
    cpy #$00
    bcc OutChoosePlayer
    cpy #$09
    bcs OutChoosePlayer
    lda simbols,y  
    beq ValidPosition
    jmp [moveDirPonter]
ValidPosition:
    ldx <choose
    sta <simbols,x
    sty <choose
    lda #SPEEDREADINPUT
    sta <framesInput
    jsr Play220
OutChoosePlayer:
    rts

;===================================================================
;  Verify end game (winer)
;       obs: future optimal code (more generic)
VerifyWinner:
    lda <turn
    pha
    lda <simbols
    beq LV2
    ; line horizon 1    
    cmp <simbols+1
    bne DiagMain
    cmp <simbols+2
    bne DiagMain
    lda #$03
    sta <turn
    sta <simbols
    sta <simbols+1
    sta <simbols+2
    jmp OutVerifyGame
DiagMain:  ; diagonal main
    cmp <simbols+4
    bne LV1
    cmp <simbols+8
    bne LV1
    lda #$03
    sta <turn
    sta <simbols
    sta <simbols+4
    sta <simbols+8
    jmp OutVerifyGame
LV1:    ; Line Vertical 1
    cmp <simbols+3
    bne LV2
    cmp <simbols+6
    bne LV2
    lda #$03
    sta <turn
    sta <simbols
    sta <simbols+3
    sta <simbols+6
    jmp OutVerifyGame
LV2:    ; Line Vertical 2
    lda <simbols+1
    beq LV3
    cmp <simbols+4
    bne LV3
    cmp <simbols+7
    bne LV3
    lda #$03
    sta <turn
    sta <simbols+1
    sta <simbols+4
    sta <simbols+7
    jmp OutVerifyGame
LV3:    ; Line Vertical 2
    lda <simbols+2
    beq LH2
    cmp <simbols+5
    bne DiagSec
    cmp <simbols+8
    bne DiagSec
    lda #$03
    sta <turn
    sta <simbols+2
    sta <simbols+5
    sta <simbols+8
    jmp OutVerifyGame
DiagSec:    ; Diagonal Secondary
    cmp <simbols+4
    bne LH2
    cmp <simbols+6
    bne LH2
    lda #$03
    sta <turn
    sta <simbols+2
    sta <simbols+4
    sta <simbols+6
    jmp OutVerifyGame
LH2:    ; Line Horizon 2
    lda <simbols+3
    beq LH3
    cmp <simbols+4
    bne LH3
    cmp <simbols+5
    bne LH3
    lda #$03
    sta <turn
    sta <simbols+3
    sta <simbols+4
    sta <simbols+5
    jmp OutVerifyGame
LH3:   ; Line Horizon 3
    lda <simbols+6
    beq OutVerifyGame
    cmp <simbols+7
    bne OutVerifyGame
    cmp <simbols+8
    bne OutVerifyGame
    lda #$03
    sta <turn
    sta <simbols+6
    sta <simbols+7
    sta <simbols+8
    jmp OutVerifyGame
OutVerifyGame:
    pla
    ldx <turn 
    cpx #$03
    bne OutWins
    ; Set a player winner
    eor #$03
    sta <winner
OutWins:
    rts

;===================================================================
;  Reset Game After Some Winner
;
ResetEndGame: 
    lda <turn
    cmp #$03
    bne OutResetGame
    lda <inputControl
    and #%00010000
    bne Reset
    lda <inputControl+1
    and #%00010000
    beq OutResetGame
Reset:
    lda #$00
    ldy #$01
    ldx #$09
    sty <turn
    sta <countFrames
    sty <framesBlink
LoopReset:
    dex
    sta <simbols,x
    bne LoopReset
    lda #$03
    sta <winner
    jsr AnyValidPosition
    jsr Play440
OutResetGame:
    rts
    
;===================================================================
;  Print string turn
;
PrintTurnPlayer:
    lda <turn
    cmp #$03
    beq EndGame
    bit PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$A8
    sta PPUADDR
    ldx #$00
    stx PPUDATA
LoopTurn:
    lda StringTurn,x
    sta PPUDATA
    inx
    cpx #$0C
    bcc LoopTurn
    jsr PrintPlayerNumber
    rts
EndGame:
    lda <winner
    cmp #$03
    beq Drawn
    jsr PrintPlayerWinner
    rts
Drawn:
    jsr PrintDrawn
    rts

;===================================================================
;  Print Number Player 
;
PrintPlayerNumber:
    bit PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$B5
    sta PPUADDR
    lda #$16
    clc 
    adc <turn
    sta PPUDATA
    lda #$00
    sta PPUDATA
    rts

;===================================================================
;  Print string Winner
;
PrintPlayerWinner:
    bit PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$A8
    sta PPUADDR
    ldx #$00
LoopWinner:
    lda StringWinner,X
    sta PPUDATA
    inx
    cpx #$0E
    bcc LoopWinner
    lda #$16
    clc 
    adc <winner
    sta PPUDATA
    rts
    
;===================================================================
;  Print string of drawn
;
PrintDrawn:
    bit PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$A8
    sta PPUADDR
    ldx #$00
    stx PPUDATA
    stx PPUDATA
LoopDrawn:
    lda StringDrawn,x
    sta PPUDATA
    inx
    cpx #$0A
    bcc LoopDrawn
    lda #$00
    sta PPUDATA
    sta PPUDATA
    rts

;===================================================================
;  Print String Created
;
PrintCreated:
    bit PPUSTATUS
    lda #$23
    sta PPUADDR
    lda #$23
    sta PPUADDR
    ldx #$00
    stx PPUDATA
    stx PPUDATA
LoopCreated:
    lda StringCreated,x
    sta PPUDATA
    inx
    cpx #$14
    bcc LoopCreated
    lda #$00
    sta PPUDATA
    sta PPUDATA
    rts

;===================================================================
;  Print Grid (BackGround)
;
PrintGrid:
    ldx #$0E 
    ldy #$02 
    lda #$04 
    sta PPUCTRL
    bit PPUSTATUS
    lda #$21
    sta PPUADDR
    lda #$0C
    sta PPUADDR
    lda #$01 
LoopColuns:
    sta PPUDATA 
    dex
    bne LoopColuns
    lda #$21
    sta PPUADDR
    lda #$11
    sta PPUADDR
    lda #$01
    ldx #$0E
    dey
    bne LoopColuns
    lda #$00 
    sta PPUCTRL
    ldx #$0E 
    ldy #$02 
    lda #$21
    sta PPUADDR
    lda #$88
    sta PPUADDR
    lda #$02 
LoopRows:
    sta PPUDATA 
    dex
    bne LoopRows
    lda #$22
    sta PPUADDR
    lda #$28
    sta PPUADDR
    lda #$02
    ldx #$0E
    dey
    bne LoopRows  
    ldx #$03
    lda #$21
    sta PPUADDR
    lda #$8C
    sta PPUADDR
    stx PPUDATA
    lda #$21
    sta PPUADDR
    lda #$91
    sta PPUADDR
    stx PPUDATA
    lda #$22
    sta PPUADDR
    lda #$2C
    sta PPUADDR
    stx PPUDATA
    lda #$22
    sta PPUADDR
    lda #$31
    sta PPUADDR
    stx PPUDATA
    jsr PrintCreated
    rts

;===================================================================
;  Sounds game
;
Play220:
    lda #%11111011
    sta PULSE1LFT
    lda #%01001001
    sta PULSE1HFT
    rts

Play440:
    lda #%11111101
    sta PULSE2LFT
    lda #%01001000
    sta PULSE2HFT
    rts

;===================================================================
;  Print Grid (BackGround)
;
Sprites:
    .byte 80,0,%10000000,74
    .byte 80,0,%11000000,82
    .byte 72,0,%00000000,74
    .byte 72,0,%01000000,82

    .byte 80,0,%10000000,112
    .byte 80,0,%11000000,120
    .byte 72,0,%00000000,112
    .byte 72,0,%01000000,120

    .byte 80,0,%10000000,152
    .byte 80,0,%11000000,160
    .byte 72,0,%00000000,152
    .byte 72,0,%01000000,160
    ;
    .byte 118,0,%10000000,74
    .byte 118,0,%11000000,82
    .byte 110,0,%00000000,74
    .byte 110,0,%01000000,82

    .byte 118,0,%10000000,112
    .byte 118,0,%11000000,120
    .byte 110,0,%00000000,112
    .byte 110,0,%01000000,120

    .byte 118,0,%10000000,152
    .byte 118,0,%11000000,160
    .byte 110,0,%00000000,152
    .byte 110,0,%01000000,160
    ;
    .byte 158,0,%10000000,74
    .byte 158,0,%11000000,82
    .byte 150,0,%00000000,74
    .byte 150,0,%01000000,82

    .byte 158,0,%10000000,112
    .byte 158,0,%11000000,120
    .byte 150,0,%00000000,112
    .byte 150,0,%01000000,120

    .byte 158,0,%10000000,152
    .byte 158,0,%11000000,160
    .byte 150,0,%00000000,152
    .byte 150,0,%01000000,160
EndSprites:
    ; Label used for auto calculate bytes for write in memory := (EndSprites-Sprites) Bytes

;===================================================================
;  String msgs
;
StringTurn:
    .byte 19,20,17,14,0,16,12,4,22,8,17,0       ; "TURN PLAYER "
StringWinner:
    .byte 21,10,14,14,8,17,0,16,12,4,22,8,17,0  ; "WINNER PLAYER "
StringDrawn:
    .byte 7,17,4,21,14,0,9,4,13,8               ; "DRAWN GAME"
StringCreated:
    .byte 6,17,8,4,19,8,7,0,5,22,0,11,20,12,10,15,17,4,19,18 ; "CREATED BY JULIORATS"

;===================================================================
;  Palettes Color data
;
Palettes:
    .byte $0F
    .byte $30, $0F, $0F
    .byte $0F
    .byte $30, $0F, $0F
    .byte $0F
    .byte $30, $0F, $0F
    .byte $0F
    .byte $30, $0F, $0F

    .byte $0F
    .byte $30, $0F, $0F
    .byte $0F
    .byte $30, $0F, $0F
    .byte $0F
    .byte $30, $0F, $0F
    .byte $0F
    .byte $30, $0F, $0F
EndPalettes

;===================================================================
;  Bank of vectors input address memory
;
    .bank 1
    .org $FFFA

    .dw NMI     ; Non Maskable Interrupt
    .dw Boot    ; Enter Point Address Code

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
    ; Tile 4: A
    .defchr $00111000,\
            $01101100,\
            $11000110,\
            $11000110,\
            $11111110,\
            $11000110,\
            $11000110,\
            $00000000
    ; Tile 5: B
    .defchr $11111100,\
            $11001110,\
            $11001110,\
            $11111100,\
            $11001110,\
            $11001110,\
            $11111100,\
            $00000000
    ; Tile 6: C
    .defchr $01111110,\
            $11100000,\
            $11100000,\
            $11100000,\
            $11100000,\
            $11100000,\
            $01111110,\
            $00000000
    ; Tile 7: D
    .defchr $11111000,\
            $11001100,\
            $11000110,\
            $11000110,\
            $11000110,\
            $11001100,\
            $11111000,\
            $00000000
    ; Tile 8: E
    .defchr $11111110,\
            $11000000,\
            $11000000,\
            $11111100,\
            $11000000,\
            $11000000,\
            $11111110,\
            $00000000
    ; Tile 9: G
    .defchr $00111110,\
            $01100000,\
            $11000000,\
            $11001110,\
            $11000110,\
            $01100110,\
            $00111110,\
            $00000000
    ; Tile 10: I
    .defchr $11111110,\
            $00111000,\
            $00111000,\
            $00111000,\
            $00111000,\
            $00111000,\
            $11111110,\
            $00000000
    ;Tile 11: J
    .defchr $00000110,\
            $00000110,\
            $00000110,\
            $00000110,\
            $11000110,\
            $11000110,\
            $01111100,\
            $00000000
    ; Tile 12: L
    .defchr $11000000,\
            $11000000,\
            $11000000,\
            $11000000,\
            $11000000,\
            $11000000,\
            $11111100,\
            $00000000
    ; Tile 13: M
    .defchr $11000110,\
            $11101110,\
            $11111110,\
            $11111110,\
            $11010110,\
            $11000110,\
            $11000110,\
            $00000000
    ; Tile 14: N
    .defchr $11000110,\
            $11100110,\
            $11110110,\
            $11111110,\
            $11011110,\
            $11001110,\
            $11000110,\
            $00000000
    ;Tile 15: O
    .defchr $01111100,\
            $11000110,\
            $11000110,\
            $11000110,\
            $11000110,\
            $11000110,\
            $01111100,\
            $00000000
    ; Tile 16: P
    .defchr $11111100,\
            $11000110,\
            $11000110,\
            $11000110,\
            $11111100,\
            $11000000,\
            $11000000,\
            $00000000
    ; Tile 17: R
    .defchr $11111100,\
            $11000110,\
            $11000110,\
            $11111100,\
            $11011000,\
            $11001100,\
            $11000110,\
            $00000000
    ;Tile 18: S
    .defchr $01111110,\
            $11000000,\
            $11000000,\
            $01111100,\
            $00000110,\
            $00000110,\
            $11111100,\
            $00000000
    ; Tile 19: T
    .defchr $11111110,\
            $00111000,\
            $00111000,\
            $00111000,\
            $00111000,\
            $00111000,\
            $00111000,\
            $00000000
    ; Tile 20: U
    .defchr $11000110,\
            $11000110,\
            $11000110,\
            $11000110,\
            $11000110,\
            $11000110,\
            $01111100,\
            $00000000
    ; Tile 21: W
    .defchr $11000110,\
            $11000110,\
            $11010110,\
            $11111110,\
            $11111110,\
            $11101110,\
            $11000110,\
            $00000000
    ; Tile 22: Y
    .defchr $11000110,\
            $11000110,\
            $11000110,\
            $11111110,\
            $00111000,\
            $00111000,\
            $00111000,\
            $00000000
    ; Tile 23: 1
    .defchr $00011000,\
            $00111000,\
            $00011000,\
            $00011000,\
            $00011000,\
            $00011000,\
            $01111110,\
            $00000000
    ; Tile 24: 2
    .defchr $01111100,\
            $11000110,\
            $00001110,\
            $00111100,\
            $01111000,\
            $11100000,\
            $11111110,\
            $00000000

;===================================================================
;===================================================================
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