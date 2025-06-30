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
OAMADDR   = $2003   ; OAM address
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
APUFRMC       = $4017   ; APU Frame Counter

;=============================  variables   =============================
; Variables (NES Internal RAM)
; ---> Zero Page used here ! $00--$FF (256 Bytes Free RAM) <---
    .zp

framesToBlink    .ds 1   ; Number of Frames to Toggle Blink (framesToBlink = #BPMBLINK)
coolDownInput    .ds 1   ; Number of Frames Input Cooldown  (coolDownInput = #SPEEDREADINPUT)
inputControl     .ds 2   ; Control Press by Player 0 and 1 (MSB-LSB)==>(A,B,SCL,STRT,UP,DOWN,LEFT,RIGHT)
moveDirPonter    .ds 2   ; Pointer use for repeat same move (LSB,MSB) (Big Endian)
simbols          .ds 9   ; vector contaim tile of ever positon of table game
turn             .ds 1   ; turn of P0 = 1, P1 = 2, Over = 3
lastGameTurn     .ds 1   ; turn of P0 = 1, P1 = 2
choose           .ds 1   ; Blank Position for next player start option
winner           .ds 1   ; Winner P0 = 1, P1 = 2, Drawn = 3
mutexNMIClear    .ds 1   ; Mutex to synchronize NMI
flagBGChange     .ds 1   ; Flag indicator BG change
flagSpriteChange .ds 1   ; Flag indicator Sprite change

; RAM section ($0200-$07FF):
    .bss

BufferDMA        .ds 256 ; DMA Buffer $0200 -- $02FF (256 Bytes / 64 Sprites)

;=============================  Code Segment   =============================
    .code
    ; Start bank code (CPU $8000-$BFFF) Mirrored to (CPU $C000-$FFFF)
    .bank 0
    .org $8000

;=============================  Non Maskable Interrupt   =============================
;   Vblank generated a NMI(Non Maskable Interrupt), this period is used to
;    modify sprites data and PPU VRAM.
;===================================================================
NMI:
    ; Save Registers in Possible Race Condition
    pha
    tya
    pha
    txa
    pha

    ; Call DMA-Sprite
    lda #high(BufferDMA)
    sta OAMDMA

    ; Check if there was a shift change using Sync Flag
    lda <flagBGChange
    bne noTurnChange
    ; Indicates synchronization that has already consumed the turn change
    inc <flagBGChange
    ; Print string of turn player
    jsr PrintTurnPlayer
noTurnChange:

    lda #$00
    ; is not zero, the process of starting sprite evaluation triggers
    ;  an OAM hardware refresh bug that causes the 8 bytes beginning
    ;  at OAMADDR & $F8 to be copied and replace the first 8 bytes of OAM.
    sta OAMADDR
    bit PPUSTATUS
    ; The PPU scroll registers share internal state with the PPU address registers.
    ; Because of this, PPUSCROLL and the nametable bits in PPUCTRL should be written
    ;  after any writes to PPUADDR.
    ; No Scroll Screen
    sta PPUSCROLL
    sta PPUSCROLL
    ; Enable NMI in VBlank
    lda #%10001000
    sta PPUCTRL

    ; Free Mutex
    inc <mutexNMIClear
    ; Restore Registers in Possible Race Condition
    pla
    tax
    pla
    tay
    pla
    ; End of NMI
    rti

;=============================  Enter Point CPU  =============================
;   Boot game, this is enter point of CPU (start of code game)
;===================================================================
Boot:
    ; Ignore IRQs
    sei
    cld
    ; Disable sound
    ldx #$40
    stx APUFRMC      ; Set IRQ APU Flag
    ; Set Stack Ponter
    ldx #$FF
    txs
    txa              ; x==$FF --> a:=x
    inx
    ; Safe Code for Resets -- PPU disabled
    stx PPUMASK
    stx PPUCTRL
    stx DMCIRQ       ; disable DMC IRQs
    ; Race condition
    ;  The vblank flag is in an unknown state after reset,
    ;  so it is cleared here to make sure that WaitVblank1:
    ;  does not exit immediately.
    bit PPUSTATUS
WaitVblank1:
    bit PPUSTATUS
    bpl WaitVblank1
    ; Clear Memory Inside NES ($0000-$07FF) Mirrored to ($0800-$1FFF) (Only Used in Game)
    ldy #$00
ClearMemory:
    sty <$00,x       ; zero page
    sta BufferDMA,x  ; OAM Buffer RAM
    inx
    bne ClearMemory
    ; Waint again for complete frame
WaitVblank2:
    bit PPUSTATUS
    bpl WaitVblank2

;============================= Set Variables =============================
    lda #$01
    sta <turn        ; Player 1 ever start
    sta <lastGameTurn
    lda #$03
    sta <winner      ; Assuming a possible tie
;===================================================================

    ; Load Sprites Palettes colors, and BackGrid (Grid Table)
    jsr LoadPalettes
    jsr LoadSprites
    jsr LoadATtable
    jsr PrintGrid
    ; Start in Center Position
    lda #$04
    sta choose
    ; Load DMA for input sprites inside VRAM (OAM Primary Memory)
    lda #high(BufferDMA)
    sta OAMDMA
    ; Enable APU
    lda #%00000011
    sta APUCTRL
    ; Pulse Channel Control Setup
    lda #%10011111
    sta PULSE1CTRL
    sta PULSE2CTRL
    ; No Scroll Screen
    lda #$00
    bit PPUSTATUS
    sta PPUSCROLL
    sta PPUSCROLL
    ; Enable Rendering Sprites and Backgrounds
    lda #%00011110
    sta PPUMASK
    ; Enable NMI in VBlank
    lda #%10001000
    sta PPUCTRL

    ; Forever loop, Logic game, waint for a Vblank and NMI call
ForeverLoop:
    ; Mutex to synchronize data change after NMI
    lda <mutexNMIClear
waitNMIMutex:
    cmp <mutexNMIClear
    beq waitNMIMutex

    ; Get Input Control Players
    jsr GetInputControl

    lda <turn
    cmp #$03
    bne noEndGame
    ; Reset game after some winner
    jsr ResetEndGame
    jmp jmpEndGame
noEndGame:
    ; Blink the selected position to play
    jsr BlinkChoose
    ; Move "cursor" in screen by input control
    jsr MoveChoosePlayer
    ; Select choose
    jsr SetChoosePlayer
jmpEndGame:

    lda <flagSpriteChange
    beq noChangeSprites
    dec <flagSpriteChange
    ; Update Sprites (Blink, Position set, Game over, etc..)
    jsr UpdateSprites
noChangeSprites:

    ; Increment Count Frame and Decrement coolDownInput
    jsr ChangeCounts

    jmp ForeverLoop

;===================================================================
;   Load Palettes colors
;
LoadPalettes:
    bit PPUSTATUS
    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR
    ldy #$08
nextPalette:
    ldx #$04
loopPalettes:
    lda Palettes-1,x
    sta PPUDATA
    dex
    bne loopPalettes
    dey
    bne nextPalette
    rts

;===================================================================
;   Load Sprites
;
LoadSprites:
    ldx #(EndSprites-Sprites)
loopSprites:
    lda Sprites-1,x
    sta BufferDMA-1,x
    dex
    bne loopSprites
    rts

;===================================================================
;  Read Control Players
;
GetInputControl:
    lda #$01
    sta $4016
    sta <inputControl+1
    lsr a
    sta $4016
loopReadControl:
    lda $4016
    and #$03
    cmp #$01
    rol <inputControl
    lda $4017
    and #$03
    cmp #$01
    rol <inputControl+1
    bcc loopReadControl
    rts

;===================================================================
;  Update Sprites (Modify DMA zone $0200-$02FF)
;
UpdateSprites:
    ldy #$80
    ldx #$09
loopPrint:
    lda <simbols-1,x
    sta BufferDMA+1,y
    sta BufferDMA+5,y
    sta BufferDMA+9,y
    sta BufferDMA+13,y
    tya
    sec
    sbc #$10
    tay
    dex
    bne loopPrint
    rts

;===================================================================
;  Get first position Clear, count by rows and coluns
;
AnyValidPosition:
    ldx #$00
loopFirstClear:
    lda <simbols,x
    beq found
    inx
    cpx #$09
    bne loopFirstClear
    lda #$03
    sta <turn
found:
    stx <choose
    rts

;===================================================================
;  Decrement Frame Counters
;
ChangeCounts:
    lda <framesToBlink
    beq noDecBlink
    dec <framesToBlink
noDecBlink:
    lda <coolDownInput
    beq noDecInput
    dec <coolDownInput
noDecInput:
    rts

;===================================================================
;  blink sprite select position player (select position)
;
BlinkChoose:
    lda <choose
    cmp #$09
    beq outBlinkChoose
    lda <framesToBlink
    bne outBlinkChoose
    lda #BPMBLINK
    sta <framesToBlink
    ldx <choose
    lda <simbols,x
    eor <turn
    sta <simbols,x
    ; Flag to Change Sprites enable
    inc <flagSpriteChange
outBlinkChoose:
    rts

;===================================================================
;  Select Choose Player
;
SetChoosePlayer:
    lda <coolDownInput
    bne outSetChoosePlayer
    ldx <turn
    lda <inputControl-1,x
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
    sta <coolDownInput
    ; Flag to Change Sprites enable
    inc <flagSpriteChange
    ; Indicates to NMI synchronization that there is a shift change to process
    dec <flagBGChange
    jmp Play440
outSetChoosePlayer:
    rts

;===================================================================
;  Move "cursor" do player
;
MoveChoosePlayer:
    lda <coolDownInput
    bne outChoosePlayer
    ldy <choose
    cpy #$09
    beq outChoosePlayer
    ldx <turn
    dex
    lda <inputControl,x
    and #%00001000        ; UP
    beq tryDown
    lda #low(up)
    sta <moveDirPonter
    lda #high(up)
    sta <moveDirPonter+1
up:
    dey
    dey
    dey
    jmp moveChoose
tryDown:
    lda <inputControl,x
    and #%00000100        ; DOWN
    beq tryLeft
    lda #low(down)
    sta <moveDirPonter
    lda #high(down)
    sta <moveDirPonter+1
down:
    iny
    iny
    iny
    jmp moveChoose
tryLeft:
    lda <inputControl,x
    and #%00000010        ; LEFT
    beq tryRight
    lda #low(left)
    sta <moveDirPonter
    lda #high(left)
    sta <moveDirPonter+1
left:
    dey
    jmp moveChoose
tryRight:
    lda <inputControl,x
    and #%00000001        ; RIGHT
    beq outChoosePlayer
    lda #low(right)
    sta <moveDirPonter
    lda #high(right)
    sta <moveDirPonter+1
right:
    iny
moveChoose:
    cpy #$00
    bcc outChoosePlayer
    cpy #$09
    bcs outChoosePlayer
    lda simbols,y
    beq validPosition
    jmp [moveDirPonter]
validPosition:
    ldx <choose
    sta <simbols,x
    sty <choose
    lda #SPEEDREADINPUT
    sta <coolDownInput
    lda #$00
    sta <framesToBlink
    ; Flag to Change Sprites enable
    inc <flagSpriteChange
    jmp Play220
outChoosePlayer:
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
    jmp outVerifyGame
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
    jmp outVerifyGame
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
    jmp outVerifyGame
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
    jmp outVerifyGame
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
    jmp outVerifyGame
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
    jmp outVerifyGame
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
    jmp outVerifyGame
LH3:   ; Line Horizon 3
    lda <simbols+6
    beq outVerifyGame
    cmp <simbols+7
    bne outVerifyGame
    cmp <simbols+8
    bne outVerifyGame
    lda #$03
    sta <turn
    sta <simbols+6
    sta <simbols+7
    sta <simbols+8
    jmp outVerifyGame
outVerifyGame:
    pla
    ldx <turn
    cpx #$03
    bne outWins
    ; Set a player winner
    eor #$03
    sta <winner
outWins:
    rts

;===================================================================
;  Reset Game After Some Winner
;
ResetEndGame:
    lda <inputControl
    and #%00010000
    bne reset
    lda <inputControl+1
    and #%00010000
    beq outResetGame
reset:
    lda <lastGameTurn
    eor <turn
    sta <turn
    sta <lastGameTurn
    lda #$00
    sta <framesToBlink
    ldx #$09
loopReset:
    dex
    sta <simbols,x
    bne loopReset
    lda #$03
    sta <winner
    lda #$04
    sta choose
    ; Flag to Change Sprites enable
    inc <flagSpriteChange
    ; Indicates to NMI synchronization that there is a shift change to process
    dec <flagBGChange
    jmp Play440
outResetGame:
    rts

;===================================================================
;  Print string turn
;
PrintTurnPlayer:
    lda <turn
    cmp #$03
    beq endGame
    bit PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$A8
    sta PPUADDR
    lda #$00
    sta PPUDATA
    ldx #$0C
loopTurn:
    lda StringTurn-1,x
    sta PPUDATA
    dex
    bne loopTurn
    lda #$16
    clc
    adc <turn
    sta PPUDATA
    stx PPUDATA
    rts
endGame:
    lda <winner
    cmp #$03
    beq drawn
    jmp PrintPlayerWinner
    ; rts
drawn:
    jmp PrintDrawn
    ; rts

;===================================================================
;  Print string Winner
;
PrintPlayerWinner:
    bit PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$A8
    sta PPUADDR
    ldx #$0E
loopWinner:
    lda StringWinner-1,X
    sta PPUDATA
    dex
    bne loopWinner
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
    lda #$A9
    sta PPUADDR
    lda #$00
    sta PPUDATA
    sta PPUDATA
    ldx #$0A
loopDrawn:
    lda StringDrawn-1,x
    sta PPUDATA
    dex
    bne loopDrawn
    stx PPUDATA
    stx PPUDATA
    rts

;===================================================================
;  Print String Created
;
PrintCreated:
    bit PPUSTATUS
    lda #$23
    sta PPUADDR
    lda #$24
    sta PPUADDR
    lda #$00
    sta PPUDATA
    sta PPUDATA
    ldx #$14
loopCreated:
    lda StringCreated-1,x
    sta PPUDATA
    dex
    bne loopCreated
    rts

;===================================================================
;  Print Grid (BackGround)
;
PrintGrid:
    ldx #$0E
    ldy #$02
    ; PPUADDR 32 bytes increment
    lda #$04
    sta PPUCTRL
    bit PPUSTATUS
    lda #$21
    sta PPUADDR
    lda #$0D
    sta PPUADDR
    lda #$01
loopColuns:
    sta PPUDATA
    dex
    bne loopColuns
    lda #$21
    sta PPUADDR
    lda #$12
    sta PPUADDR
    lda #$01
    ldx #$0E
    dey
    bne loopColuns
    ; PPUADDR 1 bytes increment
    lda #$00
    sta PPUCTRL
    ldx #$0E
    ldy #$02
    lda #$21
    sta PPUADDR
    lda #$89
    sta PPUADDR
    lda #$02
loopRows:
    sta PPUDATA
    dex
    bne loopRows
    lda #$22
    sta PPUADDR
    lda #$29
    sta PPUADDR
    lda #$02
    ldx #$0E
    dey
    bne loopRows
    ldx #$03
    lda #$21
    sta PPUADDR
    lda #$8D
    sta PPUADDR
    stx PPUDATA
    lda #$21
    sta PPUADDR
    lda #$92
    sta PPUADDR
    stx PPUDATA
    lda #$22
    sta PPUADDR
    lda #$2D
    sta PPUADDR
    stx PPUDATA
    lda #$22
    sta PPUADDR
    lda #$32
    sta PPUADDR
    stx PPUDATA
    jmp PrintCreated
    ; rts

;===================================================================
;  Load PPU attribute tables with zero (Palette 0)
;
LoadATtable:
    bit PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$00
    sta PPUADDR

    lda #$00
    ldx #$00
    ldy #$04
loopClear:
    sta PPUDATA
    dex
    bne loopClear
    dey
    bne loopClear
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
    .byte 72,0,%01000000,90
    .byte 72,0,%00000000,82
    .byte 80,0,%11000000,90
    .byte 80,0,%10000000,82

    .byte 72,0,%01000000,128
    .byte 72,0,%00000000,120
    .byte 80,0,%11000000,128
    .byte 80,0,%10000000,120

    .byte 72,0,%01000000,168
    .byte 72,0,%00000000,160
    .byte 80,0,%11000000,168
    .byte 80,0,%10000000,160

    ;

    .byte 110,0,%01000000,90
    .byte 110,0,%00000000,82
    .byte 118,0,%11000000,90
    .byte 118,0,%10000000,82

    .byte 110,0,%01000000,128
    .byte 110,0,%00000000,120
    .byte 118,0,%11000000,128
    .byte 118,0,%10000000,120

    .byte 110,0,%01000000,168
    .byte 110,0,%00000000,160
    .byte 118,0,%11000000,168
    .byte 118,0,%10000000,160

    ;

    .byte 150,0,%01000000,90
    .byte 150,0,%00000000,82
    .byte 158,0,%11000000,90
    .byte 158,0,%10000000,82

    .byte 150,0,%01000000,128
    .byte 150,0,%00000000,120
    .byte 158,0,%11000000,128
    .byte 158,0,%10000000,120

    .byte 150,0,%01000000,168
    .byte 150,0,%00000000,160
    .byte 158,0,%11000000,168
    .byte 158,0,%10000000,160
EndSprites:
    ; Label used for auto calculate bytes for write in memory := (EndSprites-Sprites) Bytes

;===================================================================
;  String msgs
;
StringTurn:
    .byte 0,17,8,22,4,12,16,0,14,17,20,19       ; "TURN PLAYER "
StringWinner:
    .byte 0,17,8,22,4,12,16,0,17,8,14,14,10,21  ; "WINNER PLAYER "
StringDrawn:
    .byte 8,13,4,9,0,14,21,4,17,7               ; "DRAWN GAME"
StringCreated:
    .byte 18,19,4,17,15,10,12,20,11,0,22,5,0,7,8,19,4,8,17,6 ; "CREATED BY JULIORATS"

;===================================================================
;  Palettes Color data
;
Palettes:
    .byte $0F,$0F,$30,$0F

;===================================================================
;  Bank of vectors input address memory
;
    .bank 1
    .org $BFFA

    .dw NMI     ; Non-Maskable Interrupt (NMI)
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