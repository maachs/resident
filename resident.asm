.model tiny
.code
org 100h

Start:  jmp Main

FRAME_LEN    equ 14d            ;length of frame
FRAME_HIGH   equ 13d            ;high of frame

FRAME_COLOR  equ 5fh            ;set color to frame
BUFFER_COLOR equ 0fh            ;set color of buffer

NUM_OF_REGS  equ 11d            ;amount of registers

VIDEO_MEM    equ 0b800h         ;video segment

CONSOLE_LEN  equ 80d            ;length of console
CONSOLE_HIGH equ 25d            ;high of console

START_KEY   equ 18h            ;scan code 'o'
TIMER_KEY   equ 14h            ;scan code 't'
DESTR_KEY   equ 0eh            ;scan code 'backspace'

FRAME_PTR    equ (CONSOLE_LEN - FRAME_LEN) * 2 - 2 * 4 + 80 * 2     ;set console ptr
NEW_LINE     equ (CONSOLE_LEN - FRAME_LEN) * 2                      ;set new line
;------------------------------------------------------------------
;MyInt08h - func to create my int08h interrupt
;------------------------------------------------------------------
MyInt08h        proc
                push sp bp si di ss es ds dx cx bx ax           ;save registers

                push cs                                         ;cs = ds
                pop ds

                cmp [TimerFrameFlag], 1d
        jne End_of_Int08h

                push si ax                                      ;save si
                mov si, offset RegistersName
                mov ah, FRAME_COLOR                             ;set color of registers

        call WriteRegisters                                     ;draw registers and their value

                pop ax si                                       ;restoring si

        End_of_Int08h:

                mov al, 20h                                     ;
                out 20h, al                                     ;end of interrupt 21h

                pop ax bx cx dx ds es ss di si bp sp            ;restoring registers

;-------------------------------------------------------------------
;               DEFINE OLD INT 08H
;-------------------------------------------------------------------
                db 0eah
                Old_Int08h_offset  dw 0
                Old_Int08h_segment dw 0

                endp
;------------------------------------------------------------------
;MyInt09h - func to create my int09h interrupt
;------------------------------------------------------------------
MyInt09h        proc
                push sp bp si di ss es ds dx cx bx ax ;save reg

                push cs
                pop ds                                ;mov code segment

                xor ax, ax
                in al, 60h                            ;60h - keyboard

                cmp al, START_KEY
        je Start_draw                                 ;if 'o(open)' draw frame

                cmp al, DESTR_KEY
        je Destr_frame                                ;if 'backspace' draw destroy frame

                cmp al, TIMER_KEY                     ;if 't(timer)' start updating registers
        je Start_updating_reg

        jmp SkipAction

        Start_draw:

                mov ah, FRAME_COLOR                  ;set frame color
                mov si, offset FrameStyle            ;set frame style
        call DrawFrame

                mov si, offset RegistersName         ;name of registers
        call WriteRegisters

        jmp End_of_Int09h


        Destr_frame:

                mov ah, BUFFER_COLOR                ;black color
                mov si, offset DestrStyle           ;set frame style

        call DrawFrame

        jmp End_of_Int09h

        Start_updating_reg:
                xor [TimerFrameFlag], 1            ;set updating mode

        jmp End_of_Int09h

        SkipAction:
                in  al, 61h             ;
                mov ah, al              ;lock keyboard with 61h port
                or  al, 80h             ;
                out 61h, al             ;
                mov al, ah              ;
                out 61h, al             ;

                mov al, 20h             ;end of interrupt with 21h
                out 20h, al             ;

        End_of_Int09h:

        pop ax bx cx dx ds es ss di si bp sp          ;restoring reg
;------------------------------------------------------------------
;               DEFINE OLD INT 09H
;------------------------------------------------------------------
                db 0eah
                Old_Int09h_offset  dw 0
                Old_Int09h_segment dw 0

                endp

;------------------------------------------------------------------
;DrawFrame - draw frame in video mem
;Entry: si - frame style
;Exit: es:[di]
;Destr: cx
;------------------------------------------------------------------
DrawFrame       proc

                push cx
                xor cx, cx                  ;save cx and cx = 0

                push VIDEO_MEM
                pop es                      ;set video segment

                mov di, FRAME_PTR            ;set frame on the center of screen

        call DrawLine                       ;first line

                mov cx, (FRAME_HIGH - 2)    ;how many times rep in cx

        count_center:
                push cx                     ;save cx in stack
                call DrawLine               ;center line
                sub si, 3h
                pop cx
        loop count_center

                add si, 3h

        call DrawLine                       ;last line
                pop cx
                ret
                endp
;-------------------------------------------------------------
;WriteRegisters - name of registers and their values in frame
;Entry: sp - stack pointer,
;Exit: es:[di]
;Destr: bp, di, cx
;-------------------------------------------------------------
WriteRegisters  proc
                push es di cx bp ax                      ;save reg

                push sp
                pop bp                                  ;get actual deep of stack in bp

                add bp, 10d                    ;set bp on ax

                push VIDEO_MEM
                pop es                                  ;set es to video mem

                mov di, FRAME_PTR + NEW_LINE + 32d       ;set video ptr

                mov cx, NUM_OF_REGS             ;amount of reg

        draw_name_reg:
                push cx                 ;save cx
                mov cx, 5d              ;(ax = ) - 5 symbols
        symbols_for_reg:
                lodsb                   ;write in video mem (ax = )
                stosw
        loop symbols_for_reg

                mov bx, ss:[bp]         ;in bx - elem of stack, ss - segment of stack, bp - offset in stack
                add bp, 2d

        call BinToHex

                add di, (CONSOLE_LEN - 5d) * 2d                ;set ptr on text line
                pop cx
        loop draw_name_reg


                pop ax bp cx di es
                ret
                endp
;-------------------------------------------------------------
;BinToHex - convert bin value into hex string
;Entry: bx - bin symbol
;Exit: es:[di]
;Destr: si, dx, es, ax
;-------------------------------------------------------------
BinToHex        proc

                push di si dx cx ax               ;save
                mov si, offset ValRegisters     ;set 00000 to future fill in with actual value of register

                mov cx, 4d                      ;value has 4 symbols

        another_digit:

                mov dx, bx                      ;dx = bx
                and dx, 0fh                     ;logic mul on 1111b
                cmp dx, 9d                      ;check on digit
        jle not_latter

                add dx, 7d                      ;7 symbols between '9' and 'a'
        not_latter:

                add dx, '0'                      ;get ascii code

                mov [si], dl                    ;write in ValRegisters
                inc si
                shr bx, 4d                      ;start with next digit
        loop another_digit

                mov cx, 4d

        draw_val:
                dec si                          ;si--
                mov al, [si]
                mov es:[di], ax                 ;es:[di] = ax(symbol in hex)
                add di, 2
        loop draw_val

                pop ax cx dx si di                 ;restoring
                ret
                endp

;-------------------------------------------------------------
;DrawLine - function to draw into video mem
;Entry: ah - color, dx - param of frame
;Exit: es:[di]
;Destroys: ax, cx
;-------------------------------------------------------------
DrawLine        proc

                xor bx, bx

                lodsb                   ;left elem
                stosw

                mov cx, FRAME_LEN
                sub cx, 2d              ;how many times repeat

                lodsb                   ;center elem
                rep stosw

                lodsb                   ;right elem
                stosw

                add di, NEW_LINE

                ret
                endp

;---------------------------------------------------------------------
;                FRAME STYLE AND VARIABLES
;---------------------------------------------------------------------
TimerFrameFlag       db 0                               ;1 - show frame
FrameStyle           db 0c9h, 0cdh, 0bbh, 0bah, 32d, 0bah, 0c8h, 0cdh, 0bch
DestrStyle           db 9d dup(3d)
RegistersName        db "Ax = ", "Bx = ", "Cx = ", "Dx = ", "Ds = ", "Es = ", "Ss = ", "Di = ", "Si = ", "Bp = ", "Sp = "
ValRegisters         db 4d dup(0)
;---------------------------------------------------------------------
;               SAVING UP TO HER
;---------------------------------------------------------------------
SaveCode:

Main:
                push 0
                pop es                          ;

                mov ax, 3509h                   ;get old interrupt vector 09h
                int 21h                         ;in bx - offset, es - segment

                mov Old_Int09h_offset,  bx
                mov Old_Int09h_segment, es      ;save old interrupt vector

                cli                             ;clear int flags

                mov ax, 2509h                   ;set int 25h
                push cs
                pop ds                           ;save code segment
                lea dx, MyInt09h
                int 21h

                sti                            ;restoring int flags

                mov ax, 3508h                   ;get old interrupt vector 08h
                int 21h                         ;in bx - offset, es - segment

                mov Old_Int08h_offset, bx
                mov Old_Int08h_segment, es      ;save old interrupt vector

                cli

                mov ax, 2508h                   ;set my int08h
                push cs
                pop ds                          ;save code segment
                lea dx, MyInt08h
                int 21h

                sti

                lea dx, SaveCode         ;saving code from start to SaveCode
                shr dx, 4
                inc dx

                mov ax, 3100h                   ;dos function 31
                int 21h

end         Start
