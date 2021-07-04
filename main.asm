JUMPS
IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
;welcome MSGs
;this is a test commit
msg_2 DB 'hello there, general kenobi$'
Welcome_MSG DB 'Welcome. Please activate caps-lock and select one of the following:$'
E_TO_E DB '[E] for Encryption$'
D_TO_D DB '[D] for Decryption$'
I_TO_I DB '[I] for Information$'
; --------------------------
;INFO strings to print 
INFO_STR_1 DB 'This program presents an implementation of a version of Shamir''s Secret Sharing in Aseembly Language. Shamir''s Secret Sharing works on a basic idea: it takes 2 points to define a line, 3 points to define a parabola (x^2) and so on.$'
INFO_STR_2 DB 'We can take a SECRET and divide it into (N) points. Any subset of (K) points used together can reconstruct the original secret. The proccess for reconstruction is based on the math behind Lagrange Polynomial Interpolation (LPI).$'
INFO_STR_3 DB 'However, the program cannot yet decrypt what it can encrypt due to software limitations. It limits the number (the secret) the user asks to encrypt to 4 digits. That number could be changed, yet it does require more changes to be made.$'
INFO_STR_4 DB 'Simply put, the program takes your desired secret and builds a function where that secret is the "free" coefficent using randomly generated numbers to complete the rest of the function. $'
; --------------------------
;HOW TO DECRYPT MSGs
DEC_STR_1 DB 'Once you obtain your fully formed function, plug in whichever value of X you desire and calculate [N] values according to the function. Then, distribute those points among the secret-keepers.$'
DEC_STR_2 DB 'To recover the original secret, we will use Lagrange Polynomial Interpolation which means we will actually construct a few sub-functions and combine them using some intimidating math. The full function can be found by solving this equation$'
DEC_STR_3 DB '          n         (        n             X-X_i   ) $'
DEC_STR_4 DB 'F(x) = SIGMA * Y_j (      BIG_PI      --------------)$'
DEC_STR_5 DB '         j=0        (  i=0; i =/= j     X_j - X_i  ) $'
; --------------------------
;You Have Selected MSGs
Selected_I DB 'You have selected: INFORMATION.$'
Selected_E DB 'You have selected: ENCRYPTION. Please enter your SECRET (LIMITED TO 4 DIGITS):$' ;(S)
Selected_D DB 'You have selected: DECRYPTION.$'
; --------------------------
Enter_N DB 'Enter N - number of parts for the SECRET:$' ; (N)
Enter_K DB 'Enter K - minimum number of parts of the SECRET needed to re-construct it:$' ;(K)
; --------------------------
;Other MSGs
Your_Secret DB 'Your SECRET = $' ;(S)
CONFIRM_SECRET DB 'CONFIRM [Y/N]?$'
ERROR_MSG DB 'INVAILD INPUT. PLEASE TRY AGAIN:$'
RE_ENTRY DB 'Please re-enter your SECRET$'
FIN_FUNCTION DB 'f(x) = $'
; --------------------------
;Arrays and Integers
CNT_DW DW 0
X_VAL_DW DW 0
X_VAL_DB DB 0
Y_VAL DW 0
K_VAL DW 0
N_VAL DB 0
randomNum dw 0
SECRET DW 0,0,0,0
DIG_1 DW 0
DIG_2 DW 0
DIG_3 DW 0
DIG_4 DW 0
SECRET_NUM DW 0
FUNCTION DW 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DIGIT_CNT dw 0
DIGIT_2 dw 0
; --------------------------
CODESEG
proc random_number_gen
push ax
push cx
push dx
    ; get clock 
    mov ah, 2Ch
    int 21h

    ; dl = millisec
    ; dh = sec
    xor dl, dh ; xor the milliseconds and the seconds together,
               ; to make it more random
     
    xor ax, ax ; clear ax

	; moving the result of the xor to ax
    mov al, dl

    xor dx, dx ; clear dx
    mov cx, 10
	add cx, 1
    div cx ; here dx contains the remainder of the division - 0 to maxNum-1
           ; and the random number we want(up to it -1)

    add dx, 1
	
	mov [randomNum], dx

pop dx
pop cx
pop ax
ret
endp random_number_gen
; --------------------------

proc SHOW_ERROR
	mov dx, offset ERROR_MSG
	mov ah, 9h
	int 21h
	ret
endp SHOW_ERROR

; --------------------------

proc NEW_LINE
	mov dl,10
	mov ah, 2
	int 21h
	ret
endp NEW_LINE

; --------------------------

Welcome:
	
	call random_number_gen
	mov ax, [randomNum]
	mov [FUNCTION+2], ax
	
	mov dx, offset Welcome_MSG
	mov ah, 9h
	int 21h
	call NEW_LINE
	call NEW_LINE
	
	mov dx, offset I_TO_I
	mov ah, 9h
	int 21h
	call NEW_LINE
	
	mov dx, offset E_TO_E
	mov ah, 9h
	int 21h
	call NEW_LINE
	
	mov dx, offset D_TO_D
	mov ah, 9h
	int 21h
	
call NEW_LINE
call NEW_LINE

; --------------------------

proc KEYPRESS_1

	mov ah, 01h
	int 21h
		
	cmp al, 45h ;E = ENCRYPTION
	je ENCRYPTION_Q_LABEL
	cmp al, 49H ; I = INFORMATION
	je PRINT_INFO_LABEL
	cmp al, 44h ;D = DECRYPTION
	je DECRYPTION_LABEL
	
	call NEW_LINE
	call NEW_LINE
	call SHOW_ERROR
	call NEW_LINE
	call NEW_LINE
	jmp welcome
	
	ret
endp KEYPRESS_1

; --------------------------

proc KEYPRESS_2
	mov ah, 01h
	int 21h
	cmp al, 89 ;Y = YES
	je CONTINUE
	cmp al, 78 ;N = NO
	je RE_INPUT
	
	call NEW_LINE
	call NEW_LINE
	call SHOW_ERROR
	call NEW_LINE
	call NEW_LINE
	jmp CONFIRMATION
	
	ret
endp KEYPRESS_2

; --------------------------

proc RE_INPUT
	call NEW_LINE
	call NEW_LINE
	
	mov dx, offset RE_ENTRY
	mov ah, 9h
	int 21h
	
	;WIPE CLEAN [SECRET]
	push si
	push cx
	
	mov si, 0 ;starting value for clearing index
	mov cx, 10
	clean_loop:
		mov [SECRET+si], 0
		inc si
	loop clean_loop
	
	pop si
	pop cx
	
	call NEW_LINE
	call NEW_LINE
	
	jmp INPUT
	
	ret
endp RE_INPUT

; --------------------------

ENCRYPTION_Q_LABEL:
proc ENCRYPTION_Q

call random_number_gen
mov ax, [randomNum]
mov [FUNCTION+4], ax
	
call NEW_LINE
call NEW_LINE

;ENCRYPTION MSG
mov dx, offset Selected_E
mov ah, 9h
int 21h

	call NEW_LINE
	call NEW_LINE

	INPUT:
		push si
		push ax
		push bx
		push dx
		xor dx, dx
		mov dx, 0
		mov [CNT_DW], 0
		
		mov bx, offset SECRET
		mov si, 0
		mov ax, 0
		mov [DIGIT_CNT], 0
	LOOP1:
		;here we cmp between out counter (dx) and our digit limit (4) plus one (5)
		inc dx
		cmp dx, 5 ;1 + digit limit (4) = 5
		je ENDINPUT
		
		inc [DIGIT_CNT]
		
		mov ah, 1
		int 21h
		mov ah, 0
		
		cmp al, 13; ENTER KEY PRESS
		je ENDINPUT
		
		mov [SECRET+si], ax
		add si ,2
		jmp loop1
	ENDINPUT:
	pop dx
	pop bx
	pop ax
	pop si
	
call NEW_LINE
call NEW_LINE

mov dx, offset Your_Secret
mov ah, 9h
int 21h

;OUTPUT
mov si, offset SECRET
dec si
mov cx, 8 ;array size * 2
OUTPUT_LOOP:
mov dx, [si]
mov ah,2
int 21h
inc si
loop OUTPUT_LOOP

call NEW_LINE
call NEW_LINE

CONFIRMATION:
mov dx, offset CONFIRM_SECRET
mov ah, 9h
int 21h

		
call NEW_LINE
call NEW_LINE

call KEYPRESS_2

call random_number_gen
mov ax, [randomNum]
mov [FUNCTION+6], ax

CONTINUE:

call NEW_LINE
call NEW_LINE

N_INPUT:
mov dx, offset Enter_N
mov ah, 9h
int 21h

call NEW_LINE
call NEW_LINE

mov ah, 1h
int 21h


;N_VAL validation
cmp al, 31h ; 0
jl SKIP_N
cmp al, 35h ; 4
jl N_CONTINUE

SKIP_N:
call NEW_LINE
call NEW_LINE
call SHOW_ERROR
MOV [N_VAL], 0
call NEW_LINE
call NEW_LINE
jmp N_INPUT

N_CONTINUE:
mov [N_VAL], al

call NEW_LINE
call NEW_LINE


K_INPUT:
mov dx, offset Enter_K
mov ah, 9h
int 21h

call NEW_LINE
call NEW_LINE

mov ah, 1h
int 21h


;K_VAL validation
cmp al, 31h ; 0
jl SKIP_K
cmp al, 35h ; 4
jl K_CONTINUE

SKIP_K:
call NEW_LINE
call NEW_LINE
call SHOW_ERROR
MOV [K_VAL], 0
call NEW_LINE
call NEW_LINE
jmp K_INPUT

K_CONTINUE:
xor ah, ah
mov [K_VAL], ax
cmp al, [N_VAL]
jge SKIP_K

call NEW_LINE
call NEW_LINE
		
call CONVERT_SECRET
ret
endp ENCRYPTION_Q

; --------------------------

proc CONVERT_SECRET
;DIGIT_CNT = number of digits in secret
mov [SECRET_NUM], 0

mov si, 0
mov ax, [SECRET+si]
sub ax, 48
mov bx, 1000
call Multiply
add [SECRET_NUM], cx
mov [DIG_1],ax
add [DIG_1], 48

mov si, 2
mov ax, [SECRET+si]
sub ax, 48
mov bx, 100
call Multiply
add [SECRET_NUM], cx
mov [DIG_2],ax
add [DIG_2], 48


mov si, 4
mov ax, [SECRET+si]
sub ax, 48
mov bx, 10
call Multiply
add [SECRET_NUM], cx
mov [DIG_2],ax
add [DIG_2], 48


mov si, 6
mov ax, [SECRET+si]
sub ax, 48
mov bx, 1
call Multiply
add [SECRET_NUM], cx
mov [DIG_2],ax
add [DIG_2], 48

call PRINT_FUNCTION
ret
endp CONVERT_SECRET

; --------------------------

proc PRINT_FUNCTION
	
	mov dl, ' '
	mov ah,2
	int 21h
	sub [K_VAL], 48d
	mov [CNT_DW], 0
	mov dx, offset FIN_FUNCTION ;f(x) = 
	mov ah, 9h
	int 21h

	;OUTPUT
	mov si, offset SECRET
	dec si
	mov cx, 8 ;array size * 2
	OUTPUT_LOOP_2:
	mov dx, [si]
	mov ah,2
	int 21h
	inc si
	loop OUTPUT_LOOP_2
	
	mov dl, ' '
	mov ah,2
	int 21h
	
	mov dl, '+'
	mov ah,2
	int 21h
	
	mov dl, ' '
	mov ah,2
	int 21h
	
	mov ah, 2Ch
    int 21h

    ; dl = millisec
    ; dh = sec
	
	mov ax, dx
	call PRINT
	return_point:
	
	mov dl, 'x'
	mov ah,2
	int 21h
	
	mov dl, '^'
	mov ah,2
	int 21h
	
	mov dl, 0 ;PWR
	add dl, 49
	mov ah,2
	int 21h
	
	dec [K_VAL]
	cmp [K_VAL], 1
	je exit
	
	mov dl, ' '
	mov ah,2
	int 21h
	
	mov dl, '+'
	mov ah,2
	int 21h
	
	mov dl, ' '
	mov ah,2
	int 21h
	
	mov ah, 2Ch
    int 21h

    ; dl = millisec
    ; dh = sec
	
	mov ax, dx
	div dl ; al = ax / dl
	call PRINT_2
	return_point_2:
	
	mov dl, 'x'
	mov ah,2
	int 21h
	
	mov dl, '^'
	mov ah,2
	int 21h
	
	mov dl, 1 ;PWR
	add dl, 49
	mov ah,2
	int 21h
	
	dec [K_VAL]
	cmp [K_VAL], 1
	je exit
	
	mov dl, ' '
	mov ah,2
	int 21h
	
	mov dl, '+'
	mov ah,2
	int 21h
	
	mov dl, ' '
	mov ah,2
	int 21h
	
	mov ah, 2Ch
    int 21h

    ; dl = millisec
    ; dh = sec
	
	mov ax, dx
	div dl ; al = ax / dl
	div dl
	call PRINT_3
	return_point_3:
	
	mov dl, 'x'
	mov ah,2
	int 21h
	
	mov dl, '^'
	mov ah,2
	int 21h
	
	mov dl, 2 ;PWR
	add dl, 49
	mov ah,2
	int 21h
	
	dec [K_VAL]
	cmp [K_VAL], 1
	je exit
	
	mov dl, ' '
	mov ah,2
	int 21h
	
	mov dl, '+'
	mov ah,2
	int 21h
	
	mov dl, ' '
	mov ah,2
	int 21h
	
	mov ah, 2Ch
    int 21h

    ; dl = millisec
    ; dh = sec
	
	mov ax, dx
	div dl ; al = ax / dl
	div dl
	div dl
	call PRINT_4
	return_point_4:
	
	mov dl, 'x'
	mov ah,2
	int 21h
	
	mov dl, '^'
	mov ah,2
	int 21h
	
	mov dl, 3 ;PWR
	add dl, 49
	mov ah,2
	int 21h

jmp exit
ret
endp PRINT_FUNCTION

; --------------------------

proc Multiply

cmp ax, 0
je mul_0

cmp bx, 0
je mul_0

jmp mul_skip

mul_0:
mov cx, 0
jmp end_of_mul

mul_skip:
;the function takes 2 values: ax & bx and returns their ax*bx in cx:dx
;ax * bx = cx:dx
mul bx ; dx:ax = ax * bx
mov cx, dx
mov dx, ax
;cx:dx = ax * bx
end_of_mul:
ret
endp Multiply

; --------------------------

proc Power

cmp bx, 0
je power_0

cmp bx, 1
je power_1

jmp CONTINUE_2

power_0:
mov cx, 1
jmp finish

power_1:
mov cx, ax
jmp finish

CONTINUE_2:
;the function takes 2 values: ax & bx and returns ax^bx in cx
;ax ^ bx = cx
;ax = 2; bx = 4 => cx =? 16
mov cx, ax ; cx = 2
Power_Loop:
mul cx ; ax = ax*cx
;bx = 4; ax = 2*2 = 4; cx = 4
;bx = 3; ax = 4*2 = 8; cx = 8
;bx = 2; ax = 8*2 = 16; cx = 16
;bx = 1; returns
mov cx, ax
dec bx
cmp bx, 2
jne Power_Loop

finish:
ret
endp Power

; --------------------------

proc PRINT
    ;this function prints the value in ax
	
    ;initilize count 
    mov cx, 0 
    mov dx, 0
	
    LABEL1: 
        ; if ax is zero 
        cmp ax,0 
        je PRINT1       
          
        ;initilize bx to 10 
        mov bx,10         
          
        ; extract the last digit 
        div bx                   
          
        ;push it in the stack 
        push dx               
          
        ;increment the count 
        inc cx               
          
        ;set dx to 0  
        xor dx,dx 
        jmp LABEL1
		
    PRINT1:
        ;check if count  
        ;is greater than zero 
        cmp cx,0 
        je return_point
          
        ;pop the top of stack 
        pop dx 
        
        ;add 48 so that it  
        ;represents the ASCII 
        ;value of digits 
		add dx,48 
          
        ;interuppt to print a 
        ;character 
        mov ah,02h 
        int 21h 
          
        ;decrease the count 
        dec cx 
        jmp PRINT1 

ret
endp PRINT


proc PRINT_2
    ;this function prints the value in ax
	
    ;initilize count 
    mov cx, 0 
    mov dx, 0
	
    LABEL2: 
        ; if ax is zero 
        cmp ax,0 
        je PRINT2  
          
        ;initilize bx to 10 
        mov bx,10         
          
        ; extract the last digit 
        div bx                   
          
        ;push it in the stack 
        push dx               
          
        ;increment the count 
        inc cx               
          
        ;set dx to 0  
        xor dx,dx 
        jmp LABEL2
		
    PRINT2:
        ;check if count  
        ;is greater than zero 
        cmp cx,0 
        je return_point_2
          
        ;pop the top of stack 
        pop dx 
          
        ;add 48 so that it  
        ;represents the ASCII 
        ;value of digits 
		add dx,48 
          
        ;interuppt to print a 
        ;character 
        mov ah,02h 
        int 21h 
          
        ;decrease the count 
        dec cx 
        jmp PRINT2

ret
endp PRINT_2

proc PRINT_3
    ;this function prints the value in ax
	
    ;initilize count 
    mov cx, 0 
    mov dx, 0
	
    LABEL3: 
        ; if ax is zero 
        cmp ax,0 
        je PRINT3
          
        ;initilize bx to 10 
        mov bx,10         
          
        ; extract the last digit 
        div bx                   
          
        ;push it in the stack 
        push dx               
          
        ;increment the count 
        inc cx               
          
        ;set dx to 0  
        xor dx,dx 
        jmp LABEL3
		
    PRINT3:
        ;check if count  
        ;is greater than zero 
        cmp cx,0 
        je return_point_3
          
        ;pop the top of stack 
        pop dx 
          
        ;add 48 so that it  
        ;represents the ASCII 
        ;value of digits 
		add dx,48 
          
        ;interuppt to print a 
        ;character 
        mov ah,02h 
        int 21h 
          
        ;decrease the count 
        dec cx 
        jmp PRINT3

ret
endp PRINT_3

proc PRINT_4
    ;this function prints the value in ax
	
    ;initilize count 
    mov cx, 0 
    mov dx, 0
	
    LABEL4: 
        ; if ax is zero 
        cmp ax,0 
        je PRINT4 
          
        ;initilize bx to 10 
        mov bx,10         
          
        ; extract the last digit 
        div bx                   
          
        ;push it in the stack 
        push dx               
          
        ;increment the count 
        inc cx               
          
        ;set dx to 0  
        xor dx,dx 
        jmp LABEL4
		
    PRINT4:
        ;check if count  
        ;is greater than zero 
        cmp cx,0 
        je return_point_4
          
        ;pop the top of stack 
        pop dx 
          
        ;add 48 so that it  
        ;represents the ASCII 
        ;value of digits 
		add dx,48 
          
        ;interuppt to print a 
        ;character 
        mov ah,02h 
        int 21h 
          
        ;decrease the count 
        dec cx 
        jmp PRINT4

ret
endp PRINT_4

; --------------------------

PRINT_INFO_LABEL:
proc PRINT_INFO
	call NEW_LINE
	call NEW_LINE
	
	mov dx, offset SELECTED_I
	mov ah, 9h
	int 21h
	
	call NEW_LINE
	call NEW_LINE
	
	mov dx, offset INFO_STR_1
	mov ah, 9h
	int 21h
	
	call NEW_LINE
	call NEW_LINE
	
	mov dx, offset INFO_STR_2
	mov ah, 9h
	int 21h
	
	call NEW_LINE
	call NEW_LINE
	
	mov dx, offset INFO_STR_3
	mov ah, 9h
	int 21h
	
	call NEW_LINE
	call NEW_LINE
	
	mov dx, offset INFO_STR_4
	mov ah, 9h
	int 21h
	
	call NEW_LINE
	call NEW_LINE
	
	jmp exit
	ret
endp PRINT_INFO
	jmp exit
	
; --------------------------

DECRYPTION_LABEL:

call NEW_LINE
call NEW_LINE

mov dx, offset Selected_D ;Selected decryption MSG
mov ah, 9h
int 21h ;inerrupt code for printing strings

call DECRYPTION

proc DECRYPTION

call NEW_LINE
call NEW_LINE

mov dx, offset DEC_STR_1
mov ah, 9h
int 21h

call NEW_LINE
call NEW_LINE

mov dx, offset DEC_STR_2
mov ah, 9h
int 21h

call NEW_LINE
call NEW_LINE

mov dx, offset DEC_STR_3
mov ah, 9h
int 21h

call NEW_LINE
call NEW_LINE

mov dx, offset DEC_STR_4
mov ah, 9h
int 21h
call NEW_LINE
call NEW_LINE
mov dx, offset DEC_STR_5
mov ah, 9h
int 21h
call NEW_LINE
call NEW_LINE
jmp exit

ret
endp DECRYPTION

;ACTUAL CODE START
start:
	mov ax, @data
	mov ds, ax
; --------------------------
		jmp Welcome
; --------------------------

exit:
	mov ax, 4c00h
	int 21h
END start
