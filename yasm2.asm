; Kompiliuojame (linux terminale): yasm nasmpvz1.asm -fbin -o nasmpvz1.com
; Paleidimas (dosbox terminale): nasmpvz1.com
%include 'yasmmac.inc'          ; Pagalbiniai makrosai
;------------------------------------------------------------------------
org 100h                        ; visos COM programos prasideda nuo 100h
                                ; Be to, DS=CS=ES=SS !

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
section .text                   ; kodas prasideda cia 

    startas:                     ; nuo cia vykdomas kodas
	
	macPutString ' Jaunius Tamulevicius, PS 1 grupe, ', crlf, '$'
	
	xor ax, ax
	mov ax, [082h]
	cmp ax, 0
	je err1
		ivedimo_nuskaitymas:
		mov ax, [082h + bx]
		cmp ax, 0Dh
		je ivedimo_pabaiga
			mov word [file_in + bx], ax
			inc bx
			jmp ivedimo_nuskaitymas
		ivedimo_pabaiga:
		mov ax, 0
		mov word [file_in + bx], ax
		
	
	macPutString 'Ivesk rasomo failo varda: ', crlf, '$'
	mov al, 255                  ; ilgiausia eilutė
	mov dx, file_out      ; 
	call procGetStr              
	macNewLine
	
	
	
	mov dx, file_in
	;mov dx, f.in
	call procFOpenForReading
	jc err2
	mov bp, sp
    sub sp, 4 
	mov [bp-2], bx
	
	mov dx, file_out
	call procFCreateOrTruncate
	jc err3
	mov [bp - 4], bx
	

	mov bx, [bp - 2]
	call pirma_eilute
		mov dx, eilute
		mov bx,[bp - 4]
		mov cx, [poslinkis]
		call procFWrite
		
										
	mov bx, [bp - 2]
	call eiluciu_nuskaitymas
	
	mov ax, [eiluciu_poslinkis]
	call procPutInt16

	
	macPutString 'viso eiluciu rasta: ', '$'
	mov ax, [treti_sk_count]
	call procPutInt16
	macNewLine
	macPutString 'rikiuojama', crlf, '$'
	
	;-------------------------------------------------------------
	xor bx, bx
	mov cx, [treti_sk_count]
	cmp cx, 2
	jl isvedimas
	sub cx, 1
	ciklo_prad:
	xor ax, ax
	mov al, 2
	xor dx, dx
	mul bx
	xchg dx, bx
	mov bx, ax
	mov si, ax
	mov ax, [treti_skaiciai + bx]
	
		push bx
		push cx
		push dx
		
		mov cx, [treti_sk_count]
		sub cx, 1
		sub cx, dx
		jmp tol
		
		ciklas:
		jmp ciklo_prad				;----------- tarpinis jump
		tol:
		
		ciklas2:
		add bx, 2								
		mov dx, [treti_skaiciai + bx]
		cmp dx, ax
		jle toliau
			;mov word [treti_skaiciai + bx], ax					;--- blogai nes padedu viena vieta aukščiau, bet ne į pačią pradžią
			;mov word [treti_skaiciai + bx - 2], dx
			
			xchg word [treti_skaiciai + si], dx
			xchg word [treti_skaiciai + bx], ax
			
			push ax
			xor ax, ax
			xor dx, dx
			mov ax, [eiluciu_poslinkis + bx]
			mov dx, [eiluciu_poslinkis + si]
			mov word [eiluciu_poslinkis + bx], dx
			mov word [eiluciu_poslinkis + si], ax
			pop ax
		toliau:
		loop ciklas2
		
		pop dx
		pop cx
		pop bx
	xchg dx, bx
	inc bx
	loop ciklas
	
	isvedimas:
	;---------------------------------------------------isvedimas--------------------------------------
	;mov dx, f_out
	;call procFCreateOrTruncate
	;jc err3
	;mov [bp - 4], bx
	
	mov cx, [treti_sk_count]
	cmp cx, 1
	jl pab
	xor si, si
	
	macNewLine
	
	; mov bx, [bp - 4]
	; mov ax, [treti_skaiciai]
	; call procInt16ToStr
	; mov cx, 2
	; call procFWrite
	; xor dx, dx
	; mov ax, [treti_skaiciai + 2]
	; call procFWrite
	
	ciklas4:
	push cx
	
	
	mov bx, eilute						;---------išvalau string buferi
	mov cl, 255
	.loop:
	mov byte [bx], 0
	inc bx
	loop .loop
	
	push bx
	push ax
	push cx
	push dx
	xchg bx, dx
	mov byte [bx + 1], 020h	
	mov byte [bx + 2], 020h	
	mov byte [bx + 3], 020h	
	mov byte [bx + 4], 020h	
	mov byte [bx + 5], 020h	
	mov byte [bx + 6], 020h	
	xchg bx, dx
	mov bx, [bp - 4]
	mov ax, [treti_skaiciai + si]
	call procInt16ToStr
				; xchg bx, dx
				; mov byte [bx + 1], 020h	
				; mov byte [bx + 2], 020h	
				; mov byte [bx + 3], 020h	
				; mov byte [bx + 4], 020h	
				; xchg bx, dx
	mov cx, 6
	call procFWrite
					; xchg bx, dx
					; mov al, [bx]
					; xchg bx, dx
					; call procFPutChar
					; xchg bx, dx
					; mov al, [bx + 1]
					; xchg bx, dx
					; call procFPutChar
					; xchg bx, dx
					; mov al, [bx + 2]
					; xchg bx, dx
					; call procFPutChar
	pop dx
	pop cx
	pop ax
	pop bx
	
	mov dx, [eiluciu_poslinkis + si]	;------nustatau poslinki į reikiamos eilutes pradzia
	mov bx, [bp - 2]
	mov al, 0
	call procFSeekFromBeginning
	
	
	mov bx, [bp - 2]
	call pirma_eilute
	
	xor cx, cx
	mov bx, [bp - 4]
	mov dx, eilute
	mov cx, ax
	call procFWrite
	
	add si, 2
	
	pop cx
	loop ciklas4
	
	
	
	jmp pab
	
	err1:
	macPutString ' nera argumento duomenu failui', crlf, '$'
	jmp pab
	
	err2:
	macPutString ' klaida nuskaitant faila', crlf, '$'
	jmp pab
	
	err3:
	macPutString ' klaida sukuriant faila', crlf, '$'

	pab:
	macPutString ' baigta', crlf, '$'
	
	
	mov bx, [bp - 4]
	call procFClose
	
	mov bx, [bp - 2]
	call procFClose
	exit
	
	
	
	eiluciu_nuskaitymas:
	push bx
	push dx
	push cx
	push di
	
	mov byte [treti_sk_count], 0
	mov ax, 1
	xor di, di
	mov di, [poslinkis]
	macNewLine
	
	pradzia:
	;-----------------------------------poslinkis
	push ax
	push si
	
	xor si, si
	mov si, [treti_sk_count]
		
	mov ax, di
	add ax, 1
	mov bx, eiluciu_poslinkis
	add bx, si
	mov word [bx + si], ax
		
	pop si
	pop ax
	
	;------------------------------------------------
	
	mov bx, trecias_sk
	mov cl, 255
	.loop:
	mov byte [bx], 0
	inc bx
	loop .loop
	
	mov dx, trecias_sk
	
	zodis1_1: 
	mov bx, [bp - 2]
	call procFGetChar
	inc di
	cmp ax, 0
	je text_end
	cmp cl, ';'
	jne zodis1_1
	
	
	zodis2_1:
	mov bx, [bp - 2]
	call procFGetChar
	inc di
	cmp cl, ';'
	jne zodis2_1
	
	
	zodis3_1:
	xor cx, cx
	mov bx, [bp - 2]
	call procFGetChar
	inc di
	cmp cl, ';'
	je zodis4_1
		xchg bx, dx
		mov byte [bx], cl
		inc bx
		xchg bx, dx
		jmp zodis3_1
	
	
	zodis4_1:
	mov bx, [bp - 2]
	call procFGetChar
	cmp ax, 1
	jne tesk2
	inc di
	cmp cl, ';'
	jne zodis4_1
	
	
	zodis5_1:
	mov bx, [bp - 2]
	call procFGetChar
	cmp ax, 0
	je tesk2
	cmp ax, 1
	jne tesk2
	cmp ax, 0
	je text_end
	inc di
	cmp cl, 0x0D
	jne zodis5_1
	
	tesk2:
	push ax
	push si
	mov dx, trecias_sk
	call procParseInt16
	
	
	xor si, si
	mov si, [treti_sk_count]		
	mov bx, treti_skaiciai
	add bx, si						
	mov word [bx + si], ax
	
		
	add word [treti_sk_count], 1
	pop si
	pop ax
	jmp pradzia
	
	
	text_end:					;-------------------------poslinkis ----------------------------
	push ax	
	push si
	
	xor si, si
	mov si, [treti_sk_count]
		
	mov ax, di
	mov bx, eiluciu_poslinkis
	    ; macPutString '    ', '$'
		; call procPutInt16
		; macNewLine
	add bx, si
	mov word [bx + si], ax
		
	pop si
	pop ax
	
	
	pop di
	pop cx
	pop dx
	pop bx
	ret
	
	
	
	pirma_eilute:
	push bx
	push dx
	push cx

	mov byte [poslinkis], 0
	xor dx, dx
	mov dx, eilute
	
	zodis1: 
	call procFGetChar
	cmp ax, 1
	jne tesk
	add byte [poslinkis], 1
	xchg bx, dx
	mov byte [bx], cl
	inc bx
	xchg bx, dx
	cmp cl, ';'
	jne zodis1
	
	zodis2:
	call procFGetChar
	cmp ax, 1
	jne tesk
	add byte [poslinkis], 1
	xchg bx, dx
	mov byte [bx], cl
	inc bx
	xchg bx, dx
	cmp cl, ';'
	jne zodis2
	
	zodis3:
	call procFGetChar
	cmp ax, 1
	jne tesk
	add byte [poslinkis], 1
	xchg bx, dx
	mov byte [bx], cl
	inc bx
	xchg bx, dx
	cmp cl, ';'
	jne zodis3
	
	zodis4:
	call procFGetChar
	cmp ax, 1
	jne tesk
	add byte [poslinkis], 1
	xchg bx, dx
	mov byte [bx], cl
	inc bx
	xchg bx, dx
	cmp cl, ';'
	jne zodis4
	
	zodis5:
	call procFGetChar
	cmp ax, 1
	jne tesk
	add byte [poslinkis], 1
	xchg bx, dx
	mov byte [bx], cl
	inc bx
	xchg bx, dx
	cmp cl, 0x0D
	jne zodis5
	
	tesk:
	mov ax, [poslinkis]
	
	pop cx
	pop dx
	pop bx
	ret
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%include 'yasmlib.asm'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


section .data                   ; duomenys

	f_out db 'rez.txt', 0
	
	file_out:
		times 255 db 00
	
	file_in:
		times 255 db 00
	
	eilute:
		times 255 db 00
		
	trecias_sk:
		times 255 db 00
	treti_skaiciai:
		times 4096 db 00
		
	treti_sk_count:								
		dw 00
	eiluciu_poslinkis:
		times 4096 db 00
		
	poslinkis:
		dw 00
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
section .bss                    ; neinicializuoti duomenys  


