;	set game state memory location
.equ    HEAD_X,         0x1000  ; Snake head s position on x
.equ    HEAD_Y,         0x1004  ; Snake head s position on y
.equ    TAIL_X,         0x1008  ; Snake tail s position on x
.equ    TAIL_Y,         0x100C  ; Snake tail s position on Y
.equ    SCORE,          0x1010  ; Score address
.equ    GSA,            0x1014  ; Game state array address

.equ    CP_VALID,       0x1200  ; Whether the checkpoint is valid.
.equ    CP_HEAD_X,      0x1204  ; Snake head s X coordinate. (Checkpoint)
.equ    CP_HEAD_Y,      0x1208  ; Snake head s Y coordinate. (Checkpoint)
.equ    CP_TAIL_X,      0x120C  ; Snake tail s X coordinate. (Checkpoint)
.equ    CP_TAIL_Y,      0x1210  ; Snake tail s Y coordinate. (Checkpoint)
.equ    CP_SCORE,       0x1214  ; Score. (Checkpoint)
.equ    CP_GSA,         0x1218  ; GSA. (Checkpoint)

.equ    LEDS,           0x2000  ; LED address
.equ    SEVEN_SEGS,     0x1198  ; 7-segment display addresses
.equ    RANDOM_NUM,     0x2010  ; Random number generator address
.equ    BUTTONS,        0x2030  ; Buttons addresses

; button state
.equ    BUTTON_NONE,    0
.equ    BUTTON_LEFT,    1
.equ    BUTTON_UP,      2
.equ    BUTTON_DOWN,    3
.equ    BUTTON_RIGHT,   4
.equ    BUTTON_CHECKPOINT,    5

; array state
.equ    DIR_LEFT,       1       ; leftward direction
.equ    DIR_UP,         2       ; upward direction
.equ    DIR_DOWN,       3       ; downward direction
.equ    DIR_RIGHT,      4       ; rightward direction
.equ    FOOD,           5       ; food

; constants
.equ    NB_ROWS,        8       ; number of rows
.equ    NB_COLS,        12      ; number of columns
.equ    NB_CELLS,       96      ; number of cells in GSA
.equ    RET_ATE_FOOD,   1       ; return value for hit_test when food was eaten
.equ    RET_COLLISION,  2       ; return value for hit_test when a collision was detected
.equ    ARG_HUNGRY,     0       ; a0 argument for move_snake when food wasn t eaten
.equ    ARG_FED,        1       ; a0 argument for move_snake when food was eaten


; initialize stack pointer
addi    sp, zero, LEDS

; main
; arguments
;     none
;
; return values
;     This procedure should never return.
main:
    ; TODO: Finish this procedure.
	call init_game

loop:

	call wait
	call get_input
	addi t3,zero,BUTTON_CHECKPOINT
	beq v0,t3,cp_pressed

loop_resume:

	call clear_leds
	call hit_test

	beq v0,zero,no_collision
	addi t1,zero,1
	beq v0,t1,food_eaten
	br end_game

cp_pressed:
	call restore_checkpoint
	addi t3,zero,1
	bne t3,v0,loop_resume
	call blink_score
	br loop
	
	



no_collision:
	addi a0,zero,ARG_HUNGRY
	call move_snake
	call draw_array
	br loop
	
food_eaten:
	addi a0,zero,ARG_FED
	ldw t1,SCORE(zero)
	addi t1,t1,1
	stw t1,SCORE(zero)
	call display_score

	call move_snake
	call create_food
	call draw_array
	call save_checkpoint
	
	beq zero,v0,loop

	call blink_score

	br loop


end_game:
	

	call clear_leds
;E
	addi a0,zero,0
	addi a1,zero,2
	call set_pixel
	addi a1,zero,3
	call set_pixel
	addi a1,zero,4
	call set_pixel
	addi a1,zero,5
	call set_pixel
	addi a1,zero,6
	call set_pixel
	addi a0,zero,1
	addi a1,zero,2
	call set_pixel
	addi a1,zero,4
	call set_pixel
	addi a1,zero,6
	call set_pixel
	addi a0,zero,2
	addi a1,zero,2
	call set_pixel
	addi a1,zero,6
	call set_pixel
;N
	addi a0,zero,4
	addi a1,zero,2
	call set_pixel
	addi a1,zero,3
	call set_pixel
	addi a1,zero,4
	call set_pixel
	addi a1,zero,5
	call set_pixel
	addi a1,zero,6
	call set_pixel

	addi a0,zero,5
	addi a1,zero,3
	call set_pixel
	addi a1,zero,4
	call set_pixel

	addi a0,zero,6
	addi a1,zero,5
	call set_pixel
	addi a1,zero,6
	call set_pixel

	addi a0,zero,7
	addi a1,zero,2
	call set_pixel
	addi a1,zero,3
	call set_pixel
	addi a1,zero,4
	call set_pixel
	addi a1,zero,5
	call set_pixel
	addi a1,zero,6
	call set_pixel

;D

	addi a0,zero,9
	addi a1,zero,2
	call set_pixel
	addi a1,zero,3
	call set_pixel
	addi a1,zero,4
	call set_pixel
	addi a1,zero,5
	call set_pixel
	addi a1,zero,6
	call set_pixel

	addi a0,zero,10
	addi a1,zero,2
	call set_pixel
	addi a1,zero,6
	call set_pixel

	addi a0,zero,11
	addi a1,zero,3
	call set_pixel
	addi a1,zero,4
	call set_pixel
	addi a1,zero,5
	call set_pixel

	call blink_score
	call get_input
	addi t3,zero,BUTTON_CHECKPOINT
	beq v0,t3,cp_after_l
	br main

cp_after_l:
	call restore_checkpoint
	addi t3,zero,1
	bne t3,v0,main
	call display_score
	br loop
	

	ret




wait:
	addi t1,zero,1
	;addi t2,zero,2
	slli t2,t1,21
wait_loop:
	beq t1,t2,end_wait
	addi t1,t1,1
	br wait_loop
    
end_wait:
	ret








	




; BEGIN: clear_leds
clear_leds:
	stw zero,LEDS(zero)
	addi t1,zero,4
	stw zero,LEDS(t1)
	addi t1,t1,4 
	stw zero,LEDS(t1)
	ret

; END: clear_leds


; BEGIN: set_pixel
set_pixel:
	cmpgei t1,a0,4
	cmpgei t2,a0,8
	slli t1,t1,2
	slli t2,t2,2
	add t3,t1,t2
	ldw t4,LEDS(t3);lOAD THE GOOD LEDS 
	sub t5,a0,t3 ;x mod 4
	slli t5,t5,3 ;8*x
	add t5,t5,a1 ;y+8*x-> coordinate
	addi t6,zero,1
	sll t6,t6,t5
	or t6,t6,t4
	stw t6,LEDS(t3)
	ret
; END: set_pixel


digit_map:
.word 0xFC ; 0
.word 0x60 ; 1
.word 0xDA ; 2
.word 0xF2 ; 3
.word 0x66 ; 4
.word 0xB6 ; 5
.word 0xBE ; 6
.word 0xE0 ; 7
.word 0xFE ; 8
.word 0xF6 ; 9

; BEGIN: display_score

display_score:
    ldw t0, SCORE(zero) ;load the score

    addi t2, zero, 100
    bge t0, t2, modulo_100 ;if score (t0) > 100 (t2)
    br continue

modulo_100 :
    sub t0, t0, t2
    bge t0, t2, modulo_100 ;if score (t0) > 100 (t2)

continue:
    add t1, zero,zero ;counter for second digit which corresponds to the dizaine
    addi t2, zero, 10
    bge t0, t2, modulo_10 ;if score (t0) > 10 (t2)
    br end_display_score 

modulo_10 :
    sub t0,t0, t2
    addi t1, t1, 1
    bge t0, t2, modulo_10 ;if score (t0) > 10 (t2)


end_display_score : 
    ;always 0 for last two 7-segment
    ldw t5, digit_map(zero)
    stw t5, SEVEN_SEGS(zero)
    stw t5, SEVEN_SEGS+4(zero)

    slli t0, t0, 2
    ldw t7, digit_map(t0) ;last digit
    stw t7, SEVEN_SEGS+12(zero)

    slli t1, t1, 2
    ldw t7, digit_map(t1) ;avant dernier digit
    stw t7, SEVEN_SEGS+8(zero)

    ret

; END: display_score






; BEGIN: init_game
init_game:

 	addi sp,sp,-4
    stw ra,0(sp)


	call clear_leds
	
	add t0,zero,zero
	addi t1,zero,96
loop_clean:
	beq t0,t1,resume_init
	slli t2,t0,2
	stw zero,GSA(t2)
	addi t0,t0,1
	br loop_clean
	
	
	
resume_init:

	addi t1,zero,DIR_RIGHT
	stw t1,GSA(zero)
	stw zero,HEAD_X(zero)
	stw zero,TAIL_X(zero)
	stw zero,HEAD_Y(zero)
	stw zero,TAIL_Y(zero)
	add a0,zero,zero
	stw zero,SCORE(zero)
	stw zero, CP_VALID(zero)
	call display_score

	call create_food
	call draw_array

	ldw ra, 0(sp)
    addi sp,sp,4
	ret
; END: init_game






; BEGIN: create_food
create_food:
    ldw t0, RANDOM_NUM(zero) ;
    andi t0, t0, 255 ;t0 is of size 32 bits and we want only lowest byte (8 last bit) this number is always inferieur a 256
    addi t1, zero, 95 
    bge t1, t0, is_in_Led_array ;if random number <= 95 (size of led array) 
    br create_food

is_in_Led_array:
    slli t0, t0, 2 ;multiply by 4
    ldw t2, GSA(t0)
    add t3, zero, zero
    beq t3, t2, is_not_snake ;if location is not in position of snake
    br create_food

is_not_snake:
    addi t4, zero, FOOD 
    stw t4, GSA(t0)
    br end_create_food

end_create_food:
    ret

; END: create_food


; BEGIN: hit_test
hit_test:
	ldw t0,HEAD_X(zero) ; get head x
	slli t6,t0,3 ;x*8
	ldw t1,HEAD_Y(zero) ;get head y
	add t6,t6,t1 ;compute coordinate(8*x+y)
	slli t6,t6,2 ;coordinate*4 pour avoir l adresse du word dans GSA
	ldw t5,GSA(t6) ;LOAD HEAD INFOS ->1 for Left, 2 for Up,3 for Down or 3 for Right

	cmpeqi t7,t5,DIR_LEFT ;DIRECTION==LEFT
	cmpeqi t2,t5,DIR_UP ;DIRECTION==UP
	cmpeqi t3,t5,DIR_DOWN ;DIRECTION==DOWN
	cmpeqi t4,t5,DIR_RIGHT ;DIRECTION==RIGHT

	sub t0,t0,t7 ;si left,x-1
	add t0,t0,t4 ;si right,x+1
	sub t1,t1,t2 ;si up,y-1
	add t1,t1,t3 ;si Down:y+1
	slli t6,t0,3 ;x*8
	add t6,t6,t1 ;compute coordinate(8*x+y)


	;TODO : CHECK THAT 0<=t6(coordinate)<=95 and x>=0 and y>=0
	blt t0,zero, hit_fail
	blt t1,zero, hit_fail
	addi t3,zero,11
	blt t3,t0,hit_fail
	addi t3,zero,7
	blt t3,t1,hit_fail


	blt t6,zero, hit_fail
	addi t5,zero,95
	blt t5,t6, hit_fail	
	;TODO: CHECK THAT THE NEW HEAD POSITION ISN T SNAKE 
	slli t6,t6,2 ;coordinate*4 pour avoir l adresse du word dans GSA
	ldw t5,GSA(t6)
	beq t5,zero,free_new_head
	addi t2,zero,5
	beq t5,t2,food_end
	br hit_fail

	
food_end:
	addi v0,zero,1
	ret	
	

free_new_head:
	addi v0,zero,0
	ret	


	
hit_fail:
	addi v0,zero,2
	ret	

; END: hit_test

; BEGIN: get_input
get_input:
;t6 for input edgeCapture
;t7 for head s coordinate
;t4 for current head s direction

;t3 = 1
;t5 = 1 (t6 transformed)

    ldw t6, BUTTONS + 4(zero) ;edgeCapture
    stw zero,BUTTONS + 4(zero) ; clear edgecapture in memory
    add t0, zero, zero
    beq t0, t6, no_edgeCapture

    ldw t2,HEAD_X(zero) ; get head x
    slli t2,t2,3 ;x8
    ldw t7,HEAD_Y(zero) ;get head y
    add t7,t2,t7 ;compute coordinate(8x+y)

    ;Load current head s direction
    slli t7,t7,2 ;multiply t3 by 4
    ldw t4, GSA(t7) ;current head s direction 




    ;if button is checkpoint
    addi t5, zero, 16
    bgeu t6, t5, is_checkPoint 


    addi t3, zero, 1
    addi t5, zero, 1
    beq t6, t3, continuity



    transformation_loop:

        addi     t5, t5, 1
        srli     t6, t6, 1
        andi     t2, t6, 1

        bne      t2, t3, transformation_loop

    ;addi t0, zero, BUTTON_CHECKPOINT
    ;beq t0, t5, is_checkPoint ;if value is checkpoint, return directly the value

continuity :

    add t6, t4, t5 ;we add values to check if sum is equal to 5 (left = 1 and right = 4 , down = 3 and up = 2)
    cmpeqi t3, t6, 5 
    beq t3,zero, no_opposites_values  ; if t2 == 0 we set t1 to 0
    br end_input

no_opposites_values:
    stw t5, GSA(t7) ;we store t5
    add v0, zero,t5 ;return values 
    br end_input

is_checkPoint:
    addi v0, zero, BUTTON_CHECKPOINT
    br end_input

no_edgeCapture:
    addi v0, zero, BUTTON_NONE

end_input :
    ret
; END: get_input






; BEGIN: draw_array
draw_array: ; need to use set pixel and clear leds
	addi sp,sp,-4
	stw ra,0(sp)


	ldw a0 , LEDS(zero) ; load the first leds(yellow part)
	add a1 , zero, zero
	add t0, zero,zero ; counter
	call leds_modify
	stw v1,LEDS(zero)

	ldw a0 , LEDS+4(zero) ; load the second leds(green part)
	addi a1 , zero, 32
	add t0, zero,zero ; counter
	call leds_modify
	stw v1,LEDS+4(zero)

	ldw a0 , LEDS+8(zero) ; load the third leds(purple part)
	addi a1 , zero, 64
	add t0, zero,zero ; counter
	call leds_modify
	stw v1,LEDS+8(zero)	

	ldw ra, 0(sp)
	addi sp,sp,4
		
	ret

	
leds_modify: ; a0: leds you want to modify ; a1: the shift for GSA array   ; return v1:the new led	
	addi t7,zero,32 ;size of an array
	bge t0,t7,end_modify ; if loop finish end the procedure
	add t1, a0,zero ; modified led
	slli t3,a1,2 ; 4*a1 to get the good cell
	ldw t2, GSA(t3) ; the gsa element corresponding to the given shift
	add t6,zero,a1 ; store a1
	addi a1,a1,1 ;increase a1 for the next loop
	addi t0, t0,1 ; counter increment
	beq t2,zero,leds_modify ; check that the element is not empty	
	addi t4,zero,1
	sll t4,t4,t6 
	or a0,a0,t4 ;set the led
	br leds_modify


end_modify:
	add v1,zero,a0
	ret
	
	
		


	
	




; END: draw_array




; BEGIN: move_snake
move_snake: ; a0: no need to remove tail
	ldw t0,HEAD_X(zero) ; get head x
	slli t6,t0,3 ;x*8
	ldw t1,HEAD_Y(zero) ;get head y
	add t6,t6,t1 ;compute coordinate(8*x+y)
	slli t6,t6,2 ;coordinate x 4 pour avoir l adresse du word dans GSA
	ldw t5,GSA(t6) ;LOAD HEAD INFOS ->1 for Left, 2 for Up,3 for Down or 3 for Right

	cmpeqi t7,t5,DIR_LEFT ;DIRECTION==LEFT
	cmpeqi t2,t5,DIR_UP ;DIRECTION==UP
	cmpeqi t3,t5,DIR_DOWN ;DIRECTION==DOWN
	cmpeqi t4,t5,DIR_RIGHT ;DIRECTION==RIGHT

	sub t0,t0,t7 ;si left,x-1
	add t0,t0,t4 ;si right,x+1
	sub t1,t1,t2 ;si up,y-1
	add t1,t1,t3 ;si Down:y+1
	stw t0,HEAD_X(zero)
	stw t1,HEAD_Y(zero)
	slli t6,t0,3 ;x x 8
	add t6,t6,t1 ;compute coordinate(8 x x+y)
	slli t6,t6,2 ;coordinate*4 pour avoir l adresse du word dans GSA
	stw t5,GSA(t6)
	beq a0,zero,tail_change

	ret

tail_change:
	ldw t0,TAIL_X(zero) ; get tail x
	slli t6,t0,3 ;x*8
	ldw t1,TAIL_Y(zero) ;get tail y
	add t6,t6,t1 ;compute coordinate(8 x x+y)
	slli t6,t6,2 ;coordinate x 4 pour avoir l adresse du word dans GSA
	ldw t5,GSA(t6) ;LOAD TAIL INFOS ->1 2 3 or 4

	cmpeqi t2,t5,DIR_UP ;DIRECTION==UP?
	cmpeqi t3,t5,DIR_DOWN ;DIRECTION==DOWN?
	cmpeqi t4,t5,DIR_RIGHT ;DIRECTION==RIGHT?
	cmpeqi t7,t5,DIR_LEFT ;DIRECTION==LEFT?

	sub t0,t0,t7 ;si left,x-1
	add t0,t0,t4 ;si right,x+1
	sub t1,t1,t2 ;si up,y-1
	add t1,t1,t3 ;si Down:y+1

	stw t0,TAIL_X(zero)
	stw t1,TAIL_Y(zero)
	stw zero,GSA(t6) ; reset l ancien tail
	ret
	
	


; END: move_snake





; BEGIN: save_checkpoint
save_checkpoint:;check score is a multiple of 10 if so save the state into checkpoint else do nothing
	ldw t0,SCORE(zero)
	addi t2, zero, 10
    bge t0, t2, modulo_10_cp ;if score (t0) >= 10 (t2)
	br palide_cp	

		
modulo_10_cp :
    sub t0,t0, t2
    addi t1, t1, 1
    bge t0, t2, modulo_10_cp ;if score (t0) > 10 (t2)
	beq t0,zero,valid_cp
	br palide_cp



valid_cp:
 	addi sp,sp,-4
    stw ra,0(sp)

	addi t3,zero,1
	stw t3,CP_VALID(zero)
	addi a0,zero, HEAD_X
	addi a1,zero, CP_HEAD_X
	call copy_from_a0_to_a1_save

	;call blink_score
	addi v0,zero,1

	ldw ra, 0(sp)
    addi sp,sp,4

	ret



palide_cp:
	add v0,zero,zero	
	ret 





copy_from_a0_to_a1_save: ;a0: adress from where you wont to copy
					;a1 : adress where you want to paste
					; copy a total of 5+96 word 101
	add t0,zero,zero
	addi t1,zero,102

copy_loop_save:
	beq t0,t1,end_copy_save
	slli t2,t0,2
	add t3,a0,t2
	ldw t4,0(t3)
	add t3,a1,t2
	stw t4,0(t3)
	addi t0,t0,1
	br copy_loop_save
	




end_copy_save:
	ret









; END: save_checkpoint


; BEGIN: restore_checkpoint
restore_checkpoint:
	ldw t0,CP_VALID(zero)
	addi t1,zero,1
	beq t0,t1,valid_restore
	add v0,zero,zero
	ret


valid_restore:
 	addi sp,sp,-4
    stw ra,0(sp)

	addi a0,zero, CP_HEAD_X
	addi a1,zero, HEAD_X
	call copy_from_a0_to_a1_restore

	;call blink_score
	

	ldw ra, 0(sp)
    addi sp,sp,4
	addi v0,zero,1
	ret


copy_from_a0_to_a1_restore: ;a0: adress from where you wont to copy
					;a1 : adress where you want to paste
					; copy a total of 5+96 word 101
	add t0,zero,zero
	addi t1,zero,102

copy_loop:
	beq t0,t1,end_copy
	slli t2,t0,2
	add t3,a0,t2
	ldw t4,0(t3)
	add t3,a1,t2
	stw t4,0(t3)
	addi t0,t0,1
	br copy_loop
	




end_copy:
	ret


; END: restore_checkpoint





; BEGIN: blink_score
blink_score:

    addi sp,sp,-4
    stw ra,0(sp)

    addi s0, zero, 3
    addi s1, zero,1
    add s2, zero, zero
loop_blink :
	call all_seg_to_zero 
    call wait
    call display_score   
    call wait
    sub s0, s0, s1
    bne s0, s2, loop_blink

	br end_blink

all_seg_to_zero:
    add t5, zero, zero 
    stw t5, SEVEN_SEGS(zero)
    stw t5, SEVEN_SEGS+4(zero)
    stw t5, SEVEN_SEGS+8(zero)
    stw t5, SEVEN_SEGS+12(zero)
	ret 

end_blink :
    ldw ra, 0(sp)
    addi sp,sp,4
    ret


; END: blink_score

