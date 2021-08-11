;  Copyright 2021, David S. Madole <david@madole.net>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program.  If not, see <https://www.gnu.org/licenses/>.


           ; Include kernal API entry points

           include bios.inc
           include kernel.inc


           ; convenience definitions

null       equ     0                   ; sometimes this is more expressive


           ; Executable program header

           org     2000h - 6
           dw      start
           dw      end-start
           dw      start

start:     org     2000h
           br      main


           ; Build information

           db      8+80h              ; month
           db      11                 ; day
           dw      2021               ; year
           dw      0                  ; build

           db      'See github.com/dmadole/Elfos-tee for more info',0


           ; Check minimum kernel version we need before doing anything else,
           ; in particular we need support for the heap manager to allocate
           ; memory for the persistent module to use.

main:      ldi     high k_ver          ; get pointer to kernel version
           phi     r7
           ldi     low k_ver
           plo     r7

           lda     r7                  ; if major is non-zero we are good
           lbnz    checkopt

           lda     r7                  ; if major is zero and minor is 4
           smi     4                   ;  or higher we are good
           lbdf    checkopt

           sep     scall               ; if not meeting minimum version
           dw      o_inmsg
           db      'ERROR: Needs kernel version 0.4.0 or higher',13,10,0
           sep     sret


           ; Check for command-line arguments. Only one option is recognized
           ; which is -a to append to an existing file.

checkopt:  ldi     1+2                 ; create + truncate flags for open
           plo     r8

scanopts:  lda     ra                  ; skip any whitespace,
           lbz     disusage            ;  error if end of line
           smi     '!'
           lbnf    scanopts

           smi     '-'-'!'             ; if not dash then end of options
           lbnz    filename

           lda     ra                  ; if a then set append
           smi     'a'
           lbz     doappend

disusage:  sep     scall               ; bad syntax so display usage hint
           dw      o_inmsg
           db      'USAGE: tee [-a] logfile program',13,10,0
           sep     sret

doappend:  ldi     1+4                 ; create + append flags for open
           plo     r8
           lbr     scanopts

doquiet:   lbr     scanopts


           ; Done processing any options, now get the output filename.

filename:  dec     ra                  ; remember start of filename
           glo     ra
           plo     rb
           ghi     ra
           phi     rb
           inc     ra

skipname:  lda     ra                  ; skip over filename
           lbz     disusage
           smi     '!'
           lbdf    skipname

           dec     ra                  ; zero-terminate filename
           ldi     0
           str     ra
           inc     ra

           ; Skip any spaces trailing the filename to get to the command.

skipspac:  lda     ra                  ; skip spaces after filename
           lbz     disusage
           smi     '!'
           lbnf    skipspac

           dec     ra                  ; backup to first character

           ; Allocate a block of memory from the heap to use to store the
           ; DTA for the file descriptor. The file descriptor itself is in
           ; the module block that will get copied to the heap.

           ldi     low 512             ; dta size
           plo     rc
           ldi     high 512
           phi     rc

           ldi     0                   ; unaligned
           plo     r7
           ldi     4                   ; permanent
           phi     r7

           sep     scall               ; get block from heap
           dw      o_alloc
           lbdf    memerror

           ldi     low dta             ; get pointer to fd and pointer
           plo     rd                  ;  to dta address
           ldi     high dta
           phi     rd

           ghi     rf                  ; point rd to block, point rf
           str     rd                  ;  to lsb of dta address field
           inc     rd
           glo     rf
           str     rd


           ; Open the output file using the parameters saved during command
           ; line scanning and with file descriptor that will be copied.

           ldi     low fd
           plo     rd
           ldi     high fd
           phi     rd

           glo     rb                  ; get filename
           plo     rf
           ghi     rb
           phi     rf

           glo     r8                  ; get flags
           plo     r7

           sep     scall               ; open output file
           dw      o_open
           lbnf    allocmem

           sep     scall               ; output error message
           dw      o_inmsg
           db      'ERROR: open output file failed',13,10,0
           sep     sret


           ; Allocate memory from the heap for the resident part of init
           ; that runs the child processes. Address of block to copy that
           ; code into will be left in RF.

allocmem:  ldi     high modend-module  ; size of permanent code module
           phi     rb
           phi     rc
           ldi     low modend-module
           plo     rb
           plo     rc

           ldi     255                 ; request page-aligned block
           phi     r7
           ldi     4
           plo     r7

           sep     scall               ; allocate block on heap
           dw      o_alloc

           lbnf    copycode            ; allocation succeeded, copy code

           sep     scall               ; close output file
           dw      o_close

memerror:  sep     scall               ; if unable to get memory
           dw      o_inmsg
           db      'ERROR: Not enough memory available',13,10,0

           sep     sret                ; return to elf/os


           ; Copy the code of the resident part of init to the memory block
           ; that was just allocated using RF for destination.

copycode:  ldi     high module         ; get source address to copy from
           phi     r9
           ldi     low module
           plo     r9

           glo     rf                  ; make a copy of block pointer
           plo     r8
           ghi     rf
           phi     r8

           smi     high module         ; save offset between source and copy
           str     r2

copyloop:  lda     r9                  ; copy code to destination address
           str     rf
           inc     rf
           dec     rb
           glo     rb
           lbnz    copyloop
           ghi     rb
           lbnz    copyloop


           ; Update kernel hooks to point to our module code. Use the offset
           ; to the heap block at M(R2) to update module addresses to match
           ; the copy in the heap. If there is a chain address needed for a
           ; hook, copy that to the module first in the same way.

           ldi     high patchtbl      ; Get point to table of patch points
           phi     r7
           ldi     low patchtbl
           plo     r7

ptchloop:  lda     r7                 ; get address to patch, a zero
           lbz     gomodule           ;  msb marks end of the table
           phi     r9
           lda     r7
           plo     r9
           inc     r9

           lda     r7                 ; if chain needed, then get address,
           lbz     notchain           ;  adjust to heap memory block
           add
           phi     rf
           ldn     r7
           plo     rf
           inc     rf

           lda     r9                 ; patch chain lbr in module code
           str     rf                 ;  to existing vector address
           inc     rf
           ldn     r9
           str     rf
           dec     r9

notchain:  inc     r7                 ; get module call point, adjust to
           lda     r7                 ;  heap, and update into vector jump
           add
           str     r9
           inc     r9
           lda     r7
           str     r9

           lbr     ptchloop

           ; Set command line pointer into RF where the module is expecting
           ; it, and also make a copy into the module to use to restore
           ; before calling o_execbin.

gomodule:  ghi     r8                 ; get pointer to word to save command
           phi     r7                 ;  line pointer
           ldi     low command
           plo     r7

           ghi     ra                 ; set pointer to rf and save into
           phi     rf                 ;  command word in module
           str     r7
           inc     r7
           glo     ra
           plo     rf
           str     r7


           ; Jump to the persistent code that has been copied into high
           ; memory. Since this address is in a register and not known, do
           ; the jump by switching P=F, then loading R3 and switching back.

           ldi     high jumpblck      ; temporarily switch pc to rf so we
           phi     r7                 ;  can load r3 for a jump
           ldi     low jumpblck
           plo     r7
           sep     r7

jumpblck:  ghi     r8                 ; load r3 with code block address
           phi     r3                 ;  and switch pc back to r3 to jump
           glo     r8
           plo     r3
           sep     r3


           ; Table of patch points to hook the kernel. The first is the point
           ; to hook, the second where to inject the old value, the third
           ; is the value to patch with. The second two fields will be 
           ; adjusted relative to the persistent module heap block.

patchtbl:  dw      o_wrmboot, wrmboot, badexit
           dw      o_type, type, teetype
           dw      o_tty, tty, teetype
           dw      o_msg, msg, teemsg
           dw      o_inmsg, inmsg, teeinmsg
           db      null


           ; Start the persistent module code on a new page so that it forms
           ; a block of page-relocatable code that will be copied to himem.

           org     (($ + 0ffh) & 0ff00h)

module:    ; Memory-resident module code starts here

           sep     scall               ; try executing it
           dw      o_exec
           bnf     endexec

           ghi     r3                  ; get pointer to pointer to command
           phi     rd
           ldi     low command
           plo     rd

           lda     rd                  ; restore pointer to command
           phi     rf
           ldn     rd
           plo     rf

           ldn     rf                  ; if absolute path, don't bother
           smi     '/'
           bz      notfound

           sep     scall               ; try default executable directory
           dw      o_execbin
           bnf     endexec

notfound:  sep     scall               ; output error
           dw      o_inmsg
           db      'ERROR: command not found',13,10,0
           br      endexec

command:   dw      0


           ; We hook o_wrmboot so that we can clean up our memory block
           ; if a program exits that way. This is much easier than trying
           ; to properly resume because to just do this it doesn't matter
           ; if the kernel has reset the stack pointer and destroyed our
           ; return chain (or if the program has), so at least do this much.
           ;
           ; What we do here is a little tricky... we setup R6 and the word
           ; on top of the stack (which isn't really that relevant) to look
           ; like o_wrmboot used SCALL to get to us instead of LBR. This
           ; lets us use the same exit mechanism at the end either way.

badexit:   glo     r6                  ; push current value of r6
           stxd
           ghi     r6
           stxd

           ldi     high o_wrmboot      ; set o_wrmboot as the return address
           phi     r6
           ldi     low o_wrmboot
           plo     r6


endexec:   ghi     r3
           phi     r7

           adi     1
           phi     rd

           ghi     r3
           smi     high module
           str     r2

           ldi     low unpatch
           plo     r7

unptloop:  lda     r7                 ; get address to patch, a zero
           bz      doreturn           ;  msb marks end of the table
           phi     r9
           lda     r7
           plo     r9
           inc     r9

           lda     r7                 ; if chain needed, then get address,
           add
           phi     rf
           lda     r7
           plo     rf
           inc     rf

           lda     rf                 ; patch chain lbr in module code
           str     r9                 ;  to existing vector address
           inc     r9
           ldn     rf
           str     r9

           br      unptloop


           ; Close output file

doreturn:  ldi     low fd
           plo     rd

           sep     scall               ; close output file
           dw      o_close


           ; Deallocate DTA heap block

           ldi     low dta             ; get pointer to dta field
           plo     rd

           lda     rd                  ; get value of dta pointer
           phi     rf
           ldn     rd
           plo     rf

           sep     scall
           dw      o_dealloc


           ; This is a little tricky too... how do you safely delete the
           ; memory block you are executing from? What we do here is jump
           ; to o_dealloc and it runs as though it was code inline to us
           ; and the SRET at its end is just the same as if we did SRET.

           ghi     r3
           phi     rf
           ldi     low module
           plo     rf

           lbr     o_dealloc           ; deallocate it and sret

           ; The above is not really strictly necessary since deleting the
           ; block in itself doesn't overwrite the memory that was in it,
           ; but this way makes less assumptions about the internal working
           ; of the heap manager by not relying on that current fact.

wrmboot:   lbr     wrmboot


           ; A table of what to "unpatch" when we are done to restore the
           ; original output device directly.

unpatch:   dw      o_wrmboot,wrmboot
           dw      o_type,type
           dw      o_tty,tty
           dw      o_msg,msg
           dw      o_inmsg,inmsg
           dw      0


           org     (($ + 0ffh) & 0ff00h)


           ; Output character in D into the output file and copy to the
           ; original output destination.

teetype:   stxd                        ; save d to restore before chain

           glo     rc                  ; save used registers
           stxd
           ghi     rc
           stxd

           glo     rd
           stxd
           ghi     rd
           stxd

           glo     rf
           stxd
           ghi     rf
           stxd

           ; Since o_write outputs from a memory location, set the buffer
           ; pointer to the stack and then push the value to the stack.

           glo     r2                  ; copy stack pointer to buffer pointer
           plo     rf
           ghi     r2
           phi     rf

           glo     re                  ; push character to output
           stxd

           ldi     1                   ; set count to 1
           plo     rc
           shr
           phi     rc

           ghi     r3                  ; point rd to fd
           phi     rd
           ldi     low fd
           plo     rd

           sep     scall               ; output character
           dw      o_write

           irx                         ; discard pushed character

           irx                         ; restore saved registers
           ldxa
           phi     rf
           ldxa
           plo     rf

           ldxa
           phi     rd
           ldxa
           plo     rd

           ldxa
           phi     rc
           ldxa
           plo     rc

           ldx                         ; restore saved character
           plo     re

type:      lbr     $                   ; chain to original type address


           ; Dummy entry to chain to original o_tty function. We replace both
           ; o_type and o_tty with tee but we need to remember the original
           ; address separately to restore separately.

tty:       lbr     $


           ; Output string pointed to be RF to file and to original output
           ; device. This is used to hook the o_msg output function.
           
teemsg:    glo     rc                  ; save registers we will use
           stxd
           ghi     rc
           stxd

           glo     rd
           stxd
           ghi     rd
           stxd

           glo     rf
           stxd
           ghi     rf
           stxd

           glo     rf                  ; copy pointer to string
           plo     rd
           ghi     rf
           phi     rd

           ldi     255                 ; set count to -1
           plo     rc
           phi     rc

msglen:    lda     rd                  ; count length of string
           inc     rc
           bnz     msglen

           ghi     r3                  ; point rd to fd
           phi     rd
           ldi     low fd
           plo     rd

           sep     scall               ; output string
           dw      o_write

           inc     rf                  ; skip terminating zero

           irx                         ; restore saved registers
           ldxa
           phi     rf
           ldxa
           plo     rf

           ldxa
           phi     rd
           ldxa
           plo     rd

           ldxa
           phi     rc
           ldx
           plo     rc

msg:       lbr     $                    ; chain to prior output


           ; Output string pointed to be RF to file and to original output
           ; device. This is used to hook the o_msg output function.

teeinmsg:  glo     rc
           stxd
           ghi     rc
           stxd

           glo     rd
           stxd
           ghi     rd
           stxd

           glo     rf
           stxd
           ghi     rf
           stxd

           glo     r6
           plo     rf
           ghi     r6
           phi     rf

           ldi     255
           plo     rc
           phi     rc

inmsglen:  lda     rf
           inc     rc
           bnz     inmsglen

           glo     r6
           plo     rf
           ghi     r6
           phi     rf

           ghi     r3
           phi     rd
           ldi     low fd
           plo     rd

           sep     scall
           dw      o_write

           irx
           ldxa
           phi     rf
           ldxa
           plo     rf

           ldxa
           phi     rd
           ldxa
           plo     rd

           ldxa
           phi     rc
           ldx
           plo     rc

inmsg:     lbr     $


           ; Include file descriptor in module image so it is initialized.

fd:        db      0,0,0,0             ; file descriptor
dta:       db      0,0
           db      0,0
           db      0
           db      0,0,0,0
           db      0,0
           db      0,0,0,0


modend:    ; End load the resident module code

end:       ; Last address used at all by the program. This will be set in 
           ; the header for the executable length so that lowmem gets set
           ; here to prevent collision with static data.

