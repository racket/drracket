/*
  MzScheme
  Copyright (c) 1995 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Re-implementation of i386 setjmp to avoid Windows-specific work,
   which messes up MzScheme's (MrEd's, really) threads. */

#include "schpriv.h"

/* Got this working for MSVC unoptimized. I'm too lazy to get it
   right with optimization. */
#pragma optimize("", off)

int scheme_setjmp(mz_jmp_buf b)
{
  __asm {
	mov ECX, [EBP+4] ; return address
	mov EAX, [EBP+8] ; b: jmpbuf
	mov EDX, [EBP]   ; old EBP
	mov [EAX], EDX
	mov [EAX+4], EBX
	mov [EAX+8], EDI
	mov [EAX+12], ESI
	mov [EAX+16], ESP
	mov [EAX+20], ECX
  }

  return 0;
}

void scheme_longjmp(mz_jmp_buf b, int v)
{
  __asm {
	mov EAX, [EBP+12] ; v: return value
	mov ECX, [EBP+8]  ; b: jmp_buf
	mov ESP, [ECX+16] ; restore old stack pointer
	mov EBP, [ECX]    ; old EBP
	mov [ESP+12], EBP
	mov EBP, ESP
	add EBP, 12
	mov EBX, [ECX+4]
	mov [ESP+8], EBX
	mov EDI, [ECX+8]
	mov [ESP], EDI
	mov ESI, [ECX+12]
	mov [ESP+4], ESI
	mov ECX, [ECX+20] ; return address
	mov [EBP+4], ECX
  }
}
