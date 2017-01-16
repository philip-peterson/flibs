/****************************************************************************** 
                                PCRE wrappers
------------------------------------------------------------------------------ 

 Wrapper functions to allow calling PCRE indirected functions from the Fortran
 side.

 Written by Paul Fossati, <paul.fossati@gmail.com>
 Copyright (c) 2014 Paul Fossati
 
 PCRE library written by Philip Hazel,
 Copyright (c) 1997-2014 University of Cambridge
 available at http://www.pcre.org
------------------------------------------------------------------------------ 
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
  
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
  
      * The name of the author may not be used to endorse or promote products
       derived from this software without specific prior written permission 
       from the author.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
******************************************************************************/
#include <stdio.h>
#include "pcre.h"

// extern void *(*pcre_malloc)(size_t);
void *pcre_malloc_wrapper (size_t size) {
    return pcre_malloc(size);
}

// extern void *(*pcre_stack_malloc)(size_t);
void *pcre_stack_malloc_wrapper (size_t size) {
    return pcre_stack_malloc(size);
}

// extern int (*pcre_callout)(pcre_callout_block *);
int pcre_callout_wrapper (pcre_callout_block *block) {
    return pcre_callout(block);
}

// extern void (*pcre_free)(void *);
void pcre_free_wrapper (void *regex) {
    pcre_free(regex);
}

// extern void (*pcre_stack_free)(void *);
void pcre_stack_free_wrapper (void *regex) {
    pcre_stack_free(regex);
}
