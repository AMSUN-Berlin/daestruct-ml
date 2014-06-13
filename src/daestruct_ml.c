/*
 * Copyright (c) 2014, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <assert.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include <daestruct.h>

#define INPUT(block) *((struct daestruct_input**)Data_custom_val(block))

void daestruct_ml_input_finalize(value v) {
  daestruct_input_delete(INPUT(v));
}

CAMLprim value daestruct_ml_input_create(value dim) {

  static struct custom_operations ida_ctxt_ops = {
    "daestruct_input",
    daestruct_ml_input_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
  };

  CAMLparam1 (dim);
  CAMLlocal1 (block);
 
  block = caml_alloc_custom(&ida_ctxt_ops, sizeof(struct daestruct_input*), 1, 10);

  int dimension = Int_val(dim);
  printf("Creating dimension: %d\n", dimension);
  struct daestruct_input* inp = daestruct_input_create(Int_val(dim)); 

  INPUT(block) = inp;

  CAMLreturn (block);
}

CAMLprim value daestruct_ml_input_set(value problem, value variable, value equation, value derivative) {
  CAMLparam4(problem, variable, equation, derivative);
  daestruct_input_set(INPUT(problem), Int_val(variable), Int_val(equation), Int_val(derivative));
  CAMLreturn (Val_unit);
}

#define RESULT(v) (*(struct daestruct_result**)Data_custom_val(v))

void daestruct_ml_result_finalize(value v) {
  daestruct_result_delete(RESULT(v));
}

CAMLprim value daestruct_ml_analyse(value problem) {
  static struct custom_operations ida_ctxt_ops = {
    "daestruct_input",
    daestruct_ml_result_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
  };

  CAMLparam1(problem);

  CAMLlocal1 (block);
 
  block = caml_alloc_custom(&ida_ctxt_ops, sizeof(struct daestruct_input*), 1, 10);
  
  RESULT(block) = daestruct_analyse(INPUT(problem));

  CAMLreturn(block);
}

CAMLprim value daestruct_ml_result_equation_index(value result, value equation) {
  CAMLparam2(result, equation);
  CAMLreturn( Val_int( daestruct_result_equation_index(RESULT(result), Int_val(equation)) ) );
}

CAMLprim value daestruct_ml_result_variable_index(value result, value variable) {
  CAMLparam2(result, variable);
  CAMLreturn( Val_int( daestruct_result_variable_index(RESULT(result), Int_val(variable)) ) );
}


 
