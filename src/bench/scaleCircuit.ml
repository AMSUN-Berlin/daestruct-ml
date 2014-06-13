(*
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
 *
 *)

(*
 * Run structural analysis of the following circuit-model:

 *       _________             ___________
 *      |         |           |           |
 *   o--| circ[0] |--- ... ---| circ[n-1] |---o
 *   |  |_________|           |___________|   |
 *   |                                        |
 *   |          _____                         |
 *   |         /     \                        |
 *   o---------|  ~  |------------------------o
 *             \_____/u0
 *
 *   Where circ[j] =
 *
 *         /
 *        /             ||                          
 *   o---o   o----------||--------------------o
 *   |                  ||uC[j]               |
 *   |                      ____              |
 *   |            o--------|____|----o        |
 *   |            |         u2[j]    |        |
 *   |   ____     |                  |        |
 *   o--|____|----o--------WWWWWW----o--------o
 *       u1[j]              uL[j]
 * 
 * Switch open:
 * Equations:
 *   0)  u0=220*sin(time*omega);
 *   1)  u0= sum j = 0 .. n-1 u1[j]+uL[j];
 *  
 *   (j = 0..n-1)
 * 
 *   2 + j*8)  u1[j]=R[1,i]*i1[j];
 *   3 + j*8)  u2[j]=R[2,i]*i2[j];
 *   4 + j*8)  uL[j]=L[j]*der(iL[j]);
 *   5 + j*8)  uL[j]=u2[j];
 *   6 + j*8)  i0=i1[j];
 *   7 + j*8)  i1[j]=i2[j]+iL[j];
 * 
 * Variables:
 *   0   1   2+j*8  3+j*8  4+j*8  5+j*8  6+j*8  7+j*8
 *   u0, i0, u1[j], u2[j], uL[j], i1[j], i2[j], iL[j]
 *
 * Switch closed:
 * Equations:
 *   0)  u0=220*sin(time*omega);
 *   1)  u0= sum j = 0 .. n-1 u1[j]+uL[j];
 *  
 *   (j = 0..n-1)
 * 
 *   2 + j*8)  u1[j]=R[1,i]*i1[j];
 *   3 + j*8)  u2[j]=R[2,i]*i2[j];
 *   4 + j*8)  uL[j]=L[j]*der(iL[j]);
 *   5 + j*8)  iC[j]=C[j]*der(uC[j]);
 *   6 + j*8)  uC[j]=u1[j]+u2[j];
 *   7 + j*8)  uL[j]=u2[j];
 *   8 + j*8)  i0=i1[j]+iC[j];
 *   9 + j*8)  i1[j]=i2[j]+iL[j];
 * 
 * Variables:
 *   0   1   2+j*8  3+j*8  4+j*8  5+j*8  6+j*8  7+j*8  8+j*8  9+j*8
 *   u0, i0, u1[j], u2[j], uC[j], uL[j], i1[j], i2[j], iC[j], iL[j]
 *)

open Batteries

type subcircuit = { is_open : bool ; unknowns : int array ; equations : int array }

let rec make_subcircuit n us eqs = 
  if (n = 0) then
    []
  else
    let sc = { is_open = false ; unknowns = Array.of_enum (Enum.range us ~until:(us+7)) ; equations = Array.of_enum (Enum.range eqs ~until:(eqs+7)) } in
    sc :: (make_subcircuit (n-1) (us+8) (eqs+8))

open Daestruct

let u1 sc = sc.unknowns.(0) 
let u2 sc = sc.unknowns.(1) 
let uL sc = sc.unknowns.(2) 
let i1 sc = sc.unknowns.(3) 
let i2 sc = sc.unknowns.(4) 
let iL sc = sc.unknowns.(5) 

let uC sc = sc.unknowns.(6)
let iC sc = sc.unknowns.(7)

let instantiate u0 i0 p s =				 
  (* u1[j]=R[1,i]*i1[j]; *)
  input_set p (u1 s) s.equations.(0) 0 ;
  input_set p (i1 s) s.equations.(0) 0 ;

  (* u2[j]=R[2,i]*i2[j] *)
  input_set p (u2 s) s.equations.(1 ) 0 ;
  input_set p (i2 s) s.equations.(1 ) 0 ;

  (* uL[j]=L[j]*der(iL[j]) *)
  input_set p (uL s) s.equations.(2 ) 0 ;
  input_set p (iL s) s.equations.(2 ) 1;

  (*i1[j]=i2[j]+iL[j] *)
  input_set p (i1 s) s.equations.(3 ) 0 ;
  input_set p (i2 s) s.equations.(3 ) 0 ;
  input_set p (iL s) s.equations.(3 ) 0 ;

  (*uL[j]=u2[j] *)
  input_set p (uL s) s.equations.(4 ) 0 ;
  input_set p (u2 s) s.equations.(4 ) 0 ;

  if (not s.is_open) then (
    (* iC[j]=C[j]*der(uC[j]) *)
    input_set p (iC s) s.equations.(5 ) 0 ;
    input_set p (uC s) s.equations.(5 ) 1;

    (*uC[j]=u1[j]+u2[j] *)
    input_set p (uC s) s.equations.(6 ) 0 ;
    input_set p (u1 s) s.equations.(6 ) 0 ;
    input_set p (u2 s) s.equations.(6 ) 0 ;

    (*i0=i1[j]+iC[j] *)
    input_set p i0 s.equations.(7 ) 0 ;
    input_set p (i1 s) s.equations.(7 ) 0 ;
    input_set p (iC s) s.equations.(7 ) 0
  ) else (
    (*i0=i1[j] *)
    input_set p i0 s.equations.(5 ) 0 ;
    input_set p (i1 s) s.equations.(5 ) 0 ;
  )

let inst_circuit n =
  
  let p = input_create (n * 8 + 2) in

  let u0 = 0 
  and i0 = 1 in

  input_set p u0 0 0;
  input_set p u0 1 0;

  let scs = make_subcircuit n 2 2 in
  
  let inst sc = 
    instantiate u0 i0 p sc ;
    input_set p (u1 sc) 1 0;
    input_set p (uL sc) 1 0
  in
  List.iter inst scs 

