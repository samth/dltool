;;; Guile DWARF reader and writer

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Parts of this file were derived from sysdeps/generic/dwarf2.h, from
;; the GNU C Library.  That file is available under the LGPL version 2
;; or later, and is copyright:
;;
;; Copyright (C) 1992, 1993, 1995, 1996, 1997, 2000, 2011
;; 	Free Software Foundation, Inc.
;; Contributed by Gary Funck (gary@intrepid.com).  Derived from the
;; DWARF 1 implementation written by Ron Guilmette (rfg@monkeys.com).

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (dlhacks dwarf)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system base target)
  #:use-module (dlhacks elf)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:export (elf->dwarf-context
            read-debuginfo))

;;;
;;; First, define a number of constants.  The figures numbers refer to
;;; the DWARF 2.0 draft specification available on http://dwarfstd.org/.
;;; Extra codes not defined in that document are taken from the dwarf2
;;; header in glibc.
;;;

;; Figures 14 and 15: Tag names and codes.
;;
(define DW_TAG_padding #x00)
(define DW_TAG_array_type #x01)
(define DW_TAG_class_type #x02)
(define DW_TAG_entry_point #x03)
(define DW_TAG_enumeration_type #x04)
(define DW_TAG_formal_parameter #x05)
(define DW_TAG_imported_declaration #x08)
(define DW_TAG_label #x0a)
(define DW_TAG_lexical_block #x0b)
(define DW_TAG_member #x0d)
(define DW_TAG_pointer_type #x0f)
(define DW_TAG_reference_type #x10)
(define DW_TAG_compile_unit #x11)
(define DW_TAG_string_type #x12)
(define DW_TAG_structure_type #x13)
(define DW_TAG_subroutine_type #x15)
(define DW_TAG_typedef #x16)
(define DW_TAG_union_type #x17)
(define DW_TAG_unspecified_parameters #x18)
(define DW_TAG_variant #x19)
(define DW_TAG_common_block #x1a)
(define DW_TAG_common_inclusion #x1b)
(define DW_TAG_inheritance #x1c)
(define DW_TAG_inlined_subroutine #x1d)
(define DW_TAG_module #x1e)
(define DW_TAG_ptr_to_member_type #x1f)
(define DW_TAG_set_type #x20)
(define DW_TAG_subrange_type #x21)
(define DW_TAG_with_stmt #x22)
(define DW_TAG_access_declaration #x23)
(define DW_TAG_base_type #x24)
(define DW_TAG_catch_block #x25)
(define DW_TAG_const_type #x26)
(define DW_TAG_constant #x27)
(define DW_TAG_enumerator #x28)
(define DW_TAG_file_type #x29)
(define DW_TAG_friend #x2a)
(define DW_TAG_namelist #x2b)
(define DW_TAG_namelist_item #x2c)
(define DW_TAG_packed_type #x2d)
(define DW_TAG_subprogram #x2e)
(define DW_TAG_template_type_param #x2f)
(define DW_TAG_template_value_param #x30)
(define DW_TAG_thrown_type #x31)
(define DW_TAG_try_block #x32)
(define DW_TAG_variant_part #x33)
(define DW_TAG_variable #x34)
(define DW_TAG_volatile_type #x35)
(define DW_TAG_format_label #x4101)
(define DW_TAG_function_template #x4102)
(define DW_TAG_class_template #x4103)
(define DW_TAG_GNU_BINCL #x4104)
(define DW_TAG_GNU_EINCL #x4105)

(define DW_TAG_lo_user #x4080)
(define DW_TAG_hi_user #xffff)

;; Figure 16: Flag that tells whether entry has a child or not.
;;
(define DW_CHILDREN_no 0)
(define DW_CHILDREN_yes 1)

;; Figures 17 and 18: Attribute names and codes.
;;
(define DW_AT_sibling #x01)
(define DW_AT_location #x02)
(define DW_AT_name #x03)
(define DW_AT_ordering #x09)
(define DW_AT_subscr_data #x0a)
(define DW_AT_byte_size #x0b)
(define DW_AT_bit_offset #x0c)
(define DW_AT_bit_size #x0d)
(define DW_AT_element_list #x0f)
(define DW_AT_stmt_list #x10)
(define DW_AT_low_pc #x11)
(define DW_AT_high_pc #x12)
(define DW_AT_language #x13)
(define DW_AT_member #x14)
(define DW_AT_discr #x15)
(define DW_AT_discr_value #x16)
(define DW_AT_visibility #x17)
(define DW_AT_import #x18)
(define DW_AT_string_length #x19)
(define DW_AT_common_reference #x1a)
(define DW_AT_comp_dir #x1b)
(define DW_AT_const_value #x1c)
(define DW_AT_containing_type #x1d)
(define DW_AT_default_value #x1e)
(define DW_AT_inline #x20)
(define DW_AT_is_optional #x21)
(define DW_AT_lower_bound #x22)
(define DW_AT_producer #x25)
(define DW_AT_prototyped #x27)
(define DW_AT_return_addr #x2a)
(define DW_AT_start_scope #x2c)
(define DW_AT_stride_size #x2e)
(define DW_AT_upper_bound #x2f)
(define DW_AT_abstract_origin #x31)
(define DW_AT_accessibility #x32)
(define DW_AT_address_class #x33)
(define DW_AT_artificial #x34)
(define DW_AT_base_types #x35)
(define DW_AT_calling_convention #x36)
(define DW_AT_count #x37)
(define DW_AT_data_member_location #x38)
(define DW_AT_decl_column #x39)
(define DW_AT_decl_file #x3a)
(define DW_AT_decl_line #x3b)
(define DW_AT_declaration #x3c)
(define DW_AT_discr_list #x3d)
(define DW_AT_encoding #x3e)
(define DW_AT_external #x3f)
(define DW_AT_frame_base #x40)
(define DW_AT_friend #x41)
(define DW_AT_identifier_case #x42)
(define DW_AT_macro_info #x43)
(define DW_AT_namelist_items #x44)
(define DW_AT_priority #x45)
(define DW_AT_segment #x46)
(define DW_AT_specification #x47)
(define DW_AT_static_link #x48)
(define DW_AT_type #x49)
(define DW_AT_use_location #x4a)
(define DW_AT_variable_parameter #x4b)
(define DW_AT_virtuality #x4c)
(define DW_AT_vtable_elem_location #x4d)
(define DW_AT_sf_names #x2101)
(define DW_AT_src_info #x2102)
(define DW_AT_mac_info #x2103)
(define DW_AT_src_coords #x2104)
(define DW_AT_body_begin #x2105)
(define DW_AT_body_end #x2106)

(define DW_AT_lo_user #x2000)
(define DW_AT_hi_user #x3fff)

;; Figure 19: Form names and codes.
;;
(define DW_FORM_addr #x01)
(define DW_FORM_block2 #x03)
(define DW_FORM_block4 #x04)
(define DW_FORM_data2 #x05)
(define DW_FORM_data4 #x06)
(define DW_FORM_data8 #x07)
(define DW_FORM_string #x08)
(define DW_FORM_block #x09)
(define DW_FORM_block1 #x0a)
(define DW_FORM_data1 #x0b)
(define DW_FORM_flag #x0c)
(define DW_FORM_sdata #x0d)
(define DW_FORM_strp #x0e)
(define DW_FORM_udata #x0f)
(define DW_FORM_ref_addr #x10)
(define DW_FORM_ref1 #x11)
(define DW_FORM_ref2 #x12)
(define DW_FORM_ref4 #x13)
(define DW_FORM_ref8 #x14)
(define DW_FORM_ref_udata #x15)
(define DW_FORM_indirect #x16)

;; Figures 22 and 23: Location atom names and codes.
;;
(define DW_OP_addr #x03)
(define DW_OP_deref #x06)
(define DW_OP_const1u #x08)
(define DW_OP_const1s #x09)
(define DW_OP_const2u #x0a)
(define DW_OP_const2s #x0b)
(define DW_OP_const4u #x0c)
(define DW_OP_const4s #x0d)
(define DW_OP_const8u #x0e)
(define DW_OP_const8s #x0f)
(define DW_OP_constu #x10)
(define DW_OP_consts #x11)
(define DW_OP_dup #x12)
(define DW_OP_drop #x13)
(define DW_OP_over #x14)
(define DW_OP_pick #x15)
(define DW_OP_swap #x16)
(define DW_OP_rot #x17)
(define DW_OP_xderef #x18)
(define DW_OP_abs #x19)
(define DW_OP_and #x1a)
(define DW_OP_div #x1b)
(define DW_OP_minus #x1c)
(define DW_OP_mod #x1d)
(define DW_OP_mul #x1e)
(define DW_OP_neg #x1f)
(define DW_OP_not #x20)
(define DW_OP_or #x21)
(define DW_OP_plus #x22)
(define DW_OP_plus_uconst #x23)
(define DW_OP_shl #x24)
(define DW_OP_shr #x25)
(define DW_OP_shra #x26)
(define DW_OP_xor #x27)
(define DW_OP_bra #x28)
(define DW_OP_eq #x29)
(define DW_OP_ge #x2a)
(define DW_OP_gt #x2b)
(define DW_OP_le #x2c)
(define DW_OP_lt #x2d)
(define DW_OP_ne #x2e)
(define DW_OP_skip #x2f)
(define DW_OP_lit0 #x30)
(define DW_OP_lit1 #x31)
(define DW_OP_lit2 #x32)
(define DW_OP_lit3 #x33)
(define DW_OP_lit4 #x34)
(define DW_OP_lit5 #x35)
(define DW_OP_lit6 #x36)
(define DW_OP_lit7 #x37)
(define DW_OP_lit8 #x38)
(define DW_OP_lit9 #x39)
(define DW_OP_lit10 #x3a)
(define DW_OP_lit11 #x3b)
(define DW_OP_lit12 #x3c)
(define DW_OP_lit13 #x3d)
(define DW_OP_lit14 #x3e)
(define DW_OP_lit15 #x3f)
(define DW_OP_lit16 #x40)
(define DW_OP_lit17 #x41)
(define DW_OP_lit18 #x42)
(define DW_OP_lit19 #x43)
(define DW_OP_lit20 #x44)
(define DW_OP_lit21 #x45)
(define DW_OP_lit22 #x46)
(define DW_OP_lit23 #x47)
(define DW_OP_lit24 #x48)
(define DW_OP_lit25 #x49)
(define DW_OP_lit26 #x4a)
(define DW_OP_lit27 #x4b)
(define DW_OP_lit28 #x4c)
(define DW_OP_lit29 #x4d)
(define DW_OP_lit30 #x4e)
(define DW_OP_lit31 #x4f)
(define DW_OP_reg0 #x50)
(define DW_OP_reg1 #x51)
(define DW_OP_reg2 #x52)
(define DW_OP_reg3 #x53)
(define DW_OP_reg4 #x54)
(define DW_OP_reg5 #x55)
(define DW_OP_reg6 #x56)
(define DW_OP_reg7 #x57)
(define DW_OP_reg8 #x58)
(define DW_OP_reg9 #x59)
(define DW_OP_reg10 #x5a)
(define DW_OP_reg11 #x5b)
(define DW_OP_reg12 #x5c)
(define DW_OP_reg13 #x5d)
(define DW_OP_reg14 #x5e)
(define DW_OP_reg15 #x5f)
(define DW_OP_reg16 #x60)
(define DW_OP_reg17 #x61)
(define DW_OP_reg18 #x62)
(define DW_OP_reg19 #x63)
(define DW_OP_reg20 #x64)
(define DW_OP_reg21 #x65)
(define DW_OP_reg22 #x66)
(define DW_OP_reg23 #x67)
(define DW_OP_reg24 #x68)
(define DW_OP_reg25 #x69)
(define DW_OP_reg26 #x6a)
(define DW_OP_reg27 #x6b)
(define DW_OP_reg28 #x6c)
(define DW_OP_reg29 #x6d)
(define DW_OP_reg30 #x6e)
(define DW_OP_reg31 #x6f)
(define DW_OP_breg0 #x70)
(define DW_OP_breg1 #x71)
(define DW_OP_breg2 #x72)
(define DW_OP_breg3 #x73)
(define DW_OP_breg4 #x74)
(define DW_OP_breg5 #x75)
(define DW_OP_breg6 #x76)
(define DW_OP_breg7 #x77)
(define DW_OP_breg8 #x78)
(define DW_OP_breg9 #x79)
(define DW_OP_breg10 #x7a)
(define DW_OP_breg11 #x7b)
(define DW_OP_breg12 #x7c)
(define DW_OP_breg13 #x7d)
(define DW_OP_breg14 #x7e)
(define DW_OP_breg15 #x7f)
(define DW_OP_breg16 #x80)
(define DW_OP_breg17 #x81)
(define DW_OP_breg18 #x82)
(define DW_OP_breg19 #x83)
(define DW_OP_breg20 #x84)
(define DW_OP_breg21 #x85)
(define DW_OP_breg22 #x86)
(define DW_OP_breg23 #x87)
(define DW_OP_breg24 #x88)
(define DW_OP_breg25 #x89)
(define DW_OP_breg26 #x8a)
(define DW_OP_breg27 #x8b)
(define DW_OP_breg28 #x8c)
(define DW_OP_breg29 #x8d)
(define DW_OP_breg30 #x8e)
(define DW_OP_breg31 #x8f)
(define DW_OP_regx #x90)
(define DW_OP_fbreg #x91)
(define DW_OP_bregx #x92)
(define DW_OP_piece #x93)
(define DW_OP_deref_size #x94)
(define DW_OP_xderef_size #x95)
(define DW_OP_nop #x96)

(define DW_OP_lo_user #x80)
(define DW_OP_hi_user #xff)

;; Figure 24: Type encodings.
;;
(define DW_ATE_void #x0)
(define DW_ATE_address #x1)
(define DW_ATE_boolean #x2)
(define DW_ATE_complex_float #x3)
(define DW_ATE_float #x4)
(define DW_ATE_signed #x5)
(define DW_ATE_signed_char #x6)
(define DW_ATE_unsigned #x7)
(define DW_ATE_unsigned_char #x8)

(define DW_ATE_lo_user #x80)
(define DW_ATE_hi_user #xff)

;; Figure 25: Access attribute.
;;
(define DW_ACCESS_public 1)
(define DW_ACCESS_protected 2)
(define DW_ACCESS_private 3)

;; Figure 26: Visibility.
;;
(define DW_VIS_local 1)
(define DW_VIS_exported 2)
(define DW_VIS_qualified 3)

;; Figure 27: Virtuality.
;;
(define DW_VIRTUALITY_none 0)
(define DW_VIRTUALITY_virtual 1)
(define DW_VIRTUALITY_pure_virtual 2)

;; Figure 28: Source language names and codes.
;;
(define DW_LANG_C89 #x0001)
(define DW_LANG_C #x0002)
(define DW_LANG_Ada83 #x0003)
(define DW_LANG_C_plus_plus #x0004)
(define DW_LANG_Cobol74 #x0005)
(define DW_LANG_Cobol85 #x0006)
(define DW_LANG_Fortran77 #x0007)
(define DW_LANG_Fortran90 #x0008)
(define DW_LANG_Pascal83 #x0009)
(define DW_LANG_Modula2 #x000a)
(define DW_LANG_Java #x000b)
(define DW_LANG_Mips_Assembler #x8001)

(define DW_LANG_lo_user #x8000)
(define DW_LANG_hi_user #xffff)

;; Figure 29: Case sensitivity.
;;
(define DW_ID_case_sensitive 0)
(define DW_ID_up_case 1)
(define DW_ID_down_case 2)
(define DW_ID_case_insensitive 3)

;; Figure 30: Calling convention.
;;
(define DW_CC_normal #x1)
(define DW_CC_program #x2)
(define DW_CC_nocall #x3)

(define DW_CC_lo_user #x40)
(define DW_CC_hi_user #xff)

;; Figure 31: Inline attribute.
;;
(define DW_INL_not_inlined 0)
(define DW_INL_inlined 1)
(define DW_INL_declared_not_inlined 2)
(define DW_INL_declared_inlined 3)

;; Figure 32: Array ordering names and codes.
(define DW_ORD_row_major 0)
(define DW_ORD_col_major 1)

;; Figure 33: Discriminant lists.
;;
(define DW_DSC_label 0)
(define DW_DSC_range 1)

;; Figure 34: "Standard" line number opcodes.
;;
(define DW_LNS_extended_op 0)
(define DW_LNS_copy 1)
(define DW_LNS_advance_pc 2)
(define DW_LNS_advance_line 3)
(define DW_LNS_set_file 4)
(define DW_LNS_set_column 5)
(define DW_LNS_negate_stmt 6)
(define DW_LNS_set_basic_block 7)
(define DW_LNS_const_add_pc 8)
(define DW_LNS_fixed_advance_pc 9)

;; Figure 35: "Extended" line number opcodes.
;;
(define DW_LNE_end_sequence 1)
(define DW_LNE_set_address 2)
(define DW_LNE_define_file 3)

;; Figure 36: Names and codes for macro information.
;;
(define DW_MACINFO_define 1)
(define DW_MACINFO_undef 2)
(define DW_MACINFO_start_file 3)
(define DW_MACINFO_end_file 4)
(define DW_MACINFO_vendor_ext 255)

;; Figure 37: Call frame information.
;;
(define DW_CFA_advance_loc #x40)
(define DW_CFA_offset #x80)
(define DW_CFA_restore #xc0)
(define DW_CFA_nop #x00)
(define DW_CFA_set_loc #x01)
(define DW_CFA_advance_loc1 #x02)
(define DW_CFA_advance_loc2 #x03)
(define DW_CFA_advance_loc4 #x04)
(define DW_CFA_offset_extended #x05)
(define DW_CFA_restore_extended #x06)
(define DW_CFA_undefined #x07)
(define DW_CFA_same_value #x08)
(define DW_CFA_register #x09)
(define DW_CFA_remember_state #x0a)
(define DW_CFA_restore_state #x0b)
(define DW_CFA_def_cfa #x0c)
(define DW_CFA_def_cfa_register #x0d)
(define DW_CFA_def_cfa_offset #x0e)
(define DW_CFA_def_cfa_expression #x0f)
(define DW_CFA_expression #x10)
(define DW_CFA_offset_extended_sf #x11)
(define DW_CFA_def_cfa_sf #x12)
(define DW_CFA_def_cfa_offset_sf #x13)
(define DW_CFA_GNU_window_save #x2d)
(define DW_CFA_GNU_args_size #x2e)
(define DW_CFA_GNU_negative_offset_extended #x2f)

(define DW_CIE_ID #xffffffff)
(define DW_CIE_VERSION 1)

(define DW_CFA_extended 0)
(define DW_CFA_low_user #x1c)
(define DW_CFA_high_user #x3f)

(define DW_ADDR_none 0)

;;;
;;; A general configuration object.
;;;

(define-record-type <dwarf-context>
  (make-dwarf-context bv
                      vaddr memsz
                      info-start info-end
                      abbrevs-start abbrevs-end
                      strtab-start strtab-end
                      word-size endianness)
  dwarf-context?
  (bv ctx-bv)
  (vaddr ctx-vaddr)
  (memsz ctx-memsz)
  (info-start ctx-info-start)
  (info-end ctx-info-end)
  (abbrevs-start ctx-abbrevs-start)
  (abbrevs-end ctx-abbrevs-end)
  (strtab-start ctx-strtab-start)
  (strtab-end ctx-strtab-end)
  (word-size ctx-word-size)
  (endianness ctx-endianness))

(define current-compilation-offset (make-parameter 0))

;;;
;;; Procedures for reading DWARF data.
;;;

(define (read-u8 ctx pos)
  (values (bytevector-u8-ref (ctx-bv ctx) pos)
          (1+ pos)))

(define (read-u16 ctx pos)
  (values (bytevector-u16-ref (ctx-bv ctx) pos (ctx-endianness ctx))
          (+ pos 2)))

(define (read-u32 ctx pos)
  (values (bytevector-u32-ref (ctx-bv ctx) pos (ctx-endianness ctx))
          (+ pos 4)))

(define (read-u64 ctx pos)
  (values (bytevector-u64-ref (ctx-bv ctx) pos (ctx-endianness ctx))
          (+ pos 8)))

(define (read-addr ctx pos)
  (case (ctx-word-size ctx)
    ((4) (read-u32 ctx pos))
    ((8) (read-u64 ctx pos))
    (else (error "unsupported word size" ctx))))

(define (read-uleb128 ctx pos)
  (let lp ((n 0) (pos pos) (shift 0))
    (let ((b (bytevector-u8-ref (ctx-bv ctx) pos)))
      (if (zero? (logand b #x80))
          (values (logior (ash b shift) n)
                  (1+ pos))
          (lp (logior (ash (logxor #x80 b) shift) n)
              (1+ pos)
              (+ shift 7))))))

(define (read-sleb128 ctx pos)
  (let lp ((n 0) (pos pos) (shift 0))
    (let ((b (bytevector-u8-ref (ctx-bv ctx) pos)))
      (if (zero? (logand b #x80))
          (values (logior (ash b shift) n
                          (if (zero? (logand #x40 b))
                              0
                              (- (ash 1 (+ shift 7)))))
                  (1+ pos))
          (lp (logior (ash (logxor #x80 b) shift) n)
              (1+ pos)
              (+ shift 7))))))

(define (read-block ctx pos len)
  (let ((bv (make-bytevector len)))
    (bytevector-copy! (ctx-bv ctx) pos bv 0 len)
    (values bv
            (+ pos len))))

(define (read-string ctx pos)
  (let ((bv (ctx-bv ctx)))
    (let lp ((end pos))
      (if (zero? (bytevector-u8-ref bv end))
          (let ((out (make-bytevector (- end pos))))
            (bytevector-copy! bv pos out 0 (- end pos))
            (values (utf8->string out)
                    (1+ end)))
          (lp (1+ end))))))

(define-record-type <abbrev>
  (make-abbrev code tag has-children? attrs forms)
  abbrev?
  (code abbrev-code)
  (tag abbrev-tag)
  (has-children? abbrev-has-children?)
  (attrs abbrev-attrs)
  (forms abbrev-forms))

(define (read-abbrev ctx pos)
  (let*-values (((code pos) (read-uleb128 ctx pos))
                ((tag pos) (read-uleb128 ctx pos))
                ((children pos) (read-u8 ctx pos)))
    (let lp ((attrs '()) (forms '()) (pos pos))
      (let*-values (((attr pos) (read-uleb128 ctx pos))
                    ((form pos) (read-uleb128 ctx pos)))
        (if (and (zero? attr) (zero? form))
            (values (make-abbrev code tag (= children DW_CHILDREN_yes)
                                 (reverse attrs) (reverse forms))
                    pos)
            (lp (cons attr attrs)
                (cons form forms)
                pos))))))

(define* (read-abbrevs ctx pos
                       #:optional (start (ctx-abbrevs-start ctx))
                       (end (ctx-abbrevs-end ctx)))
  (let lp ((abbrevs '()) (pos (+ start pos)) (max-code -1))
    (if (zero? (read-u8 ctx pos))
        (if (< pos end)
            (let ((av (make-vector (1+ max-code) #f)))
              (for-each (lambda (a)
                          (vector-set! av (abbrev-code a) a))
                        abbrevs)
              av)
            (error "Unexpected length" abbrevs pos start end))
        (let-values (((abbrev pos) (read-abbrev ctx pos)))
          (lp (cons abbrev abbrevs)
              pos
              (max (abbrev-code abbrev) max-code))))))

;; Values.
;;
(define *readers* (make-vector #x17))
(define-syntax define-value-reader
  (syntax-rules ()
    ((_ (form ctx pos) code ...)
     (define-value-reader form (lambda (ctx pos) code ...)))
    ((_ form proc)
     (vector-set! *readers* form proc))))

(define-value-reader DW_FORM_addr read-addr)

(define-value-reader (DW_FORM_block ctx pos)
  (let-values (((len pos) (read-uleb128 ctx pos)))
    (read-block ctx pos len)))

(define-value-reader (DW_FORM_block1 ctx pos)
  (let-values (((len pos) (read-u8 ctx pos)))
    (read-block ctx pos len)))

(define-value-reader (DW_FORM_block2 ctx pos)
  (let-values (((len pos) (read-u16 ctx pos)))
    (read-block ctx pos len)))

(define-value-reader (DW_FORM_block4 ctx pos)
  (let-values (((len pos) (read-u32 ctx pos)))
    (read-block ctx pos len)))

(define-value-reader DW_FORM_data1 read-u8)
(define-value-reader DW_FORM_data2 read-u16)
(define-value-reader DW_FORM_data4 read-u32)
(define-value-reader DW_FORM_data8 read-u64)
(define-value-reader DW_FORM_udata read-uleb128)
(define-value-reader DW_FORM_sdata read-sleb128)

(define-value-reader (DW_FORM_flag ctx pos)
  (values (not (zero? (bytevector-u8-ref (ctx-bv ctx) pos)))
          (1+ pos)))

(define-value-reader DW_FORM_string read-string)

(define-value-reader (DW_FORM_strp ctx pos)
  (unless (ctx-strtab-start ctx)
    (error "expected a string table" ctx))
  (let-values (((offset pos) (read-u32 ctx pos)))
    (values (read-string ctx (+ (ctx-strtab-start ctx) offset))
            pos)))

(define-value-reader (DW_FORM_ref_addr ctx pos)
  (let-values (((addr pos) (read-addr ctx pos)))
    (values (+ addr (ctx-vaddr ctx))
            pos)))

(define-value-reader (DW_FORM_ref1 ctx pos)
  (let-values (((addr pos) (read-u8 ctx pos)))
    (values (+ addr (current-compilation-offset))
            pos)))

(define-value-reader (DW_FORM_ref2 ctx pos)
  (let-values (((addr pos) (read-u16 ctx pos)))
    (values (+ addr (current-compilation-offset))
            pos)))

(define-value-reader (DW_FORM_ref4 ctx pos)
  (let-values (((addr pos) (read-u32 ctx pos)))
    (values (+ addr (current-compilation-offset))
            pos)))

(define-value-reader (DW_FORM_ref8 ctx pos)
  (let-values (((addr pos) (read-u64 ctx pos)))
    (values (+ addr (current-compilation-offset))
            pos)))

(define-value-reader (DW_FORM_ref_udata ctx pos)
  (let-values (((addr pos) (read-uleb128 ctx pos)))
    (values (+ addr (current-compilation-offset))
            pos)))

(define-value-reader (DW_FORM_indirect ctx pos)
  (let*-values (((form pos) (read-uleb128 ctx pos))
                ((val pos) (read-value ctx pos form)))
    (values (cons form val)
            pos)))

(define (read-value ctx pos form)
  ((or (and (< form (vector-length *readers*))
            (vector-ref *readers* form))
       (error "unrecognized form" form))
   ctx pos))

;; "Debugging Information Entries": DIEs.
;;
(define-record-type <die>
  (make-die abbrev vals children)
  die?
  (abbrev die-abbrev)
  (vals die-vals)
  (children die-children set-die-children!))

(define (read-die ctx pos av)
  (let*-values (((code pos) (read-uleb128 ctx pos)))
    (if (zero? code)
        (values #f pos)
        (let ((abbrev (or (vector-ref av code)
                          (error "unknown abbrev" av code))))
          (let lp ((forms (abbrev-forms abbrev))
                   (vals '())
                   (pos pos))
            (if (null? forms)
                (values (make-die abbrev (reverse vals) '())
                        pos)
                (let-values (((val pos) (read-value ctx pos (car forms))))
                  (lp (cdr forms) (cons val vals) pos))))))))

(define (read-die-tree ctx pos av)
  (let-values (((die pos) (read-die ctx pos av)))
    (cond
     ((not die)
      (values die pos))
     ((not (abbrev-has-children? (die-abbrev die)))
      (values die pos))
     (else
      (let lp ((kids '()) (pos pos))
        (let-values (((kid pos) (read-die-tree ctx pos av)))
          (if kid
              (lp (cons kid kids) pos)
              (begin
                (set-die-children! die (reverse kids))
                (values die pos)))))))))

(define (read-compilation-unit ctx pos)
  (parameterize ((current-compilation-offset pos))
    (let*-values (((len pos) (read-u32 ctx pos))
                  ((version pos) (read-u16 ctx pos))
                  ((abbrevs-offset pos) (read-u32 ctx pos))
                  ((av) (read-abbrevs ctx abbrevs-offset))
                  ((addrsize pos) (read-u8 ctx pos)))
      (read-die-tree ctx pos av))))

(define (read-debuginfo ctx)
  (let lp ((dies '()) (pos (ctx-info-start ctx)))
    (if (< pos (ctx-info-end ctx))
        (let-values (((die pos) (read-compilation-unit ctx pos)))
          (if die
              (lp (cons die dies) pos)
              (reverse dies)))
        (reverse dies))))

(define (elf->dwarf-context elf vaddr memsz)
  (let* ((sections (elf-sections-by-name elf))
         (info (assoc-ref sections ".debug_info"))
         (abbrevs (assoc-ref sections ".debug_abbrev"))
         (strtab (assoc-ref sections ".debug_str")))
    (make-dwarf-context (elf-bytes elf) vaddr memsz
                        (elf-section-offset info)
                        (+ (elf-section-offset info)
                           (elf-section-size info))
                        (elf-section-offset abbrevs)
                        (+ (elf-section-offset abbrevs)
                           (elf-section-size abbrevs))
                        (elf-section-offset strtab)
                        (+ (elf-section-offset strtab)
                           (elf-section-size strtab))
                        (elf-word-size elf)
                        (elf-byte-order elf))))
