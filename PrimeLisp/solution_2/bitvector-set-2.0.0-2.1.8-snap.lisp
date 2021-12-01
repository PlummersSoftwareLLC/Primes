; change the way "simple-bitvector-set immediate" is compiled
; uses the method of SBCL 2.1.8
; intended for SBCL 2.0.0

(in-package "SB-VM")

(defun bit-base (dword-index)
  (+ (* dword-index 4)
     (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))

(defun emit-sbit-op (inst bv index &optional word temp)
  (cond ((integerp index)
         (multiple-value-bind (dword-index bit) (floor index 32)
           (let ((disp (bit-base dword-index)))
             (cond ((typep disp '(signed-byte 32))
                    (inst* inst :dword (ea disp bv) bit))
                   (t                   ; excessive index, really?
                    (inst mov temp-reg-tn index)
                    (inst* inst (ea (bit-base 0) bv) temp-reg-tn))))))
        (t
         ;; mem/reg BT[SR] are really slow.
         (inst mov word index)
         (inst shr word (integer-length (1- n-word-bits)))
         (inst mov temp (ea (bit-base 0) bv word n-word-bytes))
         (if (functionp inst)
             (funcall inst)
             (if (eq inst 'bts)
                   (inst bts temp index)
               (inst btr temp index)))
         (inst mov (ea (bit-base 0) bv word n-word-bytes) temp))))

(define-vop (data-vector-set-with-offset/simple-bit-vector)
  (:translate data-vector-set-with-offset)
  (:policy :fast-safe)
  ;; Arg order is (VECTOR INDEX ADDEND VALUE)
  (:arg-types simple-bit-vector positive-fixnum (:constant (eql 0)) positive-fixnum)
  (:args (bv :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (value :scs (immediate any-reg signed-reg unsigned-reg control-stack
                                signed-stack unsigned-stack)))
  (:temporary (:sc unsigned-reg) word temp)
  (:info addend)
  (:ignore addend)
  (:generator 5
    ;(unpoison-element bv index)
    (if (sc-is value immediate)
        (ecase (tn-value value)
          (1 (emit-sbit-op 'bts bv index word temp))
          (0 (emit-sbit-op 'btr bv index word temp)))
        (emit-sbit-op (lambda ()
                        (assemble ()
                          (inst test :byte value
                                (if (sc-is value control-stack signed-stack unsigned-stack) #xff value))
                          (inst jmp :z ZERO)
                          (inst bts temp index)
                          (inst jmp OUT)
                          ZERO
                          (inst btr temp index)
                          OUT))
                      bv index word temp))))
