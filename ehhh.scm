;; TODO: this needs to be more robust
(define symbol-description-table '())
(define newname
  (let ((n 0))
    (lambda (name)
      (if (symbol? name)
          (newname (symbol->string name))
          (begin
            (set! n (+ n 1))
            (let ((result (string->symbol (string-append "%tmp." (number->string n)))))
              (set! symbol-description-table
                    (cons (cons result name)
                          symbol-description-table))
              result))))))

;;; --- relatively portable assembly
;;; values are machine words or floating point numbers
;;; (name attributes args
;;;   body ...)
;;; attributes: list, might contain c-abi
;;;   or (types . <alist of types>) after typechecking
(define rpasm-statement-generation
  '((ldi int number)
    (fldi float number)
    (copy int int)
    (copy-label int label)
    (call int int . int)
    (c-call int int . int)         ; distinction needed on targets whose abi sucks
    (tail-call int . int)
    (syscall int . int)
    (+ int int int) (- int int int) (* int int int) (/ int int int)
    (u/ int int int)
    (bitand int int int) (bitor int int int) (bitxor int int int)
    (bitnot int int) (lsh int int int) (rsh int int int) (arsh int int int)
    (load int int) (load-float float int)
    (f+ float float float) (f- float float float)
    (f* float float float) (f/ float float float)
    (f->i int float) (i->f float int)
    (bit-f->i int float) (bit-i->f float int)
    (store int int) (store-float int float)
    (return int)
    (label label)
    (goto-if (rel . data) label)))
;;; rel ::= eq | const-true | lt | ult | flt

(define infer-types
  (lambda (stmt)
    (let ((h (car stmt))
          (t (cdr stmt)))
      (cond ((eq? h 'goto-if)
             ;; special case (nesting)
             (let* ((condition (car t))
                    (destination (cadr t))
                    (condition-type (if (memq (car condition) '(flt))
                                        'float
                                        'int)))
               (map (lambda (var)
                      (cons var condition-type))
                    (cdr condition))))
            (else
             ;; no nesting, general
             (letrec ((match-types
                       (lambda (spec args)
                         (cond ((pair? spec)
                                (cons (cons (car args) (car spec))
                                      (match-types (cdr spec) (cdr args))))
                               ((symbol? spec)
                                (map (lambda (var)
                                       (cons var spec))
                                     args))
                               (else '())))))
               (let* ((generation (assq h rpasm-statement-generation))
                      (arg-spec (cdr generation)))
                 (match-types arg-spec t))))))))

(define set-add
  (lambda (x s)
    (if (memq x s)
        s
        (cons x s))))
(define dedupq
  (lambda (l)
    (if (null? l)
        '()
        (set-add (car l) (dedupq (cdr l))))))
(define all
  (lambda (l)
    (if (null? l)
        #t
        (if (car l)
            (all (cdr l))
            #f))))
(define alist-coherent?
  (lambda (alist)
    (let ((vars (dedupq (map car alist))))
      (all (map (lambda (var)
                  (let ((any-decl (assq var alist)))
                    (all (map (lambda (decl)
                                (equal? any-decl decl))
                              (filter (lambda (p)
                                        (eq? (car p) var))
                                      alist)))))
                vars)))))
(define dedupa
  (lambda (alist)
    (map (lambda (var)
           (cons var (cdr (assq var alist))))
         (dedupq (map car alist)))))

;; typecheck the statements
(define rpasm-typecheck
  (lambda (blk)
    (let* ((name (car blk))
           (attr (cadr blk))
           (args (caddr blk))
           (body (cdddr blk))
           (types (filter (lambda (decl)
                            (memq (cdr decl) '(int float)))
                          (append (map (lambda (var)
                                         (cons var 'int))
                                       args)
                                  (apply append
                                         (map infer-types body))))))
      (if (not (alist-coherent? types)) (error 'rpasm "types bad"))
      `(,name ,(cons (cons 'types (dedupa types)) attr) ,args
              ,@body))))

;;; --- wishful thinking assembly
;;; like relatively portable assembly, but attributes can contain
;;;   (must var reg), statements become (call var), (c-call var), (syscall),
;;;   (make-alive var), (c-return), (return)
;;; basically, everything needed for register allocation

;;; amd64-sysv specific: output needs sse and bmi2 (for shlx and friends)
;;;   the abi sucks so we use our own except when interfacing with external code
;;; register roles:
;;; rdi rsi rdx rcx: arguments (in that order) (rdi for return value)
;;; r8: return address
;;; r12 r13 r14 r15: callee-saved
;;; rsp: stack pointer, after a call the arguments disappear (so we don't
;;;   touch the red zone)
;;; rbp: frame pointer
;;; rax rbx r9 r10 r11: caller-saved
;;; xmm0-xmm15: caller-saved
;;; arguments on the stack are put consecutively, with the argument at the
;;;   highest address just before [rsp]. later arguments get higher addresses
;;;   stack alignment is 16-bytes on call/return
;;; little diagram of the stack:
;;; ---
;;; arguments put on the stack
;;; --- [rbp]
;;; safe zone for overwriting arguments for tail calls
;;; ---
;;; local variables on the stack
;;; --- [rsp]

(define amd64-gpreg '(rax rbx rcx rdx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
(define amd64-sysv-regargs '(rdi rsi rdx rcx r8 r9))
(define amd64-sysv-result 'rax)
(define amd64-sysv-callee-saved '(rbx r12 r13 r14 r15))
(define amd64-sysv-candidates '())
(define amd64-linux-args '(rax rdi rsi rdx r10 r8 r9))
(define amd64-linux-result 'rax)
(define amd64-linux-clobbered '(rcx r11))
(define amd64-regargs '(rdi rsi rdx rcx))
(define amd64-callee-saved '(r12 r13 r14 r15))
(define amd64-link 'r8)
(define amd64-result 'rdi)

(define list-take
  (lambda (l k)
    (if (or (null? l) (= k 0))
        '()
        (cons (car l) (list-take (cdr l) (- k 1))))))
(define list-drop
  (lambda (l k)
    (if (or (null? l) (= k 0))
        l
        (list-drop (cdr l) (- k 1)))))
(define listc
  (lambda (x)
    (lambda l (cons x l))))
(define make-copy (listc 'copy))
(define make-must (listc 'must))

(define amd64-sysv-call
  (lambda (stmt blk)
    (let* ((dest (cadr stmt))
           (func (caddr stmt))
           (args (cdddr stmt))
           (reg-args (list-take args (length amd64-sysv-regargs)))
           (stack-args (list-drop args (length amd64-sysv-regargs)))
           ;; branch arguments in registers out
           (split-names (map (lambda (var) (newname 'call)) reg-args))
           (split-code (map make-copy split-names reg-args))
           ;; branch goes to callee
           (split-attr (map make-must split-names (list-take amd64-sysv-regargs
                                                             (length reg-args))))
           (stack-pad (= (mod (length stack-args) 2) 1))
           (align-code (if stack-pad
                           `((ldi tmp0 8)
                             (- sp sp tmp0))
                           '()))
           (push-code (apply append
                             (map (lambda (var)
                                    `((ldi tmp0 8)
                                      (- sp sp tmp0)
                                      (store sp ,var)))
                                  (reverse stack-args))))
           ;; caller-saved registers handled after regalloc
           ;; (don't know who are alive yet)
           (the-call `((c-call ,func)))
           (result-var (newname 'result))
           (restore-size (* 8 (+ (length stack-args) (if stack-pad 1 0))))
           (restore-size-name (newname 'size))
           (retrieval `((copy ,dest ,result-var)
                        ,@(if (zero? restore-size)
                              '()
                              `((ldi tmp0 ,restore-size)
                                (+ sp sp tmp0)))))
           (result-attr `((must ,result-var ,amd64-sysv-result))))
      (values (append split-attr result-attr)
              (append split-code align-code push-code the-call retrieval)))))

(define amd64-sysv-return
  (lambda (stmt blk)
    (let* ((res (cadr stmt))
           (res-name (newname 'result))
           (code `((copy ,res-name ,res)
                   (c-return)))
           (attr `((must ,res-name ,amd64-sysv-result))))
      (values attr code))))

(define amd64-linux-syscall
  (lambda (stmt blk)
    (let* ((dest (cadr stmt))
           (args (cddr stmt))
           (_ (if (> (length args) (length amd64-linux-args))
                  (error 'amd64-linux-syscall "too many arguments")))
           (split-names (map (lambda (var) (newname 'call)) args))
           (split-code (map make-copy split-names args))
           (split-attr (map make-must split-names (list-take amd64-linux-args
                                                             (length args))))
           (the-call `((syscall)))
           (result-var (newname 'result))
           (retrieval `((copy ,dest ,result-var)))
           (result-attr `((must ,result-var ,amd64-linux-result))))
      (values (append split-attr result-attr)
              (append split-code the-call retrieval)))))

(define amd64-call
  (lambda (stmt blk)
    (let* ((dest (cadr stmt))
           (func (caddr stmt))
           (args (cdddr stmt))
           (reg-args (list-take args (length amd64-regargs)))
           (stack-args (list-drop args (length amd64-regargs)))
           (split-names (map (lambda (var) (newname 'call)) reg-args))
           (link-name (newname 'link))
           (split-code (map make-copy split-names reg-args))
           (split-attr (cons `(must ,link-name ,amd64-link)
                             (map make-must split-names (list-take amd64-regargs
                                                                   (length reg-args)))))
           (stack-pad (= (mod (length stack-args) 2) 1))
           (align-code (if stack-pad
                           `((ldi tmp0 8)
                             (- sp sp tmp0))
                           '()))
           (push-code (apply append
                             (map (lambda (var)
                                    `((ldi tmp0 8)
                                      (- sp sp tmp0)
                                      (store sp ,var)))
                                  (reverse stack-args))))
           ;; don't know how long the jump will be yet, so the offset
           ;; will get processed during assembling
           (the-call `((make-alive ,link-name)
                       (call ,func)))
           (result-var (newname 'result))
           (retrieval `((copy ,dest ,result-var)))
           (result-attr `((must ,result-var ,amd64-result))))
      (values (append split-attr result-attr)
              (append split-code align-code push-code the-call retrieval)))))

(define argument-memory-slots
  (lambda (args regargs alignment)
    (let* ((memargs (max 0 (- args regargs)))
           (rem (mod memargs alignment)))
      (if (zero? rem)
          memargs
          (+ memargs (- alignment rem))))))
(define iota
  (lambda (n)
    (letrec ((iter (lambda (n acc)
                     (if (zero? n)
                         acc
                         (iter (- n 1) (cons (- n 1) acc))))))
      (iter n '()))))

(define amd64-tail-call
  (lambda (stmt blk)
    (let* ((dest (cadr stmt))
           (func (caddr stmt))
           (args (cdddr stmt))
           (entry-args (caddr blk))
           (reg-args (list-take args (length amd64-regargs)))
           (stack-args (list-drop args (length amd64-regargs)))
           (size-attr `((maxtail ,(length stack-args))))
           (old-args-space (argument-memory-slots (length entry-args)
                                                  (length amd64-regargs)
                                                  2))
           (new-args-space (argument-memory-slots (length args)
                                                  (length amd64-regargs)
                                                  2))
           (push-code (apply append
                             (map (lambda (var index)
                                    `((ldi tmp1 ,(* 8 (+ index (- old-args-space
                                                                  new-args-space))))
                                      (+ tmp1 tmp1 bp)
                                      ;; store will use tmp0 if var is spilled
                                      (store tmp1 ,var)))
                                  stack-args
                                  (iota (length stack-args)))))
           (copy-names (map (lambda (var) (newname 'arg)) reg-args))
           (copy-code (map make-copy copy-names reg-args))
           (copy-attr (map make-must copy-names (list-take amd64-regargs
                                                           (length reg-args)))))
      (values (append size-attr copy-attr) (append push-code copy-code)))))

(define amd64-return
  (lambda (stmt blk)
    (let* ((res (cadr stmt))
           (res-name (newname 'result))
           (code `((copy ,res-name ,res)
                   (return ,res-name)))
           (attr `((must ,res-name ,amd64-result))))
      (values attr code))))

(define rpasm->amd64-linux-wtasm
  (lambda (blk)
    (let* ((name (car blk))
           (attr (cadr blk))
           (args (caddr blk))
           (body (cdddr blk))
           (c-abi (memq 'c-abi attr)))
      (letrec ((iter
                (lambda (body attr-acc body-acc)
                  (if (null? body)
                      `(,name ,attr-acc ,args ,@(reverse body-acc))
                      (let* ((cur (car body))
                             (rest (cdr body))
                             (abi-proc (assq (car cur)
                                             `((call . ,amd64-call)
                                               (c-call . ,amd64-sysv-call)
                                               ,@(if c-abi
                                                     '()
                                                     `((tail-call . ,amd64-tail-call)))
                                               (syscall . ,amd64-linux-syscall)
                                               (return . ,(if c-abi
                                                              amd64-sysv-return
                                                              amd64-return))))))
                        (if abi-proc
                            (let-values (((new-attr new-body)
                                          ((cdr abi-proc) cur blk)))
                              (iter rest
                                    (append attr-acc new-attr)
                                    (append (reverse new-body) body-acc)))
                            (iter rest
                                  attr-acc
                                  (cons cur body-acc))))))))
        (iter body
              (append attr '((must tmp0 r10)
                             (must tmp1 r11)
                             (must sp rsp)
                             (must bp rbp)))
              '((prologue)))))))

;;; abstract assembly

;;; assembly

;;; local variables on the stack are addressed relative to the frame pointer

;;; object file

(define test-ill-typed
  '(ill-typed () (in)
              (i->f float in)
              (copy-label func-addr func)
              (tail-call func-addr float)))
(define test-program
  '((entry (c-abi) ()
           (copy-label main-addr main)
           (call res main-addr)
           (return res))
    (main () ()
          (copy-label loop-addr loop)
          (ldi zero 0)
          (call res loop-addr zero)
          (return zero))
    (loop () (n)
          (ldi limit 10)
          (goto-if (lt limit n) end)
          (copy-label print-int-addr print_int)
          (c-call void-res print-int-addr n)
          (ldi next 1)
          (+ next next n)
          (copy-label loop-addr loop)
          (tail-call loop-addr next)
          (label end)
          (ldi res 0)
          (return res))))
