#lang rosette

(require rosette/lib/synthax)

(define inode_t integer?)
(define filename_t integer?)

(struct state
  (inode-nlink ; (~> inode_t integer?)
   fname-inode) ; (~> filename_t inode_t)
  #:mutable #:transparent)

(define (state-equiv? s t)
  (define-symbolic %0 integer?)
  (&& (forall (list %0) (= ((state-inode-nlink s) %0) ((state-inode-nlink t) %0)))
      (forall (list %0) (= ((state-fname-inode s) %0) ((state-fname-inode t) %0)))))

(define (update fn key value)
  (lambda (x) (if (= x key) value (fn x))))

(define (init-state)
  (define-symbolic* inode-nlink (~> inode_t integer?))
  (define-symbolic* fname-inode (~> filename_t inode_t))
  (state inode-nlink fname-inode))

(define (update-fname-inode! s key value)
  (let ([old-fn (state-fname-inode s)])
    (set-state-fname-inode! s (update old-fn key value))))

(define (update-inode-nlink! s key value)
  (let ([old-fn (state-inode-nlink s)])
    (set-state-inode-nlink! s (update old-fn key value))))

(define (contains? dir filename)
  (not (= (dir filename) 0)))

(define (rename s src dst)
  (cond
    [(not (contains? (state-fname-inode s) src)) 'ENOENT]
    [(= src dst) 'EOK]
    [else
      (when (contains? (state-fname-inode s) dst)
        (update-inode-nlink! s
          ((state-fname-inode s) dst)
          (- ((state-inode-nlink s) dst) 1)))
      (update-fname-inode! s dst ((state-fname-inode s) src))
      (update-fname-inode! s src 0)
      'EOK]))

(define-symbolic fname-inode (~> integer? integer?))
(define-symbolic inode-nlink (~> integer? integer?))
(define-symbolic src1 src2 dst1 dst2 integer?)

(define s (state inode-nlink fname-inode))
(define t (state inode-nlink fname-inode))

(define sres1 (rename s src1 dst1))
(define sres2 (rename s src2 dst2))

(define tres1 (rename t src2 dst2))
(define tres2 (rename t src1 dst1))

(define (pprint-model model)
  (displayln `((src1 . ,(evaluate src1 model))
           (dst1 . ,(evaluate dst1 model))
           ((fname-inode ,(evaluate src1 model)) . ,(evaluate (fname-inode src1) model))
           (src2 . ,(evaluate src2 model))
           (dst2 . ,(evaluate dst2 model))
           ((fname-inode ,(evaluate src2 model)) . ,(evaluate (fname-inode src2) model)))))

(define (make-test model)
  (define src1-inode (evaluate (fname-inode src1) model))
  (define src2-inode (evaluate (fname-inode src2) model))
  (define dst1-inode (evaluate (fname-inode dst1) model))
  (define dst2-inode (evaluate (fname-inode dst2) model))

  (printf "test() {\n")
  (for ([val (list src1-inode src2-inode dst1-inode dst2-inode)])
    (when (not (= val 0))
      (printf "  close(open(\"__i~v\", O_CREAT|O_RDWR, 0666));\n" val)))
  (printf "}\n"))

(define commutes
  (&& (equal? sres1 tres2)
      (equal? sres2 tres1)
      (state-equiv? s t)))

(displayln commutes)

(define model (solve commutes))
(pprint-model model)
(make-test model)

