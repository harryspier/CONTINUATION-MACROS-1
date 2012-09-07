#lang racket

; let form of call-with-continuation-prompt and call/prompt
;(let/set-prompt ([prompt: prompt] [handler: handler] args) body)
; prompt and handle are optional.
; Examples:
; (let/set-prompt () (+ 1 2 3 4))
; (let/set-prompt ([prompt: prompt-1])  (+ 1 2 3 4))
; (let/set-prompt ([prompt: prompt-1] [handler: handler-1]) (+ 1 2 3 4))
; (let/set-prompt ([prompt: prompt-1] [handler: handler-1] [val 4]) (+ 1 2 3 val))
(define-syntax let/set-prompt
  (syntax-rules (prompt: handler:)
[(_ ((prompt:  PROMPT) (handler: HANDLER) ARGS ...) BODY ...) (call-with-continuation-prompt  (λ () (let (ARGS ...) BODY ...)) PROMPT HANDLER)]
[(_ ((handler: HANDLER) (prompt:  PROMPT) ARGS ...) BODY ...) (call-with-continuation-prompt  (λ () (let (ARGS ...) BODY ...)) PROMPT HANDLER)]
[(_ ((prompt:  PROMPT)                    ARGS ...) BODY ...) (call-with-continuation-prompt  (λ () (let (ARGS ...) BODY ...)) PROMPT)]
[(_ ((handler: HANDLER)                   ARGS ...) BODY ...) (call-with-continuation-prompt  (λ () (let (ARGS ...) BODY ...)) PROMPT)]
[(_ (                                     ARGS ...) BODY ...) (call-with-continuation-prompt  (λ () (let (ARGS ...) BODY ...)))]))


; let form for call-with-composable-continuation and call/comp
;(let/get-compc ([upto: prompt]  [continuation: cont] args) body)
;(let/get-compc ([continuation: cont] [upto: prompt]  args) body)
;(let/get-compc ([continuation: cont] args) body)
; prompt is optional
; and keywords upto: and continuation: can be in any order
; If no arguments are passed to the let body then args must be ()
; as in a let form.
(define-syntax let/get-compc
  (syntax-rules (upto: continuation:)
[(_ ((upto:  PROMPT)    (continuation: CONT) ARGS ...) BODY ...) (call-with-composable-continuation  (λ (CONT) (let (ARGS ...) BODY ...)) PROMPT)]
[(_ ((continuation: CONT) (upto:     PROMPT) ARGS ...) BODY ...) (call-with-composable-continuation  (λ (CONT) (let (ARGS ...) BODY ...)) PROMPT)]
[(_ ((continuation: CONT)                      ARGS ...) BODY ...) (call-with-composable-continuation  (λ (CONT) (let (ARGS ...) BODY ...)))]))


; let form for call-with-current-continuation and call/cc
;(let/get-cc ([upto: prompt]  [continuation: cont] args) body)
;(let/get-cc ([continuation: cont] [prompt: prompt]  args) body)
;(let/get-cc ([continuation: cont] args) body)
; prompt is: optional
; and keywords upto: and continuation: can be in any order
; If no arguments are passed to the let body then args must be ()
; as in a let form.
(define-syntax let/get-cc
  (syntax-rules (upto: continuation:)
[(_ ((upto:  PROMPT)    (continuation: CONT) ARGS ...) BODY ...) (call-with-current-continuation  (λ (CONT) (let (ARGS ...) BODY ...)) PROMPT)]
[(_ ((continuation: CONT) (upto:     PROMPT) ARGS ...) BODY ...) (call-with-current-continuation  (λ (CONT) (let (ARGS ...) BODY ...)) PROMPT)]
[(_ ((continuation: CONT)                      ARGS ...) BODY ...) (call-with-current-continuation  (λ (CONT) (let (ARGS ...) BODY ...)))]))


;Simplified form of abort-current-continuation
;Takes a single body as a parameter, and the body is placed in a thunk by the macro.
(define-syntax abort-to-prompt 
  (syntax-rules () 
     [(_ BODY) (abort-current-continuation (default-continuation-prompt-tag) (thunk BODY))]
     [(_ PROMPT-TAG BODY) (abort-current-continuation PROMPT-TAG (thunk BODY))]))

;Macro to save a continuation
;Example:
;(save-this-continuation upto: prompt-1 
;                              in: saved-continuation 
;                              return: 0)
; "upto: prompt" is optional in which case it uses the default prompt.
(define-syntax save-this-continuation
  (syntax-rules (upto: in:)
    [(_ upto: PROMPT in: VAR return: VAL)
     (let/get-compc ( [upto: PROMPT] [continuation: k]) 
     (set! VAR k) VAL)]
    
    [(_ upto: PROMPT  return: VAL in: VAR)
     (let/get-compc ( [upto: PROMPT] [continuation: k]) 
     (set! VAR k) VAL)]
    
     [(_ upto: PROMPT   in: VAR)
     (let/get-compc ( [upto: PROMPT] [continuation: k]) 
     (set! VAR k) 0)]
    
    [(_ in: VAR upto: PROMPT  return: VAL)
     (let/get-compc ( [upto: PROMPT] [continuation: k]) 
     (set! VAR k) VAL)]
    
    [(_ in: VAR return: VAL  upto: PROMPT)
      (let/get-compc ( [upto: PROMPT] [continuation: k]) 
       (set! VAR k) VAL)]
    
     [(_ in: VAR  return: VAL)
      (let/get-compc ([continuation: k]) 
       (set! VAR k) VAL)]
    
    [(_ return: VAL in: VAR upto: PROMPT   )
      (let/get-compc ( [upto: PROMPT] [continuation: k]) 
       (set! VAR k) VAL)]
    
    [(_ return: VAL upto: PROMPT in: VAR )
      (let/get-compc ( [upto: PROMPT] [continuation: k]) 
       (set! VAR k) VAL)]
    
     [(_ return: VAL in: VAR  )
      (let/get-compc ( [upto: PROMPT] [continuation: k]) 
       (set! VAR k) 0)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Some examples to play with continuations and prompts


(define prompt-1 (make-continuation-prompt-tag))
(define prompt-2 (make-continuation-prompt-tag))
(define prompt-3 (make-continuation-prompt-tag))

(define (play-with-prompts [p (default-continuation-prompt-tag)])
  (+ 1 
     (let/set-prompt ([prompt: prompt-1] [val 10] )  (+ val 
        (let/set-prompt ([prompt: prompt-2] [val 100]) (+ val
           (let/set-prompt ([prompt: prompt-3] [val 1000]) (+ val
              (abort-to-prompt p 0) 2000)) 200)) 20))))

;(+ 100
;   (let/get-compc ([continuation: k]) (+ 1000) (k 1)))
;   
;   (call-with-composable-continuation
;    (λ (k) (+ 1000 (k 1)))))
(displayln "play-with-prompts")
(play-with-prompts prompt-1)
(play-with-prompts prompt-2)
(play-with-prompts prompt-3)
;(play-with-prompts)

(define (play-with-compc [p (default-continuation-prompt-tag)])
  (let/set-prompt  ([val 1] )  (+ val ; [p0 + 1 []]
     (let/set-prompt ([prompt: prompt-1] [val 10] )  (+ val  ; p0[ (+ 1 p1[])]
        (let/set-prompt ([prompt: prompt-2] [val 100]) (+ val ;p0[(+ 1 p1[(+ 10 p2[])]]]
           (let/set-prompt ([prompt: prompt-3] [val 1000]) (+ val ;p0[(+ 1 p1[(+ 10 p2[(+ 100 p3[])])])]]
              (let/get-compc ([continuation: k] [upto: p]) (+ 10000 (k 1)))))))))))) ;p0[(+ 1 p1[(+ 10 p2[(+ 100 p3[(+ 1000 [exp])])])])]

(define (play-with-curc [p (default-continuation-prompt-tag)])
  (let/set-prompt  ([val 1] )  (+ val ; [p0 + 1 []]
     (let/set-prompt ([prompt: prompt-1] [val 10] )  (+ val  ; p0[ (+ 1 p1[])]
        (let/set-prompt ([prompt: prompt-2] [val 100]) (+ val ;p0[(+ 1 p1[(+ 10 p2[])]]]
           (let/set-prompt ([prompt: prompt-3] [val 1000]) (+ val ;p0[(+ 1 p1[(+ 10 p2[(+ 100 p3[])])])]]
              (let/get-cc ([continuation: k] [upto: p]) (+ 10000 (k 1)))))))))))) ;p0[(+ 1 p1[(+ 10 p2[(+ 100 p3[(+ 1000 [exp])])])])]
 

(displayln "play-with-composable-continuations")
(play-with-compc prompt-1)
(play-with-compc prompt-2)
(play-with-compc prompt-3)

(displayln "play-with-current-continuations")
(play-with-curc prompt-1)
(play-with-curc prompt-2)
(play-with-curc prompt-3)

(displayln "mutating a continuation")
(let/set-prompt  ([val 1] )  (+ val ; [p0 + 1 []]
     (let/set-prompt ([prompt: prompt-1] [val 10] )  (+ val  ; p0[ (+ 1 p1[])]
        (let/set-prompt ([prompt: prompt-2] [mut-val 100]) (+ mut-val ;p0[(+ 1 p1[(+ 10 p2[])]]]
           (let/set-prompt ([prompt: prompt-3] [val 1000]) (+ val ;p0[(+ 1 p1[(+ 10 p2[(+ 100 p3[])])])]]
              (let/get-cc ([continuation: k] [upto: prompt-1])  
                          (set! mut-val 9000) 
                          (+ 10000 (k 1))))))))))) ;p0[(+ 1 p1[(+ 10 p2[(+ 100 p3[(+ 1000 [exp])])])])]

(newline)



(define saved-continuation #f)
(define x 10)

(let/set-prompt ([prompt: prompt-1])
   (+ 4 (+ 3 (+ 2 (save-this-continuation upto: prompt-1 
                              in: saved-continuation 
                              return: 0) x))))

(saved-continuation 0)
(set! x 100)
(saved-continuation 0)

(set! x 10)

(define saved-k #f)
(call-with-continuation-prompt 
 (lambda ()
   (+ 4 (+ 3 (+ 2 (call-with-composable-continuation
                   (lambda (k) ; k is the captured continuation
                     (set! saved-k k)
                     0) prompt-1) x)))) prompt-1)


 (saved-k 0)
 (set! x 100)
 (saved-k 0)
 