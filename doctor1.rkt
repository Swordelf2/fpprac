; заготовка "Доктора". Август 2019
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name '())
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response history))
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history)
      (case (random (if (null? history) 2 3)) ; с равной вероятностью выбирается один из двух способов построения ответа
                    ; (или 3-х если история непуста)
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
          ((2) (history-answer history)) ; 3й способ
      )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random '((you seem to think that)
                               (you feel that)
                               (why do you believe that)
                               (why do you say that)
                               ; Ex.1 phrases
                               (it seems as though)
                               (try to explain why)
                               (what in your opinions is the reason that))
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace-higher '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
						(myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
						(yourself myself))
                      phrase)
 )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
  )

; Ex.2 Iterative 'many-replace'
(define (many-replace-iterative replacement-pairs lst)
  ; pop from lst, process and push onto result one-by-one
  ; then reverse result
   (let loop ((lst lst) (result '()))
     (if (null? lst)
         (reverse result)
         (let ((pat-rep (assoc (car lst) replacement-pairs)))
           (let ((elem (if pat-rep (cadr pat-rep) (car lst))))
             (loop (cdr lst) (cons elem result))
           )
         )
     )
 )
)

; Ex.3 'many-replace' with 'map'
(define (many-replace-higher replacement-pairs lst)
  (map
   (lambda (word)
     (let ((pat-rep (assoc word replacement-pairs)))
       (if pat-rep
           (cadr pat-rep)
           word
       )
     )
   )
   lst
  )
)
          
  
; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
       (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       ; Ex. 1 phrases
                       (you do not have to be ashamed of that)
                       (would you care to elaborate?)
                       (feel free to share your feelings)
                       )
         )
)

; Ex.4 history answer
(define (history-answer history)
  (append '(earlier you said that) (pick-random history))
)