(require racket/class)

; Класс игры
(define checkers%
  (class object%
    (super-new)
    (define b starting-board) ; доска
    (init-field player-x  ; игрок черных
                player-o) ; игрок белых
    
    ; запуск игры
    (define/public (run)
      ; TODO
      ; ask players to move and make those moves
    )

    ; Метод который выполняет ход в аргументе
    ; Метод предполагает что ход допустим
    (define (make-move move))
        (set-remove! board
        
  )
)

(define human-checkers
  (new checkers% [player-x human-player-x player-o human-player-o])
)

; Start Human Game
(define (shg)
  (send human-checkers run)
)

; Класс игрока
(define player%
  (class object%
    (super-new)
    ; Метод хода игрока - этот метод выбирает ход из предложенных
    (init-field claim-move)
  )
)

; Move (ход) представляет из себя '((a1 b1) (a2 b2) ... (an bn))
; Здесь фигура двигается с клетки a1 b1 на клетку an bn
; "съедая" все фишки a2 b2 ... a_(n-1) b_(n-1)
; Возвращает 
(define human-move read)

(define human-player-x (new player% [claim-move human-move]))
(define human-player-o (new player% [claim-move human-move]))


(define (inverse12 a)
  (if (eq? a 1)
    2
    1))

; структура доски; p - множество фишек, q - множество дамок
; 1 - первый игрок, 2 - второй игрок
(struct board (p1 q1 p2 q2))

; геттеры полей структуры board
(define p1s board-p1)
(define q1s board-q1)
(define p2s board-p2)
(define q2s board-q2)

; начальная позиция
(define starting-board (board
  (set '(1 1) '(1 3) '(1 5) '(1 7)
       '(2 2) '(2 4) '(2 6) '(2 8)
       '(3 1) '(3 3) '(3 5) '(3 7))
  (set '(4 2) '(4 4))
  (set '(6 2) '(6 4) '(6 6) '(6 8)
       '(7 1) '(7 3) '(7 5) '(7 7)
       '(8 2) '(8 4) '(8 6) '(8 8))
  (set '(5 1) '(5 3))))

; фишки черных изображены x, фишки белых - o
; дамки - заглавными буквами
(define (show-board b)
  (for ([i '(8 7 6 5 4 3 2 1)])
    (printf "~a " i)
    (for ([j '(1 2 3 4 5 6 7 8)])
      (display (cond
                 ((set-member? (p1s b) (list i j)) "|x")
                 ((set-member? (p2s b) (list i j)) "|o")
                 ((set-member? (q1s b) (list i j)) "|X")
                 ((set-member? (q2s b) (list i j)) "|O")
                 (else "| "))))
    (display "|\n"))
  (displayln "   1 2 3 4 5 6 7 8    "))

; temporary
(define (sb)
  (show-board starting-board);
