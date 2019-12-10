(require racket/class
         racket/set
         racket/promise
         racket/serialize
         compatibility/mlist
         (only-in racket/list shuffle argmax))

;;--------------------------------------------------------------------
;; Реализация минимакса с альфа-бета отсечением
(define (minimax tree)
  (define (minimax-h node alpha beta max-player)
    (define (next-max x v)
      (if (or (null? x) (<= beta v)) 
                v
                (next-max (cdr x)
                      (max v (minimax-h (car x) v beta (not max-player)))))
    )
    (define (next-min x v)
      (if (or (null? x) (<= v alpha)) 
                v
                (next-min (cdr x)
                      (min v (minimax-h (car x) alpha v (not max-player)))))
    )
    (cond 
         ((number? node) node)
         ((null? node) 0.0)
         (max-player (next-max node alpha))
         (else (next-min node beta)))
          
  )
  (minimax-h tree -inf.0 +inf.0 #f)
 )


;;--------------------------------------------------------------------
;; описание класса, реализующего логику и оптимальную стратегию произвольной игры с нулевой суммой и полной информацией
(define game%
  (class object%
    (super-new)
 
    ;; виртуальные методы для задания правил игры
    (init-field my-win?         ; State -> Bool
                my-loss?        ; State -> Bool
                draw-game?      ; State -> Bool
                my-move         ; State Move -> State
                opponent-move   ; State Move -> State
                possible-moves  ; State -> (list Move)
                show-state)     ; State -> Any
 
    ;; optimal-move :: State -> Move
    ;; выбор оптимального хода по минимаксу 
    ;; из нескольких оптимальных выбирается один случайно
    (define/public ((optimal-move look-ahead) S)
      (argmax (lambda (m) (minimax (game-tree S m look-ahead)))
                 (shuffle (possible-moves S))))
 
    ;; game-tree :: State -> (Move -> (Tree of Real))
	;; построение дерева с оценками
    (define (game-tree St m look-ahead)
	   ;; вспомогательная функция, строящая закольцованный список из пары элементов
      (define (help a b) (begin (define l (mlist a)) (set-mcdr! l (mcons b l)) l))
      (define (new-ply moves i s)	  
        (cond
          ((my-win? s) +inf.0) ; в выигрышной позиции оценка = + бесконечность
          ((my-loss? s) -inf.0) ; в проигрышной = - бесконечность
          ((draw-game? s)     0) ; при ничье = 0
          ((>= i look-ahead)  (f-h s)) ; если исчерпана глубина, то используется эвристическая оценка 
          (else (map (lambda (x) (new-ply (mcdr moves) (+ 1 i) ((mcar moves) s x)))
                     (possible-moves s))) ; рассматриваем все возможные ходы и строим их оценки
		))
     (new-ply (help opponent-move my-move) 1 (my-move St m))
    )
 
    ;; make-move :: State (State -> Move) -> (Move State Symbol)
    (define/public (make-move S move)
      (cond
        ((my-loss? S)   (values '() S 'loss))         
        ((draw-game? S) (values '() S 'draw))   
        (else (let* ((m* (move S))
                     (S* (my-move S m*)))
                (cond
                  ((my-win? S*)    (values m* S* 'win)) 
                  ((draw-game? S*) (values m* S* 'draw))
                  (else            (values m* S* 'next))))))
	)
  ))
 
;;--------------------------------------------------------------------
;; Реализация класса игрока
;; Параметр `game` указывает, в какая игра решается.
(define (interactive-player game)
  (class game
    (super-new)
 
    (inherit-field show-state)
    (inherit make-move optimal-move)
 
    (init-field name
                [look-ahead 4]
                [opponent 'undefined]
                [move-method (optimal-move look-ahead)])
 
    (define/public (your-turn S)
      (define-values (m S* status) (make-move S move-method))
      (printf "\n~a makes move ~a\n" name m)
      (show-state S*)
      (case status
           ['stop (displayln "The game was interrupted.")]
           ['win  (printf "~a won the game!" name)]
           ['loss (printf "~a lost the game!" name)]
           ['draw (printf "Draw!")]
           [else (send opponent your-turn S*)]))))
 
 
;;--------------------------------------------------------------------
;; макрос для описания партнеров в игре
(define-syntax-rule 
  (define-partners game (A #:win A-wins #:move A-move) 
                        (B #:win B-wins #:move B-move))
  (begin
    (define A (class game 
                (super-new 
                 [my-win?  A-wins]
                 [my-loss? B-wins]
                 [my-move  A-move]
                 [opponent-move B-move])))
    (define B (class game 
                (super-new 
                 [my-win?  B-wins]
                 [my-loss? A-wins]
                 [my-move  B-move]
                 [opponent-move A-move])))))
 
;;--------------------------------------------------------------------
;; функция, инициализирующая игру
(define (start-game p1 p2 initial-state)
  (set-field! opponent p1 p2)
  (set-field! opponent p2 p1)
  (send p1 your-turn initial-state))
;; пример использования
;; (!(start-game player-A player-B empty-board)) 

;;--------------------------------------------------------------------
;; Описания Чекерсов
 
; структура доски; p - множество фишек, q - множество дамок
; 1 - первый игрок, 2 - второй игрок
; s - какой игрок ходит (1 или 2)
(struct board (p1 q1 p2 q2 s))

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
  (set)
  (set '(6 2) '(6 4) '(6 6) '(6 8)
       '(7 1) '(7 3) '(7 5) '(7 7)
       '(8 2) '(8 4) '(8 6) '(8 8))
  (set)
  1))

; TODO wins?
 
;; хэш-таблица для мемоизированной функции эвристичечкой оценки
(define f-h-hash (make-hash))

;; мемоизированная функция эвристической оценки позиции
(define (f-h-memo s)  
	(let ((prev-calc (hash-ref f-h-hash s #f)))
		(if prev-calc prev-calc
			(let ((current-calc (f-h s)))
				(hash-set! f-h-hash s current-calc)
				current-calc
)	)	)	)


;; функция эвристической оценки позиции
(define (f-h s) 0)
  ; TODO
 

; Move (ход) представляет из себя '((a1 b1) (a2 b2) ... (an bn))
; Здесь фигура двигается с клетки a1 b1 на клетку a2 b2
; "съедая" все фишки a3 b3 ... an bn
; Возвращает 
;; функции для осуществления ходов игроков
(define (move1 b m)
  (let* ((start-pos (car m)) ; начальная позиция фишки
         (end-pos (cadr m))  ; конечная позиция фишки
         (killed (list->set (cddr m)))  ; множество съедаемых фишек
         (new-p1s (set-remove (p1s b) start-pos))
         (new-q1s (set-remove (q1s b) start-pos))
         (new-p2s (set-subtract (p2s b) killed))
         (new-q2s (set-subtract (q2s b) killed)))
    ; фишка превращается в дамку если она либо была дамкой,
    ; либо встала на последнюю строку
    (if (or (set-member? (q1s b) start-pos)
            (eq? (car end-pos) 8))
        (board new-p1s (set-add new-q1s end-pos) new-p2s new-q2s 2)
        (board (set-add new-p1s end-pos) new-q1s new-p2s new-q2s 2)
    )
))
         
(define (move2 b m)
  (let* ((start-pos (car m)) ; начальная позиция фишки
         (end-pos (cadr m))  ; конечная позиция фишки
         (killed (list->set (cddr m)))  ; множество съедаемых фишек
         (new-p2s (set-remove (p2s b) start-pos))
         (new-q2s (set-remove (q2s b) start-pos))
         (new-p1s (set-subtract (p1s b) killed))
         (new-q1s (set-subtract (q1s b) killed)))
    ; фишка превращается в дамку если она либо была дамкой,
    ; либо встала на первую строку
    (if (or (set-member? (q2s b) start-pos)
            (eq? (car end-pos) 1))
        (board new-p1s new-q1s new-p2s (set-add new-q2s end-pos) 1)
        (board new-p1s new-q1s (set-add new-p2s end-pos) new-q2s 1)
    )
))

; находится ли клетка в пределах доски
(define (good-pos? pos)
  (and (<= (car pos) 8)
       (>= (car pos) 1)
       (<= (cadr pos) 8)
       (>= (cadr pos) 1)))

; перещаем фишку в сторону dir (сумма пар)
(define (move-in-dir pos dir)
  (list (+ (car pos) (car dir))
        (+ (cadr pos) (cadr dir))))

(define (possible-moves-checkers b)
  ; возвращает список возможных ходов для одной фишки
  ; dir1 - в какую сторону по вертикали может двигаться фишка
  ; dir1 - (+1), (-1) или (+1 -1) (если дамка)
  (define (possible-moves-single-checker pos my-pieces enemy-pieces dir1)
    (let* ((dirs (case dir
                   [('(1)) '((1 -1) (1 1))]
                   [('(-1)) '((-1 -1) (-1 1))]
                   [('(-1 1)) '((-1 -1) (-1 1) (1 -1) (1 1))]))
           ; перещаемся в каждом из возможных направлений dirs
           (new-pos-list (foldl
                (lambda (dir new-pos-list)
                  (cons (move-in-dir pos dir) new-pos-list))
                '()
                dirs))

  ; рекурсивная функция, возвращающая список всевозможных ходов в ниже указанном формате
  ; kills-only - если #t то возвращаются только ходы со взятиями
  (define (possible-moves-recursive pos kills-only)
    ; move-list - список всевозможных ходов из позиции pos в виде (end-pos kill1 kill2 ... )
    (foldl
        (lambda (dir move-list)
          (let* ((new-pos (move-in-dir pos dir)))
            ; проверяем что позиция в пределах доски
            (if (and (good-pos? new-pos))
              (cond 
                ; если в new-pos - фишка противника, то пробуем ее съесть
                [(set-member? enemy-pieces new-pos)
                    (let ((new-pos2 (move-in-dir (new-pos dir))))
                      ; проверяем что результирующая клетка пустая
                      (if (and (good-pos? new-pos2)
                               (not (set-member? my-pieces new-pos2))
                               (not (set-member? enemy-pieces new-pos2)))
                        ; рекурсивно ищем все ходы из новой позиции только со взятиями
                        (let ((later-moves (possible-moves-recursive new-pos2 #t)))
                          ; в каждом из них нужно извлечь cdr - список взятий и car - end-pos
                          ; к взятиям нужно прибавить kill1 = new-pos, end-pos оставить неизменным
                          ; и собрать обратно в список вида (end-pos kill1 kill2 ... )
                          ; и в конце append-нуть все эти ходы к move-list
                          (append (foldl
                            (lambda (move move-list-local)
                              (cons (cons (car move) (cons new-pos (cdr move))) move-list-local))
                            '()
                            later-moves)
                          move-list))
                        move-list))]
                ; иначе, если клетка пустая, то можем просто переместиться на нее если kills-only == #f
                [(and (not (set-member? my-pieces new-pos)) (eq? kills-only #f)) (cons new-pos move-list)]
                ; иначе ход в new-pos мы сделать не можем
                [else move-list])
              move-list)))
        '()
        dirs))
  )
  (if (eq? (board-s b) 1)
    ; выполняем для каждой фишки
      (foldl
        (lambda (pos union)
          (cons pos union))
        '()
        (set->list (p1s b)))
      #f
))

(define (wins1? b)
  #f)

(define (wins2? b)
  #f)
 
;; вывод игровой доски в текстовом виде
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
 
;;--------------------------------------------------------------------
;; Описание класса игры Чекерс
(define checkers%
  (class game%
    (super-new
     [draw-game?       (lambda (b) #f)]
     [possible-moves   possible-moves-checkers]
     [show-state       show-board])))
 
;; описания партнеров для крестиков-ноликов
(define-partners checkers%
  (x% #:win wins1? #:move move1)
  (o% #:win wins2? #:move move2))
 
;; объекты-игроки ИИ
(define player-A (new (interactive-player x%) [name "A"] [look-ahead 6]))
 
(define player-B (new (interactive-player o%) [name "B"] [look-ahead 6]))
 
;; объекты-игроки, принимающие ввод пользователя
;; проверка ввода частичная
(define (input-move m)
  (case m
    ('q (exit))
    (else m)))

(define user-A 
  (new (interactive-player x%) 
       [name "User X"]
       [move-method 
        (lambda (b) (input-move (read)))]
		)
 )
(define user-B 
  (new (interactive-player o%) 
       [name "User O"]
       [move-method 
        (lambda (b) (input-move (read)))]
		)
 )
;; старт игры двух человек
; (!(start-game user-A user-B empty-board))
 
;; Объекты-игроки, делающие слабые ходы.
(define dummy-A 
  (new (interactive-player x%) [name "Dummy A"] [look-ahead 0]))
(define dummy-B 
  (new (interactive-player o%) [name "Dummy B"] [look-ahead 0]))
;; старт игры двух слабых игроков
; (!(start-game dummy-A dummy-B empty-board))

(define (sg)
  (start-game player-A player-B starting-board))

(define (su)
  (show-board starting-board)
  (start-game user-A user-B starting-board))
