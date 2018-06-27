;This contains a board for players

;the following setq is created for the test purpose



; createBoard takes (createTable createGame createOption) returns board
; createTable takes (maxRaise minChip TableID gameNum playerInfo) returns Table
; createGame takes ( ([list 4] myCard) ([list 4] playerNum TotalChip battingAmt prevAction) ([list 1] chipOnTheTable minRaise SBNum turnNum openCard phase deck))

; prevaction = {0=none,1=wait,2=small_blind,3=big_blind,4=in_action,5=checked,6=bet,7=call,8=raise,9=fold}

; OPTIONS

(defun reload () (load "board.lsp"))

(defun makeNilList (num)
	(cond ((> num 0) (cons NIL (makeNilList (- num 1))))
				(T NIL)
				)
	)


;; Deck

(defun constDeck (num)
	(if (< num 1) NIL (cons num (constDeck (- num 1))))
	)

(defun randomizeDeck (lst num)
	(if (< num 1) NIL
		(let* ((rand (random num))
					(pick (nth rand lst))
					(pre (butlast lst (- num rand)))
					(post (last lst (- num (+ rand 1))))
					(newlst (append (if (listp pre) pre (list pre)) (if (listp post) post (list post))))
					)
			(cons pick (randomizeDeck newlst (- num 1)))
			)
		)
	)

(defun createDeck (num)
	(let ((initDeck (constDeck num))
				)
		(randomizeDeck initDeck num)
		)
	)





; ENDOPTIONS

;players contains (playerID Chip Win Lose)
(defun createProfile (players num)
	(cond ((null players) NIL)
				(T (cons (cons num players) (createProfile (cdr players (- num 1)))))
				)
	)

(defun createBoard (tid players)
	(let* ((ct (createTable NIL 5 tid 0 players))
				 (cg (createGame NIL NIL NIL players)) 
				 (co (createOption NIL))
				 )
		(list ct cg co)
		)
	)

(defun createTable (maxRaise minChip tableID gameNum playerInfo)
	(let ((mr (if (null maxRaise) 0 maxRaise))
				(mc (if (null minChip) 2 minChip))
				(tid (if (null tableID) (error "table ID must be provided") tableID))
				(gn (if (null gameNum) 0 gameNum))
				(pnf (if (null playerInfo) (error "player information must be provided") playerInfo))
				)
		(list mr mc tid gn pnf)
		)
	)

(defun createChart (players)
	(cond ((null lst) NIL)
				(T (cons (list (caar players) (cadar players) 0 0) (createChart (cdr players))))
				)
	)

(defun createGame (private public global players)
	(let* ((mp (length players))
				 (pv (if (null private) (makeNilList mp)) private)
				 (pu (if (null public) (createChart players) public))
				 (gb (if (null global) (makeNilList 8) global))
				 )
		(list pv pu gb)
		)
	)

(defun createOption (none)
	(list NIL NIL NIL)
	)

; createTable = (maxRaise minChip TableID 0 playerInfo)
; createGame = (forall_list none) (forall_list playernum totalchip 0 0) (0 10 1 3 0 0)

(defun initGame_initProfile (profile)
	(if (null profile) 
		NIL
		(let ((cp (car profile)))
			(cons (list (first cp) (second cp) 0 0) (initGame_initProfile (cdr profile)))
			)
		)
	)

(defun initGame (board)
	(let* ((table (first board))
				(game (second board))
				(option (third board))
				(mr (first table))
				(mc (second table))
				(tid (third table))
				(pf (fifth table))
				(card (first game))
				(profile (second game))
				(global (list 0 10 1 3 0 0))
				)
		(list (list mr mc tid 0 pf) (list card (initGame_initProfile profile) global) option)
		)
	)

; createBoard takes (createTable createGame createOption) returns board
; createTable takes (maxRaise minChip TableID gameNum playerInfo) returns Table
; createGame takes ( ([list 4] myCard) ([list 4] playerNum TotalChip battingAmt prevAction) ([list 1] chipOnTheTable minRaise SBNum turnNum openCard phase deck))

(defun changeTable (table &key mr mc tid gn pf)
	(let ((nmr (if (null mr) (first table) mr))
				(nmc (if (null mc) (second table) mc))
				(ntid (if (null tid) (third table) tid))
				(ngn (if (null gn) (fourth table) gn))
				(npf (if (null pf) (fifth table) pf))
				)
		(list nmr nmc ntid ngn npf)
		)
	)

(defun changeGame_card (game num card) ; change (num)th card set in (game) as (card)
	(if (< (num) (length game))
		(append (butlast game (- (length game) num)) (list card) (last game (- (length game) (+ num 1))))
		(error "number cannot be larger than the list")
		)
	)

; startGame -> playerAction -> CheckAction
;																					-> EndGame
;																					-> playerAction
;																					-> nextPhase
(defun startGame (board)
	(let* ((table (first board))
				 (game (second board))
				 )
		(
