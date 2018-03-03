;this contains core functions to play texas hold'em.
;require: rules.lsp

(defun dependency_load ()
	(load "rules.lsp")
	)

(defun reload ()
	(load "play_holdem.lsp")
	)

(setq PACK_SIZE 52)
(setq MAXNUM 13)

; this is for the test purpose. make it sure those setq are disabled
(setq test_




;board(info,group,const): list has current game info 
;	- info(game #, phase #, stake, deck, minimum, maximum)
;		- game #(info): number of game (count from 1)
;		- phase #(info): number of phase (count from 0; 2 is max in holdem
;		- stake(info): total amount of chips in this game
;		- deck(info): current deck in this game (will use 13 cards in 4-player holdem)
;		- minimum(info): min. amount of chips to do call/check
;		- maximum(info): max. amount of chips to raise (nil if no limit game)
;		- opened card(info): number of opened card
;	- group(chips on the board, cards on the hand, status on the game, total, player number)
;		- chips on the board(group): chips player bet in this phase
; 	- cards on the hand(group): cards player holds right now
; 	- status on the game(group): player's most previous action of this phase
; 		actions: await(0), die(1), check(2), raise(3), call(4)
;		- total(group): total amount of chip player has
;		- pid(group): player number
;	- const(base, player #, card #, tax)
;		- base(const): base chip to join the game (game will be ended when at least one player cannot pay base)
;		- player #(const): number of players in the game (usually 4)
;		- card #(const): number of cards to be used (13 cards will be in this case)
;		- tax(const): in some case, poker is not an ideal zero-sum game. for every game, a winner will lose some of his dividend based on the tax rate (0 for no tax)

;players(player_name, player_chips, player_wins, player_loses, player_gain) - given
;const - given
(defun create_new_player (players num)
	(cond ((null players) NIL)
				(T (cons 
						 (list 0 nil nil (second (car players)) num) (create_new_player (cdr players) (- num 1))))
				)
	)

(defun create_new_board (players const)
	(list (list 0 0 0 nil 0 nil nil) (create_new_player players (length players)) const)
	)

(defun set_board (board info group const)
	(let ((in (if (null info) (first board) info))
				(gr (if (null group) (second board) group))
				(co (if (null const) (third board) const))
				)
		(list in gr co)
		)
	)

(defun set_info (info gnum pnum stake deck minimum maximum ocard)
	(let ((gn (if (null gnum) (first info) gnum))
				(pn (if (null pnum) (second info) pnum))
				(st (if (null stake) (third info) stake))
				(de (if (null deck) (fourth info) deck))
				(mi (if (null minimum) (fifth info) minimum))
				(ma (if (null maximum) (sixth info) maximum))
				(oc (if (null ocard) (sixth info) ocard))
				)
		(list gn pn st de mi ma oc)
		)
	)

(defun set_player (player chips cards status total pid)
	(let ((cp (if (null chips) (first player) chips))
				(cd (if (null cards) (second player) cards))
				(st (if (null status) (third player) (cons status (third player))))
				(to (if (null total) (fourth player) total))
				(id (if (null pid) (fifth player) pid))
				)
		(list cp cd st to id)
		)
	)

; this gives next card on the deck to the first player (of the board), and make the player goes to the end of the list
; warning: this does not concern about player's current action or amount of chips
; returns NIL if number of draw is larger than deck size
(defun draw_card (board num)
	(let* ((info (first board))
				 (deck (fourth info))
				 (group (second board))
				 (player (car group))
				 (rgroup (cdr group))
				 (cards (second player))
				 (remain (butlast deck num))
				 (draw (last deck num))
				 (const (third board))
				 )
		(cond ((null remain) NIL)
					(T (set_board 
							 (set_info info nil nil nil remain nil nil nil)
							 (append rgroup (list (set_player player nil (append cards draw) nil nil nil)))
							 const
							 )
						 )
					)
		)
	)

; adjust list of players order
(defun set_player_order (group fplayer)
	(cond ((= (fifth (car group)) fplayer) group)
				(T (set_player (append (cdr group) (list (car group))) fplayer))
				)
	)


; collect base chip from all player
; you can check if there is any player couldn't pay by counting result list's size should be equal to the size of group
; this also set player's current state as 'await' regardless its previous status
(defun collect_base (group base)
	(cond ((null group) NIL)
				((< (fourth (car group)) base) NIL)
				(T (cons 
						 (set_player (car group) base nil 0 (- (fourth (car group)) base) nil) 
						 (collect_base (cdr group) base)
						 )
					 )
				)
	)
				
(defun nd_helper (remain num len)
	(let* ((ran (random len))
				(pick (nth ran remain))
				(others (append (butlast remain (- len ran)) (last remain (- len (+ ran 1)))))
				)
		(cond ((= num 0) NIL)
					(T (cons pick (nd_helper others (- num 1) (- len 1))))
					)
		)
	)

(defun nd_helper_create_card (size)
	(cond ((= 0 size) NIL)
				(T (cons size (nd_helper_create_card (- size 1))))
				)
	)
; this creates new deck for the game
; size defines number of cards in the deck
; algorithm(count, pattern, action, cards) %% not implemented yet %%: this shuffles in a specific way. use nil to disable
; uses more than 4 suites if the size is bigger than 52
(defun new_deck (size algorithm)
	(let* ((pack (floor (/ size PACK_SIZE)))
				 (csize (* (+ pack 1) PACK_SIZE))
				 (cards (nd_helper_create_card csize))
				 )
		(nd_helper cards size (length cards))
		)
	)
	
; start game, if winner is nil, then the first player will be the first
;	info(game #, phase #, stake, deck, minimum, maximum, opened card)
;	group(chips on the board, cards on the hand, status on the game, total, player number)
;	const(base, player #, card #, tax)
(defun start_game (board winner); winner=pid
	(let* ((info (first board))
				 (group (if (null winner) (second board) (set_player_order (second board) winner)))
				 (const (third board))
				 (base_chip (first const))
				 (card_num (third const))
				 (collected_group (collect_base (group base_chip)))
				 (base_ready (if (= (length collected_group) (length group)) T NIL))
				 )
		(cond ((null base_ready) NIL)
					(T (set_board (set_info info (+ (first info) 1) 0 (* (length collected_group) base_chip) (new_deck card_num nil) 0 nil nil) collected_group const))
					)
		)
	)

(defun get_player_data (group)
	(cond ((null group) null)
				(T (cons 
						 (list (third (car group)) (first (car group)) (fourth (car group)) (fifth (car group))) 
						 (get_player_action (cdr group)))
					 )
				)
	)

; this creates list of info. for player: 
;		(my card, opened card, stake, {player's action, player's chips on the board, 
;			player's total chips}(action,chip,total,pid), current minimum, current maximum)
(defun get_current_board (board pid)
	(let* ((info (first board))
				 (group (set_player_order (second board) pid))
				 (my_card (second (car group)))
				 (opened_card (seventh info))
				 (stake (third info))
				 (player_info (get_player_action (cdr group)))
				 (cmin (fifth info))
				 (cmax (sixth info))
				 )
		(list my_card opened_card stake player_info cmin cmax)
		)
	)
	
(defun player_action (board pid action)
	(let* ((info (first board))
