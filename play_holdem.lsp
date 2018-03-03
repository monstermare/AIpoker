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
(setq test_players (list
										 (list "AB1324" 4000 0 0 0)
										 (list "DD3521" 3000 0 0 0)
										 (list "HK2352" 4400 0 0 0)
										 (list "XF2233" 4900 0 0 0)
										 )
			)

(setq test_const (list (list 1 2) 4 13 0))

;	- const(base, player #, card #, tax)


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
	(list (list 0 0 0 nil 0 nil nil nil) (create_new_player players (length players)) const)
	)

(defun set_board (board info group const)
	(let ((in (if (null info) (first board) info))
				(gr (if (null group) (second board) group))
				(co (if (null const) (third board) const))
				)
		(list in gr co)
		)
	)

(defun set_info (info gnum pnum stake deck minimum maximum ocard dealer)
	(let ((gn (if (null gnum) (first info) gnum))
				(pn (if (null pnum) (second info) pnum))
				(st (if (null stake) (third info) stake))
				(de (if (null deck) (fourth info) deck))
				(mi (if (null minimum) (fifth info) minimum))
				(ma (if (null maximum) (sixth info) maximum))
				(oc (if (null ocard) (seventh info) ocard))
				(dl (if (null dealer) (eighth info) dealer))
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
; for holdem, it may not be used
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
							 (set_info info nil nil nil remain nil nil nil nil)
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
				(T (set_player_order (append (cdr group) (list (car group))) fplayer))
				)
	)


; collect base chip from all player
; you can check if there is any player couldn't pay by counting result list's size should be equal to the size of group
(defun collect_base (group base); this is a list has big and small blind (2 1)
	(cond ((null group) NIL)
				((null base) group)
				((< (fourth (car group)) (car base)) NIL)
				(T (collect_base (append (cdr group)
																 (list (set_player (car group) (car base) nil nil 
																									 (- (fourth (car group)) (car base)) nil) 
																			 )
																 )
												 (cdr base)
												 )
					 )
				)
	)
				
(defun nd_helper (remain num len)
	(cond ((= num 0) NIL)
				(T (let* ((ran (random len))
									(pick (nth ran remain))
									(others (append (butlast remain (- len ran)) (last remain (- len (+ ran 1)))))
									)
						 (cons pick (nd_helper others (- num 1) (- len 1)))
						 )
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
	(let* ((pack (ceiling (/ size PACK_SIZE)))
				 (csize (* pack PACK_SIZE))
				 (cards (nd_helper_create_card csize))
				 )
		(nd_helper cards size (length cards))
		)
	)

(defun get_shifted_group (group snum)
	(cond ((< snum 1) group)
				(T (get_shifted_group (append (cdr group) (list (car group))) (- snum 1)))
				)
	)
	
(defun sum_all (lst)
	(cond ((null lst) 0)
				(T (+ (car lst) (sum_all (cdr lst))))
				)
	)
;	info(game #, phase #, stake, deck, minimum, maximum, opened card, dealer)
;	group(chips on the board, cards on the hand, status on the game, total, player number)
;	const(base, player #, card #, tax)
(defun start_game (board)
	(let* ((info (first board))
				 (dealer (if (null (eighth info)) (fifth (car (second board))) (+ (mod (eighth info) (length group)) 1)))
				 (group (set_player_order (second board) dealer))
				 (const (third board))
				 (base_chip (first const));(2 1)
				 (card_num (third const))
				 (collected_group (collect_base (append (cdr group) (list (car group))) base_chip))
				 (base_ready (if (= (length collected_group) (length group)) T NIL))
				 )
		(cond ((null base_ready) NIL)
					(T (set_board board (set_info 
													info (+ (first info) 1) 0 ;game # phase #
													(sum_all base_chip) ;stake
													(new_deck card_num nil) 0 nil nil
													dealer)
												collected_group const))
												;(get_shifted_group collected_group (length base_chip)) const))
					)
		)
	)

(defun get_player_data (group)
	(cond ((null group) NIL)
				(T (cons 
						 (list (third (car group)) (first (car group)) (fourth (car group)) (fifth (car group))) 
						 (get_player_data (cdr group)))
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
				 (player_info (get_player_data (cdr group)))
				 (cmin (fifth info))
				 (cmax (sixth info))
				 )
		(list my_card opened_card stake player_info cmin cmax)
		)
	)
	
; 		actions: await(0), die(1), check(2), raise(3), call(4)
(defun player_action (board pid action bet)
	(let* ((info (first board))
				 (group (second board))
				 (player (car group))
				 (cob (first player));chips on the board
				 (cop (fourth player));chips on the pocket
				 (minimum (fifth info))
				 (stake (third info))
				 (mchip (- minimum cob))
				 )
		(cond ((not (= pid (fifth player))) NIL)
					((= action 1) (set_board board nil (list (cdr group) (set_player nil nil 1 nil nil) nil)))
					((and (= action 2) (= mchip 0)) (set_board board nil (get_shifted_group group 1) nil))
					((and (= action 3) (not (null bet)) (> bet mchip) (>= bet cop))
					 (let ((new_cop (- cop bet))
								 (new_min (+ minimum bet))
								 (new_cob (+ bet cob))
								 (new_stake (+ stake bet))
								 )
						 (set_board board (set_info nil nil new_stake nil new_min nil nil nil)
												(append (cdr group) (list (set_player new_cob nil 3 new_cop nil))) nil)
						 )
					 )
					((and (= action 4) (>= mchip cop))
					 (let ((new_cop (- cop mchip))
								 (new_cob (+ cob mchip))
								 (new_stake (+ stake mchip))
								 )
						 (set_board board (set_info nil nil new_stake nil nil nil nil nil)
												(append (cdr group) (list (set_player new_cob nil 4 new_cop nil))) nil)
						 )
					 )
					(T NIL)
					)
		)
	)

; offset is used for the first phase of the game (which next to the big blind start first)
; for the most case, offset will be the # of blinds 
(defun is_dealer_turn (board offset)
	(let ((dealer (if (null offset) 
									(eighth (first board)) 
									(+ (mod (+ offset (- (eighth (first board)) 1)) (length (second board))) 1))
								)
				(pid (fifth (car (second board))))
				)
		(= dealer pid)
		)
	)

(defun get_next_player (board)
	(let ((status (car (third (car (second board)))))
				)
		(cond ((= status 1) (get_next_player (set_board board nil (get_shifted_group (second board) 1) nil)))
					(T board)
					)
		)
	)

(defun ep_help (group minimum)
	(cond ((null group) T)
				((= (third (car group)) 1) (ep_help (cdr group) minimum))
				((= minimum (first (car group))) (ep_help (cdr group) minimum))
				(T NIL)
				)
	)

(defun is_end_phase (board)
	(let ((minimum (fifth (first board)))
				)
		(cond ((= minimum 0) T)
					((ep_help (second board) minimum) T)
					(T NIL)
					)
		)
	)

(defun spread_cards (deck group)
	(cond ((null group) NIL)
				(T (let* ((player (car group))
									(c1 (car deck))
									(c2 (cadr deck))
									(remain (last deck (- (length deck) 2)))
									)
						 (cons (set_player player nil (cons c1 (cons c2 (second player))) 0 nil nil) 
									 (spread_cards remain (cdr group))
									 )
						 )
					 )
				)
	)

; do spread_card first, and then remove first 8 cards from the deck
; 
(defun phase_one (board)
	(let ((

;	info(game #, phase #, stake, deck, minimum, maximum, opened card, dealer)
;	group(chips on the board, cards on the hand, status on the game, total, player number)
