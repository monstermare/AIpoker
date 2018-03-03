
(defun reload () (load "rules.lsp"))

(setq MAXNUM 13)
;(suite)*13 + (number) = cardnum
;notate: suite = {S:1,C:2,H:3,D:4}, number = {A:13,2:1,3:2,4:3,5:4,6:5,7:6,8:7,9:8,T:9,J:10,Q:11,K:12}
;high-pair-2pair-triple-straight-flush-f.house-four-s.flush-royal s.flush
;


(defun remove_nth (lst ind)
	(let ((adj (- (length lst) ind)))
		(cond ((null lst) nil)
					((< adj 0) nil)
					(T (append (butlast lst (+ adj 1)) (last lst adj)))
					)
		)
	)

(defun all_hands_list (c n l); c: nth element of list from 1 to n is counted, n: max size, l: least size of the list
	; if l.len > n, it means there is a pigeonholl where at least two pigeons are placed
	(cond
		((null l) nil)
		((> (length l) n)
		 (cond
			 ((> (length l) c) 
				(append
					(all_hands_list c n (remove_nth l c))
					(all_hands_list (+ c 1) n l)
					)
				)
			 ((= (length l) c) (all_hands_list c n (remove_nth l c)))
			 (T NIL)
			 )
		 )
		((= (length l) n) (list l))
		(T nil)
		)
	)
				 ;remove one more element
				 ;(all_hands_list 0 n (last l (- (length l) 1)))


;this returns all non-duplicated and possible combinations of given number n out of len.c
(defun all_hands (c n);c=set of cards, n=number of cards, l=list
	(cond ((< (length c) n) nil)
				(T (all_hands_list 1 n c))
				)
	)

;return the number of card
(defun num (card)
	(let ((m (mod card MAXNUM))
				)
		(cond ((and (> card 0) (= m 0)) 13)
					((> card 0) m)
					(T 0)
					)
		)
	)

;return the suite of card
(defun suite (card)
	(ceiling (/ card MAXNUM))
	)

;check if all cards have same number
(defun same_num (card)
	(cond ((null card) T)
				((listp card)
				 (cond ((null (cdr card)) T)
							 (T (and (= (num (car card)) (num (cadr card))) (same_num (cdr card))))
							 )
				 )
				(T T)
				)
	)

;convert given raw card to number
(defun card_to_num (card)
	(cond ((null card) NIL)
				((listp card)
				 (cond ((listp (car card)) (cons (card_to_num (car card)) (card_to_num (cdr card))))
							 (T (cons (num (car card)) (card_to_num (cdr card))))
							 )
				 )
				(T (num card))
				)
	)

;convert given raw card to suite
(defun card_to_suite (card)
	(cond ((null card) NIL)
				((listp card)
				 (cond ((listp (car card)) (cons (card_to_suite (car card)) (card_to_suite (cdr card))))
							 (T (cons (suite (car card)) (card_to_suite (cdr card))))
							 )
				 )
				(T (suite card))
				)
	)

(defun score_single_pair_helper (pair_set);for 'of a kind'
	(cond ((null pair_set) 0)
				((and (listp pair_set) (same_num (car pair_set))) (caar pair_set))
				((listp pair_set) (sop_helper (cdr pair_set)))
				(T 0)
				)
	)

(defun score_double_pair_helper (pair_set fpair); for t-pair and f. house
	(cond ((null pair_set) 0)
				((and (listp pair_set) (same_num (car pair_set)))
				 (cond ((null fpair) (sop_helper (cdr pair_set) (caar pair_set)))
							 (T (* fpair (caar pair_set)))
							 )
				 )
				(T (sop_helper (cdr pair_set) fpair))
				)
	)

(defun score_pair (card hands);
	(let ((pair_set (all_hands card 2)))
		(cond ((or (= hands 1) (= hands 3) (= hands 7)) (score_single_pair_helper pair_set))
					(T (score_double_pair_helper pair_set NIL))
					)
		)
	)

(defun cp_helper (pair_set)
	(cond ((null pair_set) 0)
				((and (listp pair_set) (same_num (car pair_set))) (+ 1 (cp_helper (cdr pair_set))))
				((listp pair_set) (cp_helper (cdr pair_set)))
				(T 0)
				)
	)

(defun check_pair (card)
	(let* ((pair_set (all_hands card 2))
				(score (cp_helper pair_set)))
		(cond ((= score 4) 6)
					((= score 6) 7)
					(T score)
					)
		)
	)


(defun cs_helper (card)
	(cond ((null card) NIL)
				((null (cadr card)) T)
				((= (+ (num (car card)) 1) (num (cadr card))) (cs_helper (cdr card)))
				((and (= (num (cadr card)) 13) (= (num (car card)) 4)) T)
				(T NIL)
				)
	)

(defun check_straight (card)
	(cs_helper (sort card #'<))
	)

(defun score_straight (card)
	(let ((scard (sort card #'>)))
		(cond ((and (= (car scard) 13) (< (cadr scard) 11)) 1)
					(T (car scard))
					)
		)
	)

(defun check_flush (card)
	(cond ((null card) NIL)
				((not (listp card)) T)
				((null (cadr card)) T)
				((= (suite (car card)) (suite (cadr card))) (check_flush (cdr card)))
				(T NIL)
				)
	)

(defun score_flush (card)
	(num (car (sort card #'>)))
	)


(defun highest_hand (card)
	(let ((straight (check_straight card))
				(flush (check_flush card))
				(pairs (check_pair card)))
		(cond ((and straight flush) 8)
					((> pairs 5) pairs)
					(flush 5)
					(straight 4)
					((> pairs 0) pairs)
					(T 0)
					)
		)
	)

(defun highest_card (card)
	(card_to_num (sort card #'>))
	)

(defun compare_high (c1 c2)
	(let ((n1 (num (car c1)))
				(n2 (num (car c2)))
				)
		(cond ((or (null c1) (null c2)) 0)
					((> n1 n2) 1)
					((< n1 n2) 2)
					(T (compare_high (cdr c1) (cdr c2)))
					)
		)
	)

(defun set_winner (cand hand)
	(list (car cand) hand (list (cadr cand)))
	)

(defun gw_helper (cand win); for cwin, first=card,second=hand,last=player(s)
	(cond ((null cand) win)
				(T (let* ((wcard (first win))
									(whand (second win))
									(ccard (caar cand))
									(chand (highest_hand ccard))
									(rcand (cdr cand))
									)
						 (cond ((> whand chand) (gw_helper rcand win))
									 ((< whand chand) (gw_helper rcand (set_winner (car cand) chand)))
									 (T
										 (let* ((whigh (highest_card wcard))
														(chigh (highest_card ccard))
														(result (compare_high whigh chigh))
														)
											 (cond ((> result 1) (gw_helper rcand (set_winner (car cand) chand)))
														 ((> result 0) (gw_helper rcand win))
														 (T (gw_helper rcand (list wcard whand (cons (cadar cand) (third win))))); tie
														 )
											 )
										 )
									 )
						 )
					 )
				)
	)

(defun game_winner (cards)
	(gw_helper (cdr cards) (set_winner (car cards) (highest_hand (caar cards))))
	)
