
(setq maxNum 13)
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
	(let ((m (mod card maxNum))
				)
		(cond ((and (> card 0) (= m 0)) 13)
					((> card 0) m)
					(T 0)
					)
		)
	)

;return the suite of card
(defun suite (card)
	(ceiling (/ card maxNum))
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

;helper function for score_one_pair
(defun score_pairs_rec (pair_set)
	(cond ((null pair_set) 0)
				((listp pair_set)
				 (max
					 (cond ((same_num (car pair_set)) (num (car (car pair_set))))
								 (T 0)
								 )
					 (score_pairs_rec (cdr pair_set))
					 )
				 )
				(T 0)
				)
	)

; this returns max score of one pair from the given cards
(defun score_pairs (card)
	(let ((pair_set (all_hands card 2)))
		(score_pairs_rec pair_set)
		)
	)

(defun check_pairs (pair_set)
	(cond ((null pair_set) 0)
				((and (listp pair_set) (same_num (car pair_set))) (+ 1 (check_pairs (cdr pair_set))))
				((listp pair_set) (check_pairs (cdr pair_set)))
				(T 0)
				)
	)

(defun check_straight (card)
	

																													 
