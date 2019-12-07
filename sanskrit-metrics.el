(provide 'sanskrit-metrics)

(defvar sktm_l-beg '("^[ \t]*\n[ \t]*" "\\\\begin{[^}\n]+}[ \t\n]*" "<lg[^>]*>[ \t\n]*")
  "A list used in sanskrit-metrics for context matching and string modifications before analysis, can be appended in init-file. It matches beginnung of verse: as a default empty line, random LaTeX environment, or TEI linegroup element")

(defvar sktm_l-end '("\n[ \t]*\n[ \t]*" "[ \t\n]+\\\\end{[^}\n]+}" "[^ \t\n]*</lg>")
  "A list used in sanskrit-metrics for context matching and string modifications before analysis, can be appended in init-file. It matches end of verse: as a default empty line, random LaTeX environment, or TEI linegroup element.")

(defvar sktm_l-plain-elim '("\\w+_[0-9,;\.]+" "[\.|0-9,;&]" " /+")
  "A list used in sanskrit-metrics for context matching and string modifications before analysis, can be appended in init-file. It deletes in plain text: as a default markers in the style ID_NUMBER, punctuation, and numbers.")

(defvar sktm_l-latex-elim '("%.+" "\\\\footnote[A-Z]?{[^{}]+}" "\\\\cite{[^{}]+}" "\\\\\\\\")
  "A list used in sanskrit-metrics for context matching and string modifications before analysis, can be appended in init-file. It deletes LaTeX-commands: as a default comments, plain footnotes (to make this delete footnotes with nesting macros, all macros used in a footnote must be appended either to this list or the latex-keep list), cite and line break")

(defvar sktm_l-tei-elim '("<!--.+-->" "<note[^>]*>[^<]+</note>")
  "A list used in sanskrit-metrics for context matching and string modifications before analysis, can be appended in init-file. It deletes TEI-xml elements: as a default comments and plain note elements (to delete notes with nesting elements, all elements used in a note must be appended either to this list or the tei-keep list)")

(defvar sktm_l-latex-keep '("\\\\emph{\\([^{}]+\\)}" "\\\\textbf{\\([^{}]+\\)}" "\\\\textit{\\([^{}]+\\)}" "\\\\textsc{\\([^{}]+\\)}")
  "A list used in sanskrit-metrics for context matching and string modifications before analysis, can be appended in init-file. It deletes LaTeX-commands, but keeps their content: as a default \emph, \textbf, \textit, and \textsc.")

(defvar sktm_l-tei-keep '("<l[^>]*>\\([^<]+\\)</l>" "<seg[^>]*>\\([^<]+\\)</seg>" "<hi[^>]*>\\([^<]+\\)</hi>")
  "A list used in sanskrit-metrics for context matching and string modifications before analysis, can be appended in init-file. It deletes TEI-xml elements, but keeps their content: as a default <lg>, <seg>, <hi>.")

(defun sktmetrics-revise ()
  (goto-char (point-min))
  (while (re-search-forward "\n[ \t]*% [◡—|][ ◡—|]+.+" nil t)
    (replace-match "" t nil)
    )
  (goto-char (point-max))
  )

(defun sktmetrics-narrow ()
  (if mark-active
      (narrow-to-region (region-beginning) (region-end))
    (narrow-to-region (and (re-search-backward (mapconcat 'identity sktm_l-beg "\\|") nil t)
			 (re-search-forward (mapconcat 'identity sktm_l-beg "\\|") nil t))
		      (and (re-search-forward (mapconcat 'identity sktm_l-end "\\|") nil t)
			 (re-search-backward (mapconcat 'identity sktm_l-end "\\|") nil t))))
  (delete-trailing-whitespace)
  (sktmetrics-revise)
  (insert-buffer (buffer-name))
  (when (not (equal (line-beginning-position) (point)))
    (insert "\n"))
  (narrow-to-region (point) (point-max))
  )

(defun sktmetrics-skim ()
  (while (re-search-forward (concat (mapconcat 'identity sktm_l-plain-elim "\\|") "\\|"
				    (mapconcat 'identity sktm_l-latex-elim "\\|") "\\|"
				    (mapconcat 'identity sktm_l-tei-elim "\\|") "\\|"
				    (mapconcat 'identity sktm_l-latex-keep "\\|") "\\|"
				    (mapconcat 'identity sktm_l-tei-keep "\\|")) nil t)
    ;; deletions
    (goto-char (point-min))
    (while (re-search-forward (concat (mapconcat 'identity sktm_l-plain-elim "\\|") "\\|"
				      (mapconcat 'identity sktm_l-latex-elim "\\|") "\\|"
				      (mapconcat 'identity sktm_l-tei-elim "\\|")) nil t)
      (replace-match "" t nil))
    ;; keepers
    (mapc (lambda (list)
	    (goto-char (point-min))
	    (while (re-search-forward (eval list) nil t)
	      (replace-match "\\1" t nil)))
	  (append sktm_l-latex-keep sktm_l-tei-keep)
	  )
    (goto-char (point-min))
    ))

(defun sktmetrics-quantify ()
  (mapc (lambda (list)
	  (goto-char (point-min))
	  (while (re-search-forward (eval (car list)) nil t)
	    (replace-match (cdr list) t nil))
	  )
	'(((concat "[ \t]+$\\|^[ \t]+\\|^[ \t]*\n") . "")
	  ((concat " +") . "|")
	  ((concat "^\\(\\w\\)") . "% \\1")
	  ;; last syllable of b and d is always counted long
	  ((concat "\\(^%.+\\)\\(\n%.+\\)\n\\(%.+\\)\\(\n%.+\\)") . "\\1ff\\2zz\n\\3ff\\4zz")
	  ((concat "\\(%.+[^fz\n]\\)$") . "\\1kk") 
	  ((concat "ff") . "") 
	  ((concat "zz") . "kk") 
	  ;; straddle initial consonants from b and d, append to end of a and c
	  ((concat "\\(^%.+\\)\\(\n% \\([^aāiīuūṛṝḷḹeo ]*\\).+\\)\\(\n%.+\\)\\(\n% \\([^aāiīuūṛṝḷḹeo ]*\\).+\\)") . "\\1\\3\\2\\4\\6\\5")
	  ((concat "\\([^ \n]\\)$") . "\\1 ")
	  ((concat "\\(?:[āīūṝḹeo]\\|au\\|ai\\|a?[aiuṛḷ][ḥẖḫṃ]\\)\\(|*\\)[^aāiīuūṛṝḷḹeo|\n]*\\(|*\\)[^aāiīuūṛṝḷḹeo|\n]*") . " —\\1\\2 ")
	  ((concat "\\([^ %]\\)\\([aiuṛḷ]\\)") . "\\1  \\2")
	  ((concat " [aiuṛḷ]\\(|?\\)\\(?:[kgcjṭḍtdpb]h?\\|[ṅñṇnmyrlvśṣsh]\\)?\\(|?\\) ") . " ◡\\1\\2 ")
	  ((concat " [aiuṛḷ][^ |\n]*\\(|?\\)[^ |\n]*\\(|?\\)[^ |\n]* ") . "  —\\1\\2 ")
	  ((concat "[^ \n◡—|%]") . "")
	  ((concat " +|") . "|")
	  ((concat "  +") . " ")))
  )

(defun sktmetrics-tidyup ()
  (mapc (lambda (list)
	  (goto-char (point-min))
	  (while (re-search-forward (eval (car list)) nil t)
	    (replace-match (cdr list) t nil))
	  )
	'(("◡ " . "◡  ")
	  ("— " . "—  ")
	  ("|| " . "||"))
	)
  (goto-char (point-max))
  (widen)
  (when (and (not (equal (point) (line-end-position))) (not (equal (point) (line-beginning-position))))
    (insert "\n"))
  (indent-region (re-search-backward (mapconcat 'identity sktm_l-beg "\\|") nil t) (re-search-forward (mapconcat 'identity sktm_l-end "\\|") nil t))
  )

(defun sktmetrics-match-metres ()
  (mapc (lambda (list)
	  (goto-char (point-min))
	  (while (re-search-forward (eval (car list)) nil t)
	    (replace-match (concat "\\1%" (cdr list)) t nil))
	  )
	metrlist
	)
  )

(defun sktmetrics-match-anustubhs ()
  (goto-char (point-min))
  (if
      (re-search-forward "\\(^% |* ?\\([◡—]|* \\)\\{8\\}\\)\\(\\([◡—]|* \\)\\{8\\}\\)$" nil t)
      (replace-match "\\1%A\n% \\3%B" t nil))
  (if
      (re-search-forward "\\(^% |* ?\\([◡—]|* \\)\\{8\\}\\)\\(\\([◡—]|* \\)\\{8\\}\\)$" nil t)
      (replace-match "\\1%C\n% \\3%D" t nil))
  (if
      (re-search-forward "\\(^% |* ?\\([◡—]|* \\)\\{8\\}\\)\\(\\([◡—]|* \\)\\{8\\}\\)$" nil t)
      (replace-match "\\1%E\n% \\3%F" t nil))
  (mapc (lambda (list)
	  (goto-char (point-min))
	  (while (re-search-forward (eval (car list)) nil t)
	    (replace-match (cdr list) t nil))
	  )
	anustubhlist
	)
  )

(defun sktmetrics-context ()
  (interactive)
  (sktmetrics-narrow)
  (sktmetrics-skim)
  (sktmetrics-quantify)
  (let (;;; primitives
	(ma "\\(?:—|* \\)\\{3\\}")
	(na "\\(?:◡|* \\)\\{3\\}")
	(bha "—|* \\(?:◡|* \\)\\{2\\}")
	(ja "◡|* —|* ◡|* ")
	(sa "\\(?:◡|* \\)\\{2\\}—|* ")
	(ya "◡|* \\(?:—|* \\)\\{2\\}")
	(ra "—|* ◡|* —|* ")
	(ta "\\(?:—|* \\)\\{2\\}◡|* ")
	(ga "—|* ")
	(gaz "—|+ ")
	(la "◡|* ")
	(laz "◡|+ ")
	(anc "[◡—]|* ")
	(ancz "[◡—]|+ ")
	(kha "\\(?:◡|* \\)\\{4\\}")
	(khatwoz "◡|+ \\(?:◡|* \\)\\{3\\}")
	(fourmora "\\(?:◡|* ◡|* \\|—|* \\)\\(?:◡|* ◡|* \\|—|* \\)")
	(fourmoraz "\\(?:◡|* ◡|* \\|—|* \\)\\(?:◡|* ◡|+ \\|—|+ \\)")
	(twomora "\\(?:◡|* ◡|* \\|—|* \\)")
	(twomoranoz "\\(?:◡|* ◡|*\\|—|*\\)")
	;;metre lists
	(metrlist '(((concat "\\(% |* ?" twomora ga ja ga ga ja ga twomora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" twomora ga ja ga ga ja ga twomora "◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Sarvataścapalā (30+27 morae)")

		    ((concat "\\(% |* ?" twomora ga ja ga ga ja ga twomora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Mukhacapalā (30+27 morae)")

		    ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" twomora ga ja ga ga ja ga twomora "◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Jaghanacapalā (30+27 morae)")

		    ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Āryā (30+27 morae): pathyā")

		    ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Āryā (30+27 morae): vipulā")

		    ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Udgīti (27+30 morae): pathyā")

		    ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)"). " Udgīti (27+30 morae): vipulā")

		    ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" la la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)\\(?:" la ga "◡|*\\|" la la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Gīti (30+30 morae)")

		    ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Upagīti (27+27 morae)")

		    ((concat "\\(% |* ?" fourmora fourmora fourmora fourmora fourmora "\\(?:" ja "\\|" kha "\\)" fourmora twomora ga " *\n% |* ?" fourmora fourmora fourmora fourmora fourmora "\\(?:" ja "\\|" kha "\\)" fourmora twomora ga " *$\\)") . " Āryāgīti (32+32 morae)")

		    ((concat "\\(% |* ?" sa ja sa la " *\n% |* ?" na sa ja ga " *\n% |* ?" bha na ja la ga " *\n% |* ?" sa ja sa ja ga " *$\\)") . " viṣama: Udgatā (10, 10, 11, 13)")

		    ((concat "\\(% |* ?" sa ja sa la " *\\)\n\\(% |* ?" na sa ja ga " *\\)\n\\(% |* ?" bha na ja la la " *\\)\n\\(% |* ?" sa ja sa ja ga " *$\\)") . "\n\\2%\n\\3% incorrect: last syllable short\n\\4% viṣama: Udgatā (10, 10, 11, 13)")

		    ((concat "\\(% |* ?" sa ja sa anc " *\\)\n\\(% |* ?" na sa ja ga " *\\)\n\\(% |* ?" bha na ja la ga " *\\)\n\\(% |* ?" sa ja sa ja ga " *$\\)") . " incorrect: last syllable long\n\\2%\n\\3%\n\\4% viṣama: Udgatā (10, 10, 11, 13)")

		    ((concat "\\(% |* ?" sa ja sa anc " *\\)\n\\(% |* ?" na sa ja ga " *\\)\n\\(% |* ?" bha na ja la la " *\\)\n\\(% |* ?" sa ja sa ja ga " *$\\)") . " incorrect: last syllable long\n\\2%\n\\3% incorrect: last syllable short\n\\4% viṣama: Udgatā (10, 10, 11, 13)")

		    ((concat "\\(% |* ?" sa sa ja ga " *\n% |* ?" sa bha ra la ga " *$\\)") . " ardhasama: Viyoginī (10, 11)")

		    ((concat "\\(% |* ?" sa sa ja la " *\\)\n\\(% |* ?" sa bha ra la ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Viyoginī (10, 11)")

		    ((concat "\\(% |* ?" na na ra la ga " *\n% |* ?" na ja ja ga la ga " *$\\)") . " ardhasama: Aparavaktra (11, 12)")

		    ((concat "\\(% |* ?" na na ra la la " *\\)\n\\(% |* ?" na ja ja ga la ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Aparavaktra (11, 12)")

		    ((concat "\\(% |* ?" twomora twomora twomora ra la ga " *\n% |* ?" twomora twomora twomora twomora ra la ga " *$\\)") . " Vaitālīya (14+16 morae)")

		    ((concat "\\(% |* ?" sa sa ja ga ga " *\n% |* ?" sa bha ra la ga ga " *$\\)") . " ardhasama: Mālabhāriṇī (11, 12)")

		    ((concat "\\(% |* ?" sa sa ja ga la " *\n% |* ?" sa bha ra la ga ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Mālabhāriṇī (11, 12)")

		    ((concat "\\(% |* ?" na na ra la ga ga " *\n% |* ?" na ja ja ra ga " *$\\)") . " ardhasama: Puṣpitāgrā (12, 13)")

		    ((concat "\\(% |* ?" na na ra la ga la " *\n% |* ?" na ja ja ra ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Puṣpitāgrā (12, 13)")

		    ((concat "\\(% |* ?" ta ja ra ga " *\n% |* ?" ma sa ja ga ga " *$\\)") . " ardhasama: Bhadravirāṭ")

		    ((concat "\\(% |* ?" ta ja ra la " *\n% |* ?" ma sa ja ga ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Bhadravirāṭ")

		    ((concat "\\(% |* ?" twomora twomora twomora ga ja ga ga " *\n% |* ?" twomora twomora twomora twomora ga ja ga ga " *$\\)") . " Aupacchandasaka (16+18 morae)")

		    ((concat "\\(% |* ?" ja ra la anc " *$\\)") . " Pramāṇikā (8)")

		    ((concat "\\(% |* ?" ta ta ja ga anc " *$\\)") . " Indravajrā (11)")

		    ((concat "\\(% |* ?" ja ta ja ga anc " *$\\)") . " Upendravajrā (11)")

		    ((concat "\\(% |* ?" ra na ra la ga " *$\\)") . " Rathoddhatā (11)")
		    ((concat "\\(% |* ?" ra na ra la la " *$\\)") . " Rathoddhatā (11), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ma gaz ga la ta ga ga " *$\\)") . " Śālinī (4+7)")
		    ((concat "\\(% |* ?" ma ta ta ga ga " *$\\)") . " Śālinī (4+7): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" ma ta ta ga la " *$\\)") . " Śālinī (4+7), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ra na bha ga ga " *$\\)") . " Svāgatā (11)")
		    ((concat "\\(% |* ?" ra na bha ga la " *$\\)") . " Svāgatā (11), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ta ta ja ga la ga " *$\\)") . " Indravaṃśā (12)")
		    ((concat "\\(% |* ?" ta ta ja ga la la " *$\\)") . " Indravaṃśā (12), incorrect: last syllable short")

		    ((concat "\\(% |* ?" sa sa sa la la ga " *$\\)") . " Toṭaka (12)")
		    ((concat "\\(% |* ?" sa sa sa la la la " *$\\)") . " Toṭaka (12), incorrect: last syllable short")

		    ((concat "\\(% |* ?" na bha bha ga la ga " *$\\)") . " Drutavilambita (12)")
		    ((concat "\\(% |* ?" na bha bha ga la la " *$\\)") . " Drutavilambita (12), incorrect: last syllable short")

		    ((concat "\\(% |* ?" sa ja sa la la ga " *$\\)") . " Pramitākṣarā (12)")

		    ((concat "\\(% |* ?" ja ta ja ga la ga " *$\\)") . " Vaṃśastha (12)")
		    ((concat "\\(% |* ?" ja ta ja ga la la " *$\\)") . " Vaṃśastha (12), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ga ga gaz na ja ra ga " *$\\)") . " Praharṣiṇī (3+10)")
		    ((concat "\\(% |* ?" ma na ja ra ga " *$\\)") . " Praharṣiṇī (3+10): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" ma na ja ra la " *$\\)") . " Praharṣiṇī (3+10), incorrect: last syllable short")

		    ((concat "\\(% |* ?" sa ja sa ja ga " *$\\)") . " Mañjubhāṣiṇī (13)")
		    ((concat "\\(% |* ?" sa ja sa ja la " *$\\)") . " Mañjubhāṣiṇī (13), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ja gaz la la sa ja ga " *$\\)") . " Rucirā (4+9)")
		    ((concat "\\(% |* ?" ja bha sa ja ga " *$\\)") . " Rucirā (4+9): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" ja bha sa ja la " *$\\)") . " Rucirā (4+9), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ta bha ja ja ga anc " *$\\)") . " Vasantatilaka (14)")

		    ((concat "\\(% |* ?" na na ga gaz ga ya la ga ga " *$\\)") . " Mālinī (8+7)")
		    ((concat "\\(% |* ?" na na ga ga ga ya la ga ga " *$\\)") . " Mālinī (8+7): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" na na ga ga ga ya la ga la " *$\\)") . " Mālinī (8+7), incorrect: last syllable short")

		    ((concat "\\(% |* ?" na ja bha ja ja la ga " *$\\)") . " Nardaṭaka (17)")
		    ((concat "\\(% |* ?" na ja bha ja ja la la " *$\\)") . " Nardaṭaka (17), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ja sa la gaz la sa ya la ga " *$\\)") . " Pṛthvī (8+9)")
		    ((concat "\\(% |* ?" ja sa ja sa ya la ga " *$\\)") . " Pṛthvī (8+9): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" ja sa ja sa ya la la " *$\\)") . " Pṛthvī (8+9), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ma gaz la la na gaz ga la ta ga ga " *$\\)") . " Mandākrāntā (4+6+7)")
		    ((concat "\\(% |* ?" ma bha na ta ta ga ga " *$\\)") . " Mandākrāntā (4+6+7): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" ma bha na ta ta ga la " *$\\)") . " Mandākrāntā (4+6+7), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ya ga ga gaz na sa bha la ga " *$\\)") . " Śikhariṇī (6+11)")
		    ((concat "\\(% |* ?" ya ma na sa bha la ga " *$\\)") . " Śikhariṇī (6+11): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" ya ma na sa bha la la " *$\\)") . " Śikhariṇī (6+11), incorrect: last syllable short")

		    ((concat "\\(% |* ?" na la la gaz ma gaz la ga sa la ga " *$\\)") . " Hariṇī (6+4+7)")
		    ((concat "\\(% |* ?" na sa ma ra sa la ga " *$\\)") . " Hariṇī (6+4+7): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" na sa ma ra sa la la " *$\\)") . " Hariṇī (6+4+7), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ma sa ja la la gaz ta ta ga " *$\\)") . " Śārdūlavikrīḍita (12+7)")
		    ((concat "\\(% |* ?" ma sa ja sa ta ta ga " *$\\)") . " Śārdūlavikrīḍita (12+7): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" ma sa ja sa ta ta la " *$\\)") . " Śārdūlavikrīḍita (12+7), incorrect: last syllable short")

		    ((concat "\\(% |* ?" ma ra gaz la la na la gaz ga ya la ga ga " *$\\)") . " Sragdharā (7+7+7)")
		    ((concat "\\(% |* ?" ma ra bha na ya ya la ga ga " *$\\)") . " Sragdharā (7+7+7): caesura in compound or incorrect?")
		    ((concat "\\(% |* ?" ma ra bha na ya ya la ga la " *$\\)") . " Sragdharā (7+7+7), incorrect: last syllable short")

		    ))
	(anustubhlist '(((concat "\\(% |* ?" anc la la "\\(?:" anc "\\)\\{5\\}\\)%\\([ABCDEF]\\)") . "\\1% \\2 incorrect: syllables 2 and 3 short")

		       ((concat "\\(% |* ?" anc ra "\\(?:" anc "\\)\\{4\\}\\)%\\([BDF]\\)") . "\\1% \\2 incorrect: syllables 2-4 are ra (— ◡ —)")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{4\\}" ja anc "\\)%\\([BDF]\\)") . "\\1% \\2 correct")

		       ((concat " %\\([BDF]\\)") . " % \\1 incorrect: syllables 5-7 aren't ja (◡ — ◡)")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{4\\}" ya anc "\\)%\\([ACE]\\)") . "\\1% \\2 pathyā")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" ga na  anc "\\)%\\([ACE]\\)") . "\\1% \\2 na-vipulā")

		       ((concat "\\(% |* ?" anc ra bha anc "\\)%\\([ACE]\\)") . "\\1% \\2 bha-vipulā")

		       ((concat "\\(% |* ?" anc ga ga gaz bha anc "\\)%\\([ACE]\\)") . "\\1% \\2 ma-bha-vipulā (rare)")

		       ((concat "\\(% |* ?" anc ga ga ga  bha anc "\\)%\\([ACE]\\)") . "\\1% \\2 ma-bha-vipulā (rare) incorrect: no caesura after 4th syllable")

		       ((concat "\\(% |* ?" anc ra gaz ga ga anc "\\)%\\([ACE]\\)") . "\\1% \\2 ma-vipulā")

		       ((concat "\\(% |* ?" anc ra ga  ga ga anc "\\)%\\([ACE]\\)") . "\\1% \\2 ma-vipulā incorrect: no caesura after 5th syllable")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" gaz ra anc "\\)%\\([ACE]\\)") . "\\1% \\2 ra-vipulā")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" ga  ra anc "\\)%\\([ACE]\\)") . "\\1% \\2 ra-vipulā incorrect: no caesura after 4th syllable")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" ancz sa anc "\\)%\\([ACE]\\)") . "\\1% \\2 sa-vipulā, incorrect?")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" anc sa anc "\\)%\\([ACE]\\)") . "\\1% \\2 sa-vipulā incorrect: no caesura after 4th syllable")

		       ((concat " %\\([ACE]\\)") . " % \\1 incorrect: neither pathyā nor vipulā")

		       ((concat "\\([^|] % [ACE] .*\\)") . "\\1, pādas compounded?")

		       )))
    ;;(sktmetrics-build-db)
    (sktmetrics-match-metres)
    (sktmetrics-match-anustubhs)
    (sktmetrics-tidyup)
    ))

(defun sktmetrics-build-db ()
  (setq ma "\\(?:—|* \\)\\{3\\}")
  (setq na "\\(?:◡|* \\)\\{3\\}")
  (setq bha "—|* \\(?:◡|* \\)\\{2\\}")
  (setq ja "◡|* —|* ◡|* ")
  (setq sa "\\(?:◡|* \\)\\{2\\}—|* ")
  (setq ya "◡|* \\(?:—|* \\)\\{2\\}")
  (setq ra "—|* ◡|* —|* ")
  (setq ta "\\(?:—|* \\)\\{2\\}◡|* ")
  (setq ga "—|* ")
  (setq gaz "—|+ ")
  (setq la "◡|* ")
  (setq laz "◡|+ ")
  (setq anc "[◡—]|* ")
  (setq ancz "[◡—]|+ ")
  (setq kha "\\(?:◡|* \\)\\{4\\}")
  (setq khatwoz "◡|+ \\(?:◡|* \\)\\{3\\}")
  (setq fourmora "\\(?:◡|* ◡|* \\|—|* \\)\\(?:◡|* ◡|* \\|—|* \\)")
  (setq fourmoraz "\\(?:◡|* ◡|* \\|—|* \\)\\(?:◡|* ◡|+ \\|—|+ \\)")
  (setq twomora "\\(?:◡|* ◡|* \\|—|* \\)")
  (setq twomoranoz "\\(?:◡|* ◡|*\\|—|*\\)")

  (setq metrlist '(
		   ((concat "\\(% |* ?" twomora ga ja ga ga ja ga twomora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" twomora ga ja ga ga ja ga twomora "◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Sarvataścapalā (30+27 morae)")

		   ((concat "\\(% |* ?" twomora ga ja ga ga ja ga twomora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Mukhacapalā (30+27 morae)")

		   ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" twomora ga ja ga ga ja ga twomora "◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Jaghanacapalā (30+27 morae)")

		   ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Āryā (30+27 morae): pathyā")

		   ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Āryā (30+27 morae): vipulā")

		   ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Udgīti (27+30 morae): pathyā")

		   ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmoraz "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" laz la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)"). " Udgīti (27+30 morae): vipulā")

		   ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" la la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)\\(?:" la ga "◡|*\\|" la la la "◡|*\\)\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Gīti (30+30 morae)")

		   ((concat "\\(% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *\n% |* ?" fourmora "\\(?:" ja "\\|" fourmora "\\)" fourmora "\\(?:" la ga "◡|*\\|" twomora twomoranoz "\\)\\(?:| " kha "\\| " fourmora "\\)◡|*\\(?:| " kha "\\| " fourmora "\\)" ga " *$\\)") . " Upagīti (27+27 morae)")

		   ((concat "\\(% |* ?" fourmora fourmora fourmora fourmora fourmora "\\(?:" ja "\\|" kha "\\)" fourmora twomora ga " *\n% |* ?" fourmora fourmora fourmora fourmora fourmora "\\(?:" ja "\\|" kha "\\)" fourmora twomora ga " *$\\)") . " Āryāgīti (32+32 morae)")

		   ((concat "\\(% |* ?" sa ja sa la " *\n% |* ?" na sa ja ga " *\n% |* ?" bha na ja la ga " *\n% |* ?" sa ja sa ja ga " *$\\)") . " viṣama: Udgatā (10, 10, 11, 13)")

		   ((concat "\\(% |* ?" sa ja sa la " *\\)\n\\(% |* ?" na sa ja ga " *\\)\n\\(% |* ?" bha na ja la la " *\\)\n\\(% |* ?" sa ja sa ja ga " *$\\)") . "\n\\2%\n\\3% incorrect: last syllable short\n\\4% viṣama: Udgatā (10, 10, 11, 13)")

		   ((concat "\\(% |* ?" sa ja sa anc " *\\)\n\\(% |* ?" na sa ja ga " *\\)\n\\(% |* ?" bha na ja la ga " *\\)\n\\(% |* ?" sa ja sa ja ga " *$\\)") . " incorrect: last syllable long\n\\2%\n\\3%\n\\4% viṣama: Udgatā (10, 10, 11, 13)")

		   ((concat "\\(% |* ?" sa ja sa anc " *\\)\n\\(% |* ?" na sa ja ga " *\\)\n\\(% |* ?" bha na ja la la " *\\)\n\\(% |* ?" sa ja sa ja ga " *$\\)") . " incorrect: last syllable long\n\\2%\n\\3% incorrect: last syllable short\n\\4% viṣama: Udgatā (10, 10, 11, 13)")

		   ((concat "\\(% |* ?" sa sa ja ga " *\n% |* ?" sa bha ra la ga " *$\\)") . " ardhasama: Viyoginī (10, 11)")

		   ((concat "\\(% |* ?" sa sa ja la " *\\)\n\\(% |* ?" sa bha ra la ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Viyoginī (10, 11)")

		   ((concat "\\(% |* ?" na na ra la ga " *\n% |* ?" na ja ja ga la ga " *$\\)") . " ardhasama: Aparavaktra (11, 12)")

		   ((concat "\\(% |* ?" na na ra la la " *\\)\n\\(% |* ?" na ja ja ga la ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Aparavaktra (11, 12)")

		   ((concat "\\(% |* ?" twomora twomora twomora ra la ga " *\n% |* ?" twomora twomora twomora twomora ra la ga " *$\\)") . " Vaitālīya (14+16 morae)")

		   ((concat "\\(% |* ?" sa sa ja ga ga " *\n% |* ?" sa bha ra la ga ga " *$\\)") . " ardhasama: Mālabhāriṇī (11, 12)")

		   ((concat "\\(% |* ?" sa sa ja ga la " *\n% |* ?" sa bha ra la ga ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Mālabhāriṇī (11, 12)")

		   ((concat "\\(% |* ?" na na ra la ga ga " *\n% |* ?" na ja ja ra ga " *$\\)") . " ardhasama: Puṣpitāgrā (12, 13)")

		   ((concat "\\(% |* ?" na na ra la ga la " *\n% |* ?" na ja ja ra ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Puṣpitāgrā (12, 13)")

		   ((concat "\\(% |* ?" ta ja ra ga " *\n% |* ?" ma sa ja ga ga " *$\\)") . " ardhasama: Bhadravirāṭ")

		   ((concat "\\(% |* ?" ta ja ra la " *\n% |* ?" ma sa ja ga ga " *$\\)") . " incorrect: last syllable short\n\\2% ardhasama: Bhadravirāṭ")

		   ((concat "\\(% |* ?" twomora twomora twomora ga ja ga ga " *\n% |* ?" twomora twomora twomora twomora ga ja ga ga " *$\\)") . " Aupacchandasaka (16+18 morae)")

		   ((concat "\\(% |* ?" ja ra la anc " *$\\)") . " Pramāṇikā (8)")

		   ((concat "\\(% |* ?" ta ta ja ga anc " *$\\)") . " Indravajrā (11)")

		   ((concat "\\(% |* ?" ja ta ja ga anc " *$\\)") . " Upendravajrā (11)")

		   ((concat "\\(% |* ?" ra na ra la ga " *$\\)") . " Rathoddhatā (11)")
		   ((concat "\\(% |* ?" ra na ra la la " *$\\)") . " Rathoddhatā (11), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ma gaz ga la ta ga ga " *$\\)") . " Śālinī (4+7)")
		   ((concat "\\(% |* ?" ma ta ta ga ga " *$\\)") . " Śālinī (4+7): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" ma ta ta ga la " *$\\)") . " Śālinī (4+7), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ra na bha ga ga " *$\\)") . " Svāgatā (11)")
		   ((concat "\\(% |* ?" ra na bha ga la " *$\\)") . " Svāgatā (11), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ta ta ja ga la ga " *$\\)") . " Indravaṃśā (12)")
		   ((concat "\\(% |* ?" ta ta ja ga la la " *$\\)") . " Indravaṃśā (12), incorrect: last syllable short")

		   ((concat "\\(% |* ?" sa sa sa la la ga " *$\\)") . " Toṭaka (12)")
		   ((concat "\\(% |* ?" sa sa sa la la la " *$\\)") . " Toṭaka (12), incorrect: last syllable short")

		   ((concat "\\(% |* ?" na bha bha ga la ga " *$\\)") . " Drutavilambita (12)")
		   ((concat "\\(% |* ?" na bha bha ga la la " *$\\)") . " Drutavilambita (12), incorrect: last syllable short")

		   ((concat "\\(% |* ?" sa ja sa la la ga " *$\\)") . " Pramitākṣarā (12)")

		   ((concat "\\(% |* ?" ja ta ja ga la ga " *$\\)") . " Vaṃśastha (12)")
		   ((concat "\\(% |* ?" ja ta ja ga la la " *$\\)") . " Vaṃśastha (12), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ga ga gaz na ja ra ga " *$\\)") . " Praharṣiṇī (3+10)")
		   ((concat "\\(% |* ?" ma na ja ra ga " *$\\)") . " Praharṣiṇī (3+10): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" ma na ja ra la " *$\\)") . " Praharṣiṇī (3+10), incorrect: last syllable short")

		   ((concat "\\(% |* ?" sa ja sa ja ga " *$\\)") . " Mañjubhāṣiṇī (13)")
		   ((concat "\\(% |* ?" sa ja sa ja la " *$\\)") . " Mañjubhāṣiṇī (13), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ja gaz la la sa ja ga " *$\\)") . " Rucirā (4+9)")
		   ((concat "\\(% |* ?" ja bha sa ja ga " *$\\)") . " Rucirā (4+9): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" ja bha sa ja la " *$\\)") . " Rucirā (4+9), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ta bha ja ja ga anc " *$\\)") . " Vasantatilaka (14)")

		   ((concat "\\(% |* ?" na na ga gaz ga ya la ga ga " *$\\)") . " Mālinī (8+7)")
		   ((concat "\\(% |* ?" na na ga ga ga ya la ga ga " *$\\)") . " Mālinī (8+7): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" na na ga ga ga ya la ga la " *$\\)") . " Mālinī (8+7), incorrect: last syllable short")

		   ((concat "\\(% |* ?" na ja bha ja ja la ga " *$\\)") . " Nardaṭaka (17)")
		   ((concat "\\(% |* ?" na ja bha ja ja la la " *$\\)") . " Nardaṭaka (17), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ja sa la gaz la sa ya la ga " *$\\)") . " Pṛthvī (8+9)")
		   ((concat "\\(% |* ?" ja sa ja sa ya la ga " *$\\)") . " Pṛthvī (8+9): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" ja sa ja sa ya la la " *$\\)") . " Pṛthvī (8+9), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ma gaz la la na gaz ga la ta ga ga " *$\\)") . " Mandākrāntā (4+6+7)")
		   ((concat "\\(% |* ?" ma bha na ta ta ga ga " *$\\)") . " Mandākrāntā (4+6+7): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" ma bha na ta ta ga la " *$\\)") . " Mandākrāntā (4+6+7), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ya ga ga gaz na sa bha la ga " *$\\)") . " Śikhariṇī (6+11)")
		   ((concat "\\(% |* ?" ya ma na sa bha la ga " *$\\)") . " Śikhariṇī (6+11): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" ya ma na sa bha la la " *$\\)") . " Śikhariṇī (6+11), incorrect: last syllable short")

		   ((concat "\\(% |* ?" na la la gaz ma gaz la ga sa la ga " *$\\)") . " Hariṇī (6+4+7)")
		   ((concat "\\(% |* ?" na sa ma ra sa la ga " *$\\)") . " Hariṇī (6+4+7): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" na sa ma ra sa la la " *$\\)") . " Hariṇī (6+4+7), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ma sa ja la la gaz ta ta ga " *$\\)") . " Śārdūlavikrīḍita (12+7)")
		   ((concat "\\(% |* ?" ma sa ja sa ta ta ga " *$\\)") . " Śārdūlavikrīḍita (12+7): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" ma sa ja sa ta ta la " *$\\)") . " Śārdūlavikrīḍita (12+7), incorrect: last syllable short")

		   ((concat "\\(% |* ?" ma ra gaz la la na la gaz ga ya la ga ga " *$\\)") . " Sragdharā (7+7+7)")
		   ((concat "\\(% |* ?" ma ra bha na ya ya la ga ga " *$\\)") . " Sragdharā (7+7+7): caesura in compound or incorrect?")
		   ((concat "\\(% |* ?" ma ra bha na ya ya la ga la " *$\\)") . " Sragdharā (7+7+7), incorrect: last syllable short")

		   ))
  
  (setq anustubhlist '(
		       ((concat "\\(% |* ?" anc la la "\\(?:" anc "\\)\\{5\\}\\)%\\([ABCDEF]\\)") . "\\1% \\2 incorrect: syllables 2 and 3 short")

		       ((concat "\\(% |* ?" anc ra "\\(?:" anc "\\)\\{4\\}\\)%\\([BDF]\\)") . "\\1% \\2 incorrect: syllables 2-4 are ra (— ◡ —)")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{4\\}" ja anc "\\)%\\([BDF]\\)") . "\\1% \\2 correct")

		       ((concat " %\\([BDF]\\)") . " % \\1 incorrect: syllables 5-7 aren't ja (◡ — ◡)")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{4\\}" ya anc "\\)%\\([ACE]\\)") . "\\1% \\2 pathyā")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" ga na  anc "\\)%\\([ACE]\\)") . "\\1% \\2 na-vipulā")

		       ((concat "\\(% |* ?" anc ra bha anc "\\)%\\([ACE]\\)") . "\\1% \\2 bha-vipulā")

		       ((concat "\\(% |* ?" anc ga ga gaz bha anc "\\)%\\([ACE]\\)") . "\\1% \\2 ma-bha-vipulā (rare)")

		       ((concat "\\(% |* ?" anc ga ga ga  bha anc "\\)%\\([ACE]\\)") . "\\1% \\2 ma-bha-vipulā (rare) incorrect: no caesura after 4th syllable")

		       ((concat "\\(% |* ?" anc ra gaz ga ga anc "\\)%\\([ACE]\\)") . "\\1% \\2 ma-vipulā")

		       ((concat "\\(% |* ?" anc ra ga  ga ga anc "\\)%\\([ACE]\\)") . "\\1% \\2 ma-vipulā incorrect: no caesura after 5th syllable")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" gaz ra anc "\\)%\\([ACE]\\)") . "\\1% \\2 ra-vipulā")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" ga  ra anc "\\)%\\([ACE]\\)") . "\\1% \\2 ra-vipulā incorrect: no caesura after 4th syllable")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" ancz sa anc "\\)%\\([ACE]\\)") . "\\1% \\2 sa-vipulā, incorrect?")

		       ((concat "\\(% |* ?\\(?:" anc "\\)\\{3\\}" anc sa anc "\\)%\\([ACE]\\)") . "\\1% \\2 sa-vipulā incorrect: no caesura after 4th syllable")

		       ((concat " %\\([ACE]\\)") . " % \\1 incorrect: neither pathyā nor vipulā")

		       ((concat "\\([^|] % [ACE] .*\\)") . "\\1, pādas compounded?")

		       ))

  )
