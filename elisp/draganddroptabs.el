(defface tabbar-drag-face `((t
			     :inherit tabbar-face))
  "Blank face for dragged tab.")

(defsubst tabbar-drag-p (event)
  "Return non-nil if EVENT is a mouse drag event."
  (memq 'drag (event-modifiers event)))

(defsubst tabbar-down-p (event)
  "Return non-nil if EVENT is a mouse down event."
  (memq 'down (event-modifiers event)))

(defun tabbar-make-mouse-keymap (callback)
  "Return a keymap that call CALLBACK on mouse events.
CALLBACK is passed the received mouse event."
  (let ((keymap (make-sparse-keymap)))
    ;; Pass mouse-1, mouse-2 and mouse-3 events to CALLBACK.
    ;; also pass down-mouse-1 events, so we can track motion
    ;; in case of dragging
    (define-key keymap [header-line down-mouse-1] callback)
    (define-key keymap [header-line mouse-1] callback)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-2] callback)
    (define-key keymap [header-line down-mouse-3] 'ignore)
    (define-key keymap [header-line mouse-3] callback)
    keymap))

(defconst tabbar-default-tab-keymap
  (tabbar-make-mouse-keymap 'tabbar-select-tab-callback)
  "Default keymap of a tab.")

(defun tabbar-select-tab-callback (event)
  "Handle a mouse EVENT on a tab. 
Pass mouse click events on a tab to `tabbar-click-on-tab'.
Pass mouse drag events on a tab to `tabbar-drag-tab'"
  (interactive "@e")
  (cond
   ((tabbar-click-p event)
    (let ((target (posn-string (event-start event))))
      (if (cdr target)
	  (tabbar-click-on-tab
	   (get-text-property (cdr target) 'tabbar-tab (car target))
	   event
	   (get-text-property (cdr target) 'tabbar-action (car target)))
	(print event))))
   ((tabbar-drag-p event)
    (let* ((start-target (posn-string (event-start event)))
	   (end-target (posn-string (event-end event)))
	   (end-posn (cdr-safe end-target)))
      (when end-posn
	(tabbar-drag-tab
       ;; need to handle the case where drag STARTS on tab bar, but ends elsewhere
       ;; (special case:  ends on different window or frame)
	 (get-text-property (cdr start-target) 'tabbar-tab (car start-target))
	 (get-text-property end-posn 'tabbar-tab (car end-target))
	 event))))
   ((tabbar-down-p event)
    (let* ((start-target (posn-string (event-start event)))
	   (start-tab (get-text-property (cdr start-target) 'tabbar-tab (car start-target)))
	   (first-iteration t)
	   new-event
	   is-motion-event)
      (track-mouse
	(while (progn (setq new-event (read-event))
		      (if (mouse-movement-p new-event)
			  (if first-iteration
			      (progn
				(setq first-iteration nil)
				(tabbar-line-during-drag start-tab))
			    t)
			(tabbar-line)
			(tabbar-set-template (tabbar-tab-tabset start-tab) nil)
			(tabbar-select-tab-callback new-event)
			nil
			))))))))

(defun tabbar-drag-tab (dragged-tab dropped-tab event)
  "Handle DRAGGED-TAB dragged-and-dropped onto DROPPED-TAB.
Include full mouse EVENT from drag-and-drop action."
  (let ((start-tabset (tabbar-tab-tabset dragged-tab)))
    (cond
     ;; dropped onto same tab (dragged nowhere!)
     ((eq dragged-tab dropped-tab)
      )
     ;; dropped onto some other tab
     ;; (should change this to a general handler for header-line drops,
     ;; with forking from there)
     ((eq start-tabset (tabbar-tab-tabset dropped-tab))
      (let* ((tabs (tabbar-tabs start-tabset))
	     (drop-tail-length (length (memq dropped-tab tabs)))
	     (drag-tail-length (length (memq dragged-tab tabs)))
	     (dragdrop-pair (list dragged-tab dropped-tab))
	     new-tablist)
	(when (> drag-tail-length drop-tail-length)
	  (setq dragdrop-pair (reverse dragdrop-pair)))
	(dolist (thistab (reverse tabs))
	  ;; build list of tabs.  When we hit dragged-tab, don't append it.
	  ;; When we hit dropped-tab, append dragdrop-pair
	  (cond
	   ((eq thistab dragged-tab)
	    )
	   ((eq thistab dropped-tab)
	    (setq new-tablist (append dragdrop-pair new-tablist)))
	   (t
	    (add-to-list 'new-tablist thistab))
	   ))
	(set start-tabset new-tablist)
	(tabbar-set-template start-tabset nil)
	(tabbar-display-update)))
     ;; other cases...
     )))

(defsubst tabbar-line-during-drag-tab (tab dragged-tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (let* ((close-button-image (tabbar-find-image tabbar-close-tab-button))
	 (close-button
	  (propertize "[x]"
		      'tabbar-tab tab
		      'local-map (tabbar-make-tab-keymap tab)
		      'tabbar-action 'close-tab
		      'face (if (tabbar-selected-p tab (tabbar-current-tabset))
				'tabbar-selected
			      'tabbar-unselected)
;; 		      'pointer 'hand
		      'display (tabbar-normalize-image close-button-image 0 'nomask)))
	 (the-face (cond ((and (tabbar-selected-p tab (tabbar-current-tabset))
				       (buffer-modified-p (tabbar-tab-value tab)))
				  'tabbar-selected-modified)
				 ((and (not (tabbar-selected-p tab (tabbar-current-tabset)))
				       (buffer-modified-p (tabbar-tab-value tab)))
				  'tabbar-unselected-modified)
				 ((and (tabbar-selected-p tab (tabbar-current-tabset))
				       (not (buffer-modified-p (tabbar-tab-value tab))))
				  'tabbar-selected)
				 (t 'tabbar-unselected)))
	 display-label)
    ;; if this tab is the dragged one, we need to tweak its face
    (when (eq tab dragged-tab)
      (set-face-attribute 'tabbar-drag-face nil
			  :inherit the-face
			  :foreground (face-attribute the-face :background nil t)))
    (setq display-label
	  (propertize (if tabbar-tab-label-function
			  (funcall tabbar-tab-label-function tab)
			tab)
		      'tabbar-tab tab
		      'local-map (tabbar-make-tab-keymap tab)
		      'face (if (eq tab dragged-tab)
				'tabbar-drag-face
				the-face)
;; 		      'pointer 'hand
		      ))
    (concat close-button display-label tabbar-separator-value)))

(defun tabbar-line-during-drag-format (tabset dragged-tab)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (tabbar-selected-tab tabset))
         (tabs (tabbar-view tabset))
         (padcolor (tabbar-background-color))
	 (noscroll t)
         atsel elts scrolled)
    ;; Initialize buttons and separator values.
    (or tabbar-separator-value
        (tabbar-line-separator))
    (or tabbar-home-button-value
        (tabbar-line-button 'home))
    (or tabbar-scroll-left-button-value
        (tabbar-line-button 'scroll-left))
    (or tabbar-scroll-right-button-value
        (tabbar-line-button 'scroll-right))
    ;; Make sure we're showing as many tabs as possible.
    ;; If we're not showing the 1st tab, and we're not overflowing the tab bar,
    ;;  then scroll backward.  If this leads to overflowing the tab bar, scroll
    ;;  forward 1 at the end.
    (while (and (> (get tabset 'start) 0)
		(not (tabbar-check-overflow tabset)))
      (tabbar-scroll tabset -1)
      (setq scrolled t))
    ;; if we scrolled until the tabbar overflowed, we went too far.  Back up 1 slot.
    (when (and scrolled (tabbar-check-overflow tabset))
      (tabbar-scroll tabset 1))
    (when (or (> (tabbar-start tabset) 0) (tabbar-check-overflow tabset))
      ;; not all tabs fit -- include scroll buttons
      (setq noscroll nil))
    ;; Track the selected tab to ensure it is always visible.
    (when tabbar--track-selected
      (while (not (memq sel tabs))
        (tabbar-scroll tabset -1)
        (setq tabs (tabbar-view tabset)))
      (while (and tabs (not atsel))
	(let ((thetab (car tabs)))
	  (setq elts  (cons (tabbar-line-during-drag-tab thetab dragged-tab) elts)
		atsel (eq (car tabs) sel)
		tabs  (cdr tabs))))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
        (let ((truncate-partial-width-windows nil)
              (inhibit-modification-hooks t)
              deactivate-mark ;; Prevent deactivation of the mark!
              start)
          (setq truncate-lines nil
                buffer-undo-list t)
          (apply 'insert (tabbar-line-buttons tabset noscroll))
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (tabbar-scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq tabbar--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (let ((thetab (car tabs)))
	  (setq elts  (cons (tabbar-line-during-drag-tab thetab dragged-tab) elts)
		tabs  (cdr tabs))))
    ;; Cache and return the new tab bar.
    (tabbar-set-template
     tabset
     (list (tabbar-line-buttons tabset noscroll)
           (nreverse elts)
           (propertize "%-"
                       'face (list :inherit 'tabbar-default
				   :background padcolor
                                   :foreground padcolor)
                       'pointer 'arrow
		       'local-map (tabbar-make-tab-keymap "empty tab bar"))))
    ))

(defun tabbar-line-during-drag (dragged-tab)
  "Return the header line templates that represent the tab bar.
Update the templates if tabbar-template is currently nil."
  (tabbar-current-tabset t)
  (tabbar-line-during-drag-format tabbar-current-tabset dragged-tab))