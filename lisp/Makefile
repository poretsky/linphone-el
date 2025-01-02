EMACS = emacs

all: linphone-loaddefs.el linphone.elc linphone-log.elc \
 linphone-control.elc linphone-conversation.elc linphone-display.elc \
 linphone-contacts.elc linphone-contacts-core.elc linphone-contacts-lib.elc

linphone-loaddefs.el: linphone-conversation.el linphone-control.el linphone-log.el linphone.el linphone-contacts.el linphone-display.el linphone-contacts-core.el
	$(EMACS) -batch -Q --eval \
		'(let ((make-backup-files nil) \
		       (generated-autoload-file (expand-file-name "linphone-loaddefs.el"))) \
		   (update-file-autoloads "linphone-conversation.el" t) \
		   (update-file-autoloads "linphone-control.el" t) \
		   (update-file-autoloads "linphone-log.el" t) \
		   (update-file-autoloads "linphone.el" t) \
		   (update-file-autoloads "linphone-contacts.el" t) \
		   (update-file-autoloads "linphone-display.el" t) \
		   (update-file-autoloads "linphone-contacts-core.el" t))'

clean:
	rm -f *.elc linphone-loaddefs.el

%.elc: %.el
	$(EMACS) -batch -Q -f batch-byte-compile $<
