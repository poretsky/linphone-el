EMACS = emacs

all: linphone-loaddefs.el linphone.elc linphone-log.elc \
 linphone-control.elc linphone-conversation.elc linphone-display.elc \
 linphone-contacts.elc linphone-contacts-core.elc linphone-contacts-lib.elc

linphone-loaddefs.el: linphone.el linphone-display.el \
 linphone-control.el linphone-conversation.el linphone-log.el \
 linphone-contacts.el linphone-contacts-core.el
	$(EMACS) -batch -Q --script make-loaddefs.el $@ $^

clean:
	rm -f *.elc linphone-loaddefs.el

%.elc: %.el
	$(EMACS) -batch -Q -f batch-byte-compile $<
