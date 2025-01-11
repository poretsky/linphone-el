EMACS = emacs

all: linphone-loaddefs.el linphone.elc linphone-log.elc linphone-codecs.elc \
 linphone-control.elc linphone-conversation.elc linphone-display.elc \
 linphone-contacts.elc linphone-contacts-core.elc linphone-contacts-lib.elc

linphone-loaddefs.el: linphone.el linphone-display.el \
 linphone-control.el linphone-conversation.el linphone-log.el \
 linphone-contacts.el linphone-contacts-core.el linphone-codecs.el
	$(EMACS) -batch -Q --script make-loaddefs.el $@ $^

linphone.elc: linphone.el linphone-loaddefs.el
linphone-contacts-core.elc: linphone-contacts-core.el linphone.elc
linphone-contacts-lib.elc: linphone-contacts-lib.el linphone.elc
linphone-contacts.elc: linphone-contacts.el linphone-contacts-core.elc linphone-contacts-lib.elc linphone.elc
linphone-control.elc: linphone-control.el linphone-display.elc linphone.elc
linphone-conversation.elc: linphone-conversation.el linphone-control.elc linphone-display.elc linphone.elc
linphone-display.elc: linphone-display.el linphone.elc
linphone-log.elc: linphone-log.el linphone-contacts-lib.elc linphone.elc
linphone-codecs.elc: linphone-codecs.el linphone.elc

clean:
	rm -f *.elc linphone-loaddefs.el

%.elc: %.el
	$(EMACS) -l linphone-loaddefs.el --eval '(push "." load-path)' -batch -Q -f batch-byte-compile $<
